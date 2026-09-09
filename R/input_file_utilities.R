# input_file_utils.R -------------------------------------------------------
#
# Utilities for reading and writing the ASCII "KEYWORD: arguments" input files
# that drive the NetStream Fortran programs.
#
# THE FILE FORMAT
# ---------------
# Each line of an input file has the form:
#
#     KEYWORD: PARAMETER = value, PARAMETER = value, FLAG
#
#   * Keyword lines may appear in any order; the Fortran programs look each
#     keyword up rather than reading positionally.
#   * A keyword takes zero or more arguments, separated by commas.
#   * An argument is either a "PARAMETER = value" pair or a bare value/flag.
#   * A line whose first non-blank character is "#" is a comment.
#
# Example:
#
#     REACH LENGTH: FIXED = 100., BREAK AT JUNCTIONS
#
#   "REACH LENGTH" is the keyword; there are two arguments, one of which
#   carries a value and one of which is a bare flag.
#
#
# HOW THIS FILE IS ORGANIZED
# --------------------------
#   Section 1  Path and value helpers
#              Small predicates and transformations used everywhere else:
#              testing for absent optional arguments, expanding paths, adding
#              and removing file extensions.
#
#   Section 2  Reading input files
#              get_input_file() loads a file; get_keyword() / get_args() /
#              parse_args() take it apart line by line.
#
#   Section 3  Raster format conversion
#              Converting rasters to the binary floating point (.flt) format
#              the Fortran programs read.
#
#   Section 4  The input-file writer engine
#              input_writer() is the single place that knows how to lay out a
#              keyword line. Everything in Section 5 goes through it.
#
#   Section 5  List blocks: attribute and raster lists
#
#   Section 6  One writer function per Fortran program
#              Each of these is a short declaration of which
#              keywords its program accepts. All directory validation, path
#              expansion, extension handling and text formatting happens in
#              Sections 1 and 4.

# =========================================================================
# SECTION 1.  PATH AND VALUE HELPERS
# =========================================================================

# Sentinel value the API uses to mean "this optional file argument was not
# supplied". Callers pass the string "nofile"; the writers below then omit the
# corresponding keyword line entirely.
NOFILE <- "nofile"

#' Test whether a file argument was actually supplied
#'
#' Optional file arguments arrive in several different shapes depending on how
#' the caller wrote them, and all of them need to mean the same thing: "skip
#' this keyword". This predicate collapses them into a single test.
#'
#' Treated as absent: NULL, a zero-length vector, NA, the empty string, a
#' string of only whitespace, and the sentinel words "nofile", "none" and "na"
#' in any combination of upper and lower case.
#'
#' @param path_argument The value passed for a file argument.
#' @return TRUE if the argument should be treated as "not supplied".
#' @noRd
is_missing_path <- function(path_argument) {

  # NULL and zero-length vectors carry no value at all.
  if (is.null(path_argument) || length(path_argument) == 0L) return(TRUE)

  # Work with the first element only; callers occasionally pass a length-one
  # list or factor rather than a bare character string.
  path_text <- as.character(path_argument[[1L]])

  # NA, "" and "   " all mean nothing was supplied.
  if (is.na(path_text) || !nzchar(trimws(path_text))) return(TRUE)

  # Finally, the explicit sentinel words.
  tolower(trimws(path_text)) %in% c("nofile", "none", "na")
}


#' Expand a file path to its full form
#'
#' Wraps normalizePath() with behaviour suited to writing input files.
#' normalizePath() warns (and with mustWork = TRUE, errors) when the file does
#' not exist, but *output* files legitimately do not exist yet at the moment
#' the input file is written. So the default here is not to require existence.
#'
#' Pass must_exist = TRUE for genuine input files; that produces a clear,
#' named error at the point of the mistake.
#'
#' @param file_path Path to expand, or the NOFILE sentinel.
#' @param must_exist If TRUE, stop with an error when the file is not found.
#' @param argument_label Name to use in that error message. Defaults to the
#'   caller's own variable name, so the message reads "dem not found: ...".
#' @return The expanded path, or NOFILE if no path was supplied.
#' @noRd
normalize_file_path <- function(file_path,
                                must_exist = FALSE,
                                extensions = NULL,
                                argument_label = deparse(substitute(file_path))) {

  # argument_label is a lazily evaluated default that calls substitute() on
  # file_path. It must be forced here, before file_path is reassigned below,
  # or substitute() would report the literal name "file_path" instead of the
  # caller's variable name.
  force(argument_label)

  # Absent optional arguments pass straight through as the sentinel.
  if (is_missing_path(file_path)) return(NOFILE)

  file_path <- as.character(file_path[[1L]])

  if (must_exist) require_path_exists(file_path, extensions, argument_label)

  # winslash = "\\" keeps separators consistent with the rest of the file;
  # mustWork = FALSE plus suppressWarnings allows not-yet-created outputs.
  suppressWarnings(normalizePath(file_path, winslash = "\\", mustWork = FALSE))
}

#' Raster file formats the NetStream programs read
#'
#' The Fortran programs are given a raster name *without* an extension and
#' resolve it themselves, trying each of these in turn. So the input file
#' should always carry the bare name, and a raster argument may arrive here
#' named with any of them, or with none.
#'
#' This vector drives three things at once: which extensions are stripped from
#' a name before it is written, which are accepted when confirming an input
#' raster exists, and which are listed when one cannot be found. Adding a
#' format here extends all three.
#' @noRd
RASTER_EXTENSIONS <- c("flt", "tif", "bil")


#' Require that a file exists, in either its bare or its extended form
#'
#' The NetStream programs refer to rasters and shapefiles *without* an
#' extension: the reference input files carry lines such as
#'
#'     DEM: c:\\work\\data\\postmortem\\elev_smooth
#'     INPUT ROAD SHAPEFILE: c:\\work\\data\\postmortem\\roads
#'
#' even though what sits on disk is `elev_smooth.flt` and `roads.shp`.
#' Both forms are accepted. A raster given as `dem.flt` is found
#' directly; one given as `dem` is found as `dem.flt`.
#'
#' @param file_path Path to test.
#' @param extension Extension the file may carry on disk, without the leading
#'   dot. NULL requires an exact match.
#' @param argument_label Name to use in the error message.
#' @return TRUE, invisibly, if the file was found under either name.
#' @noRd
require_path_exists <- function(file_path, extensions = NULL,
                                argument_label = "file") {

  candidate_paths <- file_path

  # Only try appending extensions when the path does not already carry one of
  # them. Appending unconditionally would test "dem.flt.tif", which cannot
  # exist and only clutters the error message below.
  if (length(extensions) > 0L) {
    already_extended <- any(vapply(
      extensions,
      function(extension) {
        grepl(paste0("\\.", extension, "$"), file_path, ignore.case = TRUE)
      },
      logical(1)
    ))
    if (!already_extended) {
      candidate_paths <- c(candidate_paths, paste0(file_path, ".", extensions))
    }
  }

  matches <- which(file.exists(candidate_paths))
  if (length(matches) > 0L) return(invisible(candidate_paths[matches[1]]))

  # Nothing matched under either name. If the directory holds files sharing
  # this base name, list them: the usual cause is a raster stored as .tif
  # where .flt was expected, and naming the alternative turns a dead end into
  # a one-word fix. list.files() returns character(0) rather than erroring on
  # a directory that does not exist, so this is safe either way.
  directory_files <- list.files(dirname(file_path))
  files_sharing_base_name <- directory_files[
    startsWith(directory_files, paste0(basename(file_path), "."))
  ]

  stop(argument_label, " not found. Looked for:\n  ",
       paste(candidate_paths, collapse = "\n  "),
       if (length(files_sharing_base_name) > 0L) {
         paste0("\n  The directory does hold: ",
                paste(files_sharing_base_name, collapse = ", "))
       } else {
         ""
       },
       call. = FALSE)
}


#' Remove a trailing file extension
#'
#' The Fortran programs expect raster names *without* the ".flt" suffix; they
#' append it themselves, along with the matching ".hdr".
#'
#' The regular expression escapes the dot and anchors on the end of the
#' string. The original code used str_detect(x, ".flt"), in which "." is a
#' regex wildcard and the pattern is unanchored, so a name such as
#' "myflt_survey.tif" tested positive and then had four characters chopped off
#' its end.
#'
#' @param file_path Path to trim, or the NOFILE sentinel.
#' @param extension Extension to remove, without the leading dot.
#' @return The path with the extension removed if it was present.
#' @noRd
remove_extension <- function(file_path, extension = "flt") {
  if (is_missing_path(file_path)) return(NOFILE)
  sub(paste0("\\.", extension, "$"), "", file_path, ignore.case = TRUE)
}


#' Append a file extension unless it is already present
#'
#' @param file_path Path to extend, or the NOFILE sentinel.
#' @param extension Extension to add, without the leading dot.
#' @return The path, guaranteed to end in the given extension.
#' @noRd


add_extension_if_missing <- function(file_path, extension = "flt") {
  if (is_missing_path(file_path)) return(NOFILE)
  already_present <- grepl(paste0("\\.", extension, "$"), file_path,
                           ignore.case = TRUE)
  ifelse(already_present, file_path, paste0(file_path, ".", extension))
}


#' Expand a raster path and strip its extension, in one step
#'
#' Expand to a full path, then drop the ".flt" the Fortran side
#' does not want to see.
#'
#' @param raster_path Raster file name, or the NOFILE sentinel.
#' @param must_exist If TRUE, stop with an error when the file is not found.
#' @param extension Extension to strip; "shp" for shapefiles.
#' @param argument_label Name to use in the not-found error message.
#' @return The expanded, extension-free path, or NOFILE.
#' @noRd
normalize_raster_path <- function(raster_path,
                                  must_exist = FALSE,
                                  extension = "flt",
                                  argument_label = deparse(substitute(raster_path))) {
  force(argument_label)
  # RASTER_EXTENSIONS is passed through so that must_exist accepts the same
  # extensionless-name-or-any-RASTER_EXTENSIONS-variant match that
  # check_input_file() in wrappers.R already applies to the same argument;
  # without this, a path that argument_checker() accepted could still fail
  # here.
  expanded_path <- normalize_file_path(raster_path,
                                       must_exist = must_exist,
                                       extensions = RASTER_EXTENSIONS,
                                       argument_label = argument_label)
  remove_extension(expanded_path, extension)
}


#' Join a directory and a file name using the platform separator
#'
#' file.path() always joins with "/", which produces mixed separators on
#' Windows ("C:\\scratch/input_align.txt"). That works, but it is confusing to
#' read in a log and inconsistent with the paths written into the file.
#'
#' @param directory Directory path; any trailing separator is dropped first.
#' @param file_name File name to append.
#' @return The joined path.
#' @noRd
join_path <- function(directory, file_name) {
  require_single_string(directory, "join_path(): directory")
  require_single_string(file_name, "join_path(): file_name")
  path_separator <- if (.Platform$OS.type == "windows") "\\" else "/"
  directory_without_trailing_slash <- sub("[\\\\/]+$", "", directory)
  paste0(directory_without_trailing_slash, path_separator, file_name)
}


#' Require a usable single character string
#'
#' Rejects NULL, NA, "", a zero-length vector and a vector of length greater
#' than one, along with non-character values such as the TRUE that arrives
#' when an argument is passed in the wrong position.
#'
#' @param value The value to check.
#' @param argument_label Name to use in the error message.
#' @return The value, invisibly, if it passes.
#' @noRd
require_single_string <- function(value, argument_label) {
  if (!is.character(value) || length(value) != 1L ||
      is.na(value) || !nzchar(trimws(value))) {
    stop(argument_label, " must be a single non-empty character string; got ",
         paste(deparse(value), collapse = " "), call. = FALSE)
  }
  invisible(value)
}


#' Standard input file name for each Fortran program
#'
#' Both the writers in Section 5 and any caller that has to hand the file to
#' the executable need to agree on this name. Keeping it in one table means
#' neither side can drift: surface_metrics.R previously built the HuntLS path
#' by hand as "input_huntLS.txt" while huntLSinput() wrote "input_huntls.txt",
#' which happens to work on Windows and fails on a case-sensitive file system.
#'
#' @noRd
INPUT_FILE_NAMES <- c(
  "makeGrids"          = "makegrids_input.txt",
  "partial"            = "partial_input.txt",
  "align"              = "input_align.txt",
  "HuntLS"             = "input_huntls.txt",
  "LShunter"           = "input_LShunter.txt",
  "LS_poly"            = "input_LS_poly.txt",
  "samplePoints"       = "input_samplePoints.txt",
  "modelDensity"       = "input_modelDensity.txt",
  "quantiles"          = "input_quantiles.txt",
  "distanceToRoad"     = "input_distanceToRoad.txt",
  "bldgrds_nochannels" = "input_bldgrds_nochannels.txt",
  "bldgrds"            = "input_bldgrds.txt",
  "bldgrds_enforce"    = "input_bldgrds_enforce.txt",
  "PFA_debris_flow"    = "input_PFA_debris_flow.txt",
  "LocalRelief"        = "input_DEV.txt",
  "resample"           = "input_resample.txt",
  "RIL"                = "input_RIL.txt",
  "netrace"            = "input_netrace.txt",
  "ValleyFloor"        = "input_valleyfloor.txt"
)


#' Full path of the input file a given program reads
#'
#' Call this to find out where a writer put its file, rather than rebuilding
#' the path by hand at the call site.
#'
#' @param program Program name, as passed to [input_writer()].
#' @param scratch_dir Scratch directory holding the input file.
#' @return The full path of that program's input file.
#' @export
input_file_for <- function(program, scratch_dir) {

  require_single_string(program, "input_file_for(): program")

  # `INPUT_FILE_NAMES[[program]]` fails with a bare "subscript out of bounds"
  # for an unlisted name, which says nothing about how to fix it. Name the
  # alternatives instead, since the usual cause is a new program whose entry
  # has not been added to the table yet.
  if (!program %in% names(INPUT_FILE_NAMES)) {
    stop("no input file name registered for program \"", program, "\".",
         "\n  Known programs: ",
         paste(names(INPUT_FILE_NAMES), collapse = ", "),
         "\n  Either add an entry to INPUT_FILE_NAMES, or pass an explicit",
         " file_name = to input_writer().",
         call. = FALSE)
  }

  require_single_string(scratch_dir, "input_file_for(): scratch_dir")
  join_path(normalize_file_path(scratch_dir), INPUT_FILE_NAMES[[program]])
}

#' Require that an argument is numeric
#'
#' @param value The value to check.
#' @param argument_label Name to use in the error message; defaults to the
#'   caller's variable name.
#' @return The value, invisibly, if it passes.
#' @noRd
require_numeric <- function(value, argument_label = deparse(substitute(value))) {
  force(argument_label)
  if (!is.numeric(value)) {
    stop(argument_label, " must be numeric", call. = FALSE)
  }
  invisible(value)
}


#' Read one field out of a raster specification, tolerating short ones
#'
#' samplePointInput() and modelDensity_input() take lists of raster
#' specifications in which the trailing fields are optional flags.
#'
#' @param raster_spec One element of an R4rasters or I4rasters list.
#' @param field_number Which field to read, counting from 1.
#' @param default Value to return when the field is absent or NA.
#' @return The field as a character string, or the default.
#' @noRd
spec_field <- function(raster_spec, field_number, default = "") {
  field_is_absent <- length(raster_spec) < field_number ||
                     is.na(raster_spec[[field_number]])
  if (field_is_absent) default else as.character(raster_spec[[field_number]])
}


#' Test whether a raster specification field carries a particular flag
#'
#' @param raster_spec One element of an R4rasters or I4rasters list.
#' @param field_number Which field to test.
#' @param flag_name Flag to look for, e.g. "MASK" or "SELECT".
#' @return TRUE if that field holds the flag, ignoring case and whitespace.
#' @noRd
spec_has_flag <- function(raster_spec, field_number, flag_name) {
  field_text <- spec_field(raster_spec, field_number)
  identical(toupper(trimws(field_text)), flag_name)
}


# =========================================================================
# SECTION 2.  READING INPUT FILES
# =========================================================================

#' Load an ASCII input file
#'
#' Reads a NetStream "KEYWORD: arguments" input file into memory so that
#' get_keyword(), get_args() and parse_args() can pick it apart.
#'
#' @param infile File name with full path. If omitted, a file-selection window
#'   opens so the file can be chosen interactively.
#'
#' @return A tibble with one row per line of the input file, in a column named
#'   `line`.
#'
#' @examples
#' \dontrun{
#' get_input_file("c:\\\\data\\\\Umpqua\\\\input_bldgrds.txt")
#' get_input_file()   # interactive file selection
#' }
#'
#' @seealso [get_keyword()], [get_args()], [parse_args()]
#' @importFrom tibble tibble
#' @export
get_input_file <- function(infile = NOFILE) {

  if (is_missing_path(infile)) {
    # No file named, so ask for one. The original test was
    # str_detect(infile, "nofile"), which is an unanchored substring match and
    # so also fired on a perfectly real path like "c:/runs/nofile_test/in.txt".
    infile <- file.choose()
  } else if (!file.exists(infile)) {
    stop("Input file not found: ", infile, call. = FALSE)
  }

  # warn = FALSE suppresses the "incomplete final line" note, which is
  # harmless in these files.
  tibble::tibble(line = readLines(infile, warn = FALSE))
}


#' Pull the text of one line out of an input-file object
#'
#' Accepts either a tibble produced by get_input_file() or a plain character
#' vector, so none of the accessors below have to care which they were handed.
#'
#' @param input_file A tibble from get_input_file(), or a character vector.
#' @param line_number Which line to read.
#' @return The line as a character string, or NA_character_ if out of range.
#' @noRd
get_line_text <- function(input_file, line_number) {
  line_text <- if (is.data.frame(input_file)) {
    input_file[[1L]][line_number]
  } else {
    input_file[line_number]
  }
  if (length(line_text) == 0L) NA_character_ else as.character(line_text)
}


#' Count the lines in an input-file object
#'
#' @param input_file A tibble from get_input_file(), or a character vector.
#' @return The number of lines.
#' @noRd
count_input_lines <- function(input_file) {
  if (is.data.frame(input_file)) nrow(input_file) else length(input_file)
}


#' Get the keyword from an input-file line
#'
#' The keyword is everything before the first colon on a line that is not a
#' comment.
#'
#' @param infile A tibble created by [get_input_file()], or a character vector.
#' @param line_num Line number in the input file.
#'
#' @return The keyword, trimmed of surrounding whitespace; `NA_character_` if
#'   the line is a comment, is blank, or contains no colon.
#'
#' @export
get_keyword <- function(infile, line_num) {

  line_text <- get_line_text(infile, line_num)

  # Three ways a line can fail to hold a keyword.
  line_is_out_of_range <- is.na(line_text)
  # A "#" only opens a comment when it is the first non-blank character. The
  # original returned NA whenever a "#" appeared anywhere on the line, so a
  # trailing comment on a real keyword line silently hid that keyword.
  line_is_comment <- !line_is_out_of_range && grepl("^\\s*#", line_text)
  line_has_no_colon <- !line_is_out_of_range &&
                       !grepl(":", line_text, fixed = TRUE)

  if (line_is_out_of_range || line_is_comment || line_has_no_colon) {
    return(NA_character_)
  }

  # Everything up to, but not including, the first colon.
  trimws(sub(":.*$", "", line_text))
}


#' Get the argument string from an input-file line
#'
#' The arguments are everything after the first colon; splitting them into
#' individual "parameter = value" pairs is [parse_args()]'s job.
#'
#' @param infile A tibble created by [get_input_file()], or a character vector.
#' @param line_num Line number in the input file.
#'
#' @return Everything after the first colon; `NA_character_` if the line holds
#'   no colon or does not exist.
#'
#' @export
get_args <- function(infile, line_num) {
  line_text <- get_line_text(infile, line_num)
  if (is.na(line_text) || !grepl(":", line_text, fixed = TRUE)) {
    return(NA_character_)
  }
  sub("^[^:]*:", "", line_text)
}


#' Parse every "parameter = value" pair in an argument string
#'
#' Splits an argument string on commas and then splits each argument on its
#' first equals sign. Arguments with no equals sign are bare flags or values;
#' they get an empty `Parameter` and carry their whole text in `Value`.
#'
#' @param arguments A character string of arguments, as returned by [get_args()].
#'
#' @return A tibble with columns `Parameter` and `Value`, one row per
#'   comma-separated argument. Both columns are trimmed of surrounding
#'   whitespace, which the original per-argument parser did not do.
#'
#' @importFrom tibble tibble
#' @export
parse_args <- function(arguments) {

  # An empty or missing argument string yields an empty result rather than an
  # error, so callers can loop over it unconditionally.
  if (is.na(arguments) || !nzchar(trimws(arguments))) {
    return(tibble::tibble(Parameter = character(), Value = character()))
  }

  # Step 1: split the line into individual arguments on commas.
  argument_strings <- trimws(strsplit(arguments, ",", fixed = TRUE)[[1L]])

  # Step 2: an argument is a parameter/value pair only if it holds "=".
  has_equals_sign <- grepl("=", argument_strings, fixed = TRUE)

  # Step 3: split the pairs; leave bare flags in the Value column.
  tibble::tibble(
    Parameter = ifelse(has_equals_sign,
                       trimws(sub("=.*$", "", argument_strings)),
                       ""),
    Value     = ifelse(has_equals_sign,
                       trimws(sub("^[^=]*=", "", argument_strings)),
                       argument_strings)
  )
}


#' Parse a single argument from an argument string
#'
#' Retained for backwards compatibility with existing call sites;
#' [parse_args()] handles a whole line at once and is usually what you want.
#'
#' @param arguments A character string of arguments, as returned by [get_args()].
#' @param i Which comma-separated argument to parse, counting from 1.
#'
#' @return A 2-element list with elements `Parameter` and `Value`. If the
#'   argument holds no equals sign, `Parameter` is empty and the whole
#'   argument is returned in `Value`.
#'
#' @export
parse_arg <- function(arguments, i = 1) {
  parsed_arguments <- parse_args(arguments)
  if (nrow(parsed_arguments) < i) {
    return(list(Parameter = "", Value = NA_character_))
  }
  list(Parameter = parsed_arguments$Parameter[[i]],
       Value     = parsed_arguments$Value[[i]])
}


#' Get every value recorded against a matching keyword in an input file
#'
#' Scans the whole file once, keeps the lines whose keyword matches, and
#' returns the first argument value from each.
#'
#' @param infile A tibble created by [get_input_file()].
#' @param pattern Regular expression matched against each line's keyword.
#'
#' @return A character vector, one element per matching line.
#'
#' @export
get_keyword_values <- function(infile, pattern) {

  n_lines <- count_input_lines(infile)
  if (n_lines == 0L) stop("Input file is empty", call. = FALSE)

  # Step 1: get the keyword on every line. Comment and blank lines come back
  # as NA.
  line_keywords <- vapply(seq_len(n_lines),
                          function(line_number) get_keyword(infile, line_number),
                          character(1))

  # Step 2: find the lines whose keyword matches the pattern.
  matching_line_numbers <- which(!is.na(line_keywords) &
                                 grepl(pattern, line_keywords))

  # Step 3: take the first argument value from each of those lines.
  vapply(matching_line_numbers,
         function(line_number) parse_arg(get_args(infile, line_number))$Value,
         character(1))
}


#' Get the DEM raster files listed in an ASCII input file
#'
#' @param infile A tibble created by [get_input_file()].
#' @return A list of DEM file names, one per DEM keyword in the file.
#' @export
get_dem <- function(infile) {
  # The original made two passes over the file, the first purely to count
  # matches so a list could be preallocated, plus a hand-maintained counter in
  # each pass. get_keyword_values() does it in one pass.
  as.list(get_keyword_values(infile, "DEM"))
}


# =========================================================================
# SECTION 3.  RASTER FORMAT CONVERSION
# =========================================================================

#' Convert a raster to binary floating point (.flt), if it is not one already
#'
#' The Fortran programs read binary floating point rasters with an
#' accompanying header. terra writes the raster in GDAL's BIL flavour, so the
#' header it produces then has to be rewritten by convert_hdr().
#'
#' @param in_raster Raster file name, with or without an extension.
#' @return The .flt file name, invisibly. No conversion is done, and no error
#'   raised, if the .flt file already exists.
#' @export
convert_to_flt <- function(in_raster) {

  # Step 1: work out the .flt name by replacing whatever extension is there.
  raster_name_without_extension <- remove_extension(in_raster, "\\w+")
  flt_file_name <- add_extension_if_missing(raster_name_without_extension, "flt")

  # Step 2: nothing to do if the input already is that file, or if a previous
  # run produced it.
  input_is_already_flt <- identical(
    normalizePath(in_raster,    mustWork = FALSE),
    normalizePath(flt_file_name, mustWork = FALSE)
  )
  if (input_is_already_flt || file.exists(flt_file_name)) {
    return(invisible(flt_file_name))
  }

  # Step 3: convert, then fix up the header.
  terra::writeRaster(terra::rast(in_raster), flt_file_name)
  convert_hdr(paste0(flt_file_name, ".hdr"))

  invisible(flt_file_name)
}


#' Remove a trailing ".flt" from a raster name
#'
#' @param in_raster Raster file name.
#' @return The name without its ".flt" extension.
#' @export
strip_flt <- function(in_raster) remove_extension(in_raster, "flt")


# =========================================================================
# SECTION 4.  THE INPUT-FILE WRITER ENGINE
# =========================================================================

#' Open an input file and return a set of functions for writing to it
#'
#' This is the one place in the file that knows how a keyword line is laid
#' out. Every writer in Section 5 opens a file with this function and then
#' does nothing but name keywords and hand over values.
#'
#' Input_writer checks that the scratch directory exists,
#' builds the output path, defines a byte-for-byte identical `write_input()`
#' closure, and emits an identical three-line header comment.
#'
#' @details
#' The returned list holds three writer functions and the output path:
#'
#' \describe{
#'   \item{`keyword(name, ...)`}{Writes one keyword line. Arguments in `...`
#'     that are named are emitted as `NAME = value`; unnamed arguments are
#'     emitted as bare values. All of them are joined with ", ". Called with
#'     no arguments at all, it writes a bare `NAME:` flag line. A `.indent`
#'     argument adds leading spaces, for the nested blocks that a few programs
#'     use.}
#'   \item{`optional(name, value, ...)`}{The same, but writes nothing at all
#'     when `value` is absent, as judged by `is_missing_path()`. Returns TRUE
#'     if a line was written. Because R evaluates arguments lazily, the
#'     remaining `...` arguments are never evaluated when the line is skipped;
#'     that is what lets a call like
#'     `optional("ROAD SHAPEFILE", roads, BUFFER = road_buffer)` be safe even
#'     when `road_buffer` was never supplied.}
#'   \item{`line(...)`}{Writes an arbitrary line, pasted together with no
#'     separator. The escape hatch for the handful of genuinely irregular
#'     blocks.}
#'   \item{`file_path`}{The full path of the file being written.}
#' }
#'
#' @param program Program name, written into the header comment.
#' @param scratch_dir Scratch directory. The input file is written here, and
#'   the directory must already exist.
#' @param file_name Base name of the input file.
#' @param overwrite If FALSE, stop rather than overwrite an existing input file.
#' @param note Optional extra comment line for the header.
#'
#' @return A list of writer functions; see Details.
#' @export
input_writer <- function(program,
                         scratch_dir,
                         file_name,
                         overwrite = TRUE,
                         note = NULL) {

  # --- Validate the destination ------------------------------------------
  if (is_missing_path(scratch_dir) || !dir.exists(scratch_dir)) {
    stop("invalid scratch folder: ", scratch_dir, call. = FALSE)
  }
  scratch_dir     <- normalize_file_path(scratch_dir)

  file_name_supplied <- !missing(file_name) &&
    !is.null(file_name) &&
    !is_missing_path(file_name)

  # The file name normally comes from the shared INPUT_FILE_NAMES table, so
  # that callers can locate the file afterwards with input_file_for().
  input_file_path <- if (file_name_supplied) {
    join_path(scratch_dir, file_name)
  } else {
    input_file_for(program, scratch_dir)
  }


  # --- Decide what to do about an existing file --------------------------
  if (file.exists(input_file_path)) {
    if (isTRUE(overwrite)) {
      message("overwriting ", input_file_path)
    } else {
      stop(input_file_path, " exists. Set overwrite = TRUE to overwrite.",
           call. = FALSE)
    }
  }

  # --- The low-level line writer -----------------------------------------
  # Everything below funnels through here. append = FALSE truncates the file,
  # which is how the first header line starts a fresh file.
  write_text_line <- function(..., append = TRUE) {
    cat(..., "\n", file = input_file_path, sep = "", append = append)
  }

  # --- Header comment ----------------------------------------------------
  write_text_line("# Input file for ", program, append = FALSE)
  if (!is.null(note)) write_text_line("#   ", note)
  write_text_line("# Created by input_file_utilities.R")
  write_text_line("# On ", format(Sys.time()))
  write_text_line("")

  # --- Argument formatting -----------------------------------------------
  # Turns the "..." of a keyword call into the comma-separated argument text
  # that follows the colon. Named arguments become "NAME = value"; unnamed
  # arguments are written as bare values.
  format_arguments <- function(arguments) {

    # Drop NULLs so that an optional group of coefficients can be passed as
    # NULL without leaving a dangling comma.
    arguments <- arguments[!vapply(arguments, is.null, logical(1))]
    if (length(arguments) == 0L) return("")

    # list() returns NULL for names when nothing was named at all.
    argument_names <- names(arguments)
    if (is.null(argument_names)) argument_names <- rep("", length(arguments))

    formatted_arguments <- vapply(seq_along(arguments), function(index) {
      # A single argument may itself be a vector, e.g. a run of regression
      # coefficients; those are flattened into the same comma-separated list.
      value_text <- paste0(as.character(arguments[[index]]), collapse = ", ")
      if (nzchar(argument_names[index])) {
        paste0(argument_names[index], " = ", value_text)
      } else {
        value_text
      }
    }, character(1))

    paste0(formatted_arguments, collapse = ", ")
  }

  # --- Write one keyword line --------------------------------------------
  write_keyword_line <- function(keyword, ..., .indent = 0L) {
    argument_text <- format_arguments(list(...))
    write_text_line(
      strrep(" ", .indent),
      keyword,
      ":",
      # A keyword with no arguments is written as a bare flag line, with no
      # trailing space after the colon.
      if (nzchar(argument_text)) paste0(" ", argument_text) else ""
    )
    invisible(TRUE)
  }

  # --- Write one keyword line, or nothing if its value is absent ----------
  write_optional_keyword_line <- function(keyword, value, ..., .indent = 0L) {
    if (is_missing_path(value)) return(invisible(FALSE))
    write_keyword_line(keyword, value, ..., .indent = .indent)
  }

  list(
    keyword   = write_keyword_line,
    optional  = write_optional_keyword_line,
    line      = write_text_line,
    file_path = input_file_path
  )
}


# =========================================================================
# SECTION 5.  LIST BLOCKS: ATTRIBUTE LIST AND RASTER LIST
# =========================================================================
#
# Many programs take nested list blocks in their input
# files. All use the same ATTRIBUTE LIST grammar. Nothing in either block is program-specific,
#  so all of it lives here.
#
#
# THE ATTRIBUTE LIST GRAMMAR
# --------------------------
#     ATTRIBUTE LIST:
#       NAME: ARG = value, ARG = value, FLAG
#       NAME: OUTPUT FIELD = field, EQUATION, REPLACE
#            TERM: FIELD = f1, COEF = c, EXPONENT = e1, FIELD = f2, EXPONENT = e2
#       END EQUATION:
#     END LIST:
#
# Each entry names one attribute to compute for every channel node. An entry carrying
# an EQUATION flag is followed by one or more TERM lines and an END EQUATION
# line; a term is a product of powers of fields computed earlier in the list,
# scaled by a coefficient.
#
# Blocks are terminated with an END LIST: keyword. Attributes also take program-specific arguments -- e.g., netrace's
# GRADIENT entry takes CHANNEL WIDTHS -- so
# attribute_spec() accepts arbitrary named arguments through "...".
#
#
# THE RASTER LIST GRAMMAR
# -----------------------
#     RASTER LIST:
#       FILE: path, STATISTIC, OUTPUT FIELD NAME = field, FLAG, FLAG
#     END LIST:
#
# Each entry names a raster to sample along the channel network, the statistic
# to reduce it with, and the output field to write the result into.
#
#
# READING AS WELL AS WRITING
# --------------------------
# get_attribute_list() and get_raster_list() parse these blocks out of an
# existing input file and return exactly the objects the writers consume, so a
# working configuration can be read in, adjusted in R, and written back:
#
#     lines <- get_input_file("input_netrace.txt")
#     attrs <- get_attribute_list(lines)
#     attrs <- c(attrs, list(attribute_spec("SIDE SLOPE",
#                                           output_field = "SIDESLOPE")))
#     netrace_input(dem, scratch_dir, out_nodes, attribute_list = attrs)


# ---- Constructors -------------------------------------------------------

#' Describe one term of an attribute equation
#'
#' An equation attribute is a product of powers of other attribute fields,
#' scaled by a coefficient:
#'
#'     value = coef * FIELD1 ^ exponent1 * FIELD2 ^ exponent2 * ...
#'
#' written as
#'
#'     TERM: FIELD = f1, COEF = c, EXPONENT = e1, FIELD = f2, EXPONENT = e2
#'
#' The coefficient appears once, after the first field.
#'
#' @param coef Numeric: the leading coefficient. Omit for a coefficient of 1.
#' @param ... Named `FIELD = exponent` pairs, in the order they are to be
#'   written. Field names are the OUTPUT FIELD names of attributes defined
#'   earlier in the list, so order matters within the list as a whole: an
#'   equation can only refer to a field already computed.
#'
#' @return A term object for [attribute_spec()].
#'
#' @examples
#' \dontrun{
#' # Mean annual flow after Lorenson et al. (1994):
#' #   0.014545408 * AREA_SQKM^0.99 * MNANPRC_M^1.593
#' equation_term(0.014545408, AREA_SQKM = 0.99, MNANPRC_M = 1.593)
#' }
#' @export
equation_term <- function(coef = NULL, ...) {
  exponents <- list(...)
  names_given <- names(exponents)
  if (length(exponents) == 0L || is.null(names_given) ||
      any(!nzchar(names_given))) {
    stop("equation_term() needs at least one named FIELD = exponent argument",
         call. = FALSE)
  }
  list(coef = coef, exponents = exponents)
}


#' Describe one entry in an attribute list
#'
#' Becomes one line inside the ATTRIBUTE LIST block. Entries with an equation
#' are followed by their TERM lines and an END EQUATION line; the EQUATION
#' flag is added automatically whenever `terms` is non-empty, so it cannot be
#' left off by accident.
#'
#' @param name Attribute name exactly as the program knows it, e.g. "NODE ID",
#'   "CONTRIBUTING AREA", "MEAN ANNUAL PRECIP".
#' @param ... Any further arguments the attribute takes, which vary by program
#'   and by attribute. Named arguments are written as `NAME = value`; unnamed
#'   arguments are written as bare flags. netrace's gradient attribute, for
#'   instance, is `attribute_spec("GRADIENT", \code{`CHANNEL WIDTHS`} = 20,
#'   output_field = "GRAD20CW")`. Because these come before the named formals
#'   below, those formals must be given by their full names.
#' @param output_field Field name to write into the output node shapefile.
#'   Omit to let the program choose.
#' @param file Raster to sample the attribute from, for attributes read off a
#'   grid such as mean annual precipitation.
#' @param units Units of that raster, e.g. "mm".
#' @param type Storage type in the output shapefile, e.g. "I4".
#' @param field_length Field width in the output shapefile, written as the
#'   LENGTH argument.
#' @param replace If TRUE, overwrite an existing field of the same name.
#' @param terms List of [equation_term()] objects. Supplying any makes this an
#'   equation attribute.
#'
#' @return An attribute object for [write_attribute_list()].
#' @export
attribute_spec <- function(name,
                           ...,
                           output_field = NULL,
                           file = NOFILE,
                           units = NULL,
                           type = NULL,
                           field_length = NULL,
                           replace = FALSE,
                           terms = list()) {

  # A single bare term rather than a list of terms is a common slip; accept it
  # here rather than failing obscurely at write time.
  if (length(terms) > 0L && !is.null(terms$exponents)) terms <- list(terms)

  list(name = name,
       extra = list(...),
       output_field = output_field,
       file = file,
       units = units,
       type = type,
       field_length = field_length,
       replace = replace,
       terms = terms)
}


#' Describe one entry in a raster list
#'
#' Becomes one FILE line inside the RASTER LIST block: a raster to sample along
#' the channel network, the statistic used to reduce it, and the field the
#' result is written into.
#'
#' @param file Raster to sample (full path).
#' @param statistic Bare-flag statistic, e.g. "MEAN". A character vector writes
#'   several.
#' @param ... Any further named arguments the program accepts, written as
#'   `NAME = value`.
#' @param output_field_name Field name for the result, written as the
#'   OUTPUT FIELD NAME argument.
#' @param flags Character vector of further bare flags, e.g. "DO NODES".
#'
#' @return A raster-list object for [write_raster_list()].
#'
#' @examples
#' \dontrun{
#' raster_list_entry("d:/data/pfa/nehalem/grad1_30",
#'                   statistic = "MEAN",
#'                   output_field_name = "meanGrad",
#'                   flags = "DO NODES")
#' }
#' @export
raster_list_entry <- function(file,
                              statistic = "MEAN",
                              ...,
                              output_field_name = NULL,
                              flags = character(0)) {
  list(file = file,
       statistic = statistic,
       extra = list(...),
       output_field_name = output_field_name,
       flags = flags)
}


# ---- Writers ------------------------------------------------------------

#' Write one TERM line of an attribute equation
#'
#' @param writer A writer list from [input_writer()].
#' @param term A term from [equation_term()].
#' @param indent Leading spaces.
#' @return TRUE, invisibly.
#' @noRd
write_equation_term <- function(writer, term, indent = 6L) {

  arguments <- list()
  field_names <- names(term$exponents)

  for (index in seq_along(field_names)) {

    # FIELD and EXPONENT repeat, so the argument list is built with duplicate
    # names. A list tolerates that; a named vector would not.
    field_argument <- list(field_names[index])
    names(field_argument) <- "FIELD"
    arguments <- c(arguments, field_argument)

    # The coefficient is written once, after the first field.
    if (index == 1L && !is.null(term$coef)) {
      arguments <- c(arguments, list(COEF = term$coef))
    }

    exponent_argument <- list(term$exponents[[index]])
    names(exponent_argument) <- "EXPONENT"
    arguments <- c(arguments, exponent_argument)
  }

  do.call(writer$keyword, c(list("TERM"), arguments, list(.indent = indent)))
}


#' Write one attribute entry, and its equation block if it has one
#'
#' Argument order follows the reference input files: FILE first, then any
#' program-specific arguments, then OUTPUT FIELD, UNITS, TYPE and LENGTH, then
#' the EQUATION and REPLACE flags.
#'
#' @param writer A writer list from [input_writer()].
#' @param attribute An attribute from [attribute_spec()].
#' @param indent Leading spaces for the attribute line.
#' @return TRUE, invisibly.
#' @noRd
write_attribute_entry <- function(writer, attribute, indent = 4L) {

  arguments <- list()

  if (!is_missing_path(attribute$file)) {
    arguments <- c(arguments,
                   list(FILE = normalize_raster_path(attribute$file)))
  }

  # Program-specific arguments, e.g. netrace's CHANNEL WIDTHS = 20.
  if (length(attribute$extra) > 0L) {
    arguments <- c(arguments, attribute$extra)
  }

  if (!is.null(attribute$output_field)) {
    arguments <- c(arguments, list(`OUTPUT FIELD` = attribute$output_field))
  }
  if (!is.null(attribute$units)) {
    arguments <- c(arguments, list(UNITS = attribute$units))
  }
  if (!is.null(attribute$type)) {
    arguments <- c(arguments, list(TYPE = attribute$type))
  }
  if (!is.null(attribute$field_length)) {
    arguments <- c(arguments, list(LENGTH = attribute$field_length))
  }

  # EQUATION and REPLACE are bare flags, so they go in unnamed.
  has_equation <- length(attribute$terms) > 0L
  if (has_equation) arguments <- c(arguments, list("EQUATION"))
  if (isTRUE(attribute$replace)) arguments <- c(arguments, list("REPLACE"))

  do.call(writer$keyword,
          c(list(attribute$name), arguments, list(.indent = indent)))

  if (has_equation) {
    for (term in attribute$terms) {
      write_equation_term(writer, term, indent = indent + 2L)
    }
    writer$keyword("END EQUATION", .indent = indent)
  }

  invisible(TRUE)
}


#' Check that every equation attribute's FIELDs are already defined
#'
#' An equation attribute's [equation_term()]s reference other attributes by
#' their OUTPUT FIELD name (falling back to the bare attribute `name` when no
#' `output_field` was given, since that is the field name the Fortran side
#' falls back to as well) -- and, per [equation_term()], those fields "must
#' be already computed", i.e. written by an attribute earlier in the same
#' list. Silently writing a FIELD that is not yet defined -- a typo, or an
#' attribute listed out of order -- produces a file the Fortran side either
#' errors on or (as with a misspelled EXPONENT, mentioned elsewhere) silently
#' misparses, rather than failing at the point the mistake was made. This
#' collects every such problem and raises one error naming all of them,
#' rather than letting the first one through to a run-time failure.
#'
#' @param attribute_list List of [attribute_spec()] objects, in write order.
#' @return TRUE, invisibly. Stops naming every FIELD referenced before (or
#'   without ever) being defined.
#' @noRd
check_attribute_equation_fields <- function(attribute_list) {

  problems <- character(0)
  defined <- character(0)

  for (attribute in attribute_list) {
    for (term in attribute$terms) {
      missing_fields <- setdiff(names(term$exponents), defined)
      for (field in missing_fields) {
        problems <- c(problems, sprintf(
          "attribute '%s' equation references FIELD %s, which is not defined by an earlier attribute in the list",
          attribute$name, field))
      }
    }
    field_name <- if (!is.null(attribute$output_field)) attribute$output_field else attribute$name
    defined <- c(defined, field_name)
  }

  if (length(problems) > 0L) {
    stop("problem with attribute list:\n  ",
         paste(problems, collapse = "\n  "), call. = FALSE)
  }
  invisible(TRUE)
}


#' Write a whole ATTRIBUTE LIST block
#'
#' @param writer A writer list from [input_writer()].
#' @param attribute_list List of [attribute_spec()] objects.
#' @param indent Leading spaces for the ATTRIBUTE LIST and end keywords.
#' @param end_keyword Keyword closing the block. RIL uses "END LIST"; netrace
#'   uses "END ATTRIBUTE LIST".
#'
#' @return The number of attributes written, invisibly.
#' @export
write_attribute_list <- function(writer,
                                 attribute_list,
                                 indent = 2L,
                                 end_keyword = "END LIST") {
  if (length(attribute_list) == 0L) return(invisible(0L))

  check_attribute_equation_fields(attribute_list)

  writer$keyword("ATTRIBUTE LIST", .indent = indent)
  for (attribute in attribute_list) {
    write_attribute_entry(writer, attribute, indent = indent + 2L)
  }
  writer$keyword(end_keyword, .indent = indent)

  invisible(length(attribute_list))
}


#' Write a whole RASTER LIST block
#'
#' @param writer A writer list from [input_writer()].
#' @param raster_list List of [raster_list_entry()] objects.
#' @param indent Leading spaces for the RASTER LIST and end keywords.
#' @param end_keyword Keyword closing the block.
#'
#' @return The number of entries written, invisibly.
#' @export
write_raster_list <- function(writer,
                              raster_list,
                              indent = 0L,
                              end_keyword = "END LIST") {
  if (length(raster_list) == 0L) return(invisible(0L))

  writer$keyword("RASTER LIST", .indent = indent)

  for (entry in raster_list) {
    # The raster path is the one unnamed leading argument; the statistic and
    # any further flags are bare too.
    arguments <- list(normalize_raster_path(entry$file, must_exist = TRUE))
    for (statistic in entry$statistic) arguments <- c(arguments, list(statistic))
    if (length(entry$extra) > 0L) arguments <- c(arguments, entry$extra)
    if (!is.null(entry$output_field_name)) {
      arguments <- c(arguments,
                     list(`OUTPUT FIELD NAME` = entry$output_field_name))
    }
    for (flag in entry$flags) arguments <- c(arguments, list(flag))

    do.call(writer$keyword,
            c(list("FILE"), arguments, list(.indent = indent + 2L)))
  }

  writer$keyword(end_keyword, .indent = indent)
  invisible(length(raster_list))
}


#' Write one keyword line whose arguments come from a named vector
#'
#' Several RIL_input() parameter groups (e.g. `closest_node`, `hollow_gradient`)
#' arrive as a single named numeric vector, one element per "NAME = value"
#' argument on the keyword line, in the order given. This turns that vector
#' into the argument list [input_writer()]'s `keyword()` expects, the same way
#' `write_equation_term()` does for a TERM line's FIELD/EXPONENT pairs.
#'
#' @param writer A writer list from [input_writer()].
#' @param keyword Keyword to write.
#' @param values A named numeric (or character) vector; each element becomes
#'   one `NAME = value` argument.
#' @param indent Leading spaces.
#' @return TRUE, invisibly.
#' @noRd
write_keyword_group <- function(writer, keyword, values, indent = 0L) {
  arguments <- as.list(values)
  names(arguments) <- names(values)
  do.call(writer$keyword, c(list(keyword), arguments, list(.indent = indent)))
}


# ---- Parsers ------------------------------------------------------------

#' Line numbers spanning a named list block
#'
#' Finds the opening keyword and the matching end keyword, and returns the
#' line numbers strictly between them.
#'
#' @param infile A tibble from [get_input_file()].
#' @param start_keyword Keyword opening the block, matched exactly.
#' @param end_pattern Regular expression matching any keyword that closes it.
#' @return Integer vector of interior line numbers; empty if not found.
#' @noRd
list_block_lines <- function(infile, start_keyword, end_pattern) {

  n_lines <- count_input_lines(infile)
  keywords <- vapply(seq_len(n_lines),
                     function(i) get_keyword(infile, i), character(1))

  # Match the opening keyword exactly, so "ATTRIBUTE LIST" does not also
  # match "END ATTRIBUTE LIST".
  opening_lines <- which(!is.na(keywords) & keywords == start_keyword)
  if (length(opening_lines) == 0L) return(integer(0))
  first_line <- opening_lines[1]

  after <- seq.int(first_line + 1L, n_lines)
  closing <- after[!is.na(keywords[after]) & grepl(end_pattern, keywords[after])]
  last_line <- if (length(closing) > 0L) closing[1] - 1L else n_lines

  if (last_line < first_line + 1L) integer(0) else seq.int(first_line + 1L, last_line)
}


#' Parse an ATTRIBUTE LIST block out of an input file
#'
#' Returns the same [attribute_spec()] objects the writers consume, so a
#' working input file can be read in, edited in R, and written back out.
#'
#' Recognized arguments (FILE, OUTPUT FIELD, UNITS, TYPE, LENGTH, and the
#' REPLACE flag) are mapped onto the corresponding fields. Anything else is
#' kept in `extra` and re-emitted verbatim, so a block using arguments this
#' file has never seen still round-trips.
#'
#' @param infile A tibble created by [get_input_file()].
#' @param start_keyword Keyword opening the block.
#' @param end_pattern Regular expression matching the keyword that closes it.
#'   The default accepts both the RIL and netrace terminators.
#'
#' @return A list of [attribute_spec()] objects, empty if the file has no
#'   attribute list.
#' @export
get_attribute_list <- function(infile,
                               start_keyword = "ATTRIBUTE LIST",
                               end_pattern = "^END( ATTRIBUTE)? LIST$") {

  block_lines <- list_block_lines(infile, start_keyword, end_pattern)
  if (length(block_lines) == 0L) return(list())

  attribute_list <- list()
  current <- NULL

  # Close off whatever attribute is being built and push it onto the result.
  flush_current <- function() {
    if (!is.null(current)) attribute_list[[length(attribute_list) + 1L]] <<- current
    current <<- NULL
  }

  for (line_number in block_lines) {

    keyword <- get_keyword(infile, line_number)
    if (is.na(keyword)) next            # comment or blank line inside the block
    parsed <- parse_args(get_args(infile, line_number))

    if (keyword == "TERM") {
      # Walk the FIELD / COEF / EXPONENT run, closing a field-exponent pair
      # each time an EXPONENT is reached.
      coef <- NULL
      exponents <- list()
      pending_field <- NULL
      for (row in seq_len(nrow(parsed))) {
        parameter <- toupper(parsed$Parameter[row])
        value <- parsed$Value[row]
        if (parameter == "FIELD") {
          pending_field <- value
        } else if (parameter == "COEF") {
          coef <- suppressWarnings(as.numeric(value))
        } else if (parameter == "EXPONENT" && !is.null(pending_field)) {
          exponents[[pending_field]] <- suppressWarnings(as.numeric(value))
          pending_field <- NULL
        }
      }
      if (!is.null(current) && length(exponents) > 0L) {
        current$terms[[length(current$terms) + 1L]] <-
          list(coef = coef, exponents = exponents)
      }
      next
    }

    if (grepl("^END EQUATION$", keyword)) next   # the terms are already attached

    # Anything else opens a new attribute.
    flush_current()
    current <- attribute_spec(keyword)

    for (row in seq_len(nrow(parsed))) {
      parameter <- toupper(parsed$Parameter[row])
      value <- parsed$Value[row]
      if (parameter == "FILE") {
        current$file <- value
      } else if (parameter == "OUTPUT FIELD") {
        current$output_field <- value
      } else if (parameter == "UNITS") {
        current$units <- value
      } else if (parameter == "TYPE") {
        current$type <- value
      } else if (parameter == "LENGTH") {
        current$field_length <- value
      } else if (parameter == "") {
        # A bare flag. REPLACE is understood; EQUATION is implied by the TERM
        # lines that follow, so it is dropped rather than stored twice.
        if (toupper(value) == "REPLACE") {
          current$replace <- TRUE
        } else if (toupper(value) != "EQUATION") {
          current$extra <- c(current$extra, list(value))
        }
      } else {
        # An argument this file does not know about; keep it verbatim.
        unknown_argument <- list(value)
        names(unknown_argument) <- parsed$Parameter[row]
        current$extra <- c(current$extra, unknown_argument)
      }
    }
  }

  flush_current()
  attribute_list
}


#' Read an ATTRIBUTE LIST block straight from a file path
#'
#' A thin wrapper around [get_input_file()] + [get_attribute_list()] for
#' callers that only have a path, not an already-loaded input-file tibble --
#' e.g. a driver script that lets its own parameter file point at a *separate*
#' text file holding an arbitrary, hand-edited `ATTRIBUTE LIST:` / `END LIST:`
#' block, so the attribute set to compute isn't hardcoded to
#' [ril_default_attributes()] or any other fixed R-side list.
#'
#' The file need not be a complete RIL/netrace input file: anything outside
#' the `ATTRIBUTE LIST:` ... `END LIST:` block is ignored, so a file holding
#' nothing but that block works fine.
#'
#' @param path Path to a text file containing an `ATTRIBUTE LIST:` block.
#' @param start_keyword,end_pattern Passed through to [get_attribute_list()].
#'
#' @return A list of [attribute_spec()] objects, empty if the file has no
#'   attribute list.
#' @seealso [get_attribute_list()], [RIL_input()]
#' @export
read_attribute_list_file <- function(path,
                                     start_keyword = "ATTRIBUTE LIST",
                                     end_pattern = "^END( ATTRIBUTE)? LIST$") {
  get_attribute_list(get_input_file(path),
                     start_keyword = start_keyword,
                     end_pattern = end_pattern)
}


#' Parse a RASTER LIST block out of an input file
#'
#' @param infile A tibble created by [get_input_file()].
#' @param start_keyword Keyword opening the block.
#' @param end_pattern Regular expression matching the keyword that closes it.
#'
#' @return A list of [raster_list_entry()] objects, empty if the file has no
#'   raster list.
#' @export
get_raster_list <- function(infile,
                            start_keyword = "RASTER LIST",
                            end_pattern = "^END( RASTER)? LIST$") {

  block_lines <- list_block_lines(infile, start_keyword, end_pattern)
  if (length(block_lines) == 0L) return(list())

  raster_list <- list()

  for (line_number in block_lines) {

    keyword <- get_keyword(infile, line_number)
    if (is.na(keyword) || keyword != "FILE") next
    parsed <- parse_args(get_args(infile, line_number))
    if (nrow(parsed) == 0L) next

    bare_values <- parsed$Value[parsed$Parameter == ""]
    named_rows  <- which(parsed$Parameter != "")

    # The first bare argument is the raster; the second, if any, is the
    # statistic; the rest are flags such as DO NODES.
    entry <- raster_list_entry(
      file      = bare_values[1],
      statistic = if (length(bare_values) > 1L) bare_values[2] else character(0),
      flags     = if (length(bare_values) > 2L) bare_values[-(1:2)] else character(0)
    )

    for (row in named_rows) {
      if (toupper(parsed$Parameter[row]) == "OUTPUT FIELD NAME") {
        entry$output_field_name <- parsed$Value[row]
      } else {
        unknown_argument <- list(parsed$Value[row])
        names(unknown_argument) <- parsed$Parameter[row]
        entry$extra <- c(entry$extra, unknown_argument)
      }
    }

    raster_list[[length(raster_list) + 1L]] <- entry
  }

  raster_list
}


# ---- Backwards-compatible aliases ---------------------------------------

#' @rdname attribute_spec
#' @export
ril_attribute <- attribute_spec

#' @rdname equation_term
#' @export
ril_term <- equation_term


# =========================================================================
# SECTION 6.  ONE WRITER FUNCTION PER FORTRAN PROGRAM
# =========================================================================
#
# Each function below follows the same three steps:
#
#   1. Validate any non-path arguments that have a required type.
#   2. Open the input file with input_writer(), which validates the scratch
#      directory and writes the header.
#   3. Name the keywords the program expects, in the order the program's
#      documentation lists them, passing each path through
#      normalize_file_path() or normalize_raster_path() on the way.
#
# Within step 3 the keywords are grouped by role with comment headers: inputs
# the program reads, parameters that control it, and outputs it produces.


#' Create an input file for Fortran program MakeGrids
#'
#' MakeGrids measures a set of terrain attributes over a fixed-diameter
#' neighbourhood of every DEM cell and writes one raster per attribute. It is
#' called by the functions in surface_metrics.
#'
#' @param dem File name (full path) of the input DEM.
#' @param length_scale Diameter in meters over which to measure attributes.
#' @param scratch_dir Directory for temporary files; the input file is written
#'   here.
#' @param rasters Character vector, one element per output raster. Each
#'   element holds two strings separated by a comma: the attribute type, then
#'   the output file name. For example
#'   `"GRADIENT, c:/work/umpqua_grad.flt"`.
#' @param overwrite If TRUE, allow overwriting an existing input file.
#'
#' @return The input file path, invisibly. Side effect: writes the file.
#' @export
makegrids_input <- function(dem,
                            length_scale,
                            scratch_dir,
                            rasters,
                            overwrite = TRUE) {

  require_numeric(length_scale)

  writer <- input_writer("makeGrids", scratch_dir,
                         "makegrids_input.txt", overwrite)

  # --- Input and parameters ----------------------------------------------
  writer$keyword("DEM", normalize_raster_path(dem, must_exist = TRUE))
  writer$keyword("SCRATCH DIRECTORY", normalize_file_path(scratch_dir))
  writer$keyword("LENGTH SCALE", length_scale)

  # --- One GRID line per requested attribute ------------------------------
  for (raster_spec in rasters) {

    # Split the specification on its first comma into attribute type and
    # output file name. Delegates to parse_raster_spec() (defined in
    # wrappers.R) so the "TYPE, file" grammar is defined in exactly one place.
    spec             <- parse_raster_spec(raster_spec)
    attribute_type   <- spec$type
    output_file_name <- spec$file

    # The file name may already carry its own "OUTPUT FILE =" prefix; if not,
    # supply one.
    if (!grepl("OUTPUT FILE", output_file_name)) {
      output_file_name <- paste0("OUTPUT FILE = ", output_file_name)
    }

    # MakeGrids wants the extension present on its outputs.
    #
    output_file_name <- add_extension_if_missing(output_file_name, "flt")

    writer$keyword("GRID", attribute_type, output_file_name)
  }

  invisible(writer$file_path)
}


#' Create an input file for Fortran program Partial
#'
#' Partial builds a raster giving the contributing area to each DEM cell for a
#' storm of specified duration, so the contributing area reflects how far
#' subsurface flow can travel within the storm rather than the full upslope
#' catchment.
#'
#' @param dem File name (full path) of the input DEM.
#' @param k Saturated hydraulic conductivity, in meters per hour.
#' @param d Storm duration, in hours.
#' @param length_scale Diameter in meters for smoothing the DEM.
#' @param scratch_dir Scratch directory; the input file is written here.
#' @param out_raster Output binary floating point (.flt) raster.
#' @param overwrite If TRUE, allow overwriting an existing input file.
#'
#' @return The input file path, invisibly.
#' @export
accum_input <- function(dem,
                        k,
                        d,
                        length_scale,
                        scratch_dir,
                        out_raster,
                        overwrite = TRUE) {

  require_numeric(length_scale)

  writer <- input_writer("partial", scratch_dir,
                         "partial_input.txt", overwrite)

  # --- Input --------------------------------------------------------------
  writer$keyword("DEM", normalize_raster_path(dem, must_exist = TRUE))
  writer$keyword("SCRATCH DIRECTORY", normalize_file_path(scratch_dir))

  # --- Parameters ---------------------------------------------------------
  writer$keyword("LENGTH SCALE", length_scale)
  writer$keyword("DURATION", d)
  writer$keyword("CONDUCTIVITY", k)

  # --- Output -------------------------------------------------------------
  writer$keyword("OUTPUT RASTER", normalize_file_path(out_raster))

  invisible(writer$file_path)
}


#' Create an input file for Fortran program Align
#'
#' Align co-registers two overlapping DTMs. It characterizes the frequency
#' distribution of elevation differences as functions of slope gradient and
#' aspect, uses that to set slope- and aspect-dependent thresholds for
#' filtering those differences, assembles and solves a set of linear equations
#' for the optimal x-y-z shift of one DTM, then writes the shifted DTM along
#' with elevation-difference and outlier rasters.
#'
#' @param refDTM Reference DTM (full path).
#' @param alignDTM DTM to be aligned to the reference (full path).
#' @param refGrnd ground-return density raster for the reference dataset.
#' @param alignGrnd ground-return density raster for the align dataset.
#' @param refDSM Reference surface-height raster (optional).
#' @param alignDSM DSM to align (optional).
#' @param iterations Number of iterations used to solve for the shift.
#' @param k Number of interquartile ranges for the Tukey's fence that
#'   identifies outlying elevation differences.
#' @param dampener Dampener applied to the shift at each iteration.
#' @param outDTM Output aligned DTM.
#' @param tileNx,tileNy Number of tiles in the x and y directions.
#' @param overlap Overlap between tiles, in meters.
#' @param radius Radius in meters for measuring slope and aspect.
#' @param nslope Number of gradient bins.
#' @param maxSlope Maximum gradient for binning.
#' @param nAzimuth Number of azimuth bins.
#' @param outbins Output csv of elevation differences binned by slope and
#'   aspect.
#' @param outDif Output elevation-difference raster (.flt).
#' @param outOutlier Output outlier raster (.flt).
#' @param scratch_dir Scratch directory.
#' @param overwrite If TRUE, allow overwriting an existing input file.
#'
#' @return The input file path, invisibly.
#' @export
align_input <- function(refDTM,
                        alignDTM,
                        refGrnd = NOFILE,
                        alignGrnd = NOFILE,
                        refDSM = NOFILE,
                        alignDSM = NOFILE,
                        iterations,
                        k,
                        dampener,
                        outDTM,
                        tileNx,
                        tileNy,
                        overlap,
                        radius,
                        nslope,
                        maxSlope,
                        nAzimuth,
                        outbins,
                        outDif,
                        outOutlier,
                        scratch_dir,
                        overwrite = TRUE) {

  writer <- input_writer("align", scratch_dir, "input_align.txt", overwrite)

  # --- Inputs. The two ground density and DSM rasters are optional and are skipped when the
  #     caller leaves them at NOFILE. ------------------------------------
  writer$keyword("REFERENCE DEM",
                 normalize_raster_path(refDTM, must_exist = TRUE))
  writer$keyword("DEM TO ALIGN",
                 normalize_raster_path(alignDTM, must_exist = TRUE))
  writer$optional("REFERENCE DENSITY RASTER", normalize_raster_path(refGrnd))
  writer$optional("ALIGN DENSITY RASTER", normalize_raster_path(alignGrnd))
  writer$optional("REFERENCE DSM", normalize_raster_path(refDSM))
  writer$optional("DSM TO ALIGN",  normalize_raster_path(alignDSM))

  # --- Solution parameters ------------------------------------------------
  writer$keyword("RADIUS", radius)
  writer$keyword("ITERATIONS", iterations)
  writer$keyword("K", k)
  writer$keyword("DAMPENER", dampener)

  # --- Tiling: the DTM is solved in overlapping tiles ---------------------
  writer$keyword("TILES", X = tileNx, Y = tileNy, OVERLAP = overlap)

  # --- Binning of elevation differences by slope and aspect ---------------
  writer$keyword("BINS",
                 `SLOPE BINS`   = nslope,
                 `MAX SLOPE`    = maxSlope,
                 `AZIMUTH BINS` = nAzimuth,
                 OUTPUT         = normalize_file_path(outbins))

  # --- Outputs ------------------------------------------------------------
  writer$keyword("OUTPUT DEM", normalize_file_path(outDTM))
  writer$keyword("OUTPUT DIFFERENCE RASTER", normalize_file_path(outDif))
  writer$keyword("OUTPUT OUTLIER RASTER", normalize_file_path(outOutlier))
  writer$keyword("SCRATCH DIRECTORY", normalize_file_path(scratch_dir))

  invisible(writer$file_path)
}


#' Create an input file for Fortran program HuntLS
#'
#' HuntLS reads the "outlier" raster created by Align. The outlier value k
#' gives the number of interquartile ranges by which a DoD elevation
#' difference falls outside the interquartile range:
#' k = (de - q1)/(q3 - q1) below q1, k = (de - q3)/(q3 - q1) above q3, and
#' zero for differences lying between q1 and q3. HuntLS groups the outlier
#' cells into patches and associates those patches with field-mapped landslide
#' locations surveyed for the Post Mortem study (Stewart et al., 2013).
#'
#' @param DEM Reference DTM, the one used for Align.
#' @param Outlier Input outlier raster (.flt) created by Align.
#' @param DoD Input elevation-difference raster (.flt) created by Align.
#' @param Accum Input flow accumulation raster created by bldgrds.
#' @param AccumThreshold Contributing area above which landslide patches are
#'   precluded.
#' @param LSpnts Input point shapefile of mapped landslide points.
#' @param IDfield Name of the attribute-table field holding the record ID.
#' @param Radius Search radius in meters for matching landslide points to
#'   outlier patches.
#' @param AspectLength Length in meters over which to calculate aspect.
#' @param GradLength Length in meters over which to calculate gradient.
#' @param OutlierThreshold Minimum absolute k value for a cell to join a patch.
#' @param ScratchDir Scratch directory.
#' @param OutPatch Output patch raster (.flt).
#' @param OutGrad Output gradient raster (.flt).
#' @param Outcsv Output comma-delimited table.
#' @param overwrite If TRUE, allow overwriting an existing input file.
#'
#' @return 0 on success.
#' @export
huntLSinput <- function(DEM,
                        Outlier,
                        DoD,
                        Accum,
                        AccumThreshold,
                        LSpnts,
                        IDfield,
                        Radius,
                        AspectLength,
                        GradLength,
                        OutlierThreshold,
                        ScratchDir,
                        OutPatch,
                        OutGrad,
                        Outcsv,
                        overwrite = TRUE) {

  writer <- input_writer("HuntLS", ScratchDir, "input_huntls.txt", overwrite)

  # --- Input rasters. Outlier had its ".flt" stripped in the original but
  #     was never expanded to a full path. -------------------------------
  writer$keyword("DEM",
                 normalize_raster_path(DEM, must_exist = TRUE))
  writer$keyword("INPUT OUTLIER RASTER",
                 normalize_raster_path(Outlier, must_exist = TRUE))
  writer$keyword("INPUT ELEVATION DIFFERENCE RASTER",
                 normalize_raster_path(DoD, must_exist = TRUE))
  writer$keyword("INPUT FLOW ACCUMULATION RASTER",
                 normalize_raster_path(Accum, must_exist = TRUE))

  # --- Input landslide points, with the field that identifies each record --
  writer$keyword("INPUT LANDSLIDE POINT SHAPEFILE",
                 normalize_file_path(LSpnts),
                 `ID FIELD` = IDfield)

  # --- Parameters ---------------------------------------------------------
  writer$keyword("ACCUMULATION THRESHOLD", AccumThreshold)
  writer$keyword("SEARCH RADIUS", Radius)
  writer$keyword("ASPECT LENGTH SCALE", AspectLength)
  writer$keyword("GRADIENT LENGTH SCALE", GradLength)
  writer$keyword("OUTLIER THRESHOLD", OutlierThreshold)

  # --- Outputs ------------------------------------------------------------
  writer$keyword("SCRATCH DIRECTORY", normalize_file_path(ScratchDir))
  writer$keyword("OUTPUT PATCH RASTER", normalize_file_path(OutPatch))
  writer$keyword("OUTPUT GRADIENT RASTER", normalize_file_path(OutGrad))
  writer$keyword("OUTPUT TABLE", normalize_file_path(Outcsv))

  0L
}


#' Create an input file for Fortran program LShunter
#'
#' LShunter identifies potential landslide sites from patches of outlying
#' elevation change on a DoD, delineated on the outlier raster created by
#' Align. It works in two rounds, applying a strict set of thresholds to find
#' patch cores and then a looser set to grow them, so most of its parameters
#' come in pairs.
#'
#' @param DEM Elevation raster. Only needed when no Gradient raster is
#'   supplied, in which case LShunter computes gradient itself.
#' @param Outlier Outlier raster (.flt) created by Align.
#' @param threshold1,threshold2 Maximum k value for a patch in the 1st and 2nd
#'   rounds, e.g. -5.0 then -1.5.
#' @param Gradient Input gradient raster (.flt) from MakeGrids (optional).
#' @param min1,min2 Minimum gradient for a patch, 1st and 2nd rounds.
#' @param Accum Input flow accumulation raster (.flt) created by bldgrds.
#' @param maxAccum1,maxAccum2 Maximum flow accumulation for a patch, 1st and
#'   2nd rounds.
#' @param Roads Input road-layer shapefile (optional).
#' @param road_buffer Buffer in meters around the road layer. Only used when
#'   a road layer is supplied.
#' @param MinSize Minimum patch size in square meters.
#' @param GradLength Length in meters over which to calculate gradient. Only
#'   used when no Gradient raster is supplied.
#' @param OutPatch Output patch raster (.flt).
#' @param ScratchDir Scratch directory.
#' @param overwrite If TRUE, allow overwriting an existing input file.
#'
#' @return 0 on success.
#' @export
LShunterInput <- function(DEM = NOFILE,
                          Outlier,
                          threshold1,
                          threshold2,
                          Gradient = NOFILE,
                          min1,
                          min2,
                          Accum,
                          maxAccum1,
                          maxAccum2,
                          Roads = NOFILE,
                          road_buffer,
                          MinSize,
                          GradLength,
                          OutGrad = NOFILE,
                          OutPatch,
                          ScratchDir,
                          overwrite = TRUE) {

  writer <- input_writer("LShunter", ScratchDir,
                         "input_LShunter.txt", overwrite)

  # --- Elevation, only needed when gradient must be computed --------------
  writer$optional("DEM", normalize_raster_path(DEM))

  # --- The outlier raster from Align, with the outlier thresholds ------------
  writer$keyword("OUTLIER",
                 normalize_raster_path(Outlier, must_exist = TRUE),
                 THRESHOLD1 = threshold1,
                 THRESHOLD2 = threshold2)

  # --- Gradient. Either a precomputed raster is supplied, or LShunter is
  #     told the length scale over which to compute gradient itself. -------
  if (is_missing_path(Gradient)) {
    writer$keyword("GRADIENT", MIN1 = min1, MIN2 = min2)
    writer$keyword("GRADIENT LENGTH SCALE", GradLength)
  } else {
    writer$keyword("GRADIENT",
                   FILE = normalize_raster_path(Gradient),
                   MIN1 = min1,
                   MIN2 = min2)
  }

  # --- Flow accumulation, with its per-round ceilings ---------------------
  writer$keyword("FLOW ACCUMULATION",
                 normalize_raster_path(Accum, must_exist = TRUE),
                 MAX1 = maxAccum1,
                 MAX2 = maxAccum2)

  # --- Optional road layer, used to exclude road-related earth movement.
  writer$optional("ROAD SHAPEFILE",
                  normalize_file_path(Roads),
                  BUFFER = road_buffer)

  # --- Remaining parameter and outputs ------------------------------------
  writer$keyword("MINIMUM SIZE", MinSize)
  writer$keyword("OUTPUT PATCH RASTER", normalize_file_path(OutPatch))
  writer$keyword("SCRATCH DIRECTORY", normalize_file_path(ScratchDir))

  0L
}


#' Create an input file for Fortran program LS_poly
#'
#' LS_poly characterizes mapped landslide polygons against a set of terrain
#' predictors and delineates the initiation zone within each polygon. Each
#' predictor raster may either be supplied precomputed (the `in*` arguments)
#' or computed by LS_poly at a stated radius (the `out*` arguments); supply
#' whichever pair suits, and leave the other at NOFILE.
#'
#' @param DEM Input DEM (.flt or .tif), full path.
#' @param polyFile Input landslide polygon shapefile.
#' @param polyID Name of the ID field for the input polygons.
#' @param inGrad,inTan,inProf Precomputed gradient, tangential curvature and
#'   profile curvature rasters (each optional).
#' @param inFoS Input factor-of-safety raster.
#' @param outGrad,outTan,outProf Gradient, tangential curvature and profile
#'   curvature rasters for LS_poly to compute and write (each optional).
#' @param gradRadius,tanRadius,profRadius Radii in meters used when computing
#'   the corresponding output raster.
#' @param outNodes Output node point shapefile (.shp).
#' @param outCsv Output csv of patch statistics.
#' @param outInit Output initiation zone raster (.flt or .tif).
#' @param scratchDir Scratch directory.
#' @param overwrite If TRUE, allow overwriting an existing input file.
#'
#' @return 0 on success.
#' @export
LS_poly_input <- function(DEM,
                          polyFile,
                          polyID,
                          inGrad = NOFILE,
                          inTan = NOFILE,
                          inProf = NOFILE,
                          inFoS,
                          outGrad = NOFILE,
                          gradRadius = 0,
                          outTan = NOFILE,
                          tanRadius = 0,
                          outProf = NOFILE,
                          profRadius = 0,
                          outNodes,
                          outCsv,
                          outInit,
                          scratchDir,
                          overwrite = TRUE) {

  writer <- input_writer("LS_poly", scratchDir,
                         "input_LS_poly.txt", overwrite)

  # --- Elevation and the landslide polygons -------------------------------
  writer$keyword("DEM", normalize_raster_path(DEM, must_exist = TRUE))
  writer$keyword("LANDSLIDE POLYGON FILE",
                 normalize_raster_path(polyFile,
                                       must_exist = TRUE,
                                       extension = "shp"),
                 `ID FIELD` = polyID)

  # --- Predictors supplied precomputed ------------------------------------
  writer$optional("INPUT GRADIENT RASTER",
                  normalize_raster_path(inGrad))
  writer$optional("INPUT TANGENTIAL CURVATURE RASTER",
                  normalize_raster_path(inTan))
  writer$optional("INPUT PROFILE CURVATURE RASTER",
                  normalize_raster_path(inProf))

  # --- Predictors for LS_poly to compute, each with its own radius --------
  writer$optional("OUTPUT GRADIENT RASTER",
                  normalize_raster_path(outGrad), RADIUS = gradRadius)
  writer$optional("OUTPUT TANGENTIAL CURVATURE RASTER",
                  normalize_raster_path(outTan), RADIUS = tanRadius)
  writer$optional("OUTPUT PROFILE CURVATURE RASTER",
                  normalize_raster_path(outProf), RADIUS = profRadius)

  # --- Factor of safety ---------------------------------------------------
  writer$keyword("INPUT FOS RASTER",
                 normalize_raster_path(inFoS, must_exist = TRUE))

  # --- Outputs ------------------------------------------------------------
  writer$keyword("OUTPUT NODE POINT SHAPEFILE",
                 normalize_file_path(outNodes))
  # BUG FIX: the original wrote `outCSV`, which is not a formal argument of
  # this function, so every call failed here with "object 'outCSV' not found".
  writer$keyword("OUTPUT CSV FILE", normalize_file_path(outCsv))
  writer$keyword("OUTPUT INITIATION RASTER", normalize_file_path(outInit))
  writer$keyword("SCRATCH DIRECTORY", normalize_file_path(scratchDir))

  # The original assigned returnCode <- 0 but never returned it.
  0L
}


#' Create an input file for Fortran program SamplePoints
#'
#' SamplePoints generates point samples inside and outside mapped landslide
#' initiation zones, working from the initiation-zone raster produced by
#' LS_poly. The supplied predictor rasters define the range of values found
#' within the initiation patches; a raster mask then delineates the area
#' elsewhere that falls within that range. Random samples are drawn inside the
#' zones and outside them but within the mask, with a buffer around each point
#' preventing samples from crowding one another and a margin keeping them away
#' from zone edges. Predictor values are then binned over the whole zone and
#' mask area and over the two point samples, so the frequency distributions can
#' be compared both between inside and outside and between area and sample.
#'
#' @param inRaster Input initiation zone raster (.flt) from LS_poly.
#' @param areaPerSample Area in square meters represented by each sample point;
#'   this sets the target number of points inside the initiation zones.
#' @param buffer_in,buffer_out Buffer distance in meters around inside and
#'   outside points.
#' @param margin Distance in meters from a zone edge within which points are
#'   not placed.
#' @param ratio Ratio of outside points to inside points.
#' @param nbins Number of bins per predictor.
#' @param R4rasters List of single-precision real predictor rasters. Each
#'   element is a character vector of
#'   c(name, file, lower cutoff, upper cutoff, "MASK", "SELECT"); the last two
#'   are optional flags.
#' @param I4rasters List of integer predictor rasters, whose values are nominal
#'   classes such as landform type. Each element is c(name, file, "MASK"), the
#'   last being an optional flag.
#' @param minPatch Minimum patch size in square meters for patches delineated
#'   in the raster mask.
#' @param inPoints,outPoints Output shapefiles for the inside and outside
#'   point samples.
#' @param outInitPoints Output point shapefile holding the initiation point
#'   within each zone (optional).
#' @param outMask Output mask raster (.flt).
#' @param outInit Output raster (.flt) of the initiation zones actually
#'   sampled.
#' @param table Output table of binning results.
#' @param scratchDir Scratch directory.
#' @param normalize If TRUE, expand raster and shapefile paths. This whole
#'   block was commented out in the original file, most likely because
#'   normalizePath() errored on output files that did not exist yet;
#'   normalize_file_path() no longer does. Set FALSE to restore the original
#'   behaviour of writing paths through unchanged.
#' @param overwrite If TRUE, allow overwriting an existing input file.
#'
#' @return 0 on success.
#' @export
samplePointInput <- function(inRaster,
                             areaPerSample,
                             buffer_in,
                             buffer_out,
                             margin,
                             ratio,
                             nbins,
                             R4rasters,
                             I4rasters,
                             minPatch,
                             inPoints,
                             outPoints,
                             outInitPoints = NOFILE,
                             outMask,
                             outInit,
                             table,
                             scratchDir,
                             normalize = TRUE,
                             overwrite = TRUE) {

  # Path expansion is switchable here, so this small wrapper stands in for a
  # direct call to the normalizers everywhere below.
  expand_path <- function(path, is_raster = TRUE) {
    if (!normalize) return(path)
    if (is_raster) normalize_raster_path(path) else normalize_file_path(path)
  }

  writer <- input_writer("samplePoints", scratchDir,
                         "input_samplePoints.txt", overwrite)

  # --- Input zone raster --------------------------------------------------
  writer$keyword("INITIATION ZONE RASTER", expand_path(inRaster))

  # --- Sampling parameters ------------------------------------------------
  writer$keyword("SAMPLE AREA",    areaPerSample)
  writer$keyword("BUFFER INSIDE",  buffer_in)
  writer$keyword("BUFFER OUTSIDE", buffer_out)
  writer$keyword("MARGIN", margin)
  writer$keyword("RATIO",  ratio)
  writer$keyword("NBINS",  nbins)

  # --- Continuous predictors. The count is written first, then one indented
  #     line per raster giving its name, file, cutoffs and any flags. -------
  if (length(R4rasters) > 0L) {
    writer$keyword("R4 RASTERS", length(R4rasters))
    for (raster_spec in R4rasters) {
      spec_line <- paste0("  ", spec_field(raster_spec, 1), ": ",
                          expand_path(spec_field(raster_spec, 2)),
                          ", lower = ", spec_field(raster_spec, 3),
                          ", upper = ", spec_field(raster_spec, 4))
      # spec_field() and spec_has_flag() tolerate specifications shorter than
      # six fields; the original indexed raster_spec[5] directly and threw a
      # subscript error on any four-element specification.
      if (spec_has_flag(raster_spec, 5, "MASK")) {
        spec_line <- paste0(spec_line, ", MASK")
      }
      if (spec_has_flag(raster_spec, 6, "SELECT")) {
        spec_line <- paste0(spec_line, ", SELECT")
      }
      writer$line(spec_line)
    }
  }

  # --- Nominal-class predictors, same layout with fewer fields ------------
  if (length(I4rasters) > 0L) {
    writer$keyword("I4 RASTERS", length(I4rasters))
    for (raster_spec in I4rasters) {
      spec_line <- paste0("  ", spec_field(raster_spec, 1), ": ",
                          expand_path(spec_field(raster_spec, 2)))
      if (spec_has_flag(raster_spec, 3, "MASK")) {
        spec_line <- paste0(spec_line, ", MASK")
      }
      writer$line(spec_line)
    }
  }

  # --- Mask parameter and outputs -----------------------------------------
  writer$keyword("MINIMUM MASK PATCH", minPatch)
  writer$keyword("OUTPUT INPOINT SHAPEFILE",
                 expand_path(inPoints, is_raster = FALSE))
  writer$keyword("OUTPUT OUTPOINT SHAPEFILE",
                 expand_path(outPoints, is_raster = FALSE))
  writer$optional("OUTPUT INITIATION POINT SHAPEFILE",
                  expand_path(outInitPoints, is_raster = FALSE))
  writer$keyword("OUTPUT MASK RASTER", expand_path(outMask))
  writer$keyword("OUTPUT INITIATION ZONE RASTER", expand_path(outInit))
  writer$keyword("OUTPUT TABLE", expand_path(table, is_raster = FALSE))
  writer$keyword("SCRATCH DIRECTORY", normalize_file_path(scratchDir))

  0L
}


#' Create an input file for Fortran program modelDensity
#'
#' modelDensity builds a density raster for a Poisson point model. The
#' covariate rasters and their fitted coefficients are specified in this input
#' file, along with the points the model was built from. Besides the density
#' raster, modelDensity writes an ROC curve csv computed from the modeled
#' density and the input points, a proportion-of-points raster derived from
#' the density raster, and a csv giving the actual proportion of points
#' falling in each 10 percent increment of that proportion raster.
#'
#' @param mask Input mask raster (.flt) bounding the modeled area.
#' @param init_pnts Initiation points (.shp) the model was built from.
#' @param intercept Model intercept.
#' @param R4rasters List of continuous covariates. Each element is a list of
#'   `(name, file, polynomial order m, coefficient 1, ..., coefficient m)`.
#' @param I4rasters List of integer factor covariates. Each element is a list
#'   of `(name, file, number of classes n, minimum class, maximum class,
#'   class 1, coefficient 1, ..., class n, coefficient n)`.
#' @param unit "KM" to convert output units to kilometers.
#' @param prop_raster Output proportion raster (.flt).
#' @param density_raster Output density raster (.flt).
#' @param ROC Output ROC csv file.
#' @param scratch_dir Scratch directory.
#' @param overwrite If TRUE, allow overwriting an existing input file.
#'
#' @return 0 on success.
#' @export
modelDensity_input <- function(mask,
                               init_pnts,
                               intercept,
                               R4rasters,
                               I4rasters,
                               unit,
                               prop_raster,
                               density_raster,
                               ROC,
                               scratch_dir,
                               overwrite = TRUE) {

  # BUG FIX: the original built its output path from `scratchDir`, which does
  # not exist in this function's scope; every call failed before writing
  # anything.
  writer <- input_writer("modelDensity", scratch_dir,
                         "input_modelDensity.txt", overwrite)

  # --- Model domain, calibration points and intercept ---------------------
  writer$keyword("MASK RASTER",
                 normalize_raster_path(mask, must_exist = TRUE))
  writer$keyword("INITIATION POINT SHAPEFILE",
                 normalize_file_path(init_pnts))
  writer$keyword("INTERCEPT", intercept)

  # --- Continuous covariates ----------------------------------------------
  # One line each: name, file, polynomial order, then that many coefficients.
  if (length(R4rasters) > 0L) {
    writer$keyword("R4 RASTERS", length(R4rasters))
    for (covariate in R4rasters) {
      covariate_name    <- covariate[[1]]
      covariate_raster  <- normalize_raster_path(covariate[[2]])
      polynomial_order  <- as.integer(covariate[[3]])
      # Coefficients occupy the elements after the order, one per power.
      coefficients <- if (polynomial_order > 0L) {
        unlist(covariate[seq.int(4L, 3L + polynomial_order)])
      } else {
        NULL
      }
      writer$keyword(covariate_name, covariate_raster,
                     polynomial_order, coefficients, .indent = 2L)
    }
  }

  # --- Nominal-class covariates -------------------------------------------
  # A header line per raster giving name, file, class count and class range,
  # followed by one line per class. On those lines the keyword is just the
  # sequential class number, the first argument is the class value and the
  # second is that class's coefficient.
  if (length(I4rasters) > 0L) {
    writer$keyword("I4 RASTERS", length(I4rasters))
    for (covariate in I4rasters) {
      covariate_name   <- covariate[[1]]
      covariate_raster <- normalize_raster_path(covariate[[2]])
      n_classes        <- as.integer(covariate[[3]])
      minimum_class    <- covariate[[4]]
      maximum_class    <- covariate[[5]]

      writer$keyword(covariate_name, covariate_raster,
                     n_classes, minimum_class, maximum_class, .indent = 2L)

      # Class values and coefficients are interleaved from element 6 onward,
      # so class i occupies elements (4 + 2i) and (5 + 2i).
      for (class_index in seq_len(n_classes)) {
        class_value       <- covariate[[4L + 2L * class_index]]
        class_coefficient <- covariate[[5L + 2L * class_index]]
        writer$keyword(as.character(class_index),
                       class_value, class_coefficient, .indent = 2L)
      }
    }
  }

  # --- Units and outputs. ROC was never expanded in the original. ---------
  writer$keyword("UNITS", unit)
  writer$keyword("OUTPUT PROPORTION RASTER",
                 normalize_raster_path(prop_raster))
  writer$keyword("OUTPUT DENSITY RASTER",
                 normalize_raster_path(density_raster))
  writer$keyword("OUTPUT ROC CSV", normalize_file_path(ROC))
  writer$keyword("SCRATCH DIRECTORY", normalize_file_path(scratch_dir))

  0L
}


#' Create an input file for Fortran program Quantiles
#'
#' Quantiles reads a raster and computes quantiles over a moving circular
#' window. At each window position it finds the interquartile range, removes
#' values lying outside a Tukey's fence with k = 1.5, and recalculates the
#' quartiles from what remains. All of its output rasters are optional; supply
#' only the ones you need and leave the rest at NOFILE.
#'
#' @param in_raster Input raster (full path).
#' @param radius Radius in meters for the moving window.
#' @param buffer Spacing between moving-window centre points, in raster cells.
#' @param out_outlier Output outlier raster (optional).
#' @param out_q1,out_q2,out_q3 Output first-quartile, median and
#'   third-quartile rasters (each optional).
#' @param out_mean Output mean raster (optional).
#' @param out_zscore Output z-score raster (optional).
#' @param out_prob Output probability raster (optional).
#' @param scratch_dir Scratch directory.
#' @param overwrite If TRUE, allow overwriting an existing input file.
#'
#' @return The input file path, invisibly.
#' @export
quantiles_input <- function(in_raster = NOFILE,
                            radius = 0,
                            buffer = 0,
                            out_outlier = NOFILE,
                            out_q1 = NOFILE,
                            out_q2 = NOFILE,
                            out_q3 = NOFILE,
                            out_mean = NOFILE,
                            out_zscore = NOFILE,
                            out_prob = NOFILE,
                            scratch_dir = NOFILE,
                            overwrite = TRUE) {

  writer <- input_writer("quantiles", scratch_dir,
                         "input_quantiles.txt", overwrite)

  # --- Input and window parameters ----------------------------------------
  writer$keyword("INPUT RASTER",
                 normalize_raster_path(in_raster, must_exist = TRUE))
  writer$keyword("RADIUS", radius)
  writer$keyword("BUFFER", buffer)

  # --- Optional outputs ---------------------------------------------------
  # Seven near-identical normalizePath blocks and seven near-identical write
  # blocks in the original collapse into this table plus one loop. Adding an
  # eighth output is now a single line.
  optional_output_rasters <- list(
    "OUTPUT OUTLIER RASTER"     = out_outlier,
    "OUTPUT Q1 RASTER"          = out_q1,
    "OUTPUT Q2 RASTER"          = out_q2,
    "OUTPUT Q3 RASTER"          = out_q3,
    "OUTPUT MEAN RASTER"        = out_mean,
    "OUTPUT ZSCORE RASTER"      = out_zscore,
    "OUTPUT PROBABILITY RASTER" = out_prob
  )
  for (keyword in names(optional_output_rasters)) {
    writer$optional(keyword,
                    normalize_file_path(optional_output_rasters[[keyword]]))
  }

  writer$keyword("SCRATCH DIRECTORY", normalize_file_path(scratch_dir))

  invisible(writer$file_path)
}


#' Create an input file for Fortran program distanceToRoad
#'
#' distanceToRoad builds a raster giving the distance in meters to the nearest
#' road for every DEM grid point, out to a stated radius.
#'
#' @param dem Input DEM (full path).
#' @param radius Radius in meters to extend outward from a road.
#' @param road_file Input road polyline shapefile.
#' @param out_raster Output binary floating point (.flt) raster.
#' @param scratch_dir Scratch directory; the input file is written here.
#' @param overwrite If TRUE, allow overwriting an existing input file.
#'
#' @return The input file path, invisibly.
#' @export
distanceToRoad_input <- function(dem,
                                 radius = 1000,
                                 road_file,
                                 out_raster,
                                 scratch_dir,
                                 overwrite = TRUE) {

  writer <- input_writer("distanceToRoad", scratch_dir,
                         "input_distanceToRoad.txt", overwrite)

  # --- Inputs. road_file was never expanded to a full path in the original. -
  writer$keyword("DEM", normalize_raster_path(dem, must_exist = TRUE))
  writer$keyword("ROAD SHAPEFILE",
                 normalize_file_path(road_file, must_exist = TRUE))

  # --- Output and parameter -----------------------------------------------
  writer$keyword("OUTPUT RASTER", normalize_file_path(out_raster))
  writer$keyword("SCRATCH DIRECTORY", normalize_file_path(scratch_dir))
  writer$keyword("RADIUS", radius)

  invisible(writer$file_path)
}


#' Create an input file for Fortran program bldgrds, with channels turned off
#'
#' bldgrds does flow routing for a DEM. This input file specifies routing
#' without channel delineation, so flow directions are determined entirely by
#' D-infinity rather than being forced down mapped channels.
#'
#' @param dem Input DEM (full path).
#' @param aspect_length Length in meters over which aspect is measured.
#' @param plan_length Length in meters over which plan curvature is measured.
#' @param grad_length Length in meters over which gradient is measured.
#' @param out_raster Output binary floating point (.flt) raster.
#' @param scratch_dir Scratch directory; the input file is written here.
#' @param overwrite If TRUE, allow overwriting an existing input file.
#'
#' @return The input file path, invisibly.
#' @export
bldgrds_nochannels_input <- function(dem,
                                     aspect_length,
                                     plan_length,
                                     grad_length,
                                     out_raster,
                                     scratch_dir,
                                     overwrite = TRUE) {

  writer <- input_writer("bldgrds_nochannels", scratch_dir,
                         "input_bldgrds_nochannels.txt", overwrite)

  # --- Input --------------------------------------------------------------
  writer$keyword("DEM", normalize_raster_path(dem, must_exist = TRUE))

  # --- The three length scales controlling the derivative calculations ----
  writer$keyword("USE SMOOTHED ASPECT", `LENGTH SCALE` = aspect_length)
  writer$keyword("PLAN CURVATURE LENGTH SCALE", plan_length)
  writer$keyword("GRADIENT LENGTH SCALE", grad_length)

  # --- A bare flag keyword: no arguments, just the keyword and its colon --
  writer$keyword("NO CHANNELS")

  # --- Outputs ------------------------------------------------------------
  writer$keyword("OUTPUT FLOW ACCUMULATION RASTER",
                 normalize_file_path(out_raster))
  writer$keyword("SCRATCH DIRECTORY", normalize_file_path(scratch_dir))

  invisible(writer$file_path)
}


#' The attribute list bldgrds falls back to when none is supplied
#'
#' bldgrds builds a channel node-list database (and, if requested, a node
#' point shapefile) via the same `attributeList` machinery RIL uses
#' (`ChannelNode_Module.f90`). This reproduces the bare elevation/area
#' attribute block of a working Sprague-River reference run. That reference
#' run's precipitation-dependent chain (mean annual precipitation feeding
#' Lorenson, Marcus and Roberts, 1994, for mean annual flow, and White,
#' McCullough, Justice and Kelsey, 2011, for channel width and depth) is NOT
#' reproduced here -- see the note on [ril_default_attributes()]: a
#' `precip_raster` is a property of the `MEAN ANNUAL PRECIP` attribute entry
#' itself, supplied only via an attribute-list text file read with
#' [read_attribute_list_file()] (see `bldgrds_attributes_example.txt` in the
#' UnstableSlopes repo).
#'
#' @return A list of [attribute_spec()] objects.
#' @export
bldgrds_default_attributes <- function() {

  list(
    attribute_spec("ELEVATION"),
    attribute_spec("CONTRIBUTING AREA", output_field = "AREA_SQKM")
  )
}


#' Create an input file for Fortran program bldgrds
#'
#' bldgrds computes flow direction and D-infinity contributing area for a
#' DEM, then traces the channel network downstream and writes it out as a
#' node-list database, consumed by downstream programs such as netrace.
#' Channel initiation is by area-slope threshold, separately calibrated for
#' low- and high-gradient terrain, refined against a local relief raster and
#' plan curvature. Optionally it also excavates the DEM along mapped
#' road-crossing/culvert lines, masks out mapped water bodies before tracing
#' channels, and writes a node point shapefile.
#'
#' Unlike most of the other `*_input()` builders, bldgrds has no single
#' `OUTPUT ... RASTER` argument naming "the" output raster, so there is
#' nothing here for a mode-3 (read-only) wrapper to read back afterwards.
#'
#' This reproduces the keyword grammar of a working reference run (for the
#' Sprague River project) rather than the older, structurally different
#' `bldGrds2.f90` this repo's `GridUtilities` checkout currently has --  see
#' the "bldgrds" entry in this package's `CLAUDE.md` for that discrepancy.
#' Treat this builder, not that file, as the authority on bldgrds' current
#' input format until the two are reconciled.
#'
#' @param dem Input DEM (full path).
#' @param scratch_dir Scratch directory; the input file is written here.
#' @param aspect_length Length in meters over which aspect is smoothed
#'   (`USE SMOOTHED ASPECT: LENGTH SCALE`).
#' @param plan_length Length in meters over which plan curvature is measured.
#' @param gradient_length_scale Length in meters over which gradient is
#'   measured.
#' @param d8_plan_coefficient,d8_aspect_coefficient Weights on plan curvature
#'   and aspect in the D8 flow-direction calculation (`D8 COEFFICIENTS`).
#' @param d8_aspect_length,d8_plan_length Length scales in meters for the
#'   aspect and plan curvature used in that same D8 calculation
#'   (`D8 LENGTH SCALES`).
#' @param initiation_buffer_inner,initiation_buffer_outer Inner and outer
#'   buffer distances in meters around a candidate initiation point
#'   (`INITIATION BUFFER`).
#' @param initiation_area_override Contributing area in square meters above
#'   which channel initiation is forced regardless of other thresholds
#'   (`INITIATION BUFFER: AREA OVERRIDE`).
#' @param local_relief_threshold_high,local_relief_threshold_low High and low
#'   local-relief thresholds for channel initiation (`LOCAL RELIEF
#'   THRESHOLD`).
#' @param area_slope_threshold_low_gradient,area_slope_threshold_high_gradient
#'   Contributing-area threshold for channel initiation in low- and
#'   high-gradient terrain, respectively.
#' @param plan_curvature_threshold_low_gradient,plan_curvature_threshold_high_gradient
#'   Plan curvature threshold for channel initiation in low- and
#'   high-gradient terrain, respectively.
#' @param minimum_threshold_flow_length Minimum flow length in meters for a
#'   channel-initiation threshold to apply.
#' @param minimum_channel_length Minimum channel length in meters.
#' @param local_relief_raster Optional: precomputed local relief raster
#'   (e.g. from [DEV()]/LocalRelief), reused instead of bldgrds calculating
#'   its own.
#' @param use_existing_files If TRUE, reuse existing intermediate files
#'   (`USE EXISTING FILES`) rather than recalculating them.
#' @param calibrate If TRUE, run in calibration mode.
#' @param excavate_line Optional: polyline shapefile (e.g. road crossings) to
#'   excavate through the DEM, clearing culvert blockages.
#' @param excavate_line_buffer Buffer in meters around `excavate_line`. Only
#'   meaningful with `excavate_line`.
#' @param water_mask Optional: water-body polygon/raster mask.
#' @param water_mask_min_patch_size Minimum patch size in square meters for
#'   `water_mask`. Only meaningful with `water_mask`.
#' @param water_mask_set_to_min_elevation If TRUE, set the DEM to its minimum
#'   elevation within each water mask patch.
#' @param water_mask_incise_to_center If TRUE, incise the DEM toward the
#'   center of each water mask patch.
#' @param water_mask_min_gradient Minimum gradient enforced within the water
#'   mask.
#' @param water_mask_buffer_radius Buffer radius in meters around the water
#'   mask.
#' @param water_mask_preclude_initiation If TRUE, preclude channel
#'   initiation within the water mask.
#' @param node_shapefile Optional: output node point shapefile
#'   (`OUTPUT NODE POINT SHAPEFILE`).
#' @param node_splits Optional: number of pieces to split each channel
#'   segment into for the node point shapefile. Only meaningful with
#'   `node_shapefile`.
#' @param attribute_list List of [attribute_spec()] objects, written as an
#'   `ATTRIBUTE LIST` block after every other keyword (bldgrds, like RIL,
#'   reads this block separately from -- and after -- the rest of the input
#'   file, so it must come last). Defaults to
#'   [bldgrds_default_attributes()]. **A node point shapefile cannot be
#'   created without an `ATTRIBUTE LIST` block**, so passing
#'   `attribute_list = list()` together with a `node_shapefile` is an error
#'   here rather than a silently broken run.
#' @param overwrite If TRUE, allow overwriting an existing input file.
#'
#' @return The input file path, invisibly.
#' @export
bldgrds_input <- function(dem,
                          scratch_dir,
                          aspect_length,
                          plan_length,
                          gradient_length_scale,
                          d8_plan_coefficient,
                          d8_aspect_coefficient,
                          d8_aspect_length,
                          d8_plan_length,
                          initiation_buffer_inner,
                          initiation_buffer_outer,
                          initiation_area_override,
                          local_relief_threshold_high,
                          local_relief_threshold_low,
                          area_slope_threshold_low_gradient,
                          area_slope_threshold_high_gradient,
                          plan_curvature_threshold_low_gradient,
                          plan_curvature_threshold_high_gradient,
                          minimum_threshold_flow_length,
                          minimum_channel_length = 0,
                          local_relief_raster = NOFILE,
                          use_existing_files = FALSE,
                          calibrate = FALSE,
                          excavate_line = NOFILE,
                          excavate_line_buffer = NULL,
                          water_mask = NOFILE,
                          water_mask_min_patch_size = NULL,
                          water_mask_set_to_min_elevation = FALSE,
                          water_mask_incise_to_center = FALSE,
                          water_mask_min_gradient = NULL,
                          water_mask_buffer_radius = NULL,
                          water_mask_preclude_initiation = FALSE,
                          node_shapefile = NOFILE,
                          node_splits = NULL,
                          attribute_list = bldgrds_default_attributes(),
                          overwrite = TRUE) {

  # A node point shapefile is built from the ATTRIBUTE LIST block; bldgrds
  # has no fallback for it the way it does for the node-list database, so
  # catch an empty list here rather than let it fail silently downstream.
  if (!is_missing_path(node_shapefile) && length(attribute_list) == 0L) {
    stop("bldgrds_input(): node_shapefile was supplied but attribute_list ",
         "is empty. A node point shapefile requires an ATTRIBUTE LIST ",
         "block; pass bldgrds_default_attributes() or your own list.",
         call. = FALSE)
  }

  writer <- input_writer("bldgrds", scratch_dir, "input_bldgrds.txt", overwrite)

  # --- DEM and scratch space -----------------------------------------------
  writer$keyword("DEM FILE", normalize_raster_path(dem, must_exist = TRUE))
  writer$keyword("SCRATCH", normalize_file_path(scratch_dir))

  # --- Road-crossing/culvert excavation, water masking (each optional) -----
  writer$optional("EXCAVATE LINE",
                  normalize_file_path(excavate_line, extension = "shp"),
                  BUFFER = excavate_line_buffer)
  writer$optional("WATER MASK",
                  normalize_file_path(water_mask),
                  `MINIMUM PATCH SIZE` = water_mask_min_patch_size,
                  if (isTRUE(water_mask_set_to_min_elevation)) "SET TO MINIMUM ELEVATION",
                  if (isTRUE(water_mask_incise_to_center)) "INCISE TO CENTER",
                  `MINIMUM GRADIENT` = water_mask_min_gradient,
                  `BUFFER RADIUS` = water_mask_buffer_radius,
                  if (isTRUE(water_mask_preclude_initiation)) "PRECLUDE INITIATION")

  # --- A bare flag keyword: no arguments, just the keyword and its colon ---
  if (isTRUE(calibrate)) writer$keyword("CALIBRATE")

  # --- Derivative length scales ---------------------------------------------
  writer$keyword("USE SMOOTHED ASPECT", `LENGTH SCALE` = aspect_length)
  writer$keyword("PLAN CURVATURE LENGTH SCALE", plan_length)
  writer$keyword("GRADIENT LENGTH SCALE", gradient_length_scale)

  # --- D8 flow-direction weighting ------------------------------------------
  write_keyword_group(writer, "D8 COEFFICIENTS",
                      c(PLAN = d8_plan_coefficient, ASPECT = d8_aspect_coefficient))
  write_keyword_group(writer, "D8 LENGTH SCALES",
                      c(ASPECT = d8_aspect_length, PLAN = d8_plan_length))

  # --- Channel initiation ----------------------------------------------------
  write_keyword_group(writer, "INITIATION BUFFER",
                      c(INNER = initiation_buffer_inner,
                        OUTER = initiation_buffer_outer,
                        `AREA OVERRIDE` = initiation_area_override))
  writer$optional("LOCAL RELIEF RASTER",
                  normalize_raster_path(local_relief_raster, must_exist = TRUE))
  write_keyword_group(writer, "LOCAL RELIEF THRESHOLD",
                      c(HIGH = local_relief_threshold_high,
                        LOW = local_relief_threshold_low))
  writer$keyword("AREA SLOPE THRESHOLD LOW GRADIENT",
                 area_slope_threshold_low_gradient)
  writer$keyword("AREA SLOPE THRESHOLD HIGH GRADIENT",
                 area_slope_threshold_high_gradient)
  writer$keyword("PLAN CURVATURE THRESHOLD LOW GRADIENT",
                 plan_curvature_threshold_low_gradient)
  writer$keyword("PLAN CURVATURE THRESHOLD HIGH GRADIENT",
                 plan_curvature_threshold_high_gradient)
  writer$keyword("MINIMUM THRESHOLD FLOW LENGTH", minimum_threshold_flow_length)
  writer$keyword("MINIMUM CHANNEL LENGTH", minimum_channel_length)

  # --- Reuse existing intermediate files, or not ----------------------------
  writer$keyword("USE EXISTING FILES", if (isTRUE(use_existing_files)) "YES" else "NO")

  # --- Output ----------------------------------------------------------------
  writer$optional("OUTPUT NODE POINT SHAPEFILE",
                  normalize_file_path(node_shapefile), SPLITS = node_splits)

  # --- Attribute list. Must come last: ReadInput() reads it via a separate
  #     input%readlist() call made only after its main keyword-reading loop
  #     has finished with the rest of the file (the same order RIL_input()
  #     uses for the same reason). ---------------------------------------
  writer$line("")
  write_attribute_list(writer, attribute_list, indent = 0L,
                       end_keyword = "END ATTRIBUTE LIST")

  invisible(writer$file_path)
}


#' Create an input file for Fortran program bldgrds, enforcing an existing channel network
#'
#' bldgrds normally initiates new channels from area-slope/plan-curvature/local-relief
#' thresholds (see [bldgrds_input()]). This builder instead traces the channel network from an
#' existing, previously-mapped channel-network polyline shapefile (`channel_mask`), excavating
#' ("digging") it into the DEM, and precludes any new channel initiation outside that mask --
#' `NO NEW CHANNELS` is always written. None of [bldgrds_input()]'s channel-initiation-criteria
#' arguments (aspect/plan-curvature/D8 length scales, initiation buffer, area-slope/plan-
#' curvature/local-relief thresholds, minimum flow length, ...) are needed or accepted here,
#' since no new initiation happens.
#'
#' Reproduces the keyword grammar of a working reference run (Skykomish project): `DEM FILE`,
#' `SCRATCH`, `CHANNEL MASK` (w/ `FILE`, `DIG`, `RADIUS`, `DIRECTIONAL`, `INIT ALL`),
#' `NO NEW CHANNELS`, `OUTPUT NODE POINT SHAPEFILE` (w/ `SPLITS`), `DRAINAGE WING RASTER`,
#' `HAND RASTER` (w/ `FLOW THRESHOLD`, `NORMALIZE`), `TWI RASTER` (w/ `GRADIENT LENGTH SCALE`),
#' and an `ATTRIBUTE LIST` block -- closed with `END LIST`, NOT `END ATTRIBUTE LIST` like
#' [bldgrds_input()] writes. That's a real discrepancy between the two reference files this
#' package's `bldgrds_input()`/`bldgrds_enforce_input()` are each built from, not a typo here --
#' see the "bldgrds" entries in this package's `CLAUDE.md`; the two haven't been reconciled.
#'
#' @param dem Input DEM (full path).
#' @param scratch_dir Scratch directory; the input file is written here.
#' @param channel_mask Existing channel-network polyline shapefile to enforce (`CHANNEL MASK:
#'   FILE`).
#' @param channel_mask_dig Depth (DEM elevation units) to excavate/burn `channel_mask` into the
#'   DEM (`CHANNEL MASK: DIG`).
#' @param channel_mask_radius Radius used when excavating `channel_mask` into the DEM (`CHANNEL
#'   MASK: RADIUS`).
#' @param channel_mask_directional If TRUE, treat `channel_mask` as directional (`CHANNEL MASK:
#'   DIRECTIONAL`).
#' @param channel_mask_init_all If TRUE, seed channel initiation at every `channel_mask` cell,
#'   not just its ends (`CHANNEL MASK: INIT ALL`).
#' @param node_shapefile Optional: output node point shapefile (`OUTPUT NODE POINT SHAPEFILE`).
#' @param node_splits Optional: number of pieces to split each channel segment into for
#'   `node_shapefile`. Only meaningful with `node_shapefile`.
#' @param drainage_wing_raster Optional: output drainage wing raster (`DRAINAGE WING RASTER`).
#' @param hand_raster Optional: output HAND (height above nearest drainage) raster (`HAND
#'   RASTER`).
#' @param hand_flow_threshold Flow-accumulation threshold (as a proportion) for `hand_raster`
#'   (`HAND RASTER: FLOW THRESHOLD`). Only meaningful with `hand_raster`.
#' @param hand_normalize If TRUE, normalize `hand_raster` (`HAND RASTER: NORMALIZE`). Only
#'   meaningful with `hand_raster`.
#' @param twi_raster Optional: output topographic wetness index raster (`TWI RASTER`).
#' @param twi_gradient_length_scale Length in meters over which gradient is measured for
#'   `twi_raster` (`TWI RASTER: GRADIENT LENGTH SCALE`). Only meaningful with `twi_raster`.
#' @param attribute_list List of [attribute_spec()] objects, written as an `ATTRIBUTE LIST`
#'   block after every other keyword (same ordering constraint as [bldgrds_input()] -- must come
#'   last). Defaults to [bldgrds_default_attributes()]. **A node point shapefile cannot be
#'   created without an `ATTRIBUTE LIST` block**, so passing `attribute_list = list()` together
#'   with a `node_shapefile` is an error here rather than a silently broken run.
#' @param overwrite If TRUE, allow overwriting an existing input file.
#'
#' @return The input file path, invisibly.
#' @export
bldgrds_enforce_input <- function(dem,
                                  scratch_dir,
                                  channel_mask,
                                  channel_mask_dig,
                                  channel_mask_radius = 0,
                                  channel_mask_directional = FALSE,
                                  channel_mask_init_all = FALSE,
                                  node_shapefile = NOFILE,
                                  node_splits = NULL,
                                  drainage_wing_raster = NOFILE,
                                  hand_raster = NOFILE,
                                  hand_flow_threshold = NULL,
                                  hand_normalize = FALSE,
                                  twi_raster = NOFILE,
                                  twi_gradient_length_scale = NULL,
                                  attribute_list = bldgrds_default_attributes(),
                                  overwrite = TRUE) {

  # A node point shapefile is built from the ATTRIBUTE LIST block; bldgrds has no fallback for
  # it the way it does for the node-list database, so catch an empty list here rather than let
  # it fail silently downstream. Same check as bldgrds_input().
  if (!is_missing_path(node_shapefile) && length(attribute_list) == 0L) {
    stop("bldgrds_enforce_input(): node_shapefile was supplied but attribute_list is empty. ",
         "A node point shapefile requires an ATTRIBUTE LIST block; pass ",
         "bldgrds_default_attributes() or your own list.", call. = FALSE)
  }

  writer <- input_writer("bldgrds_enforce", scratch_dir, "input_bldgrds_enforce.txt", overwrite)

  # --- DEM and scratch space -----------------------------------------------
  writer$keyword("DEM FILE", normalize_raster_path(dem, must_exist = TRUE))
  writer$keyword("SCRATCH", normalize_file_path(scratch_dir))

  # --- Enforce the existing channel network; preclude any new initiation ---------------------
  # channel_mask is a polyline shapefile, not a raster -- normalize_file_path() (not
  # normalize_raster_path(), which strips a raster extension and checks existence against
  # RASTER_EXTENSIONS) with extensions = "shp" matches how other shapefile arguments are
  # checked elsewhere (e.g. distance_to_road()'s road_shapefile).
  writer$keyword("CHANNEL MASK",
                 FILE = normalize_file_path(channel_mask, must_exist = TRUE,
                                            extensions = "shp"),
                 DIG = channel_mask_dig,
                 RADIUS = channel_mask_radius,
                 if (isTRUE(channel_mask_directional)) "DIRECTIONAL",
                 if (isTRUE(channel_mask_init_all)) "INIT ALL")
  writer$keyword("NO NEW CHANNELS")

  # --- Outputs ---------------------------------------------------------------
  writer$optional("OUTPUT NODE POINT SHAPEFILE",
                  normalize_file_path(node_shapefile), SPLITS = node_splits)
  writer$optional("DRAINAGE WING RASTER", normalize_file_path(drainage_wing_raster))
  writer$optional("HAND RASTER", normalize_file_path(hand_raster),
                  `FLOW THRESHOLD` = hand_flow_threshold,
                  if (isTRUE(hand_normalize)) "NORMALIZE")
  writer$optional("TWI RASTER", normalize_file_path(twi_raster),
                  `GRADIENT LENGTH SCALE` = twi_gradient_length_scale)

  # --- Attribute list. Must come last: ReadInput() reads it via a separate
  #     input%readlist() call made only after its main keyword-reading loop
  #     has finished with the rest of the file (the same order bldgrds_input()/RIL_input()
  #     use for the same reason). Closed with END LIST here, not END ATTRIBUTE LIST -- see the
  #     note in this function's docs above. -------------------------------------------------
  writer$line("")
  write_attribute_list(writer, attribute_list, indent = 0L, end_keyword = "END LIST")

  invisible(writer$file_path)
}


#' Create an input file for Fortran program PFA_debris_flow
#'
#' PFA_debris_flow is one of a sequence of programs used to assemble data
#' files for the PFA recalibration of the landslide initiation and
#' debris-flow-runout models. Debris-flow runout tracks from the DOGAMI
#' Special Paper 53 study are matched to lidar-DEM flow paths, and the terrain
#' attributes associated with runout extent are assembled and written in a
#' form suitable for a Cox survival model with time-dependent covariates.
#'
#' The multinomial logistic regression coefficients for the probability of
#' scour, deposition and transitional flow are inputs here; their calibration
#' against the ODF 1996 Storm Study surveys is described in the Quatro file
#' PFA_runout.
#'
#' @param dem Input DEM (full path).
#' @param init_points Initiation-point shapefile.
#' @param geo_poly Rock-type polygon shapefile.
#' @param stand_age LEMMA stand-age .flt raster.
#' @param tracks DOGAMI debris-flow-track polyline shapefile.
#' @param radius Search radius in meters for matching a DEM flow path to a
#'   DOGAMI track.
#' @param initRadius Search radius in meters around initiation points.
#' @param length_scale Length in meters over which to measure elevation
#'   derivatives.
#' @param slope_intercept,slope_coef Intercept and coefficient of the slope
#'   term.
#' @param bulk_coef Bulking coefficient.
#' @param init_width,init_length Initiation zone dimensions in meters.
#' @param DF_width Debris-flow track width in meters.
#' @param alpha Proportion of the debris-flow cross-sectional volume deposited
#'   per unit length of track.
#' @param uncensored If TRUE, treat all endpoint tracks as uncensored.
#' @param scratch_dir Scratch directory.
#' @param out_surv Output file for the Cox survival model.
#' @param out_point Output initiation point shapefile, carrying scour and
#'   deposit volume fields.
#' @param out_kaplanMeier Output Kaplan-Meier file. NOTE: this argument was
#'   accepted but never written by the original function, and is still not
#'   written here, because the keyword the Fortran expects is unknown. Add a
#'   `writer$optional("OUTPUT KAPLAN-MEIER FILE", ...)` line at the marked
#'   spot once it is confirmed.
#' @param coef List of 20 multinomial logistic regression coefficients.
#'   Elements 1-10 are the scour terms and 11-20 the transition terms, each
#'   run being intercept, gradient, normal curvature, tangent curvature, stand
#'   age, then five rock-type terms.
#' @param overwrite If TRUE, allow overwriting an existing input file.
#'
#' @return The input file path, invisibly.
#' @export
PFA_debris_flow_input <- function(dem,
                                  init_points,
                                  geo_poly,
                                  stand_age,
                                  tracks,
                                  radius,
                                  initRadius,
                                  length_scale,
                                  slope_intercept,
                                  slope_coef,
                                  bulk_coef,
                                  init_width,
                                  init_length,
                                  DF_width,
                                  alpha,
                                  uncensored,
                                  scratch_dir,
                                  out_surv,
                                  out_point,
                                  out_kaplanMeier,
                                  coef,
                                  overwrite = TRUE) {

  # Fail early and clearly rather than deep inside the coefficient block.
  stopifnot(length(coef) >= 20L)

  writer <- input_writer("PFA_debris_flow", scratch_dir,
                         "input_PFA_debris_flow.txt", overwrite)

  # --- Input layers -------------------------------------------------------
  writer$keyword("DEM", normalize_raster_path(dem, must_exist = TRUE))
  writer$keyword("INITIATION POINT SHAPEFILE",
                 normalize_file_path(init_points))
  writer$keyword("TRACK LINE SHAPEFILE", normalize_file_path(tracks))
  writer$keyword("ROCK TYPE POLYGON SHAPEFILE",
                 normalize_file_path(geo_poly))
  writer$keyword("STAND AGE RASTER", normalize_raster_path(stand_age))

  # --- Matching and measurement parameters --------------------------------
  writer$keyword("RADIUS", radius)
  writer$keyword("INITIATION POINT RADIUS", initRadius)
  writer$keyword("LENGTH SCALE", length_scale)

  # --- Runout model parameters --------------------------------------------
  writer$keyword("SLOPE",
                 INTERCEPT = slope_intercept, COEFFICIENT = slope_coef)
  writer$keyword("BULKING FACTOR", COEFFICIENT = bulk_coef)
  writer$keyword("INITIATION DIMENSION",
                 WIDTH = init_width, LENGTH = init_length)
  writer$keyword("TRACK WIDTH", DF_width)
  writer$keyword("ALPHA", alpha)

  # --- Censoring flag. Accepts a logical or the strings "TRUE"/"true". -----
  treat_as_uncensored <- isTRUE(uncensored) ||
    grepl("TRUE", as.character(uncensored), ignore.case = TRUE)
  if (treat_as_uncensored) writer$keyword("UNCENSORED")

  # --- Outputs ------------------------------------------------------------
  writer$keyword("SCRATCH DIRECTORY", normalize_file_path(scratch_dir))
  writer$keyword("OUTPUT SURVIVAL FILE", normalize_file_path(out_surv))
  writer$keyword("OUTPUT INITIATION POINT SHAPEFILE",
                 normalize_file_path(out_point))
  # Add the Kaplan-Meier output line here once its keyword is confirmed.
  writer$line("")

  # --- Multinomial logistic regression coefficients -----------------------
  # The scour and transition blocks have identical structure and differ only
  # in their label and in a ten-element offset into `coef`, so one helper
  # writes both. In the original the twenty lines were spelled out twice.
  writer$keyword("MULTINOMIAL LOGISTIC REGRESSION COEFFICIENTS LIST")

  write_coefficient_block <- function(outcome_label, coefficient_offset) {
    writer$keyword(paste0("  ", outcome_label, " INTERCEPT"),
                   coef[[coefficient_offset + 1L]])
    writer$keyword(paste0("  ", outcome_label, " GRADIENT"),
                   coef[[coefficient_offset + 2L]])
    writer$keyword(paste0("  ", outcome_label, " NORMAL CURVATURE"),
                   coef[[coefficient_offset + 3L]])
    writer$keyword(paste0("  ", outcome_label, " TANGENT CURVATURE"),
                   coef[[coefficient_offset + 4L]])
    writer$keyword(paste0("  ", outcome_label, " STAND AGE"),
                   coef[[coefficient_offset + 5L]])
    writer$keyword(paste0("  ", outcome_label, " ROCK TYPE"),
                   SEDIMENTARY           = coef[[coefficient_offset + 6L]],
                   VOLCANIC              = coef[[coefficient_offset + 7L]],
                   `IGNEOUS-METAMORPHIC` = coef[[coefficient_offset + 8L]],
                   VOLCANICLASTIC        = coef[[coefficient_offset + 9L]],
                   UNCONSOLIDATED        = coef[[coefficient_offset + 10L]])
  }

  write_coefficient_block("SCOUR",       0L)
  write_coefficient_block("TRANSITION", 10L)

  writer$keyword("END LIST")

  invisible(writer$file_path)
}


#' Create an input file for Fortran program LocalRelief, requesting DEV only
#'
#' LocalRelief builds a deviation-from-local-elevation (DEV) raster among
#' several other products; DEV is the only one requested here, so the
#' downsampling keywords are both pinned to 1.
#'
#' @param dem Input DEM (full path).
#' @param radius Radius in meters over which DEV is calculated.
#' @param out_raster Output binary floating point (.flt) raster.
#' @param scratch_dir Scratch directory; the input file is written here.
#' @param overwrite If TRUE, allow overwriting an existing input file.
#'
#' @return The input file path, invisibly.
#' @export
DEV_input <- function(dem,
                      radius,
                      out_raster,
                      scratch_dir,
                      overwrite = TRUE) {

  writer <- input_writer("LocalRelief", scratch_dir,
                         "input_DEV.txt", overwrite,
                         note = "getting DEV only.")

  # --- Input and radius ---------------------------------------------------
  writer$keyword("DEM", normalize_raster_path(dem, must_exist = TRUE))
  writer$keyword("RADIUS", radius)

  # --- No downsampling: evaluate DEV at every cell ------------------------
  writer$keyword("DOWN SAMPLE", 1)
  writer$keyword("SAMPLE INTERVAL", 1)

  # --- Output -------------------------------------------------------------
  writer$keyword("OUTPUT DEV RASTER", normalize_file_path(out_raster))
  writer$keyword("SCRATCH DIRECTORY", normalize_file_path(scratch_dir))

  invisible(writer$file_path)
}


#' Create an input file for Fortran program resample
#'
#' resample downsamples a raster in whole multiples of its pixel size, with no
#' interpolation: output pixel corners coincide with input pixel corners, the
#' pixels are simply bigger. It is used mostly on DEMs, where the pixels are
#' better thought of as grid points or cells.
#'
#' @param in_raster Input raster (full path).
#' @param skip Number of cells to skip between retained cells.
#' @param out_raster Output resampled raster.
#' @param scratch_dir Scratch directory; the input file is written here.
#' @param overwrite If TRUE, allow overwriting an existing input file.
#'
#' @return The input file path, invisibly.
#' @export
resample_input <- function(in_raster,
                           skip,
                           out_raster,
                           scratch_dir,
                           overwrite = TRUE) {

  writer <- input_writer("resample", scratch_dir,
                         "input_resample.txt", overwrite)

  # BUG FIX: the original expanded and stripped the input raster name into a
  # local variable called `dem`, then wrote the untouched `in_raster` to the
  # file, so neither the path expansion nor the extension stripping ever
  # reached the Fortran program.
  writer$keyword("INPUT RASTER",
                 normalize_raster_path(in_raster, must_exist = TRUE))
  writer$keyword("OUTPUT RASTER", normalize_file_path(out_raster))
  writer$keyword("SKIP", skip)
  writer$keyword("SCRATCH DIRECTORY", normalize_file_path(scratch_dir))

  invisible(writer$file_path)
}

#' The attribute list from the reference RIL input file
#'
#' Reproduces the bare identifier/area/geometry attribute block of the Post
#' Mortem RIL run. The reference run's precipitation-dependent chain (mean
#' annual precipitation sampled from a raster, feeding published
#' hydraulic-geometry equations for mean annual flow, channel width and
#' depth -- Kresch, D.L., 1998, Water Resources Investigations Report
#' 98-4160, for mean annual flow; Magirl and Olsen, 2009, for channel width
#' and depth) is NOT reproduced here: a `precip_raster` is a property of the
#' `MEAN ANNUAL PRECIP` attribute entry itself, not a separate argument to
#' this function, so it can only be supplied by writing that attribute (and
#' whatever depends on it) directly into an attribute-list text file read
#' with [read_attribute_list_file()] -- see `RIL_attributes_example.txt` in
#' the UnstableSlopes repo for a ready-to-copy example carrying that same
#' chain.
#'
#' @return A list of [attribute_spec()] objects.
#' @export
ril_default_attributes <- function() {

  list(
    attribute_spec("NODE ID"),
    attribute_spec("ORDER"),
    attribute_spec("CONTRIBUTING AREA", output_field = "AREA_SQKM"),
    attribute_spec("AS2"),
    attribute_spec("PLAN"),
    attribute_spec("SLOPE"),
    attribute_spec("LENGTH_M"),
    attribute_spec("FLUVIAL", type = "I4", field_length = 1)
  )
}

#'--- RIL_input, Create an input file for Fortran program RIL ---
#'
#' RIL delineates a channel network on a DEM and classifies the terrain around
#' it, producing a raster of riparian, inner-gorge and hollow landform classes
#' together with an optional node point shapefile carrying per-node attributes.
#'
#' The work happens in four stages, and the parameters below are grouped to
#' match:
#' \enumerate{
#'   \item Channel initiation: where the network begins, set by the AS2, plan
#'     curvature and gradient thresholds, and where it becomes fluvial rather
#'     than colluvial.
#'   \item Valley floor delineation, by channel depth.
#'   \item Hollow delineation, by gradient and by tangential and profile
#'     curvature, each with a primary and a secondary threshold.
#'   \item Inner gorge delineation, by gradient.
#' }
#' Stages 2 to 4 each end with a hole-filling and small-patch-removal pass, so
#' each has its own maximum hole size and minimum patch size.
#'
#' Every parameter defaults to the value used in the Post Mortem reference run,
#' so a minimal call needs only `dem`, `scratch_dir` and `out_RIL`.
#'
#' Grouped thresholds are given as named numeric vectors whose names are
#' written verbatim, so `c(PRIMARY = 0.3, SECONDARY = 0.25)` becomes
#' `PRIMARY = 0.3, SECONDARY = 0.25`.
#'
#' @param dem Input DEM (full path).
#' @param scratch_dir Scratch directory; the input file is written here.
#' @param out_RIL Output RIL raster (.flt). The one required output.
#' @param radius Radius in meters for calculating elevation derivatives.
#' @param closest_node Named vector of the CLOSEST NODE RASTER search
#'   parameters: NUM WIDTHS, MAX RADIUS, MIN RADIUS.
#' @param as2_threshold AS2 threshold in square meters for channel initiation.
#' @param plan_curvature_threshold Plan curvature threshold for initiation.
#' @param gradient_threshold Gradient threshold for initiation, based on a
#'   centered-window gradient.
#' @param fluvial_area_threshold Contributing area in square kilometers above
#'   which a channel is treated as fluvial.
#' @param valley_depth_max Maximum channel depth in meters for delineating the
#'   valley floor.
#' @param valley_buffer Meters to buffer the valley floor by, where doing so
#'   yields an inner gorge.
#' @param valley_max_hole,hollow_max_hole,gorge_max_hole Fill holes smaller
#'   than this many square meters, per stage.
#' @param valley_min_patch,gorge_min_patch Remove patches smaller than this
#'   many square meters, per stage.
#' @param hollow_gradient Named vector: PRIMARY, SECONDARY, DIF1, DIF2 and
#'   MIN POLY GRAD gradient thresholds for hollows.
#' @param hollow_tangential Named vector: PRIMARY, SECONDARY and MIN POLY TAN
#'   tangential curvature thresholds for hollows.
#' @param hollow_profile Named vector: PRIMARY, SECONDARY and MIN POLY PROF
#'   profile curvature thresholds for hollows.
#' @param grad_proportion Named vector GRAD and PROP: a polygon is kept when at
#'   least PROP of it exceeds gradient GRAD.
#' @param tan_proportion Named vector TAN and PROP, as above for tangential
#'   curvature.
#' @param fill_hollow_embayments If TRUE, write the FILL HOLLOW EMBAYMENTS flag.
#' @param hollow_area_threshold Minimum maximum-contributing-area within a
#'   hollow, in square meters.
#' @param hollow_min_patch Named vector INITIAL and FINAL: minimum patch size in
#'   square meters before and after hollow processing.
#' @param gorge_gradient Named vector: PRIMARY, SECONDARY, DIF1 and DIF2
#'   gradient thresholds for inner gorges.
#' @param slope_thresholds Named vector SLOPE and STEEP: upslope-gradient
#'   thresholds classifying the remaining hillslope into "other", non-steep
#'   and steep terrain.
#' @param curve_thresholds Named vector CONVERGENT and DIVERGENT: tangential
#'   curvature thresholds splitting hillslope terrain into convergent,
#'   divergent and planar classes.
#' @param edge_smoothing_iterations Smoothing iterations applied to all output
#'   polygon edges.
#' @param road_shapefile Input road polyline shapefile (optional).
#' @param road_buffer Buffer in meters around roads. Used only when a road
#'   shapefile is supplied.
#' @param in_closest_node,in_dist_to_channel,in_drainage_wing,in_valley_floor,in_upgrad,in_downgrad,in_grad,in_tangential,in_plan,in_prof
#'   Optional precomputed input rasters. Each pairs with the matching `out_`
#'   argument from a previous run. RIL only skips recomputing elevation
#'   derivatives when *all six* of in_upgrad, in_downgrad, in_grad,
#'   in_tangential, in_plan and in_prof are supplied; partial sets are
#'   ignored and every derivative is recalculated.
#' @param out_closest_node,out_dist_to_channel,out_drainage_wing,out_valley_floor,out_upgrad,out_downgrad,out_grad,out_tangential,out_plan,out_prof
#'   Optional intermediate rasters to write out for reuse.
#' @param out_nodes Optional output node point shapefile carrying the
#'   attributes.
#' @param out_zero_order Optional output zero-order basin raster.
#' @param attribute_list List of [attribute_spec()] objects to compute for each
#'   node. Defaults to [ril_default_attributes()].
#' @param use_ltd If FALSE, write the USE STANDARD D8 flag so channel-node
#'   drainage wings are built with standard D8 flow paths rather than D8-LTD.
#' @param debug If TRUE, write the DEBUG flag. RIL then writes extra
#'   diagnostic rasters (upgrad, downgrad, node ID) to hardcoded paths under
#'   `c:\\temp`, which must already exist.
#' @param overwrite If TRUE, allow overwriting an existing input file.
#'
#' @return The input file path, invisibly.
#' @export
RIL_input <- function(dem,
                      scratch_dir,
                      out_RIL,
                      radius = 7.5,
                      closest_node = c(`NUM WIDTHS` = 0,
                                       `MAX RADIUS` = 250,
                                       `MIN RADIUS` = 250),
                      as2_threshold = 1000,
                      plan_curvature_threshold = 0.005,
                      gradient_threshold = 0.5,
                      fluvial_area_threshold = 0.07,
                      valley_depth_max = 5.0,
                      valley_buffer = 2.5,
                      valley_max_hole = 1000,
                      valley_min_patch = 20,
                      hollow_gradient = c(PRIMARY = 0.3,
                                          SECONDARY = 0.25,
                                          DIF1 = 1.0,
                                          DIF2 = 0.45,
                                          `MIN POLY GRAD` = 0.2),
                      hollow_tangential = c(PRIMARY = -0.005,
                                            SECONDARY = -0.02,
                                            `MIN POLY TAN` = 0.0),
                      hollow_profile = c(PRIMARY = -0.01,
                                         SECONDARY = -0.06,
                                         `MIN POLY PROF` = -0.015),
                      grad_proportion = c(GRAD = 0.5, PROP = 0.1),
                      tan_proportion = c(TAN = 0.0, PROP = 0.5),
                      fill_hollow_embayments = TRUE,
                      hollow_area_threshold = 5,
                      hollow_max_hole = 1000,
                      hollow_min_patch = c(INITIAL = 0, FINAL = 300),
                      gorge_gradient = c(PRIMARY = 0.7,
                                         SECONDARY = 0.45,
                                         DIF1 = 0.95,
                                         DIF2 = 0.7),
                      gorge_max_hole = 1000,
                      gorge_min_patch = 0,
                      slope_thresholds = c(SLOPE = 0.4,
                                           STEEP = 0.7),
                      curve_thresholds = c(CONVERGENT = 0.01,
                                           DIVERGENT = -0.01),
                      edge_smoothing_iterations = 11,
                      road_shapefile = NOFILE,
                      road_buffer = 15,
                      in_closest_node = NOFILE,
                      in_dist_to_channel = NOFILE,
                      in_drainage_wing = NOFILE,
                      in_valley_floor = NOFILE,
                      in_upgrad = NOFILE,
                      in_downgrad = NOFILE,
                      in_grad = NOFILE,
                      in_tangential = NOFILE,
                      in_plan = NOFILE,
                      in_prof = NOFILE,
                      out_closest_node = NOFILE,
                      out_dist_to_channel = NOFILE,
                      out_drainage_wing = NOFILE,
                      out_valley_floor = NOFILE,
                      out_upgrad = NOFILE,
                      out_downgrad = NOFILE,
                      out_grad = NOFILE,
                      out_tangential = NOFILE,
                      out_plan = NOFILE,
                      out_prof = NOFILE,
                      out_nodes = NOFILE,
                      out_zero_order = NOFILE,
                      attribute_list = ril_default_attributes(),
                      use_ltd = TRUE,
                      debug = FALSE,
                      overwrite = TRUE) {

  writer <- input_writer("RIL", scratch_dir, overwrite = overwrite)

  # --- Input DEM and general parameters -----------------------------------
  writer$keyword("DEM", normalize_raster_path(dem, must_exist = TRUE))
  writer$keyword("SCRATCH DIRECTORY", normalize_file_path(scratch_dir))
  writer$keyword("RADIUS", radius)
  write_keyword_group(writer, "CLOSEST NODE RASTER", closest_node)
  if (!isTRUE(use_ltd)) writer$keyword("USE STANDARD D8")
  if (isTRUE(debug)) writer$keyword("DEBUG")

  # --- Channel initiation -------------------------------------------------
  writer$line("")
  writer$line("# Channel-initiation parameters")
  writer$keyword("AS2 THRESHOLD", as2_threshold, .indent = 2L)
  writer$keyword("PLAN CURVATURE THRESHOLD", plan_curvature_threshold,
                 .indent = 2L)
  writer$keyword("GRADIENT THRESHOLD", gradient_threshold, .indent = 2L)
  writer$keyword("AREA THRESHOLD FOR FLUVIAL CHANNEL", fluvial_area_threshold,
                 .indent = 2L)

  # --- Valley floor -------------------------------------------------------
  writer$line("")
  writer$line("# Valley-floor parameters")
  writer$keyword("VALLEY DEPTH MAX", valley_depth_max, .indent = 2L)
  writer$keyword("VALLEY BUFFER", valley_buffer, .indent = 2L)
  writer$keyword("MAXIMUM HOLE SIZE VALLEY", valley_max_hole, .indent = 2L)
  writer$keyword("MINIMUM PATCH SIZE VALLEY", valley_min_patch, .indent = 2L)

  # --- Hollows ------------------------------------------------------------
  writer$line("")
  writer$line("# Hollow parameters")
  write_keyword_group(writer, "HOLLOW GRADIENT THRESHOLDS",
                      hollow_gradient, indent = 2L)
  write_keyword_group(writer, "HOLLOW TANGENTIAL CURVATURE THRESHOLDS",
                      hollow_tangential, indent = 2L)
  write_keyword_group(writer, "HOLLOW PROFILE CURVATURE THRESHOLDS",
                      hollow_profile, indent = 2L)
  write_keyword_group(writer, "GRAD PROPORTION GREATER THAN",
                      grad_proportion, indent = 2L)
  write_keyword_group(writer, "TAN PROPORTION GREATER THAN",
                      tan_proportion, indent = 2L)
  if (isTRUE(fill_hollow_embayments)) {
    writer$keyword("FILL HOLLOW EMBAYMENTS", .indent = 2L)
  }
  writer$keyword("HOLLOW AREA THRESHOLD", hollow_area_threshold, .indent = 2L)
  writer$keyword("MAXIMUM HOLE SIZE HOLLOW", hollow_max_hole, .indent = 2L)
  write_keyword_group(writer, "MINIMUM PATCH SIZE HOLLOW",
                      hollow_min_patch, indent = 2L)

  # --- Inner gorge --------------------------------------------------------
  writer$line("")
  writer$line("# Inner-gorge parameters")
  write_keyword_group(writer, "INNER GORGE GRADIENT THRESHOLDS",
                      gorge_gradient, indent = 2L)
  writer$keyword("MAXIMUM HOLE SIZE GORGE", gorge_max_hole, .indent = 2L)
  writer$keyword("MINIMUM PATCH SIZE GORGE", gorge_min_patch, .indent = 2L)

  # --- Hillslope ---------------------------------------------------------
  writer$line("")
  writer$line("# Hillslope parameters")
  write_keyword_group(writer, "SLOPE THRESHOLDS",
                      slope_thresholds, indent = 2L)
  write_keyword_group(writer, "CURVE THRESHOLDS",
                      curve_thresholds, indent = 2L)

  # --- Polygon smoothing and roads ----------------------------------------
  writer$line("")
  writer$keyword("EDGE SMOOTHING ITERATIONS", edge_smoothing_iterations,
                 .indent = 2L)
  writer$optional("INPUT ROAD SHAPEFILE",
                  normalize_file_path(road_shapefile),
                  BUFFER = road_buffer, .indent = 2L)


  # --- Optional precomputed inputs ----------------------------------------
  writer$line("")
  writer$line("# Optional input files")
  optional_inputs <- list(
    "INPUT CLOSEST NODE RASTER"          = in_closest_node,
    "INPUT DISTANCE TO CHANNEL RASTER"   = in_dist_to_channel,
    "INPUT DRAINAGE WING RASTER"         = in_drainage_wing,
    "INPUT VALLEY FLOOR RASTER"          = in_valley_floor,
    "INPUT UPGRAD RASTER"                = in_upgrad,
    "INPUT DOWNGRAD RASTER"              = in_downgrad,
    "INPUT GRADIENT RASTER"              = in_grad,
    "INPUT TANGENTIAL CURVATURE RASTER"  = in_tangential,
    "INPUT PLAN CURVATURE RASTER"        = in_plan,
    "INPUT PROFILE CURVATURE RASTER"     = in_prof
  )
  for (keyword in names(optional_inputs)) {
    writer$optional(keyword,
                    normalize_raster_path(optional_inputs[[keyword]],
                                          must_exist = TRUE),
                    .indent = 2L)
  }

  # --- Optional intermediate outputs --------------------------------------
  writer$line("")
  writer$line("# Optional output files")
  optional_outputs <- list(
    "OUTPUT CLOSEST NODE RASTER"         = out_closest_node,
    "OUTPUT DISTANCE TO CHANNEL RASTER"  = out_dist_to_channel,
    "OUTPUT DRAINAGE WING RASTER"        = out_drainage_wing,
    "OUTPUT VALLEY FLOOR RASTER"         = out_valley_floor,
    "OUTPUT UPGRAD RASTER"               = out_upgrad,
    "OUTPUT DOWNGRAD RASTER"             = out_downgrad,
    "OUTPUT GRADIENT RASTER"             = out_grad,
    "OUTPUT TANGENTIAL CURVATURE RASTER" = out_tangential,
    "OUTPUT PLAN CURVATURE RASTER"       = out_plan,
    # NB: RIL.f90 abbreviates this keyword to "PROF" (unlike the matching
    # "INPUT PROFILE CURVATURE RASTER" keyword above, which spells it out).
    "OUTPUT PROF CURVATURE RASTER"       = out_prof,
    "OUTPUT ZERO ORDER BASINS"           = out_zero_order
  )
  for (keyword in names(optional_outputs)) {
    writer$optional(keyword,
                    normalize_raster_path(optional_outputs[[keyword]]),
                    .indent = 2L)
  }
  writer$optional("OUTPUT NODE POINT SHAPEFILE",
                  normalize_file_path(out_nodes), .indent = 2L)

  # --- The one required output --------------------------------------------
  writer$line("")
  writer$line("# Required output file")
  writer$keyword("OUTPUT RIL RASTER", normalize_raster_path(out_RIL),
                 .indent = 2L)

  # --- Attribute list. RIL closes the block with END LIST. ----------------
  # RIL.f90 looks up 'AS2', 'PLAN' and 'SLOPE' attribute fields on the channel
  # node list it builds from this block, and aborts if any is missing. Now
  # that attribute_list can be supplied freely rather than always coming from
  # ril_default_attributes() (which already includes all three), a
  # caller-supplied list that omits one would otherwise write a file RIL.exe
  # rejects at run time -- so append whichever are missing.
  attribute_names <- vapply(attribute_list, function(a) a$name, character(1))
  for (required in setdiff(c("AS2", "PLAN", "SLOPE"), attribute_names)) {
    attribute_list <- c(attribute_list, list(attribute_spec(required)))
  }

  writer$line("")
  writer$line("# Node attributes")
  write_attribute_list(writer, attribute_list,
                       indent = 2L, end_keyword = "END LIST")

  invisible(writer$file_path)
}


#' Default attribute list for Fortran program ValleyFloor
#'
#' Reproduces the bare identifier/area portion of the `ATTRIBUTE LIST` block
#' of a working reference run (Cherry project): node and channel identifiers
#' plus contributing area. That reference run's precipitation-dependent chain
#' (mean annual precipitation feeding a Puget Sound regional mean-annual-flow
#' equation -- Kresch, 1998, WRIR96-4208 -- and Magirl and Olsen, 2009, for
#' channel width and depth) is NOT reproduced here -- see the note on
#' [ril_default_attributes()]: a `precip_raster` is a property of the
#' `MEAN ANNUAL PRECIP` attribute entry itself, supplied only via an
#' attribute-list text file read with [read_attribute_list_file()] (see
#' `valleyfloor_attributes_example.txt` in the UnstableSlopes repo).
#'
#' @return A list of [attribute_spec()] objects.
#' @seealso [valleyfloor_input()]
#' @export
valleyfloor_default_attributes <- function() {

  list(
    attribute_spec("NODE ID", output_field = "NODE_ID"),
    attribute_spec("CHANNEL ID"),
    attribute_spec("CONTRIBUTING AREA", len = 12, deccnt = 4,
                   output_field = "AREA_SQKM")
  )
}


#' Locate ValleyFloor's binary output data file
#'
#' ValleyFloor writes its per-channel results (distance/height/depth above
#' channel, valley width, node attributes) to a binary data file named
#' `valleyfloor_<ID>.dat`, in the *DEM's own directory* -- `ValleyFloor.f90`
#' builds this path as `TRIM(refDEM%path)//'valleyfloor_'//TRIM(dataID)//
#' '.dat'`, not anywhere under `scratch_dir`. The file is opened
#' unconditionally on every run (for reading, if `read_data = TRUE`; for
#' writing/replacing otherwise), regardless of which, if any, output rasters
#' were requested -- it is ValleyFloor's real output, and the only one
#' that's never optional.
#'
#' `<ID>` is `data_id` if one was supplied (matching an explicit
#' `DATA ID` keyword -- see [valleyfloor_input()]'s `data_id` argument, only
#' honored in `all_channels = TRUE` mode); otherwise it falls back to the ID
#' `DEM_module`'s `resolveDEMname()` derives from the DEM's own file name:
#' everything after the first underscore in its (extensionless) base name,
#' e.g. `elev_Cherry` -> `Cherry`, or the whole base name if it has no
#' underscore.
#'
#' @param dem Input DEM (full path), exactly as passed to
#'   [valleyfloor_input()]/[valleyfloor()].
#' @param data_id Optional: an explicit data ID, matching `data_id` as
#'   actually passed to [valleyfloor_input()]/[valleyfloor()]. Omit (or pass
#'   `NOFILE`) to derive the ID from `dem` the same way ValleyFloor itself
#'   does when no `DATA ID` keyword is written.
#'
#' @return The full path of the `valleyfloor_<ID>.dat` file.
#' @seealso [valleyfloor_input()], [valleyfloor()]
#' @export
valleyfloor_dat_file <- function(dem, data_id = NOFILE) {

  dem_path <- normalize_raster_path(dem)
  # dirname() hands back forward slashes even when dem_path is
  # backslash-separated; re-normalize so join_path() below doesn't produce a
  # path mixing both separators.
  dem_dir  <- normalizePath(dirname(dem_path), winslash = "\\", mustWork = FALSE)
  base     <- basename(dem_path)

  resolved_id <- if (!is_missing_path(data_id)) {
    as.character(data_id)
  } else {
    # resolveDEMname(): DEMID is everything after the first underscore in
    # the base name, or the whole base name if there is none.
    underscore <- regexpr("_", base, fixed = TRUE)
    if (underscore > 0L) substring(base, underscore + 1L) else base
  }

  join_path(dem_dir, paste0("valleyfloor_", resolved_id, ".dat"))
}


#' Create an input file for Fortran program ValleyFloor
#'
#' ValleyFloor builds a cell-by-cell height/depth-above-channel surface
#' across the valley surrounding each selected channel, then, when requested,
#' measures valley width at a series of depth-above-channel thresholds and/or
#' (with `method = 4`) fits a TIN-based flood-inundation surface. It always
#' requires an `ATTRIBUTE LIST` block -- `ValleyFloor.f90` aborts with "No
#' attributes specified" if `input%readList()` returns none -- so
#' `attribute_list` defaults to [valleyfloor_default_attributes()] rather
#' than an empty list.
#'
#' ValleyFloor's actual output is a binary per-channel data file,
#' `valleyfloor_<ID>.dat`, written unconditionally next to the DEM (see
#' [valleyfloor_dat_file()]) -- not any of the `out_elev`/`out_depth`/
#' `out_elev_bil`/`out_depth_bil`/`out_flood_height`/`out_flood_depth`/
#' `out_d8` rasters below, all of which are genuinely optional: ValleyFloor
#' runs fine with none of them requested, and this builder never requires
#' one.
#'
#' Reproduces the keyword grammar of a working reference run (Cherry
#' project). Several keywords that appear (commented out) in older copies of
#' `input_valleyfloor.txt` -- `INPUT WATER MASK RASTER`, `INPUT REACH
#' SHAPEFILE`, `CHANNEL BUFFER`, `VALLEY MASK`, and every legacy mode switch
#' except `MEASURE VALLEY WIDTHS` (`CALCULATE HEIGHT ABOVE CHANNEL`,
#' `CREATE TOPOGRAPHICALLY DEFINED CHANNEL MASK`, `DETERMINE FLOW TYPE FOR
#' CHANNEL NODES`, `MAP VALLEY FLOOR LANDFORMS`) -- have **no matching
#' `CASE` in `ValleyFloor.f90`'s `readInputFile()`**; its `SELECT CASE` has
#' no `CASE DEFAULT`, so they would be silently parsed and dropped as
#' no-ops rather than doing anything, and so are not written here. In
#' particular, height-above is triggered by requesting `out_elev`/
#' `out_depth`/`out_elev_bil`/`out_depth_bil`, not by a
#' "calculate height above" switch. See this package's `CLAUDE.md` for that
#' finding.
#'
#' The `ATTRIBUTE LIST` block is read by a separate, shared `ReadList()`
#' routine (`..\\modules\\Utilities.f90`) that rewinds the input file and
#' rescans it from the top looking specifically for that keyword. So, unlike
#' the "must come last" constraint documented for [RIL_input()] and
#' [bldgrds_input()], its position in the file does not actually matter here
#' -- confirmed against `ReadList()`'s source, not just inferred -- because
#' `ValleyFloor.f90`'s own main keyword loop also has no `CASE DEFAULT` and so
#' just ignores the block's lines as it passes over them on its way to
#' whatever keyword comes next. This writes it in the same position as the
#' reference file anyway: after the main keywords, before
#' `HEIGHT ABOVE STEP LIST`/`VALLEY WIDTH WINDOW IN CHANNEL WIDTHS`.
#'
#' @param dem Input DEM (full path).
#' @param scratch_dir Scratch directory; the input file is written here.
#' @param all_channels If TRUE (the default), process every channel at least
#'   `min_chan_width` wide (`ALL CHANNELS`). Set FALSE and supply
#'   `channel_list` to process only specific channel numbers instead
#'   (`CHANNEL LIST`).
#' @param channel_list Integer vector of channel numbers to process. Only
#'   used, and required, when `all_channels = FALSE`.
#' @param min_chan_width,min_chan_area Minimum channel width (meters) or
#'   contributing area for a channel to be processed. Only meaningful with
#'   `all_channels = TRUE`.
#' @param write_data,read_data If TRUE, write/read the per-channel binary
#'   data files (`WRITE DATA`/`READ DATA`).
#' @param overwrite_data If TRUE, overwrite existing per-channel data files
#'   (`OVERWRITE EXISTING DATA FILES`), or, with `channel_list`, the bare
#'   `OVERWRITE` flag.
#' @param data_id Optional: a data-file ID tag (`DATA ID`). Only meaningful
#'   with `all_channels = TRUE`.
#' @param method Height-above algorithm: 1 for the default per-cell
#'   weighted-average method, 4 for the TIN-based method (required for
#'   `out_flood_height`/`out_flood_depth`).
#' @param sampling_interval DEM sampling interval, in cells, for the
#'   height-above calculation (`SAMPLING INTERVAL`).
#' @param valley_buffer Named vector: `CHANNEL WIDTHS`, `MIN RADIUS` and
#'   `MAX RADIUS` bounding how far from the channel to search for valley
#'   cells (`VALLEY BUFFER`).
#' @param expansion_factor,second_expansion Multipliers expanding the search
#'   radius around each valley cell (`EXPANSION FACTOR`); `second_expansion`
#'   is optional.
#' @param mask_by_watershed If TRUE, restrict the valley mask to the local
#'   watershed (`MASK BY WATERSHED`).
#' @param max_depth_dif Maximum channel-depth difference used to limit the
#'   distance-to-channel search (`MAXIMUM DEPTH DIFFERENCE`).
#' @param min_elev_dif Minimum absolute elevation difference that
#'   height-above is forced to extend to (`MINIMUM ELEVATION DIFFERENCE`).
#' @param weighting_exponent Exponent weighting nearby channel cells more
#'   heavily in the height-above calculation (`WEIGHTING EXPONENT`).
#' @param smoothing_iterations Smoothing passes applied to the height-above
#'   surface (`SMOOTHING ITERATIONS`).
#' @param inundation_smoothing_max_radius Maximum radius in meters for
#'   smoothing the flood-inundation surface (`INUNDATION SMOOTHING MAX
#'   RADIUS`). Only meaningful with `method = 4`.
#' @param max_depths_above,max_depths_below Limits, in channel depths, on how
#'   far above/below the channel height-above is mapped (`MAXIMUM DEPTHS
#'   ABOVE`/`MAXIMUM DEPTHS BELOW`).
#' @param max_dif_dist_chan_dist_node Maximum allowed difference between
#'   distance-to-channel measured by cell and by node
#'   (`MAXIMUM DIFFERENCE, DIST CHAN DIST NODE`).
#' @param monotonic If not NULL, write the `MONOTONIC` flag forcing each
#'   channel profile to increase in elevation upstream; TRUE/FALSE write
#'   `YES`/`NO`.
#' @param minimum_channel_length Optional: minimum channel length in meters
#'   (`MINIMUM CHANNEL LENGTH`).
#' @param fill_dem_holes If TRUE, fill holes in the DEM before processing
#'   (`FILL DEM HOLES`).
#' @param fill_dem_holes_max_size,fill_dem_holes_min_elev Optional: maximum
#'   hole size (square meters) and minimum fill elevation. Only meaningful
#'   with `fill_dem_holes = TRUE`.
#' @param input_d8 Optional: precomputed D8 flow-direction raster
#'   (`INPUT D8 RASTER`), reused instead of recalculating it.
#' @param out_elev,out_depth Optional: output height-above-channel elevation
#'   and depth rasters (`OUTPUT ELEV RASTER`/`OUTPUT DEPTH RASTER`). Either
#'   one triggers the height-above calculation.
#' @param out_elev_bil,out_depth_bil Optional: the same, written as `.bil`
#'   rather than `.flt` (`OUTPUT ELEV BIL`/`OUTPUT DEPTH BIL`).
#' @param out_flood_height,out_flood_depth Optional: output flood-inundation
#'   height and depth rasters (`OUTPUT FLOOD HEIGHT RASTER`/`OUTPUT FLOOD
#'   DEPTH RASTER`). Require `method = 4`.
#' @param out_d8 Optional: output D8 flow-direction raster
#'   (`OUTPUT D8 RASTER`).
#' @param measure_valley_widths If TRUE, measure valley widths at each of
#'   `height_above_steps` (`MEASURE VALLEY WIDTHS`).
#' @param height_above_steps Numeric vector of channel-depth steps, in
#'   channel depths above the channel, at which to measure valley width
#'   (`HEIGHT ABOVE STEP LIST`).
#' @param valley_width_window Window length, in channel widths, over which
#'   valley width is averaged along the channel (`VALLEY WIDTH WINDOW IN
#'   CHANNEL WIDTHS`).
#' @param attribute_list List of [attribute_spec()] objects to compute for
#'   each channel node, written as an `ATTRIBUTE LIST` block. Defaults to
#'   [valleyfloor_default_attributes()]; ValleyFloor requires at least one
#'   attribute.
#' @param upstream_node,downstream_node Optional: restrict processing to the
#'   channel segment between these two node IDs.
#' @param time_it If TRUE, write the `TIME IT` flag, timing the height-above
#'   calculation.
#' @param debug If TRUE, write the `DEBUG` flag.
#' @param overwrite If TRUE, allow overwriting an existing input file.
#'
#' @return The input file path, invisibly.
#' @seealso [valleyfloor_default_attributes()], [valleyfloor()]
#' @export
valleyfloor_input <- function(dem,
                              scratch_dir,
                              all_channels = TRUE,
                              channel_list = NULL,
                              min_chan_width = 1.0,
                              min_chan_area = NULL,
                              write_data = TRUE,
                              read_data = FALSE,
                              overwrite_data = TRUE,
                              data_id = NOFILE,
                              method = 4,
                              sampling_interval = 1,
                              valley_buffer = c(`CHANNEL WIDTHS` = 150,
                                                `MIN RADIUS` = 20,
                                                `MAX RADIUS` = 1000),
                              expansion_factor = 1.5,
                              second_expansion = 3.0,
                              mask_by_watershed = TRUE,
                              max_depth_dif = 15,
                              min_elev_dif = 2.0,
                              weighting_exponent = 1.0,
                              smoothing_iterations = 0,
                              inundation_smoothing_max_radius = 20,
                              max_depths_above = 12,
                              max_depths_below = -12,
                              max_dif_dist_chan_dist_node = 0.25,
                              monotonic = NULL,
                              minimum_channel_length = NULL,
                              fill_dem_holes = FALSE,
                              fill_dem_holes_max_size = NULL,
                              fill_dem_holes_min_elev = NULL,
                              input_d8 = NOFILE,
                              out_elev = NOFILE,
                              out_depth = NOFILE,
                              out_elev_bil = NOFILE,
                              out_depth_bil = NOFILE,
                              out_flood_height = NOFILE,
                              out_flood_depth = NOFILE,
                              out_d8 = NOFILE,
                              measure_valley_widths = TRUE,
                              height_above_steps = c(0, 0.25, 0.5, 0.75, 1.0,
                                                     1.5, 2.0, 2.5, 3.0, 4.0,
                                                     5.0, 7.5, 10.0),
                              valley_width_window = 20,
                              attribute_list = valleyfloor_default_attributes(),
                              upstream_node = NULL,
                              downstream_node = NULL,
                              time_it = FALSE,
                              debug = FALSE,
                              overwrite = TRUE) {

  writer <- input_writer("ValleyFloor", scratch_dir, "input_valleyfloor.txt",
                         overwrite)

  # --- Basic instructions ---------------------------------------------------
  writer$keyword("MEASURE VALLEY WIDTHS",
                 if (isTRUE(measure_valley_widths)) "YES" else "NO")

  # --- DEM and scratch space -------------------------------------------------
  writer$keyword("DEM", normalize_raster_path(dem, must_exist = TRUE))
  writer$optional("INPUT D8 RASTER",
                  normalize_raster_path(input_d8, must_exist = TRUE))
  writer$keyword("METHOD", method)
  writer$keyword("SAMPLING INTERVAL", sampling_interval)

  if (isTRUE(fill_dem_holes)) {
    writer$keyword("FILL DEM HOLES",
                   `MAX HOLE SIZE` = fill_dem_holes_max_size,
                   `MIN ELEV` = fill_dem_holes_min_elev)
  }

  # --- Channel selection ------------------------------------------------------
  if (isTRUE(all_channels)) {
    all_channels_args <- list(`MINIMUM WIDTH` = min_chan_width)
    if (!is.null(min_chan_area)) {
      all_channels_args <- c(all_channels_args, list(`MINIMUM AREA` = min_chan_area))
    }
    if (isTRUE(write_data))  all_channels_args <- c(all_channels_args, list("WRITE DATA"))
    if (isTRUE(read_data))   all_channels_args <- c(all_channels_args, list("READ DATA"))
    if (!is_missing_path(data_id)) {
      all_channels_args <- c(all_channels_args, list(`DATA ID` = data_id))
    }
    all_channels_args <- c(all_channels_args, list(
      `OVERWRITE EXISTING DATA FILES` = if (isTRUE(overwrite_data)) "YES" else "NO"
    ))
    do.call(writer$keyword, c(list("ALL CHANNELS"), all_channels_args))
  } else {
    if (is.null(channel_list) || length(channel_list) == 0L) {
      stop("valleyfloor_input(): channel_list must be supplied when ",
           "all_channels = FALSE", call. = FALSE)
    }
    writer$keyword("CHANNEL LIST", if (isTRUE(overwrite_data)) "OVERWRITE" else NULL)
    for (i in seq_along(channel_list)) {
      writer$keyword(as.character(i), channel_list[i], .indent = 2L)
    }
    writer$keyword("END LIST")
  }

  if (!is.null(monotonic)) {
    writer$keyword("MONOTONIC", if (isTRUE(monotonic)) "YES" else "NO")
  }
  if (!is.null(minimum_channel_length)) {
    writer$keyword("MINIMUM CHANNEL LENGTH", minimum_channel_length)
  }
  if (!is.null(upstream_node))   writer$keyword("UPSTREAM NODE", upstream_node)
  if (!is.null(downstream_node)) writer$keyword("DOWNSTREAM NODE", downstream_node)
  writer$keyword("MAXIMUM DIFFERENCE, DIST CHAN DIST NODE",
                 max_dif_dist_chan_dist_node)

  # --- Valley-floor geometry --------------------------------------------------
  write_keyword_group(writer, "VALLEY BUFFER", valley_buffer)
  writer$keyword("EXPANSION FACTOR", expansion_factor,
                 `SECOND EXPANSION` = second_expansion)
  if (isTRUE(mask_by_watershed)) writer$keyword("MASK BY WATERSHED")
  writer$keyword("MAXIMUM DEPTH DIFFERENCE", max_depth_dif)
  writer$keyword("MINIMUM ELEVATION DIFFERENCE", min_elev_dif)
  writer$keyword("WEIGHTING EXPONENT", weighting_exponent)
  writer$keyword("SMOOTHING ITERATIONS", smoothing_iterations)

  # --- Flood inundation (method = 4) ------------------------------------------
  writer$keyword("INUNDATION SMOOTHING MAX RADIUS",
                 inundation_smoothing_max_radius)
  writer$keyword("MAXIMUM DEPTHS ABOVE", max_depths_above)
  writer$keyword("MAXIMUM DEPTHS BELOW", max_depths_below)

  if (isTRUE(time_it)) writer$keyword("TIME IT")
  if (isTRUE(debug))   writer$keyword("DEBUG")

  # --- Outputs -----------------------------------------------------------------
  writer$optional("OUTPUT ELEV RASTER",        normalize_raster_path(out_elev))
  writer$optional("OUTPUT DEPTH RASTER",       normalize_raster_path(out_depth))
  writer$optional("OUTPUT ELEV BIL",           normalize_file_path(out_elev_bil))
  writer$optional("OUTPUT DEPTH BIL",          normalize_file_path(out_depth_bil))
  writer$optional("OUTPUT FLOOD HEIGHT RASTER", normalize_raster_path(out_flood_height))
  writer$optional("OUTPUT FLOOD DEPTH RASTER",  normalize_raster_path(out_flood_depth))
  writer$optional("OUTPUT D8 RASTER",          normalize_raster_path(out_d8))

  # --- Attribute list. ReadList() rewinds and rescans the whole file for
  #     this keyword, so -- unlike RIL_input()/bldgrds_input() -- it does not
  #     actually have to come last; see Details above. Written here to match
  #     the reference file's own layout. -------------------------------------
  writer$line("")
  write_attribute_list(writer, attribute_list, indent = 0L, end_keyword = "END LIST")

  # --- Valley-width steps and window, after the attribute list, as in the
  #     reference file. ---------------------------------------------------------
  writer$line("")
  writer$keyword("HEIGHT ABOVE STEP LIST")
  for (i in seq_along(height_above_steps)) {
    writer$keyword(paste0("STEP ", i), height_above_steps[i], .indent = 2L)
  }
  writer$keyword("END LIST")
  writer$keyword("VALLEY WIDTH WINDOW IN CHANNEL WIDTHS", valley_width_window)

  invisible(writer$file_path)
}
