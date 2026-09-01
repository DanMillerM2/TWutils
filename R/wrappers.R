# wrappers.R --------------------------------------------------------
#
# Wrappers around the NetStream Fortran executables. Each function here
# does the same three things:
#
#   1. Check its arguments.
#   2. Build the program's ASCII input file, using a writer from
#      input_file_utilitiess.R.
#   3. Run the executable, then read whatever it produced back into R.


# =========================================================================
# SECTION 0.  LOCAL HELPERS
# =========================================================================

#' Collect argument problems and report them all at once
#'
#' The original file checked arguments with a repeated pattern:
#'
#'     if (radius == 0.) { print("Radius not specified"); err <- -1 }
#'
#' and then, many lines later, `if (err < 0) stop("Error with input
#' arguments")`. That prints the detail and errors with the summary, so the
#' two halves can end up in different places in a log, and only the summary
#' is catchable by the caller.
#'
#' This accumulates the problems instead and raises one error naming all of
#' them, so a caller fixing a call sees everything wrong with it in one pass.
#'
#' @return A list of check functions plus `report()`, which raises the error.
#' @noRd
argument_checker <- function() {

  problems <- character(0)
  note <- function(message_text) problems <<- c(problems, message_text)

  # A file that must already exist. Raster arguments are normally given
  # without an extension, because that is the form the Fortran programs
  # resolve for themselves, so the bare name and every RASTER_EXTENSIONS
  # variant of it all count as a match.
  check_input_file <- function(path, label, extensions = RASTER_EXTENSIONS) {
    if (is_missing_path(path)) {
      note(paste0(label, " not specified"))
      return(invisible(NULL))
    }
    # Delegate, so that which extensions count as a match -- and the wording
    # used when none do -- stay defined in one place, alongside the writers
    # that apply the same rule. require_path_exists() signals an error;
    # this checker collects problems rather than raising them one at a time,
    # so the message is captured rather than propagated.
    tryCatch(require_path_exists(path, extensions, label),
             error = function(condition) note(conditionMessage(condition)))
    invisible(NULL)
  }

  # A directory that must already exist.
  check_directory <- function(path, label) {
    if (is_missing_path(path)) {
      note(paste0(label, " not specified"))
    } else if (!dir.exists(path)) {
      note(paste0(label, " does not exist: ", path))
    }
    invisible(NULL)
  }

  # A path that only has to be named; it does not exist yet because the
  # program is about to create it.
  check_supplied <- function(value, label) {
    if (is_missing_path(value)) note(paste0(label, " not specified"))
    invisible(NULL)
  }

  # A numeric argument that must be present and in range. The original used
  # sentinel values (0, -9999) to mean "not supplied", so the same test
  # covers both "missing" and "out of bounds".
  check_number <- function(value, label, minimum = 0, allow_minimum = FALSE) {
    is_unusable <- !is.numeric(value) || length(value) == 0L || is.na(value[1]) ||
      identical(as.numeric(value[1]), -9999) ||
      (allow_minimum && value[1] < minimum) ||
      (!allow_minimum && value[1] <= minimum)
    if (is_unusable) {
      note(paste0(label, " not specified or out of range"))
    }
    invisible(NULL)
  }

  report <- function() {
    if (length(problems) > 0L) {
      stop("problem with arguments:\n  ",
           paste(problems, collapse = "\n  "), call. = FALSE)
    }
    invisible(TRUE)
  }

  list(input_file = check_input_file,
       directory  = check_directory,
       supplied   = check_supplied,
       number     = check_number,
       note       = note,
       report     = report)
}


#' --- Run one of the NetStream executables against an input file ---
#'
#' Locate the executable, paste a command string, call system(), test the
#' exit code.
#'
#' @param program_name Base name of the executable, without ".exe".
#' @param input_file Full path of the ASCII input file to hand it.
#' @param executable_dir Directory holding the executable. There is no
#'   default location: the caller must always supply it.
#'
#' @return The exit code (0), invisibly. Stops on any non-zero exit code.
#' @noRd
run_program <- function(program_name, input_file, executable_dir = NULL) {

  if (is.null(executable_dir) || is_missing_path(executable_dir)) {
    stop("directory for executable file not provided", call. = FALSE)
  }

  executable <- file.path(executable_dir, paste0(program_name, ".exe"))
  if (!file.exists(executable)) {
    stop("executable not found: ", executable, call. = FALSE)
  }

  # Both paths are quoted, so directories containing spaces work.
  command <- paste0('"', executable, '" "', input_file, '"')

  exit_code <- system(command, wait = TRUE)
  if (exit_code != 0) {
    stop(program_name, " failed with exit code ", exit_code, call. = FALSE)
  }

  invisible(exit_code)
}


#' --- Split "TYPE, file name" raster specifications into their two parts ---
#'
#' The `rasters` argument of elev_deriv() and makegrids_input() is a character
#' vector whose elements each hold an attribute type and an output file name
#' separated by a comma.
#'
#' @param raster_specs Character vector of "TYPE, file" strings.
#' @return A list with character vectors `type` and `file`.
#' @noRd
parse_raster_spec <- function(raster_specs) {
  raster_specs <- unlist(raster_specs, use.names = FALSE)
  list(
    type = trimws(sub(",.*$", "", raster_specs)),
    file = trimws(sub("^[^,]*,", "", raster_specs))
  )
}


#' --- Line numbers in an input file whose keyword matches a pattern ---
#'
#' @param input_lines A tibble from get_input_file().
#' @param pattern Regular expression matched against each keyword.
#' @return An integer vector of line numbers.
#' @noRd
keyword_line_numbers <- function(input_lines, pattern) {
  n_lines <- if (is.data.frame(input_lines)) nrow(input_lines) else length(input_lines)
  keywords <- vapply(seq_len(n_lines),
                     function(i) get_keyword(input_lines, i),
                     character(1))
  which(!is.na(keywords) & grepl(pattern, keywords))
}


#' --- Require that an input file contains a set of keywords ---
#'
#' Replaces the `dem_found` / `dir_found` / `scale_found` flag variables and
#' the long if/else-if chain that set them, which appeared in five functions.
#'
#' @param input_lines A tibble from get_input_file().
#' @param patterns Character vector of keyword patterns that must be present.
#' @return TRUE, invisibly. Stops naming every missing keyword.
#' @noRd
require_keywords <- function(input_lines, patterns) {
  is_present <- vapply(patterns,
                       function(p) length(keyword_line_numbers(input_lines, p)) > 0L,
                       logical(1))
  if (!all(is_present)) {
    stop("bad input file format; missing keyword(s): ",
         paste(patterns[!is_present], collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}


#' --- Read the first argument value of a keyword in an input file ---
#'
#' @param input_lines A tibble from get_input_file().
#' @param pattern Keyword pattern.
#' @param default Value to return when the keyword is absent.
#' @return The first argument value on the first matching line.
#' @noRd
keyword_value <- function(input_lines, pattern, default = NOFILE) {
  matching_lines <- keyword_line_numbers(input_lines, pattern)
  if (length(matching_lines) == 0L) return(default)
  parse_arg(get_args(input_lines, matching_lines[1]))$Value
}


# =========================================================================
# SECTION 1.  ELEVATION DERIVATIVES
# =========================================================================

#' --- elev_deriv, Elevation Derivatives ---
#'
#' Provide \code{SpatRasters} of elevation derivatives. elev_deriv operates in
#' one of three modes, depending on which arguments are supplied:
#' \enumerate{
#'   \item As a wrapper for the Fortran makegrids executable, with an existing
#'     makegrids input file.
#'   \item As a wrapper for makegrids, with the input file constructed here.
#'   \item To read existing raster files from disk.
#' }
#' In modes 1 and 2, makegrids creates the requested rasters and writes them
#' to disk as floating point binary files; they are then read and returned as
#' a \code{SpatRaster}. In mode 3 the rasters are read straight from disk.
#'
#' @param input_file Character: an existing makegrids input file (optional).
#'   Selects mode 1.
#' @param rasters Character vector. Each element holds two strings separated
#'   by a comma: the derivative type, then the file name. These are input
#'   files to read in mode 3 and output files to write in mode 2. Available
#'   derivatives are GRADIENT, PLAN CURVATURE, PROFILE CURVATURE, NORMAL SLOPE
#'   CURVATURE, TANGENTIAL CURVATURE and MEAN CURVATURE.
#' @param dem Character: file name (full path) of the DEM. Supplying it
#'   selects mode 2; omitting both it and `input_file` selects mode 3.
#' @param length_scale Numeric: diameter in meters over which to measure the
#'   requested derivatives.
#' @param scratch_dir Character: scratch directory. The makegrids input file
#'   is written here.
#' @param executable_dir Character: directory holding MakeGrids.exe. There is
#'   no default location: it must always be supplied.
#'
#' @return A \code{SpatRaster} with one layer per requested derivative.
#' @export
elev_deriv <- function(input_file = NOFILE,
                       rasters = character(0),
                       dem = NOFILE,
                       length_scale = 0,
                       scratch_dir = NOFILE,
                       executable_dir = NULL) {

  run_makegrids <- TRUE

  if (!is_missing_path(input_file)) {

    # --- Mode 1: an existing input file tells us everything --------------
    # get_input_file() replaces the file.choose() / file.exists() / readLines()
    # / tibble() block, and errors on an empty or missing file itself.
    input_lines <- get_input_file(input_file)
    require_keywords(input_lines, c("DEM", "SCRATCH DIRECTORY", "LENGTH SCALE"))

    # Each GRID line carries the derivative type as its first argument and the
    # output file as its second.
    grid_lines <- keyword_line_numbers(input_lines, "GRID")
    grid_arguments <- lapply(grid_lines,
                             function(i) parse_args(get_args(input_lines, i)))
    raster_types <- vapply(grid_arguments, function(a) a$Value[1], character(1))
    raster_files <- vapply(grid_arguments, function(a) a$Value[2], character(1))

  } else if (is_missing_path(dem)) {

    # --- Mode 3: read rasters that already exist -------------------------
    if (length(rasters) == 0L) {
      stop("must provide a DEM or an existing raster file to read",
           call. = FALSE)
    }
    raster_spec  <- parse_raster_spec(rasters)
    raster_types <- raster_spec$type
    # makegrids currently reads only .flt.
    raster_files <- add_extension_if_missing(raster_spec$file, "flt")

    check <- argument_checker()
    for (raster_file in raster_files) check$input_file(raster_file, raster_file)
    check$report()

    run_makegrids <- FALSE

  } else {

    # --- Mode 2: build an input file, then run makegrids -----------------
    check <- argument_checker()
    check$input_file(dem, "dem")
    check$directory(scratch_dir, "scratch_dir")
    check$number(length_scale, "length_scale")
    if (length(rasters) == 0L) {
      check$note("must request at least one derivative to calculate")
    }
    check$report()

    raster_spec  <- parse_raster_spec(rasters)
    raster_types <- raster_spec$type
    raster_files <- add_extension_if_missing(raster_spec$file, "flt")

    # makegrids_input() now returns the path of the file it wrote, so the
    # hand-built paste0(scratch_dir, "\\makegrids_input.txt") is gone.
    input_file <- makegrids_input(dem, length_scale, scratch_dir, rasters)
  }

  if (run_makegrids) run_program("MakeGrids", input_file, executable_dir)

  # terra::rast() accepts a vector of file names and returns one layer per
  # file, replacing the accumulate-with-c() loop.
  out_grid <- terra::rast(raster_files)
  names(out_grid) <- raster_types
  out_grid
}


# =========================================================================
# SECTION 2.  FLOW ACCUMULATION
# =========================================================================

#' --- contributing_area, Contributing area for a storm of fixed duration ---
#'
#' Provide a \code{SpatRaster} giving the upslope contributing area to each
#' DEM cell for shallow subsurface groundwater flow. D-infinity flow paths are
#' traced upslope from each cell using a spatially variable Darcy velocity
#' (v = k * sin(gradient)) that depends on the specified, spatially uniform,
#' saturated hydraulic conductivity and on the gradient of each cell.
#'
#' Operates in the same three modes as [elev_deriv()].
#'
#' @param input_file Character: an existing "partial" input file (optional).
#' @param raster Character: output contributing-area raster, or an existing
#'   one to read in mode 3.
#' @param dem Character: file name (full path) of the DEM.
#' @param length_scale Numeric: length in meters over which to smooth the DEM.
#'   This is the length used to measure gradient and curvature and to guide
#'   flow directions.
#' @param k Numeric: uniform saturated hydraulic conductivity, meters per hour.
#' @param d Numeric: storm duration in hours.
#' @param scratch_dir Character: scratch directory.
#' @param executable_dir Character: directory holding Partial.exe.
#'
#' @return A \code{SpatRaster} of contributing area for a storm of d hours.
#' @export
contributing_area <- function(input_file = NOFILE,
                              raster = NOFILE,
                              dem = NOFILE,
                              length_scale = 0,
                              k = 0,
                              d = 0,
                              scratch_dir = NOFILE,
                              executable_dir = NULL) {

  run_partial <- TRUE

  if (!is_missing_path(input_file)) {

    # --- Mode 1: existing input file -------------------------------------
    input_lines <- get_input_file(input_file)
    require_keywords(input_lines,
                     c("DEM", "SCRATCH DIRECTORY", "LENGTH SCALE",
                       "DURATION", "CONDUCTIVITY"))
    raster <- add_extension_if_missing(
      keyword_value(input_lines, "OUTPUT RASTER"), "flt")

  } else if (is_missing_path(dem)) {

    # --- Mode 3: read an existing raster ---------------------------------
    check <- argument_checker()
    check$input_file(raster, "raster")
    check$report()
    raster <- add_extension_if_missing(raster, "flt")
    run_partial <- FALSE

  } else {

    # --- Mode 2: build an input file, then run partial -------------------
    check <- argument_checker()
    check$input_file(dem, "dem")
    check$directory(scratch_dir, "scratch_dir")
    check$supplied(raster, "output raster")
    check$number(k, "k (saturated hydraulic conductivity)")
    check$number(d, "d (storm duration)")
    check$number(length_scale, "length_scale")
    check$report()

    input_file <- accum_input(dem, k, d, length_scale, scratch_dir, raster)
    raster <- add_extension_if_missing(raster, "flt")
  }

  if (run_partial) run_program("Partial", input_file, executable_dir)

  terra::rast(raster)
}


#' --- bldgrds_nochannels, Total contributing area ---
#'
#' Provide a \code{SpatRaster} giving the total upslope contributing area to
#' each DEM cell, with D-infinity flow paths traced upslope from every cell
#' and no channel delineation.
#'
#' Operates in the same three modes as [elev_deriv()].
#'
#' @param input_file Character: an existing bldgrds input file (optional).
#' @param raster Character: output flow accumulation raster, or an existing
#'   one to read in mode 3.
#' @param dem Character: file name (full path) of the DEM.
#' @param aspect_length Numeric: length in meters over which aspect is
#'   measured.
#' @param plan_length Numeric: length in meters over which plan curvature is
#'   measured.
#' @param grad_length Numeric: length in meters over which gradient is
#'   measured.
#' @param scratch_dir Character: scratch directory.
#' @param executable_dir Character: directory holding bldgrds.exe.
#'
#' @return A \code{SpatRaster} of total contributing area.
#' @export
bldgrds_nochannels <- function(input_file = NOFILE,
                               raster = NOFILE,
                               dem = NOFILE,
                               aspect_length = 0,
                               plan_length = 0,
                               grad_length = 0,
                               scratch_dir = NOFILE,
                               executable_dir = NULL) {

  run_bldgrds <- TRUE

  if (!is_missing_path(input_file)) {

    # --- Mode 1: existing input file -------------------------------------
    input_lines <- get_input_file(input_file)
    require_keywords(input_lines,
                     c("DEM", "SCRATCH DIRECTORY", "USE SMOOTHED ASPECT",
                       "PLAN CURVATURE LENGTH SCALE", "GRADIENT LENGTH SCALE"))
    raster <- add_extension_if_missing(
      keyword_value(input_lines, "OUTPUT FLOW ACCUMULATION RASTER"), "flt")

  } else if (is_missing_path(dem)) {

    # --- Mode 3: read an existing raster ---------------------------------
    check <- argument_checker()
    check$input_file(raster, "raster")
    check$report()
    raster <- add_extension_if_missing(raster, "flt")
    run_bldgrds <- FALSE

  } else {

    # --- Mode 2: build an input file, then run bldgrds -------------------
    check <- argument_checker()
    check$input_file(dem, "dem")
    check$directory(scratch_dir, "scratch_dir")
    check$supplied(raster, "output raster")
    check$number(aspect_length, "aspect_length")
    check$number(plan_length, "plan_length")
    check$number(grad_length, "grad_length")
    check$report()

    input_file <- bldgrds_nochannels_input(dem, aspect_length, plan_length,
                                           grad_length, raster, scratch_dir)
    raster <- add_extension_if_missing(raster, "flt")
  }

  if (run_bldgrds) run_program("bldgrds", input_file, executable_dir)

  terra::rast(raster)
}


# =========================================================================
# SECTION 3.  DISTANCE TO ROAD
# =========================================================================

#' Distance to the nearest road, in meters
#'
#' Provide a \code{SpatRaster} giving the distance to the nearest road for
#' every DEM cell. Operates in the same three modes as [elev_deriv()].
#'
#' @param input_file Character: an existing distanceToRoad input file
#'   (optional).
#' @param raster Character: output raster, or an existing one to read in
#'   mode 3.
#' @param dem Character: file name (full path) of the DEM.
#' @param road_shapefile Character: polyline shapefile of roads.
#' @param radius Numeric: distance in meters to extend the search for a road.
#' @param scratch_dir Character: scratch directory.
#' @param executable_dir Character: directory holding distanceToRoad.exe.
#'
#' @return A \code{SpatRaster} of distance to the closest road.
#' @export
distance_to_road <- function(input_file = NOFILE,
                             raster = NOFILE,
                             dem = NOFILE,
                             road_shapefile = NOFILE,
                             radius = 0,
                             scratch_dir = NOFILE,
                             executable_dir = NULL) {

  run_distance_to_road <- TRUE

  if (!is_missing_path(input_file)) {

    # --- Mode 1: existing input file -------------------------------------
    input_lines <- get_input_file(input_file)
    require_keywords(input_lines,
                     c("DEM", "SCRATCH DIRECTORY", "RADIUS", "ROAD SHAPEFILE"))
    raster <- add_extension_if_missing(
      keyword_value(input_lines, "OUTPUT RASTER"), "flt")

  } else if (is_missing_path(dem)) {

    # --- Mode 3: read an existing raster ---------------------------------
    check <- argument_checker()
    check$input_file(raster, "raster")
    check$report()
    raster <- add_extension_if_missing(raster, "flt")
    run_distance_to_road <- FALSE

  } else {

    # --- Mode 2: build an input file, then run distanceToRoad ------------
    check <- argument_checker()
    check$input_file(dem, "dem")
    check$input_file(road_shapefile, "road_shapefile", extensions = "shp")
    check$directory(scratch_dir, "scratch_dir")
    check$supplied(raster, "output raster")
    check$number(radius, "radius")
    check$report()

    input_file <- distanceToRoad_input(dem, radius, road_shapefile,
                                       raster, scratch_dir)
    raster <- add_extension_if_missing(raster, "flt")
  }

  if (run_distance_to_road) {
    run_program("distanceToRoad", input_file, executable_dir)
  }

  terra::rast(raster)
}


# =========================================================================
# SECTION 4.  DEBRIS-FLOW RUNOUT
# =========================================================================

#' Terrain attributes along debris-flow runout paths
#'
#' A wrapper for Fortran program PFA_debris_flow. Produces a csv that can be
#' read into a data frame for a Cox survival model with time-dependent
#' covariates.
#'
#' The multinomial logistic regression coefficients for the probability of
#' scour and deposition are hard-wired below. They were calibrated against the
#' ODF 1996 Storm Study surveys; see the Quatro file PFA_runout.
#'
#' @param dem Character: file name (full path) of the DEM.
#' @param init_points Character: initiation-point shapefile.
#' @param geo_poly Character: rock-type polygon shapefile.
#' @param stand_age Character: LEMMA stand-age .flt raster.
#' @param tracks Character: DOGAMI debris-flow-track polyline shapefile.
#' @param radius Numeric: search radius in meters for matching a DEM flow path
#'   to a DOGAMI track.
#' @param initRadius Numeric: search radius in meters around initiation points.
#' @param length_scale Numeric: length in meters over which to measure
#'   elevation derivatives.
#' @param slope_intercept,slope_coef Numeric: intercept and coefficient of the
#'   slope term.
#' @param bulk_coef Numeric: coefficient for the linear bulking equation.
#' @param init_width,init_length Numeric: initiation zone dimensions in meters.
#' @param DF_width Numeric: debris-flow track width in meters.
#' @param alpha Numeric: proportion of debris-flow cross-sectional volume
#'   deposited per unit length.
#' @param uncensored If TRUE, treat all endpoint tracks as uncensored even
#'   without mapped deposition.
#' @param scratch_dir Character: scratch directory. Outputs are written here.
#' @param executable_dir Character: directory holding PFA_debris_flow.exe.
#'
#' @return A data frame read from the survival csv.
#' @export
PFA_debris_flow <- function(dem = NOFILE,
                            init_points = NOFILE,
                            geo_poly = NOFILE,
                            stand_age = NOFILE,
                            tracks = NOFILE,
                            radius = 0,
                            initRadius = 0,
                            length_scale = 0,
                            slope_intercept = 0,
                            slope_coef = 0,
                            bulk_coef = 0,
                            init_width = 0,
                            init_length = 0,
                            DF_width = 0,
                            alpha = 0,
                            uncensored = FALSE,
                            scratch_dir = NOFILE,
                            executable_dir = NULL) {

  # --- Check arguments ----------------------------------------------------
  check <- argument_checker()
  check$input_file(dem, "dem")
  check$input_file(init_points, "init_points", extensions = "shp")
  check$input_file(geo_poly, "geo_poly", extensions = "shp")
  check$input_file(stand_age, "stand_age")
  check$input_file(tracks, "tracks", extensions = "shp")
  check$directory(scratch_dir, "scratch_dir")
  check$number(radius, "radius")
  check$number(initRadius, "initRadius")
  check$number(length_scale, "length_scale")
  check$number(alpha, "alpha")
  check$report()

  # --- Model coefficients -------------------------------------------------
  # Hard-wired here, but they could equally be read from a file. Elements
  # 1-10 are the scour terms and 11-20 the transition terms; each run is
  # intercept, gradient, normal curvature, tangent curvature, stand age, then
  # the five rock types.
  coef <- c(
    -5.590219,      # scour intercept
    12.124613,      # scour gradient
    -25.90498,      # scour normal curvature
    62.17922,       # scour tangent curvature
    -0.0018062539,  # scour stand age
    0.,             # scour sedimentary
    0.8585555,      # scour volcanic
    0.5779966,      # scour igneous-metamorphic
    1.89509936,     # scour volcaniclastic
    1.160043335,    # scour unconsolidated
    -1.632,         # transitional intercept
    2.313637,       # transitional gradient
    10.49279,       # transitional normal curvature
    35.33434,       # transitional tangent curvature
    -0.0006935715,  # transitional stand age
    0.,             # transitional sedimentary
    -0.1992108,     # transitional volcanic
    -1.0804838,     # transitional igneous-metamorphic
    0.05590894,     # transitional volcaniclastic
    -0.006225561    # transitional unconsolidated
  )

  # --- Output file names, built with join_path rather than paste0 ---------
  out_surv        <- join_path(scratch_dir, "out_surv.csv")
  out_point       <- join_path(scratch_dir, "out_point")
  out_kaplanMeier <- join_path(scratch_dir, "out_KaplanMeier.csv")

  input_file <- PFA_debris_flow_input(dem, init_points, geo_poly, stand_age,
                                      tracks, radius, initRadius, length_scale,
                                      slope_intercept, slope_coef, bulk_coef,
                                      init_width, init_length, DF_width, alpha,
                                      uncensored, scratch_dir,
                                      out_surv, out_point, out_kaplanMeier,
                                      coef)

  run_program("PFA_debris_flow", input_file, executable_dir)

  read.csv(out_surv)
}


# =========================================================================
# SECTION 5.  DEVIATION FROM MEAN ELEVATION
# =========================================================================

#' --- DEV, Deviation from mean elevation ---
#'
#' Provide a \code{SpatRaster} of deviation from mean elevation (DEV) over a
#' specified radius. For the ith DEM grid point,
#' DEV_i = (e_i - mean(e)) / sd(e), where mean(e) and sd(e) are taken over all
#' DEM points within the radius.
#'
#' Operates in the same three modes as [elev_deriv()].
#'
#' @param input_file Character: an existing LocalRelief input file (optional).
#' @param raster Character: output DEV raster, or an existing one to read in
#'   mode 3.
#' @param dem Character: file name (full path) of the DEM.
#' @param radius Numeric: radius in meters over which to calculate DEV.
#' @param scratch_dir Character: scratch directory.
#' @param executable_dir Character: directory holding DEV.exe.
#'
#' @return A \code{SpatRaster} of DEV values.
#' @export
DEV <- function(input_file = NOFILE,
                raster = NOFILE,
                dem = NOFILE,
                radius = 0,
                scratch_dir = NOFILE,
                executable_dir = NULL) {

  run_dev <- TRUE

  if (!is_missing_path(input_file)) {

    # --- Mode 1: existing input file -------------------------------------
    input_lines <- get_input_file(input_file)
    require_keywords(input_lines, c("DEM", "SCRATCH DIRECTORY", "RADIUS"))
    raster <- add_extension_if_missing(
      keyword_value(input_lines, "OUTPUT DEV RASTER"), "flt")

  } else if (is_missing_path(dem)) {

    # --- Mode 3: read an existing raster ---------------------------------
    check <- argument_checker()
    check$input_file(raster, "raster")
    check$report()
    raster <- add_extension_if_missing(raster, "flt")
    run_dev <- FALSE

  } else {

    # --- Mode 2: build an input file, then run LocalRelief ---------------
    check <- argument_checker()
    check$input_file(dem, "dem")
    check$directory(scratch_dir, "scratch_dir")
    check$supplied(raster, "output raster")
    check$number(radius, "radius")
    check$report()

    input_file <- DEV_input(dem, radius, raster, scratch_dir)
    raster <- add_extension_if_missing(raster, "flt")
  }

  if (run_dev) run_program("DEV", input_file, executable_dir)

  terra::rast(raster)
}


# =========================================================================
# SECTION 6.  RESAMPLING
# =========================================================================

#' Resample a raster to lower resolution
#'
#' Read an input DEM (.flt or .tif) and write a DEM of lower resolution.
#' No interpolation is done: output cell corners coincide with input cell
#' corners, the cells are simply bigger.
#'
#' @param in_raster Character: the raster to be downsampled.
#' @param skip Integer: the output cell length is skip times the input cell
#'   length. With a 1 m input DEM, skip = 2 gives a 2 m output DEM.
#' @param out_raster Character: the output DEM, written as a .flt file.
#' @param scratch_dir Character: scratch directory.
#' @param executable_dir Character: directory holding resample.exe.
#'
#' @return The output raster path, invisibly.
#' @export
resample <- function(in_raster = NOFILE,
                     skip = 0,
                     out_raster = NOFILE,
                     scratch_dir = NOFILE,
                     executable_dir = NULL) {

  check <- argument_checker()
  check$input_file(in_raster, "in_raster")
  check$directory(scratch_dir, "scratch_dir")
  check$supplied(out_raster, "out_raster")
  check$number(skip, "skip")
  check$report()

  input_file <- resample_input(in_raster, skip, out_raster, scratch_dir)
  run_program("resample", input_file, executable_dir)

  invisible(add_extension_if_missing(out_raster, "flt"))
}


# =========================================================================
# SECTION 7.  DTM CO-REGISTRATION
# =========================================================================

#' --- align, Co-register two DTMs ---
#'
#' A wrapper for Fortran program align. See [align_input()] for a description
#' of the method.
#'
#' @param refDTM Character: reference DTM (full path).
#' @param alignDTM Character: DTM to align to the reference.
#' @param refDSM Character: reference DSM (optional).
#' @param alignDSM Character: DSM to align (optional).
#' @param iterations Numeric: number of iterations used to solve for the shift.
#' @param k Numeric: number of interquartile ranges from q1 and q3 to use as a
#'   Tukey's fence for outlier removal.
#' @param dampener Numeric: dampener for the shift, 1.0 or less.
#' @param outDTM Character: output aligned DTM.
#' @param tileNx,tileNy Numeric: number of tiles in x and y.
#' @param overlap Numeric: overlap between tiles.
#' @param radius Numeric: radius in meters for measuring slope and aspect.
#' @param nslope Numeric: number of gradient bins.
#' @param maxSlope Numeric: maximum gradient for binning.
#' @param nAzimuth Numeric: number of azimuth bins.
#' @param outbins Character: output csv of elevation differences binned by
#'   slope and aspect.
#' @param outDif Character: output elevation-difference raster (.flt).
#' @param outOutlier Character: output outlier raster (.flt).
#' @param scratch_dir Character: scratch directory.
#' @param executable_dir Character: directory holding the executable.
#' @param program_name Character: base name of the executable.
#'
#' @return 0 on success. Stops with a message on any failure.
#' @export
align <- function(refDTM = NOFILE,
                  alignDTM = NOFILE,
                  refDSM = NOFILE,
                  alignDSM = NOFILE,
                  iterations = 5,
                  k = 0,
                  dampener = 1,
                  outDTM = NOFILE,
                  tileNx = 0,
                  tileNy = 0,
                  overlap = 0.5,
                  radius = 15,
                  nslope = 7,
                  maxSlope = 1.0,
                  nAzimuth = 8,
                  outbins = NOFILE,
                  outDif = NOFILE,
                  outOutlier = NOFILE,
                  scratch_dir = NOFILE,
                  executable_dir = NOFILE,
                  program_name = "align") {

  check <- argument_checker()
  check$input_file(refDTM,   "refDTM")
  check$input_file(alignDTM, "alignDTM")
  check$number(k, "k (Tukey's fence parameter)", minimum = 0,
               allow_minimum = TRUE)
  check$supplied(outDTM,     "outDTM")
  check$supplied(outbins,    "outbins")
  check$supplied(outDif,     "outDif")
  check$supplied(outOutlier, "outOutlier")
  check$directory(scratch_dir, "scratch_dir")
  check$directory(executable_dir, "executable_dir")
  check$report()

  # The "TerrainWorksUtils::" prefixes are dropped: this file is part of that
  # package, and a package cannot reference its own namespace before install.
  input_file <- align_input(refDTM, alignDTM, refDSM, alignDSM,
                            iterations, k, dampener, outDTM,
                            tileNx, tileNy, overlap, radius,
                            nslope, maxSlope, nAzimuth,
                            outbins, outDif, outOutlier, scratch_dir)

  run_program(program_name, input_file, executable_dir)
  0L
}


# =========================================================================
# SECTION 8.  LANDSLIDE DETECTION
# =========================================================================

#' --- huntLS, Match outlier patches to mapped landslide points ---
#'
#' A wrapper for Fortran program HuntLS. See [huntLSinput()] for a description
#' of the outlier k values it works from.
#'
#' @param DEM Character: the reference DTM used for align.
#' @param Outlier Character: input outlier raster (.flt) created by align.
#' @param DoD Character: input DoD raster (.flt) created by align.
#' @param Accum Character: input flow accumulation raster created by bldgrds.
#' @param AccumThreshold Numeric: contributing area above which landslide
#'   patches are precluded.
#' @param LSpnts Character: input point shapefile of mapped landslide points.
#' @param IDfield Character: attribute field holding the record ID.
#' @param Radius Numeric: search radius in meters for matching points to
#'   patches.
#' @param AspectLength Numeric: length in meters for calculating aspect.
#' @param GradLength Numeric: length in meters for calculating gradient.
#' @param OutlierThreshold Numeric: minimum absolute k value for a patch.
#' @param ScratchDir Character: scratch directory.
#' @param OutPatch Character: output patch raster (.flt).
#' @param OutGrad Character: output gradient raster (.flt).
#' @param Outcsv Character: output comma-delimited table.
#' @param executable_dir Character: directory holding huntLS.exe.
#'
#' @return 0 on success. Stops with a message on any failure.
#' @export
huntLS <- function(DEM = NOFILE,
                   Outlier = NOFILE,
                   DoD = NOFILE,
                   Accum = NOFILE,
                   AccumThreshold = -9999,
                   LSpnts = NOFILE,
                   IDfield = NOFILE,
                   Radius = -9999,
                   AspectLength = -9999,
                   GradLength = -9999,
                   OutlierThreshold = -9999,
                   ScratchDir = NOFILE,
                   OutPatch = NOFILE,
                   OutGrad = NOFILE,
                   Outcsv = NOFILE,
                   executable_dir = NOFILE) {

  check <- argument_checker()
  check$directory(ScratchDir, "ScratchDir")
  check$directory(executable_dir, "executable_dir")
  check$input_file(DEM,     "DEM")
  check$input_file(Outlier, "Outlier")
  check$input_file(DoD,     "DoD")
  check$input_file(Accum,   "Accum")
  check$input_file(LSpnts,  "LSpnts", extensions = "shp")
  check$supplied(IDfield,  "IDfield")
  check$supplied(OutPatch, "OutPatch")
  check$supplied(OutGrad,  "OutGrad")
  check$supplied(Outcsv,   "Outcsv")
  check$number(AccumThreshold,   "AccumThreshold")
  check$number(Radius,           "Radius")
  check$number(AspectLength,     "AspectLength")
  check$number(GradLength,       "GradLength")
  check$number(OutlierThreshold, "OutlierThreshold", minimum=-9999, allow_minimum = TRUE)
  check$report()

  huntLSinput(DEM, Outlier, DoD, Accum, AccumThreshold,
              LSpnts, IDfield, Radius, AspectLength, GradLength,
              OutlierThreshold, ScratchDir, OutPatch, OutGrad, Outcsv)

  # huntLSinput() returns a status code rather than a path, so ask the shared
  # table where it put the file. The original hard-coded "input_huntLS.txt",
  # while the writer writes "input_huntls.txt".
  input_file <- input_file_for("HuntLS", ScratchDir)

  run_program("huntLS", input_file, executable_dir)
  0L
}


#' --- LShunter, Identify potential landslide sites from outlier patches ---
#'
#' A wrapper for Fortran program LShunter, which finds candidate landslide
#' sites on a DoD using the outlier raster created by align. It works in two
#' rounds, so most thresholds come in pairs.
#'
#' @param DEM Character: elevation raster. Needed only when no gradient raster
#'   is supplied.
#' @param Outlier Character: outlier raster (.flt) created by align.
#' @param threshold1,threshold2 Numeric: outlier thresholds, 1st and 2nd round.
#' @param Gradient Character: input gradient raster (.flt), optional.
#' @param min1,min2 Numeric: minimum gradient, 1st and 2nd round.
#' @param Accum Character: input flow accumulation raster (.flt).
#' @param maxAccum1,maxAccum2 Numeric: maximum flow accumulation, 1st and 2nd
#'   round.
#' @param Roads Character: input road shapefile, optional.
#' @param road_buffer Numeric: buffer in meters around roads.
#' @param minSize Numeric: minimum patch size in square meters.
#' @param GradLength Numeric: length in meters over which to calculate
#'   gradient. Needed only when no gradient raster is supplied.
#' @param OutGrad Character: output gradient raster (.flt), optional.
#' @param OutPatch Character: output patch raster (.flt).
#' @param ScratchDir Character: scratch directory.
#' @param Executable_dir Character: directory holding LShunter.exe.
#'
#' @return 0 on success. Stops with a message on any failure.
#' @export
LShunter <- function(DEM = NOFILE,
                     Outlier = NOFILE,
                     threshold1 = -9999,
                     threshold2 = -9999,
                     Gradient = NOFILE,
                     min1 = -9999,
                     min2 = -9999,
                     Accum = NOFILE,
                     maxAccum1 = -9999,
                     maxAccum2 = -9999,
                     Roads = NOFILE,
                     road_buffer = -9999,
                     minSize = -9999,
                     GradLength = -9999,
                     OutGrad = NOFILE,
                     OutPatch = NOFILE,
                     ScratchDir = NOFILE,
                     Executable_dir = NOFILE) {

  check <- argument_checker()
  check$directory(ScratchDir, "ScratchDir")
  check$directory(Executable_dir, "Executable_dir")
  check$input_file(Outlier, "Outlier")
  check$input_file(Accum,   "Accum")
  check$number(threshold1, "threshold1", minimum = -Inf)
  check$number(threshold2, "threshold2", minimum = -Inf)
  check$number(min1, "min1")
  check$number(min2, "min2")
  check$number(maxAccum1, "maxAccum1")
  check$number(maxAccum2, "maxAccum2")
  check$number(minSize, "minSize")
  check$supplied(OutPatch, "OutPatch")

  # Gradient is optional, but if it is absent LShunter has to compute it, and
  # that needs both a DEM and a length scale.
  if (is_missing_path(Gradient)) {
    check$input_file(DEM, "DEM (required when no Gradient raster is given)")
    check$number(GradLength,
                 "GradLength (required when no Gradient raster is given)")
  } else {
    check$input_file(Gradient, "Gradient")
  }

  # road_buffer only matters when a road layer is supplied.
  if (!is_missing_path(Roads)) {
    check$input_file(Roads, "Roads", extensions = "shp")
    check$number(road_buffer, "road_buffer")
  }
  check$report()

  LShunterInput(DEM, Outlier, threshold1, threshold2,
                Gradient, min1, min2, Accum, maxAccum1, maxAccum2,
                Roads, road_buffer, minSize, GradLength,
                outGrad, OutPatch, ScratchDir)

  input_file <- input_file_for("LShunter", ScratchDir)

  run_program("LShunter", input_file, Executable_dir)
  0L
}


# =========================================================================
# SECTION 9.  LANDSLIDE POLYGON ANALYSIS
# =========================================================================

#' Characterize mapped landslide polygons
#'
#' A wrapper for Fortran program LS_poly. LS_poly reads a polygon shapefile of
#' landslide scars, generates centerlines through each polygon, and builds a
#' linked-node list along them. A likely initiation zone is delineated from
#' the upslope end of each polygon, extending downslope a distance equal to
#' the average polygon width. Statistics are calculated for those zones,
#' including gradient, tangential curvature, profile curvature and factor of
#' safety.
#'
#' Gradient and curvature rasters may either be read from disk (the `in*`
#' arguments) or calculated by LS_poly, in which case a radius must be given
#' and the result can be written out (the `out*` arguments) for reuse. A
#' factor-of-safety raster must always be supplied; generate it with the FoS
#' program.
#'
#' @param DEM Character: input DEM (.flt or .tif), full path.
#' @param polyFile Character: input landslide polygon shapefile.
#' @param polyID Character: name of the ID field for the input polygons.
#' @param inGrad,inTan,inProf Character: precomputed gradient, tangential
#'   curvature and profile curvature rasters (each optional).
#' @param inFoS Character: input factor-of-safety raster.
#' @param outGrad,outTan,outProf Character: rasters for LS_poly to compute and
#'   write (each optional).
#' @param gradRadius,tanRadius,profRadius Numeric: radii in meters used when
#'   computing the corresponding output raster.
#' @param outNodes Character: output node point shapefile (.shp).
#' @param outCsv Character: output csv of patch statistics.
#' @param outInit Character: output initiation zone raster.
#' @param scratchDir Character: scratch directory.
#' @param executableDir Character: directory holding LS_poly.exe.
#'
#' @return 0 on success. Stops with a message on any failure.
#' @export
LS_poly <- function(DEM = NOFILE,
                    polyFile = NOFILE,
                    polyID = NOFILE,
                    inGrad = NOFILE,
                    inTan = NOFILE,
                    inProf = NOFILE,
                    inFoS = NOFILE,
                    outGrad = NOFILE,
                    gradRadius = -9999,
                    outTan = NOFILE,
                    tanRadius = -9999,
                    outProf = NOFILE,
                    profRadius = -9999,
                    outNodes = NOFILE,
                    outCsv = NOFILE,
                    outInit = NOFILE,
                    scratchDir = NOFILE,
                    executableDir = NOFILE) {

  check <- argument_checker()
  check$directory(scratchDir, "scratchDir")
  check$directory(executableDir, "executableDir")
  check$input_file(DEM,      "DEM")
  check$input_file(polyFile, "polyFile", extensions = "shp")
  check$input_file(inFoS,    "inFoS")
  check$supplied(polyID,   "polyID")
  check$supplied(outNodes, "outNodes")
  check$supplied(outCsv,   "outCsv")
  check$supplied(outInit,  "outInit")

  # Each optional input raster only has to exist if it was named at all.
  for (raster_argument in list(c(inGrad, "inGrad"),
                               c(inTan,  "inTan"),
                               c(inProf, "inProf"))) {
    if (!is_missing_path(raster_argument[1])) {
      check$input_file(raster_argument[1], raster_argument[2])
    }
  }

  # Each output raster LS_poly is asked to compute needs its own radius.
  if (!is_missing_path(outGrad)) check$number(gradRadius, "gradRadius")
  if (!is_missing_path(outTan))  check$number(tanRadius,  "tanRadius")
  if (!is_missing_path(outProf)) check$number(profRadius, "profRadius")
  check$report()

  LS_poly_input(DEM, polyFile, polyID, inGrad, inTan, inProf, inFoS,
                outGrad, gradRadius, outTan, tanRadius, outProf, profRadius,
                outNodes, outCsv, outInit, scratchDir)

  input_file <- input_file_for("LS_poly", scratchDir)

  run_program("LS_poly", input_file, executableDir)
  0L
}


#' Generate point samples inside and outside landslide initiation zones
#'
#' A wrapper for Fortran program samplePoints. See [samplePointInput()] for a
#' description of the sampling scheme.
#'
#' @param inRaster Character: input initiation zone raster (.flt) from LS_poly.
#' @param areaPerSample Numeric: area in square meters per sample point.
#' @param buffer_in,buffer_out Numeric: buffer distance in meters around
#'   inside and outside points.
#' @param margin Numeric: distance in meters from a zone edge within which
#'   points are not placed.
#' @param ratio Numeric: ratio of outside points to inside points.
#' @param nbins Numeric: number of bins per predictor.
#' @param R4rasters List: single-precision real predictor rasters.
#' @param I4rasters List: integer (nominal class) predictor rasters.
#' @param minPatch Numeric: minimum patch size in square meters for the mask.
#' @param inPoints,outPoints Character: output shapefiles for the inside and
#'   outside point samples.
#' @param outMask Character: output mask raster (.flt).
#' @param outInit Character: output raster of the initiation zones sampled.
#' @param outInitPoints Character: output shapefile of the initiation point
#'   within each zone (optional).
#' @param table Character: output table of binning results.
#' @param scratchDir Character: scratch directory.
#' @param executableDir Character: directory holding samplePoints.exe.
#'
#' @return 0 on success. Stops with a message on any failure.
#' @export
samplePoints <- function(inRaster = NOFILE,
                         areaPerSample = -9999,
                         buffer_in = -9999,
                         buffer_out = -9999,
                         margin = -9999,
                         ratio = -9999,
                         nbins = -9999,
                         R4rasters = list(),
                         I4rasters = list(),
                         minPatch = -9999,
                         inPoints = NOFILE,
                         outPoints = NOFILE,
                         outMask = NOFILE,
                         outInit = NOFILE,
                         outInitPoints = NOFILE,
                         table = NOFILE,
                         scratchDir = NOFILE,
                         executableDir = NOFILE) {

  check <- argument_checker()
  check$directory(scratchDir, "scratchDir")
  check$directory(executableDir, "executableDir")
  # check$input_file() already tries both "x" and "x.flt", so the nested
  # file.exists() test in the original is no longer needed.
  check$input_file(inRaster, "inRaster")
  check$number(areaPerSample, "areaPerSample")
  check$number(buffer_in,  "buffer_in",  minimum = 0, allow_minimum = TRUE)
  check$number(buffer_out, "buffer_out", minimum = 0, allow_minimum = TRUE)
  check$number(margin,     "margin",     minimum = 0, allow_minimum = TRUE)
  check$number(ratio,   "ratio")
  check$number(nbins,   "nbins")
  check$number(minPatch, "minPatch")
  check$supplied(inPoints,  "inPoints")
  check$supplied(outPoints, "outPoints")
  check$supplied(outMask,   "outMask")
  check$supplied(outInit,   "outInit")
  check$supplied(table,     "table")
  check$report()

  samplePointInput(inRaster, areaPerSample, buffer_in, buffer_out, margin,
                   ratio, nbins, R4rasters, I4rasters, minPatch,
                   inPoints, outPoints, outInitPoints, outMask, outInit,
                   table, scratchDir)

  input_file <- input_file_for("samplePoints", scratchDir)

  run_program("samplePoints", input_file, executableDir)
  0L
}


# =========================================================================
# SECTION 10.  POISSON POINT DENSITY MODEL
# =========================================================================

#' Build a density raster for a Poisson point model
#'
#' A wrapper for Fortran program modelDensity. Besides the density raster,
#' modelDensity writes an ROC curve csv computed from the modeled density and
#' the input points, a proportion-of-points raster derived from the density
#' raster, and a csv giving the actual proportion of points in each 10 percent
#' increment of that proportion raster.
#'
#' @param mask Character: input mask raster (.flt).
#' @param init_pnts Character: initiation points (.shp).
#' @param intercept Numeric: model intercept.
#' @param R4rasters List: continuous covariate rasters, each giving name,
#'   file, number of coefficients and the coefficient values.
#' @param I4rasters List: integer factor covariate rasters, each giving name,
#'   file, number of classes, minimum and maximum class, then the class values
#'   and their coefficients.
#' @param unit Character: "KM" to convert units to kilometers.
#' @param prop_raster Character: output proportion raster (.flt).
#' @param density_raster Character: output density raster (.flt).
#' @param ROC Character: output ROC csv file.
#' @param scratch_dir Character: scratch directory.
#' @param executable_dir Character: directory holding modelDensity.exe.
#'
#' @return 0 on success. Stops with a message on any failure.
#' @export
modelDensity <- function(mask = NOFILE,
                         init_pnts = NOFILE,
                         intercept = -1.e30,
                         R4rasters = list(),
                         I4rasters = list(),
                         unit = "unknown",
                         prop_raster = NOFILE,
                         density_raster = NOFILE,
                         ROC = NOFILE,
                         scratch_dir = NOFILE,
                         executable_dir = NOFILE) {

  # BUG FIX: the original referred to `scratchDir` in four places -- the two
  # dir.exists() tests, the print(), and the input_file path -- but the
  # parameter is `scratch_dir`. Every call failed on the first line of the
  # function body.
  check <- argument_checker()
  check$directory(scratch_dir, "scratch_dir")
  check$directory(executable_dir, "executable_dir")
  check$input_file(mask, "mask")
  check$input_file(init_pnts, "init_pnts", extensions = "shp")
  check$supplied(prop_raster,    "prop_raster")
  check$supplied(density_raster, "density_raster")
  check$supplied(ROC,            "ROC")
  if (identical(intercept, -1.e30)) check$note("intercept not specified")
  if (identical(unit, "unknown"))   check$note("unit not specified")
  check$report()

  modelDensity_input(mask, init_pnts, intercept, R4rasters, I4rasters,
                     unit, prop_raster, density_raster, ROC, scratch_dir)

  input_file <- input_file_for("modelDensity", scratch_dir)

  run_program("modelDensity", input_file, executable_dir)
  0L
}


# =========================================================================
# SECTION 11.  MOVING-WINDOW QUANTILES
# =========================================================================

#' Quantiles over a moving circular window
#'
#' A wrapper for Fortran program quantiles. For each window position the
#' interquartile range is found, outliers beyond a Tukey's fence with k = 1.5
#' are removed, and the quartiles are recalculated from what remains. Each
#' pixel with a value z below q1 is assigned (z - q1)/(q3 - q1) and each pixel
#' above q3 is assigned (z - q3)/(q3 - q1), giving the number of interquartile
#' ranges by which the value is extreme. Z scores are also available.
#'
#' All output rasters are optional; name only the ones you want.
#'
#' @param in_raster Character: input raster (full path).
#' @param radius Numeric: radius in meters for the moving window.
#' @param buffer Numeric: spacing between window centre points, in cells.
#' @param out_outlier Character: output outlier raster (optional).
#' @param out_q1,out_q2,out_q3 Character: output quartile rasters (optional).
#' @param out_mean Character: output mean raster (optional).
#' @param out_zscore Character: output z-score raster (optional).
#' @param out_prob Character: output probability raster (optional).
#' @param scratch_dir Character: scratch directory.
#' @param executable_dir Character: directory holding the executable.
#' @param program_name Character: base name of the executable.
#'
#' @return 0 on success. Stops with a message on any failure.
#' @export
quantiles <- function(in_raster = NOFILE,
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
                      executable_dir = NOFILE,
                      program_name = "quantiles") {

  check <- argument_checker()
  check$input_file(in_raster, "in_raster")
  check$directory(scratch_dir, "scratch_dir")
  check$directory(executable_dir, "executable_dir")
  check$number(radius, "radius")
  check$number(buffer, "buffer", minimum = 0, allow_minimum = TRUE)
  check$report()

  input_file <- quantiles_input(in_raster, radius, buffer,
                                out_outlier, out_q1, out_q2, out_q3,
                                out_mean, out_zscore, out_prob, scratch_dir)

  run_program(program_name, input_file, executable_dir)
  0L
}


#' --- RIL, Delineate a channel network and its riparian landforms ---
#'
#' A wrapper for Fortran program RIL. RIL finds channel initiation points on a
#' DEM, traces the channel network downstream, and classifies the terrain
#' around it into valley floor, hollow and inner-gorge landforms. It writes a
#' raster of those classes and, optionally, a node point shapefile carrying a
#' list of per-node attributes.
#'
#' Every tuning parameter is passed straight through to [RIL_input()], which
#' documents them and supplies defaults from the Post Mortem reference run. A
#' minimal call therefore needs only a DEM, a scratch directory and an output
#' raster name.
#'
#' Existence of the optional input rasters and the road shapefile is checked
#' inside [RIL_input()], which names the offending argument if one is missing.
#'
#' @param dem Character: input DEM (full path).
#' @param scratch_dir Character: scratch directory. The RIL input file is
#'   written here.
#' @param out_RIL Character: output RIL raster (.flt).
#' @param ... Further arguments passed to [RIL_input()]: thresholds, the
#'   optional input and output rasters, the road shapefile, and
#'   `attribute_list`.
#' @param executable_dir Character: directory holding RIL.exe. Defaults to
#'   get_executable_path().
#'
#' @return A \code{SpatRaster} of the output RIL classes.
#'
#'
#' @seealso [RIL_input()], [ril_attribute()], [ril_term()],
#'   [ril_default_attributes()]
#' @export
RIL <- function(dem = NOFILE,
                scratch_dir = NOFILE,
                out_RIL = NOFILE,
                ...,
                executable_dir = NULL) {

  # Only the three arguments this wrapper names itself are checked here; the
  # rest are validated by RIL_input(), which knows which are paths and which
  # are thresholds. Anything misspelled in ... surfaces there as an
  # "unused argument" error rather than being silently dropped.
  check <- argument_checker()
  check$input_file(dem, "dem")
  check$directory(scratch_dir, "scratch_dir")
  check$supplied(out_RIL, "out_RIL")
  check$report()

  input_file <- RIL_input(dem = dem,
                          scratch_dir = scratch_dir,
                          out_RIL = out_RIL,
                          ...)

  run_program("RIL", input_file, executable_dir)

  terra::rast(add_extension_if_missing(out_RIL, "flt"))
}
