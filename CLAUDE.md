# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

TWutils (`DESCRIPTION` names the package `TWutils`; older code/docs, including the vignette's
`library(TerrainWorksUtils)` call, still refer to it as `TerrainWorksUtils` — that rename is
incomplete) is a Windows-only R package that wraps a set of compiled Fortran command-line
executables (in `inst/DEMutilities/files/`) for computing terrain/DEM (digital elevation model)
derivatives, and provides R helpers for building and evaluating landslide initiation prediction
models from those derivatives.

**Package scaffolding is present but not filled in.** `DESCRIPTION` and `NAMESPACE` exist but are
unedited `usethis::create_package()` skeletons: `DESCRIPTION`'s Title/Author/License are placeholder
text and it declares no `Imports` despite `R/` using `terra`, `stringr`, `tibble`, `caret`,
`randomForest`, `ROCR`, `methods`, `stats` throughout; `NAMESPACE` has no exports even though most
public functions carry roxygen `@export` tags. Run `devtools::document()` (or `roxygen2::roxygenise()`)
to regenerate `NAMESPACE`/`man/` before the package will actually load those exports, and expect to
fill in `DESCRIPTION`'s `Imports` field for a real build.

## Architecture

### Fortran executable wrappers (the core pattern)

Precompiled Windows `.exe` files live in `inst/DEMutilities/files/` (checked in via Git LFS per
`.gitattributes`, which tracks `*.tif`, `*.exe`, `*.flt`). Only a subset of the executables the R code
wraps are actually present there today: `MakeGrids.exe`, `partial.exe`, `LocalRelief.exe`,
`bldgrds.exe`, `distanceToRoad.exe`, `PFA_debris_flow.exe`. `R/wrappers.R` also defines wrappers for
`align`, `huntLS`, `LShunter`, `LS_poly`, `samplePoints`, `modelDensity`, `quantiles` and `resample`,
whose executables are not currently checked in — calls to those wrappers will fail at the
`run_program()` step until the corresponding `.exe` is added. Unlike earlier versions of this code,
there is no auto-discovery of the executable directory: every wrapper takes an explicit
`executable_dir` argument and errors if it's missing.

Each executable is driven by an ASCII "KEYWORD: argument" input file (grammar and read/write helpers
in `R/input_file_utilities.R`: `get_input_file()`, `get_keyword()`, `get_args()`, `parse_args()`/
`parse_arg()` for reading; `input_writer()` plus the `keyword()`/`optional()`/`line()` closures it
returns for writing). Some programs (`RIL`, `netrace`) also take nested `ATTRIBUTE LIST` / `RASTER LIST`
blocks, built with `attribute_spec()`/`equation_term()`/`raster_list_entry()` and written/read via
`write_attribute_list()`/`write_raster_list()`/`get_attribute_list()`.

The R-side wrapper functions in `R/wrappers.R` (`elev_deriv()`, `contributing_area()`,
`bldgrds_nochannels()`, `distance_to_road()`, `DEV()`, `PFA_debris_flow()`, `resample()`, `align()`,
`huntLS()`, `LShunter()`, `LS_poly()`, `samplePoints()`, `modelDensity()`, `quantiles()`) mostly follow
a shared three-mode pattern:

1. **Existing input file mode** — parse a pre-written keyword input file and invoke the executable.
2. **Build-and-run mode** — given a DEM + parameters, write a new keyword input file (via the
   corresponding `*_input()` builder in `R/input_file_utilities.R`), then invoke the executable.
3. **Read-only mode** — skip execution entirely and read already-computed `.flt` raster(s) from disk.

(The later, more special-purpose wrappers — `align`, `huntLS`, `LShunter`, `LS_poly`, `samplePoints`,
`modelDensity`, `quantiles` — only support mode 2: build the input file and run.)

Argument validation goes through `argument_checker()` in `R/wrappers.R`, which accumulates every
problem with a call (missing file, missing directory, out-of-range number) and raises one error
naming all of them, rather than failing on the first bad argument. Executables are invoked via
`run_program()` (`system(command, wait = TRUE)`, non-zero exit code raises an error); this is
Windows-only and raster I/O is via `terra::rast()`/`terra::writeRaster()`. Outputs are floating-point
binary `.flt` grids (with a paired `.hdr` header). Raster arguments are conventionally passed
*without* a file extension (the Fortran side resolves it); `RASTER_EXTENSIONS` in
`R/input_file_utilities.R` (`flt`, `tif`, `bil`) is the single source of truth for which extensions
count as a match when checking a raster argument exists. `convert_hdr()` in `R/utils.R` converts a
GDAL BIL `.hdr` into the binary-floating-point header format these Fortran tools expect (BIL is
upper-left referenced; `.flt` is lower-left referenced, so `YLLCORNER` is derived from
`ULYMAP - NROWS*CELLSIZE`).

When adding a new wrapped executable: add an `*_input()` builder in `R/input_file_utilities.R` (via
`input_writer()`), add an entry to `INPUT_FILE_NAMES` there, and add a mode-dispatching wrapper in
`R/wrappers.R` that validates arguments with `argument_checker()` and calls `run_program()`.

### Modeling pipeline

`R/dem_to_model.R`'s `dem_to_model()` is the top-level pipeline: for each DEM + initiation-points pair,
it computes elevation derivatives via `elev_deriv()`/`contributing_area()`, builds training data with
`create_training_data_with_buffer()` (and optionally `create_analysis_region_mask()`), then trains a
random forest via `build_k_fold_rf_model()` (`R/build_models.R`, a `caret::train()` wrapper).
`R/calcRocStats.R` computes ROC/AUC/precision/accuracy via the `ROCR` package, and
`R/plotting_functions.R` plots per-class metric distributions.

**Important:** `create_training_data_with_buffer()` and `create_analysis_region_mask()` are called from
`dem_to_model()` but are not defined anywhere in this repo — per git history ("Moved LSutils functions
to LSutils"), they were relocated to a separate sibling package/repo called `LSutils`. Anything touching
`dem_to_model()` likely needs that package available.

### Raster utilities

`R/utils.R` also has general-purpose `terra`-based helpers unrelated to the Fortran wrappers:
`alignRasters()` (reprojects/resamples a list of rasters onto a reference raster), `extractRasterValues()`
(extracts all layers of a raster at point locations into a data frame), `sample_from_polygons()`,
and `applyCats()`/`fixFactorRaster()` for reconciling categorical/factor raster level IDs across rasters.

## Key dependencies

`terra`, `stringr`, `tibble`, `caret`, `randomForest`, `ROCR`, `methods`, `stats` (referenced via
`@import`/`::` but, per above, not yet declared in `DESCRIPTION` — must be installed manually in the R
environment).

## Data

`inst/extdata/elevation.{flt,hdr,prj}` is a small example DEM. The vignette (`vignettes/DEMutilities.Rmd`)
references a different example file (`elev_scottsburg/elev_scottsburg.flt`) and a hardcoded network path
for `output_dir` — the vignette is not currently runnable as-is and would need updating to use
`inst/extdata/elevation.flt`.
