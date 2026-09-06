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
`huntLS()`, `LShunter()`, `LS_poly()`, `samplePoints()`, `modelDensity()`, `quantiles()`, `bldgrds()`)
mostly follow a shared three-mode pattern:

1. **Existing input file mode** — parse a pre-written keyword input file and invoke the executable.
2. **Build-and-run mode** — given a DEM + parameters, write a new keyword input file (via the
   corresponding `*_input()` builder in `R/input_file_utilities.R`), then invoke the executable.
3. **Read-only mode** — skip execution entirely and read already-computed `.flt` raster(s) from disk.

(The later, more special-purpose wrappers — `align`, `huntLS`, `LShunter`, `LS_poly`, `samplePoints`,
`modelDensity`, `quantiles`, `bldgrds` — only support mode 2: build the input file and run. `bldgrds`
has no mode 3 for the same reason `RIL`/`huntLS`/etc. don't: there is no single output raster keyword
to read back — see below.)

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

## Consistency with the Fortran source repos

The Fortran source behind `R/wrappers.R`/`R/input_file_utilities.R` lives in sibling repos checked
out alongside this one: `..\GridUtilities` (DEM/grid drivers), `..\ChannelUtilities` (channel-network
drivers), `..\LandslideUtilities` (landslide-initiation/susceptibility drivers), and `..\modules`
(the shared library the others depend on — see each repo's own `CLAUDE.md`).
Checking the R side against the Fortran source as currently checked out surfaces the following —
treat all of this as time-of-writing findings to re-verify, not settled fact, since both sides move
independently and there is no compiler or CI linking them.

**Confirmed consistent:**

- The `KEYWORD: value[, SUBFIELD=value] # comment` grammar `input_writer()` emits matches
  `InputFileModule` in `..\modules\Utilities.f90` exactly, as do the nested `ATTRIBUTE LIST`/
  `RASTER LIST` blocks (`write_attribute_list()`/`write_raster_list()`) against that same file's
  `ReadList` subroutine and `AttributeType`/raster-list handling (`..\ChannelUtilities\RIL\RIL.f90`
  reads a list via `input%readList(attributeList)`, confirming the RIL wiring from the recent
  "accept an arbitrary attribute list" change).
- `elev_deriv()` → `MakeGrids.exe` ← `GridUtilities/MakeGrids.f90` (`PROGRAM MakeGrids`), built by
  `gridUtilities\Projects\MakeGrids`.
- `contributing_area()` → `Partial.exe` ← `GridUtilities/partial.f90` (`PROGRAM partial`), built by
  `gridUtilities\Projects\partialAccum` (the checked-in `partial.exe` is that project's output
  renamed/relocated — the project itself would default to `partialAccum.exe`).
- `RIL()` → `RIL.exe` ← `ChannelUtilities/RIL/RIL.f90` (`PROGRAM RIL`).
- `PFA_debris_flow()` → `PFA_debris_flow.exe` ← `ChannelUtilities/DF_track/DF_track.f90`
  (`PROGRAM DF_track` — filename/program name don't match the wrapper name, but a comment in that
  file referencing `...\PFA_debris_flow\input_DF_track.txt` confirms it's built under a
  `PFA_debris_flow` project).
- `huntLS()` → `LandslideUtilities/HuntLS.f90` (`PROGRAM HuntLS`, built by
  `LandslideUtilities\Projects\huntLS`) — every keyword `huntLSinput()` writes (`DEM`,
  `INPUT OUTLIER RASTER`, `OUTLIER THRESHOLD`, `INPUT ELEVATION DIFFERENCE RASTER`,
  `INPUT FLOW ACCUMULATION RASTER`, `ACCUMULATION THRESHOLD`, `INPUT LANDSLIDE POINT SHAPEFILE`
  w/ `ID FIELD`, `SEARCH RADIUS`, `ASPECT LENGTH SCALE`, `GRADIENT LENGTH SCALE`,
  `OUTPUT PATCH RASTER`, `OUTPUT GRADIENT RASTER`, `OUTPUT TABLE`, `SCRATCH DIRECTORY`) matches a
  `CASE` in `HuntLS.f90`'s keyword parser exactly.
- `LShunter()` → `LandslideUtilities/LShunter.f90` (`PROGRAM LShunter`, built by
  `LandslideUtilities\Projects\LShunter`) — `LShunterInput()`'s keywords (`DEM`, `OUTLIER` w/
  `THRESHOLD1`/`THRESHOLD2`, `GRADIENT` w/ `MIN1`/`MIN2`, `GRADIENT LENGTH SCALE`,
  `FLOW ACCUMULATION` w/ `MAX1`/`MAX2`, `ROAD SHAPEFILE` w/ `BUFFER`, `MINIMUM SIZE`,
  `OUTPUT PATCH RASTER`, `SCRATCH DIRECTORY`) all match.
- `LS_poly()` → `LandslideUtilities/initPoly.f90` (`PROGRAM initPoly`, built by
  `LandslideUtilities\Projects\LS_poly` — note the project/wrapper name `LS_poly` doesn't match the
  file or `PROGRAM` name `initPoly`) — `LS_poly_input()`'s keywords (`DEM`,
  `LANDSLIDE POLYGON FILE` w/ `ID FIELD`, `INPUT`/`OUTPUT GRADIENT RASTER` w/ `RADIUS`,
  `INPUT`/`OUTPUT TANGENTIAL CURVATURE RASTER`, `INPUT`/`OUTPUT PROFILE CURVATURE RASTER`,
  `INPUT FOS RASTER`, `OUTPUT NODE POINT SHAPEFILE`, `OUTPUT CSV FILE`, `OUTPUT INITIATION RASTER`,
  `SCRATCH DIRECTORY`) all match. `inFoS` is meant to come from `LandslideUtilities/FoS.f90`
  (`PROGRAM FactorOfSafety`), which has no R wrapper of its own yet.
- `samplePoints()` → `LandslideUtilities/sample.f90` (`PROGRAM samplePoints`, built by
  `LandslideUtilities\Projects\SamplePoints`) — `samplePointInput()`'s keywords
  (`INITIATION ZONE RASTER`, `SAMPLE AREA`, `BUFFER INSIDE`/`OUTSIDE`, `MARGIN`, `RATIO`, `NBINS`,
  `R4 RASTERS`, `I4 RASTERS`, `MINIMUM MASK PATCH`, `OUTPUT INPOINT`/`OUTPOINT`/
  `INITIATION POINT SHAPEFILE`, `OUTPUT MASK RASTER`, `OUTPUT INITIATION ZONE RASTER`,
  `OUTPUT TABLE`, `SCRATCH DIRECTORY`) all match (`sample.f90`'s `CASE('SAMPLE AREA')` has no space
  before the `(`, unlike its neighbors — easy to miss with a `CASE (` search).
- `bldgrds()`/`bldgrds_input()` are verified against a real working reference input file (from the
  Sprague River project) rather than against `GridUtilities/bldGrds2.f90` — see the "found
  inconsistent" note below on why that source can't be trusted for this program. The reference
  file's keywords (`DEM FILE`, `SCRATCH`, `EXCAVATE LINE` w/ `BUFFER`, `WATER MASK` w/ `MINIMUM
  PATCH SIZE`/`SET TO MINIMUM ELEVATION`/`INCISE TO CENTER`/`MINIMUM GRADIENT`/`BUFFER RADIUS`/
  `PRECLUDE INITIATION`, `CALIBRATE`, `USE SMOOTHED ASPECT` w/ `LENGTH SCALE`,
  `PLAN CURVATURE LENGTH SCALE`, `GRADIENT LENGTH SCALE`, `D8 COEFFICIENTS`, `D8 LENGTH SCALES`,
  `INITIATION BUFFER`, `LOCAL RELIEF RASTER`/`LOCAL RELIEF THRESHOLD`,
  `AREA SLOPE THRESHOLD LOW/HIGH GRADIENT`, `PLAN CURVATURE THRESHOLD LOW/HIGH GRADIENT`,
  `MINIMUM THRESHOLD FLOW LENGTH`, `MINIMUM CHANNEL LENGTH`, `USE EXISTING FILES`,
  `OUTPUT NODE POINT SHAPEFILE` w/ `SPLITS`, and an `ATTRIBUTE LIST`/`END ATTRIBUTE LIST` block)
  are all reproduced by `bldgrds_input()`, and verified end-to-end by generating a file from the
  reference's own values and confirming it comes out the same, keyword for keyword. Everything
  from the earlier, `bldGrds2.f90`-derived version that this reference doesn't demonstrate
  (`DEM UNITS`, `CONDITIONED DEM`/`FILLED DEM`, `SLOPE FILE`/`PLAN CURVATURE FILE`/`BCON FILE`,
  `LOW`/`HIGH GRADIENT LIMIT`, `LOWER`/`UPPER PROPORTION`, `PLAN CURVATURE MINIMUM`, `NO CHANNELS`,
  `USE SCRATCH FILE`, `PATH`/`DEM ID`/the `REFERENCE ...` keywords) was dropped rather than kept
  alongside the verified set. `bldgrds()` returns `0L` rather than a raster — there is still no
  single `OUTPUT ... RASTER` keyword naming "the" output.
  `bldgrds_default_attributes()` was rewritten to match: it now reproduces the reference file's own
  attribute block (`ELEVATION`, `CONTRIBUTING AREA` → `AREA_SQKM`, then, when a `precip_raster` is
  given, `MEAN ANNUAL PRECIP`/`MEAN ANNUAL FLOW`/`WIDTH`/`DEPTH` chained via the Lorenson, Marcus and
  Roberts (1994) and White, McCullough, Justice and Kelsey (2011) equations — the same pattern
  `ril_default_attributes()` uses for RIL, with different citations/coefficients) rather than the
  `NODE ID`/`ELEV_M`/`AREA_KM2`/`CHANNEL_ID`/`STRM_ORDER` list guessed from `bldGrds2.f90` earlier.
  The **node point shapefile still requires an explicit `ATTRIBUTE LIST` block** (bldgrds has no
  fallback for that case, unlike for the node-list database), the block still has to be the last
  thing in the file (`ReadInput()` reads it via a separate `input%readlist()` call made only after
  its main keyword loop finishes — the same ordering RIL needs), and `bldgrds_input()` still errors
  if a caller pairs `node_shapefile` with an explicitly empty `attribute_list` rather than silently
  writing a broken file — none of that changed with this rewrite, only the actual keyword/attribute
  vocabulary matching it.
- `bldgrds_enforce()`/`bldgrds_enforce_input()` are a third bldgrds scenario, alongside
  `bldgrds()`/`bldgrds_input()` (initiate new channels from thresholds) and
  `bldgrds_nochannels()`/`bldgrds_nochannels_input()` (flow accumulation only, no channel
  network): trace the channel network from an existing, previously-mapped channel-network
  **polyline shapefile** (`CHANNEL MASK`, excavated into the DEM) instead, with `NO NEW CHANNELS`
  always written to preclude any initiation outside it — so none of `bldgrds_input()`'s
  channel-initiation-criteria arguments apply or are accepted here. Verified the same way as
  `bldgrds_input()` above: checked keyword-for-keyword against a second working reference input
  file (Skykomish project), not against `bldGrds2.f90`. That reference's `ATTRIBUTE LIST` block
  is closed with `END LIST`, not `END ATTRIBUTE LIST` like `bldgrds_input()` writes — a real
  discrepancy between the two reference files, not a typo in either one; `bldgrds_enforce_input()`
  matches its own reference (`END LIST`) rather than being reconciled with `bldgrds_input()`'s.
  The Skykomish reference's own `ATTRIBUTE LIST` block (not `bldgrds_default_attributes()` —
  a different, generic reference) also had a real, confirmed bug worth knowing about elsewhere:
  its `MEAN ANNUAL FLOW` equation's second `TERM` used `XPONENT=1.37` instead of `EXPONENT=1.37`,
  which `read_attribute_list_file()` silently drops rather than erroring on — parsing the
  equation as `MEANANNCMS = 0.017165249 * AREA_SQKM^0.985` with no precipitation term at all,
  not the two-variable equation clearly intended. Confirmed by testing
  `read_attribute_list_file()` against both spellings directly.
  `channel_mask` was originally treated as a raster (`check$input_file()` defaulting to
  `RASTER_EXTENSIONS`, `bldgrds_enforce_input()` using `normalize_raster_path()`) — wrong, since
  `CHANNEL MASK: FILE` is a polyline shapefile like `LShunter()`'s `Roads` or
  `distance_to_road()`'s `road_shapefile`, not a `.flt`/`.tif`/`.bil` grid. Fixed in both places to
  check/normalize against `extensions = "shp"` instead, the same convention those other shapefile
  arguments use; confirmed by testing that a bare `channel_mask` name now resolves against a
  `.shp` file rather than a same-named `.flt`, and that a `.flt`-only path is correctly rejected
  where it previously would have passed.

**Found inconsistent — worth fixing or re-checking before relying on these:**

- **`DEV()` cannot actually run against what's checked in.** `run_program("DEV", ...)` looks for
  `DEV.exe`, matching `GridUtilities/DEV.f90` (`PROGRAM makeDEV`, built by `gridUtilities\Projects\DEV`)
  and its `OUTPUT DEV RASTER` keyword, which `DEV_input()` writes correctly. But the only executable
  checked into `inst/DEMutilities/files/` is `LocalRelief.exe`, built from the *different* program
  `GridUtilities/LocalRelief2.f90` (`PROGRAM LocalRelief`) — and `LocalRelief2.f90` has no
  `OUTPUT DEV RASTER` case at all (it only understands `OUTPUT LOCAL RASTER`). `DEV_input()` itself
  is inconsistent about this: it calls `input_writer("LocalRelief", ...)` (line ~2511 of
  `input_file_utilities.R`) even though the file it writes, and the program `DEV()` actually runs,
  is `DEV`/`makeDEV`, not `LocalRelief`. Either add a real `DEV.exe` to `inst/DEMutilities/files/`, or
  rewrite `DEV()`/`DEV_input()` to target `LocalRelief.exe`'s actual keyword set.
- **`GridUtilities/bldGrds2.f90`, as checked out in this repo, is not the source behind bldgrds' real
  input format — confirmed, not just suspected.** A working reference input file (from the Sprague
  River project, pasted directly into this conversation) uses `EXCAVATE LINE`, `WATER MASK` with six
  subfields, `D8 COEFFICIENTS`/`D8 LENGTH SCALES`, `INITIATION BUFFER`, `LOCAL RELIEF RASTER`/
  `THRESHOLD`, `AREA SLOPE THRESHOLD LOW/HIGH GRADIENT`, `PLAN CURVATURE THRESHOLD LOW/HIGH
  GRADIENT`, `MINIMUM THRESHOLD FLOW LENGTH`, `USE EXISTING FILES`, `OUTPUT NODE POINT SHAPEFILE`,
  and an `ATTRIBUTE LIST` closed with `END ATTRIBUTE LIST` — none of which exist anywhere in
  `GridUtilities/bldGrds2.f90`'s current `SELECT CASE`, and none of which resemble that file's own
  much larger, differently-organized keyword set (`CONDITIONED DEM`, `CHANNEL MASK`,
  `SMOOTHING WINDOW`, `HAND RASTER`, `TWI RASTER`, ...). Whatever repo/branch/revision the real
  `bldgrds.exe` was actually built from, it is not this checkout of `GridUtilities` — check
  `git branch -a`/long-lived branches there (`bldgrds_D8`, `bldgrds_Nov11`/`bldgrds_Nov19`) before
  trusting *anything* here cross-referenced against `bldGrds2.f90` specifically for bldgrds (RIL,
  MakeGrids, partial, etc. are unaffected — this finding is bldgrds-specific).
  `bldgrds_input()`/`bldgrds_default_attributes()` (see above) were rewritten against this reference
  file rather than against `bldGrds2.f90`, and are the ones to trust now.
  One consequence worth noting: `bldgrds_nochannels_input()`'s `USE SMOOTHED ASPECT` (w/
  `LENGTH SCALE`), `PLAN CURVATURE LENGTH SCALE` and `GRADIENT LENGTH SCALE` keywords — flagged
  above as not matching `bldGrds2.f90` — turn out to match this real reference file exactly. Its
  `OUTPUT FLOW ACCUMULATION RASTER` keyword still doesn't appear in the reference file, though, so
  that part is still unverified; prefer `bldgrds()` regardless, since its whole keyword set (not
  just three keywords) is now checked against a real, working input file.
- **`align()`'s build project doesn't build `align.f90`.** `GridUtilities/align.f90` itself declares
  `PROGRAM align`, but `gridUtilities\Projects\align` (which is what actually produces `align.exe`)
  instead compiles `GridUtilities/alignSlope2.f90` (`PROGRAM alignSlope`) — one of several
  variants (`align.f90`, `alignNoSlope.f90`, `alignSlope.f90`, `alignSlope2.f90`) with the same
  general purpose. Before changing `align_input()`'s keyword set, confirm against `alignSlope2.f90`,
  not `align.f90`.
- **`distance_to_road()` and `resample()` still have no Fortran source anywhere under
  `c:\work\sandbox\repos`**, so their keyword grammars can't currently be checked against source:
  - `distanceToRoad.f90` (`PROGRAM distanceToRoad`) exists only in a separate `roadUtilities` repo
    that has not been migrated into this same tree.
  - `resample`'s own build project (`gridUtilities\Projects\resample\resample.vfproj`) points at a
    `resample.f90` under an old clone location that no longer exists on this machine either — its
    source isn't currently locatable at all.
- **`modelDensity()` has source but no visible build project.** `GridUtilities/modelDensity.f90`
  (`Program ModelDensity`) exists, unlike the group above, but no `gridUtilities\Projects\modelDensity`
  (or similarly named) project was found — unclear what currently builds the executable this wrapper
  runs.
- **`netrace` is wired up on the input-file side but has no R wrapper yet.** `INPUT_FILE_NAMES`
  carries a `"netrace"` entry and `input_file_utilities.R`'s `ATTRIBUTE LIST`/`RASTER LIST` writers
  exist to serve it, and `ChannelUtilities/Netrace/Netrace2.f90` is the corresponding Fortran source
  — but `R/wrappers.R` has no `netrace()` function yet (only `RIL()` calls the shared
  attribute/raster-list machinery so far).

## Key dependencies

`terra`, `stringr`, `tibble`, `caret`, `randomForest`, `ROCR`, `methods`, `stats` (referenced via
`@import`/`::` but, per above, not yet declared in `DESCRIPTION` — must be installed manually in the R
environment).

## Data

`inst/extdata/elevation.{flt,hdr,prj}` is a small example DEM. The vignette (`vignettes/DEMutilities.Rmd`)
references a different example file (`elev_scottsburg/elev_scottsburg.flt`) and a hardcoded network path
for `output_dir` — the vignette is not currently runnable as-is and would need updating to use
`inst/extdata/elevation.flt`.
