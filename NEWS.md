# scfmutils (development version)

## Dependency changes

* scfmutils now needs R >= 4.4 (was 4.2), the oldest R version its checks run on (#19).

- `calibrateFireRegimePolys()`: when no spread probability in the calibration range gives a
  polygon's mean fire size, it now uses the end of the range whose fitted size is closer, not always
  the bottom of the range. A polygon whose mean fire size was just above the range got the lowest
  spread probability, so its fires barely spread;
- `comparePredictions_summaryDT()`: the achieved fire return interval (and the internal mean annual area
  burned) again count the area burned inside the reporting area, from fires ignited there or outside
  (`burnSummary` groups 1 and 3). This fixes a regression in scfmutils 0.0.9 (cbcd0b5): the
  groups-1-and-3 table was overwritten by the groups-1-and-2 one used for mean fire size, so a fire
  that crossed the reporting boundary was counted by where it was ignited, not where it burned;
- new functions `calcIgnitions()`, `countBurnedPixelsInSAR()`, and `fireRegimeParRaster()`,
  moved from the `scfmIgnition`, `scfmSpread`, and `scfmEscape`/`scfmIgnition`/`scfmSpread` modules
  (#13). `calcIgnitions()` drops the module version's unused `fireRegimePolys` argument, and
  `countBurnedPixelsInSAR()` takes the reporting-area pixels as `pixInSAR` instead of reading them
  from the module;
- `calibrateFireRegimePolys()` no longer fails (`'xmin' not less than 'xmax'`) when every flammable
  cell of a fire regime polygon has 8 flammable neighbours, e.g., with no non-flammable cells
  (PredictiveEcology/scfm#22): `p0` is then `hatP0(pEscape, 8)` exactly;
- `calibrateFireRegimePolys()` estimates `p0` correctly from 4-neighbour counts: `escapeProbDelta()`
  assumed 0 to 8 neighbours, so with `neighbours = 4` it recycled the weights and underestimated `p0`;
- `getFirePoints_NFDB_scfm()` downloads from `.../current_version/NFDB_point_shp.zip`: CFS renamed the
  archive from `NFDB_point.zip`, which now returns HTTP 404, so no newer release could be fetched;
- add `withr` to Imports from Suggests, for use in examples;
- streamline `prepInputsFireRegimePolys()` for BEC;
- `prepInputsFireRegimePolys()` gets a new argument `subsetType` to retrieve entire polygons
  bordering `studyArea` or only the spatial intersection (default, preserving previous behaviour) 
- add a title parameter for `scfmDiagnostics` plots 

# scfmutils 2.0.8

- `.makeLandscapeAttr()` properly handles cases with missing neighbours;

# scfmutils 2.0.7

- `calibrateFireRegimePolys()` gracefully exits if polygons have no fires;

# scfmutils 2.0.6

- fix attempt to use fire regime attributes when not using FRT/FRU polygons;

# scfmutils 2.0.5

- add Erni et al. (2020) fire regime polygon attributes to FRT and FRU polygons;

# scfmutils 2.0.4

- add `withr` to Suggests (used with testing);
- fix plotting to handle cases with no escaped fires;
- more robust de-slivering;

# scfmutils 2.0.3

- require `reproducible >= 2.1.0`;
- updates for recent changes to `reproducible`: pass `to` arguments to `reproducible::prepInputs()` and related functions, instead of passing `studyArea` or `rasterToMatch`;
- new option `scfmutils.driver.plot.scam` (default `TRUE`) to allow user to disable creation of scam plots in `calibrateFireRegimePolys()`;

# scfmutils 2.0.2

- warn instead of error if spread model calibration fails in `calibrateFireRegimePolys()`;
- improve documentation;

# scfmutils 2.0.1

- add `fpCompare` to Imports, for improved floating point number comparisons;
- bump minimum versions requirements for dependency packages `SpaDES.core`, `SpaDES.tools`, and `reproducible`;
- fix issue with per-polygon flammable cell calculations in  `.makeLandscapeAttr()` that was introduced by switching to `sf` for fire regime polygons;
- modified `comparePredictions_fireDistribution()` to use arguments `fireRegimePoints`, `burnSummary`, and `size` instead of `dt`;
- improve CRS checks in `checkForIssues()`;
- documentation improvements;

# scfmutils 2.0.0

- fully implement use of `sf` polygons object instead of lists of polygon attributes;
- bump minimum versions of `Require`, `reproducible`, `SpaDES.core`, `SpaDES.tools`;
- new `ggplot` wrappers for plotting scfm rasters;
- documentation improvements;

# scfmutils v1.0.0

**Transitional version: introduces several breaking changes**

- transitioning to using `sf` polygons object instead of lists of polygon attributes;
- remove arguments `cellSize` and `neighbours` from `calibrateFireRegimePolys()`;
- `calcZonalRegimePars()` now returns a `data.table` instead of a list;
- fix issue with `prepInputsFireRegimePolys()` when url supplied by user;
- warn instead of error if scam plots can't be created;

# scfmutils 0.0.13

- drop support for R 4.1;
- remove `LandR` dependency;
- improved documentation;
- improved messaging;

# scfmutils 0.0.12

- add package `bcdata` to Suggests to allow use of BEC zones etc. `prepInputsfireRegimePolys()`;

# scfmutils 0.0.11

- fix for 0 neighbour instance in `makeLandscapeAttr()`;

# scfmutils 0.0.10

- add package `googledrive` to Suggests;
- increase the minimum `SpaDES.core` and `SpaDES.tools` versions following `terra` migration and other changes;
- update `prepInputsFireRegimePolys()` example to use `terra`;
- documentation improvements;

# scfmutils 0.0.9
- transition to using `terra` instead of `raster` package;
- remove `fasterize` dependency;
- use R native pipe instead of `magrittr`'s;
- drop R 4.0 support (reproducible requires R >= 4.1)
- add plotting functions for escapes and historical fire distributions;
- improved diagnostic plot calculations using `studyAreaReporting`;
- internal package restructuring;

# scfmutils 0.0.8

- use `%>%` for pipe (R < v4.2);
- add Erni et al. (2020) FRTs + FRUs to `prepInputsFireRegimePolys()`;

# scfmutils 0.0.7

- corrections calculating stats for diagnostics;

# scfmutils 0.0.6

- `prepInputsFireRegimePolys()` from BEC NDTs: allow either col name;
- ensure `plotPath` exists for scam plots;
- fix bug in `comparePredictions_summaryDT()`;
- fix bug in `deSliver()` and clarify documentation;
- pass 'optimizer' argument to `scam()` in `calibrateFireRegimePolys()`;
- additional messaging for driver calibration;
- save scam plots per poly during driver calibration;

# scfmutils 0.0.5

- add `scfmRegime` functions;
- add functions `checkForIssues()` and `deSliver()` for working with fire regime polygons;
- add BECNDT to `prepInputsFireRegimePolys()`;

# scfmutils 0.0.4

- increase minimum `SpaDES.core` version requirement;
- rename `PolyId` to `PolyID`;
- fix bug in `executeDesign()`;
- improved messaging;

# scfmutils 0.0.3

- use `theme_bw()` instead of `theme_minimal()` in diagnostic plots;

# scfmutils 0.0.2

- export `fireRegimePolyTypes()`;
- export `comparePredictions_annualIgnitions()`;

# scfmutils 0.0.1

- requires R >= 4.0;
- add `times` argument to `comparePredictions_fireReturnInterval()`;
- pass only what's used from `simList` to `comparePredictions_summaryDT()`;
- add note re: output CRS using `prepInputsFireRegimePolys()`;
- pass `startTime` to `.executeDesignInternal()`;
- fix `.makeLandscapeAttr()`;
- add functions from `scfmLandCoverInit`;
- fix use of `inherits`;
- add `prepInputsFireRegimePolys()`;
- use `pkgdown` for website;
