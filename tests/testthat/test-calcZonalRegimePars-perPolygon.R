## calcZonalRegimePars(): fireMultiplier and fireMultiplierWeights given per polygon, as a table keyed
## by PolyID or by any column of firePolys (e.g. an eco-zone label shared by several polygons).

## three identical polygons; 2 and 5 share the label "A"
.ppArgs <- function() {
  ids <- c(2L, 5L, 7L)
  sizes <- c(rep(0.2, 150), round(exp(seq(log(7), log(30000), length.out = 50)), 2))
  list(firePolys = data.frame(PolyID = ids, ecoregion = c("A", "A", "B"),
                              cellSize = 6.25, burnyArea = 5e5),
       firePoints = data.frame(PolyID = rep(ids, each = length(sizes)), SIZE_HA = rep(sizes, 3)),
       epochLength = 40, maxSizeFactor = 1.2, fireSizeColumnName = "SIZE_HA",
       targetBurnRate = NA, targetMaxFireSize = NA)
}

## as the modules call it: lapply over unique(PolyID); returns one row per polygon
.ppRun <- function(...) {
  dots <- list(...)
  args <- .ppArgs()
  args[names(dots)] <- dots # replace whole arguments (modifyList() would merge data.frame columns)
  out <- withCallingHandlers(
    lapply(unique(args$firePolys$PolyID), function(id) {
      do.call(calcZonalRegimePars, c(list(polygonID = id), args))
    }),
    warning = function(w) {
      if (grepl("convergence failure", conditionMessage(w))) invokeRestart("muffleWarning")
    }
  )
  as.data.frame(data.table::rbindlist(out, fill = TRUE))
}

.parCols <- c("ignitionRate", "pEscape", "xBar", "lxBar", "xMax", "emfs_ha", "empiricalBurnRate")
.row <- function(out, id) unlist(out[out$PolyID == id, .parCols])

test_that("a single fireMultiplier still applies to every polygon", {
  base <- .ppRun()
  x2 <- .ppRun(fireMultiplier = 2)
  for (id in c(2L, 5L, 7L)) expect_equal(.row(x2, id)[["xBar"]], .row(base, id)[["xBar"]] * 2^(1 / 3))
})

test_that("a fireMultiplier table keyed by PolyID applies to the polygons it lists", {
  base <- .ppRun()
  x2 <- .ppRun(fireMultiplier = 2)
  out <- expect_message(
    .ppRun(fireMultiplier = data.frame(PolyID = c(5L, 7L), fireMultiplier = c(2, 1))),
    "Polygon 2 .*not in `fireMultiplier`"
  )
  expect_equal(.row(out, 2L), .row(base, 2L)) # not listed: default 1
  expect_equal(.row(out, 5L), .row(x2, 5L))
  expect_equal(.row(out, 7L), .row(base, 7L))
})

test_that("a table keyed by a label applies to every polygon with that label", {
  base <- .ppRun()
  x3 <- .ppRun(fireMultiplier = 3)
  out <- .ppRun(fireMultiplier = data.frame(ecoregion = c("B", "A"), fireMultiplier = c(1, 3)))
  expect_equal(.row(out, 2L), .row(x3, 2L))
  expect_equal(.row(out, 5L), .row(x3, 5L))
  expect_equal(.row(out, 7L), .row(base, 7L))
})

test_that("fireMultiplierWeights can be given per polygon", {
  w <- data.frame(ecoregion = c("A", "B"), ignition = c(0, 1 / 3), escape = c(0, 1 / 3),
                  size = c(1, 1 / 3))
  out <- .ppRun(fireMultiplier = 2, fireMultiplierWeights = w)
  sizeOnly <- .ppRun(fireMultiplier = 2, fireMultiplierWeights = c(ignition = 0, escape = 0, size = 1))
  equal <- .ppRun(fireMultiplier = 2)
  expect_equal(.row(out, 2L), .row(sizeOnly, 2L))
  expect_equal(.row(out, 7L), .row(equal, 7L))
})

test_that("polygons missing from a weights table get the default weights, with a message", {
  w <- data.frame(PolyID = 7L, ignition = 0, escape = 0, size = 1)
  out <- expect_message(.ppRun(fireMultiplier = 2, fireMultiplierWeights = w),
                        "not in `fireMultiplierWeights`")
  expect_equal(.row(out, 2L), .row(.ppRun(fireMultiplier = 2), 2L))
})

test_that("an sf or data.table table works like a data.frame", {
  ref <- .ppRun(fireMultiplier = data.frame(ecoregion = c("A", "B"), fireMultiplier = c(2, 1)))
  dt <- .ppRun(fireMultiplier = data.table::data.table(ecoregion = c("A", "B"), fireMultiplier = c(2, 1)))
  expect_equal(dt, ref)
  pts <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)))
  sfTab <- sf::st_sf(ecoregion = c("A", "B"), fireMultiplier = c(2, 1), geometry = pts)
  expect_equal(.ppRun(fireMultiplier = sfTab), ref)
  ## a geometry column with another name
  sfTab2 <- sf::st_sf(ecoregion = c("A", "B"), fireMultiplier = c(2, 1), shape = pts)
  expect_equal(.ppRun(fireMultiplier = sfTab2), ref)
})

test_that("an ordinary column called geometry can be the key", {
  args <- .ppArgs()
  args$firePolys$geometry <- args$firePolys$ecoregion
  ref <- .ppRun(fireMultiplier = data.frame(ecoregion = c("A", "B"), fireMultiplier = c(2, 1)))
  out <- do.call(.ppRun, c(list(fireMultiplier = data.frame(geometry = c("A", "B"), fireMultiplier = c(2, 1))),
                           args["firePolys"]))
  expect_equal(out, ref)
  ## and an extra ordinary column called geometry is an error, not silently dropped
  expect_error(.ppRun(fireMultiplier = data.frame(ecoregion = "A", geometry = "x", fireMultiplier = 2)),
               "one key column")
})

test_that("a polygon with several values of the key is an error", {
  args <- .ppArgs()
  args$firePolys <- rbind(args$firePolys, transform(args$firePolys[1, ], ecoregion = "C"))
  expect_error(do.call(.ppRun, c(list(fireMultiplier = data.frame(ecoregion = "A", fireMultiplier = 2)),
                                 args["firePolys"])),
               "2 values of ecoregion")
})

test_that("targetBurnRate keeps priority over a per-polygon fireMultiplier", {
  ## unnamed, so it is taken by position (2nd element -> PolyID 2 here) both before and after the
  ## separate fix for named targets (fix/calcZonalRegimePars-target-names)
  tbr <- c(NA, 0.03)
  base <- .ppRun(targetBurnRate = tbr)
  out <- expect_message(
    .ppRun(targetBurnRate = tbr, fireMultiplier = data.frame(ecoregion = "A", fireMultiplier = 3)),
    "fireMultiplier is ignored in polygon 2"
  )
  expect_equal(.row(out, 2L), .row(base, 2L))
})

test_that("scaleMaxFireSize works with a per-polygon fireMultiplier", {
  x2 <- .ppRun(fireMultiplier = 2, scaleMaxFireSize = TRUE)
  out <- .ppRun(fireMultiplier = data.frame(ecoregion = c("A", "B"), fireMultiplier = c(2, 1)),
                scaleMaxFireSize = TRUE)
  expect_equal(.row(out, 5L), .row(x2, 5L))
  expect_equal(.row(out, 7L), .row(.ppRun(), 7L))
})

test_that("badly formed tables are errors", {
  expect_error(.ppRun(fireMultiplier = data.frame(fireMultiplier = 2)), "one key column")
  expect_error(.ppRun(fireMultiplier = data.frame(PolyID = 2L, ecoregion = "A", fireMultiplier = 2)),
               "one key column")
  expect_error(.ppRun(fireMultiplier = data.frame(zone = "A", fireMultiplier = 2)),
               "not a column of `firePolys`")
  expect_error(.ppRun(fireMultiplier = data.frame(ecoregion = c("A", "A"), fireMultiplier = c(2, 3))),
               "more than once")
  expect_error(.ppRun(fireMultiplier = data.frame(ecoregion = "A", mult = 2)), "one key column")
  expect_error(.ppRun(fireMultiplier = 2,
                      fireMultiplierWeights = data.frame(ecoregion = "A", ignition = 0.5, escape = 0.5,
                                                         size = 0.5)),
               "sum to 1")
})
