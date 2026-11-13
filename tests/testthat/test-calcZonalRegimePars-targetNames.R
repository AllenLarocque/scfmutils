## calcZonalRegimePars(): targetBurnRate and targetMaxFireSize are named by PolyID. scfmDataPrep and
## scfmRegime pass polygonID as an integer (unique(PolyID)), so the targets must be looked up by name,
## not by position.

## two identical polygons: 150 small fires and 50 escaped fires of 7 to 30000 ha each
.twoPolyArgs <- function(ids = c(2L, 5L)) {
  sizes <- c(rep(0.2, 150), round(exp(seq(log(7), log(30000), length.out = 50)), 2))
  list(firePolys = data.frame(PolyID = ids, cellSize = 6.25, burnyArea = 5e5),
       firePoints = data.frame(PolyID = rep(ids, each = length(sizes)), SIZE_HA = rep(sizes, 2)),
       epochLength = 40, maxSizeFactor = 1.2, fireSizeColumnName = "SIZE_HA")
}

## as the modules call it: lapply over unique(PolyID), an integer vector
.runPolys <- function(ids, targetBurnRate, targetMaxFireSize) {
  args <- .twoPolyArgs(ids)
  out <- withCallingHandlers(
    lapply(unique(args$firePolys$PolyID), function(id) {
      do.call(calcZonalRegimePars, c(list(polygonID = id, targetBurnRate = targetBurnRate,
                                          targetMaxFireSize = targetMaxFireSize), args))
    }),
    warning = function(w) {
      if (grepl("convergence failure", conditionMessage(w))) invokeRestart("muffleWarning")
    }
  )
  data.table::rbindlist(out, fill = TRUE)
}

test_that("targetBurnRate is applied to the polygon it is named for", {
  out <- .runPolys(c(2L, 5L), targetBurnRate = c("2" = NA, "5" = 0.03),
                   targetMaxFireSize = c("2" = NA, "5" = NA))
  expect_equal(out[PolyID == 2]$pEscape, 0.25) # empirical
  expect_equal(out[PolyID == 5]$pEscape, 0.5)  # raised towards the target
  expect_gt(out[PolyID == 5]$xBar, out[PolyID == 2]$xBar)
})

test_that("names in a different order from the PolyIDs are matched by name", {
  out <- .runPolys(1:2, targetBurnRate = c("2" = 0.03, "1" = NA),
                   targetMaxFireSize = c("2" = NA, "1" = NA))
  expect_equal(out[PolyID == 1]$pEscape, 0.25)
  expect_equal(out[PolyID == 2]$pEscape, 0.5)
})

test_that("targetMaxFireSize is applied to the polygon it is named for", {
  out <- .runPolys(c(2L, 5L), targetBurnRate = c("2" = NA, "5" = NA),
                   targetMaxFireSize = c("2" = NA, "5" = 50000))
  expect_equal(out[PolyID == 5]$emfs_ha, 50000)
  expect_equal(out[PolyID == 5]$xMax, 50000)
  expect_false(isTRUE(out[PolyID == 2]$emfs_ha == 50000))
  expect_equal(out[PolyID == 2]$xMax, 30000)
})

test_that("a polygon with no named target keeps its empirical values", {
  out <- .runPolys(c(2L, 5L), targetBurnRate = c("5" = 0.03), targetMaxFireSize = c("5" = NA))
  expect_equal(out[PolyID == 2]$pEscape, 0.25)
  expect_equal(out[PolyID == 5]$pEscape, 0.5)
})

test_that("unnamed targets are still taken by position (unchanged behaviour)", {
  out <- .runPolys(1:2, targetBurnRate = c(NA, 0.03), targetMaxFireSize = c(NA, NA))
  expect_equal(out[PolyID == 1]$pEscape, 0.25)
  expect_equal(out[PolyID == 2]$pEscape, 0.5)
})

test_that("PolyIDs 1..n with targets named in that order give the same results as before", {
  ## where the old positional lookup was right, results must not change
  out <- .runPolys(1:2, targetBurnRate = c("1" = NA, "2" = 0.03),
                   targetMaxFireSize = c("1" = NA, "2" = 50000))
  ref <- .runPolys(1:2, targetBurnRate = c(NA, 0.03), targetMaxFireSize = c(NA, 50000))
  expect_equal(out, ref)
})

test_that("partly named targets are still taken by position (unchanged behaviour)", {
  out <- .runPolys(1:2, targetBurnRate = c("1" = NA, 0.03), targetMaxFireSize = c(NA, NA))
  expect_equal(out[PolyID == 1]$pEscape, 0.25)
  expect_equal(out[PolyID == 2]$pEscape, 0.5)
  out <- .runPolys(1:2, targetBurnRate = c(NA, NA), targetMaxFireSize = c("1" = NA, 50000))
  expect_equal(out[PolyID == 2]$emfs_ha, 50000)
  expect_equal(out[PolyID == 1]$xMax, 30000)
})
