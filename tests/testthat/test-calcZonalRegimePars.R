## synthetic polygon: 150 small fires and 50 escaped fires of 7 to 30000 ha
.zonalArgs <- function(...) {
  firePolys <- data.frame(PolyID = 1, cellSize = 6.25, burnyArea = 5e5)
  sizes <- c(rep(0.2, 150), round(exp(seq(log(7), log(30000), length.out = 50)), 2))
  firePoints <- data.frame(PolyID = 1, SIZE_HA = sizes)
  args <- list(polygonID = "1", firePolys = firePolys, firePoints = firePoints, epochLength = 40,
               maxSizeFactor = 1.2, fireSizeColumnName = "SIZE_HA",
               targetBurnRate = c("1" = NA), targetMaxFireSize = c("1" = NA))
  modifyList(args, list(...))
}

## the Hannon-Dahiya estimator does not converge on this polygon; that warning is not under test
.calcZonal <- function(args) {
  withCallingHandlers(
    as.list(do.call(calcZonalRegimePars, args)),
    warning = function(w) {
      if (grepl("convergence failure", conditionMessage(w))) invokeRestart("muffleWarning")
    }
  )
}

## outputs of scfmutils 2.0.9.9005 on the polygon above
.upstreamDefault <- list(
  ignitionRate = 1e-05, pEscape = 0.25, xBar = 3823.2442, lxBar = 6.12741686810249,
  xMax = 30000, emfs_ha = 35542.3451086944, empiricalBurnRate = 0.0095596105, PolyID = "1"
)
.upstreamTargetBurnRate <- modifyList(.upstreamDefault,
                                      list(pEscape = 0.5, xBar = 5999.05853904822))
.upstreamTargetMaxFireSize <- modifyList(.upstreamDefault, list(xMax = 50000, emfs_ha = 50000))

test_that("NA targets give the same results as before", {
  expect_equal(.calcZonal(.zonalArgs()), .upstreamDefault)
  expect_equal(.calcZonal(.zonalArgs(targetBurnRate = c("1" = 0.03))), .upstreamTargetBurnRate)
  expect_warning(
    out <- .calcZonal(.zonalArgs(targetBurnRate = c("1" = 0.001))),
    "ratio cannot be < 1"
  )
  expect_equal(out, .upstreamDefault)
  expect_equal(.calcZonal(.zonalArgs(targetMaxFireSize = c("1" = 50000))),
               .upstreamTargetMaxFireSize)
})

test_that("NULL targets (the argument defaults) are treated as not set", {
  expect_equal(.calcZonal(.zonalArgs(targetBurnRate = NULL, targetMaxFireSize = NULL)),
               .upstreamDefault)
  expect_equal(.calcZonal(.zonalArgs(targetBurnRate = NULL)), .upstreamDefault)
  expect_equal(.calcZonal(.zonalArgs(targetMaxFireSize = NULL)), .upstreamDefault)
  expect_equal(.calcZonal(.zonalArgs(targetBurnRate = NULL, targetMaxFireSize = c("1" = 50000))),
               .upstreamTargetMaxFireSize)
})

test_that("targets not named for the polygon are treated as not set", {
  expect_equal(.calcZonal(.zonalArgs(targetBurnRate = c("2" = 0.03))), .upstreamDefault)
})
