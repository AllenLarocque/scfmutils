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

## outputs of scfmutils 2.0.9.9005 (before fireMultiplier) on the polygon above
.upstreamDefault <- list(
  ignitionRate = 1e-05, pEscape = 0.25, xBar = 3823.2442, lxBar = 6.12741686810249,
  xMax = 30000, emfs_ha = 35542.3451086944, empiricalBurnRate = 0.0095596105, PolyID = "1"
)
.upstreamTargetBurnRate <- modifyList(.upstreamDefault,
                                      list(pEscape = 0.5, xBar = 5999.05853904822))
.upstreamTargetMaxFireSize <- modifyList(.upstreamDefault, list(xMax = 50000, emfs_ha = 50000))

test_that("defaults give the same results as before fireMultiplier", {
  expect_equal(.calcZonal(.zonalArgs()), .upstreamDefault)
  expect_equal(.calcZonal(.zonalArgs(fireMultiplier = 1)), .upstreamDefault)
})

test_that("targetBurnRate results are unchanged", {
  expect_equal(.calcZonal(.zonalArgs(targetBurnRate = c("1" = 0.03))), .upstreamTargetBurnRate)
  expect_warning(
    out <- .calcZonal(.zonalArgs(targetBurnRate = c("1" = 0.001))),
    "ratio cannot be < 1"
  )
  expect_equal(out, .upstreamDefault)
  expect_equal(.calcZonal(.zonalArgs(targetMaxFireSize = c("1" = 50000))),
               .upstreamTargetMaxFireSize)
})

test_that("targetBurnRate takes priority over fireMultiplier", {
  expect_message(
    out <- .calcZonal(.zonalArgs(targetBurnRate = c("1" = 0.03), fireMultiplier = 3)),
    "fireMultiplier is ignored"
  )
  expect_equal(out, .upstreamTargetBurnRate)
})

test_that("fireMultiplier scales the components via partitionBurnRateMultiplier()", {
  for (method in c("weighted", "stepwise")) {
    out <- .calcZonal(.zonalArgs(fireMultiplier = 2, fireMultiplierMethod = method))
    expected <- partitionBurnRateMultiplier(2, ignitionRate = 1e-05, pEscape = 0.25,
                                            meanFireSize = 3823.2442, method = method)
    expect_equal(out$ignitionRate, expected$ignitionRate)
    expect_equal(out$pEscape, expected$pEscape)
    expect_equal(out$xBar, expected$meanFireSize)
    ## everything else is unchanged
    expect_equal(out[c("lxBar", "xMax", "emfs_ha", "empiricalBurnRate", "PolyID")],
                 .upstreamDefault[c("lxBar", "xMax", "emfs_ha", "empiricalBurnRate", "PolyID")])
  }
})

test_that("fireMultiplierWeights are passed on", {
  w <- c(ignition = 1, escape = 0, size = 0)
  out <- .calcZonal(.zonalArgs(fireMultiplier = 2, fireMultiplierWeights = w))
  expect_equal(out$ignitionRate, 2e-05)
  expect_equal(out$pEscape, 0.25)
  expect_equal(out$xBar, 3823.2442)
})

test_that("scaleMaxFireSize scales the maximum by the mean-size factor", {
  out <- .calcZonal(.zonalArgs(fireMultiplier = 2, scaleMaxFireSize = TRUE))
  sizeFactor <- out$xBar / .upstreamDefault$xBar
  expect_gt(sizeFactor, 1)
  expect_equal(out$emfs_ha, .upstreamDefault$emfs_ha * sizeFactor)
  expect_equal(out$xMax, .upstreamDefault$xMax * sizeFactor)
})

test_that("targetMaxFireSize takes priority over scaleMaxFireSize", {
  out <- .calcZonal(.zonalArgs(fireMultiplier = 2, scaleMaxFireSize = TRUE,
                               targetMaxFireSize = c("1" = 50000)))
  expect_equal(out$emfs_ha, 50000)
  expect_equal(out$xMax, 50000)
})

test_that("fireMultiplier of 0 gives no fire", {
  out <- .calcZonal(.zonalArgs(fireMultiplier = 0))
  expect_equal(out$ignitionRate, 0)
  expect_equal(out$pEscape, 0)
  expect_equal(out$xBar, .upstreamDefault$xBar)
})

test_that("negative fireMultiplier is rejected", {
  expect_error(.calcZonal(.zonalArgs(fireMultiplier = -1)), "multiplier")
})
