## product of the factors applied to the three components
.totalFactor <- function(out, ignitionRate, pEscape, meanFireSize) {
  (out$ignitionRate / ignitionRate) * (out$pEscape / pEscape) * (out$meanFireSize / meanFireSize)
}

test_that("multiplier of 1 returns inputs unchanged", {
  for (method in c("weighted", "stepwise")) {
    out <- partitionBurnRateMultiplier(1, ignitionRate = 1e-4, pEscape = 0.2, meanFireSize = 500,
                                       method = method)
    expect_equal(out, list(ignitionRate = 1e-4, pEscape = 0.2, meanFireSize = 500, sizeFactor = 1))
  }
})

test_that("multiplier of 0 gives no ignitions and no escapes, keeping mean size", {
  for (method in c("weighted", "stepwise")) {
    out <- partitionBurnRateMultiplier(0, ignitionRate = 1e-4, pEscape = 0.2, meanFireSize = 500,
                                       method = method)
    expect_equal(out, list(ignitionRate = 0, pEscape = 0, meanFireSize = 500, sizeFactor = 1))
  }
})

test_that("weighted split changes the burn rate by exactly the multiplier", {
  weightSets <- list(
    c(ignition = 0.3, escape = 0.4, size = 0.3),
    c(ignition = 1 / 3, escape = 1 / 3, size = 1 / 3),
    c(ignition = 0, escape = 0.5, size = 0.5),
    c(ignition = 1, escape = 0, size = 0)
  )
  for (w in weightSets) {
    for (m in c(0.25, 0.5, 0.9, 1.5, 2, 5, 20)) {
      out <- partitionBurnRateMultiplier(m, ignitionRate = 1e-4, pEscape = 0.02, meanFireSize = 300,
                                         weights = w)
      expect_equal(.totalFactor(out, 1e-4, 0.02, 300), m)
      expect_equal(out$sizeFactor, out$meanFireSize / 300)
    }
  }
})

test_that("default weights give the documented factors for a doubling", {
  out <- partitionBurnRateMultiplier(2, ignitionRate = 1, pEscape = 0.1, meanFireSize = 1)
  expect_equal(out$ignitionRate, 2^0.3)
  expect_equal(out$pEscape, 0.1 * 2^0.4)
  expect_equal(out$meanFireSize, 2^0.3)
})

test_that("escape is capped at 1 and the excess goes to ignition and size by weight", {
  w <- c(ignition = 0.2, escape = 0.6, size = 0.2)
  out <- partitionBurnRateMultiplier(10, ignitionRate = 1e-4, pEscape = 0.5, meanFireSize = 300,
                                     weights = w)
  expect_equal(out$pEscape, 1)
  expect_equal(.totalFactor(out, 1e-4, 0.5, 300), 10)
  ## equal ignition and size weights, so the leftover is split equally
  expect_equal(out$ignitionRate / 1e-4, out$meanFireSize / 300)
})

test_that("escape cap warns when ignition and size weights are both 0", {
  w <- c(ignition = 0, escape = 1, size = 0)
  expect_warning(
    out <- partitionBurnRateMultiplier(4, ignitionRate = 1e-4, pEscape = 0.5, meanFireSize = 300,
                                       weights = w),
    "cannot be reached"
  )
  expect_equal(out$pEscape, 1)
  expect_equal(out$ignitionRate, 1e-4)
  expect_equal(out$meanFireSize, 300)
})

test_that("stepwise matches ratioPartition2()", {
  for (m in c(0.5, 1.5, 2, 3, 8, 16, 40)) {
    for (pEsc in c(0.02, 0.3)) {
      old <- ratioPartition2(targetBurnRate = m, empiricalBurnRate = 1,
                             pEscape = pEsc, xBar = 300, rate = 1e-4)
      new <- partitionBurnRateMultiplier(m, ignitionRate = 1e-4, pEscape = pEsc,
                                         meanFireSize = 300, method = "stepwise")
      expect_equal(new$ignitionRate, old$rate)
      expect_equal(new$pEscape, old$pEscape)
      expect_equal(new$meanFireSize, old$xBar)
      expect_equal(new$sizeFactor, old$xBar / 300)
    }
  }
})

test_that("no escaped fires: warning and inputs unchanged", {
  for (method in c("weighted", "stepwise")) {
    expect_warning(
      out <- partitionBurnRateMultiplier(2, ignitionRate = 1e-4, pEscape = 0, meanFireSize = 0,
                                         method = method),
      "no escaped fires"
    )
    expect_equal(out, list(ignitionRate = 1e-4, pEscape = 0, meanFireSize = 0, sizeFactor = 1))
  }
})

test_that("invalid multipliers are rejected", {
  for (m in list(-1, NA_real_, Inf, NaN, c(1, 2), "2", NULL)) {
    expect_error(
      partitionBurnRateMultiplier(m, ignitionRate = 1e-4, pEscape = 0.2, meanFireSize = 500),
      "multiplier"
    )
  }
})

test_that("invalid weights are rejected", {
  bad <- list(
    unnamed = c(0.3, 0.4, 0.3),
    missingSize = c(ignition = 0.3, escape = 0.4),
    wrongName = c(ignition = 0.3, escape = 0.4, sizes = 0.3),
    sumNot1 = c(ignition = 0.5, escape = 0.4, size = 0.3),
    negative = c(ignition = -0.1, escape = 0.8, size = 0.3),
    missingValue = c(ignition = NA, escape = 0.7, size = 0.3)
  )
  for (w in bad) {
    expect_error(
      partitionBurnRateMultiplier(2, ignitionRate = 1e-4, pEscape = 0.2, meanFireSize = 500,
                                  weights = w),
      "weights"
    )
  }
})

test_that("weights may be given in any order", {
  a <- partitionBurnRateMultiplier(3, 1e-4, 0.2, 500,
                                   weights = c(ignition = 0.2, escape = 0.5, size = 0.3))
  b <- partitionBurnRateMultiplier(3, 1e-4, 0.2, 500,
                                   weights = c(size = 0.3, ignition = 0.2, escape = 0.5))
  expect_equal(a, b)
})
