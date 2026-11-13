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
  args <- modifyList(.ppArgs(), list(...))
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
