#' `scfmRegime`: `calcZonalRegimePars`
#'
#' Estimates the fire regime parameters of one fire regime polygon from historical fire points.
#'
#' @param polygonID character. `PolyID` of the polygon to estimate.
#' @param firePolys fire regime polygons (`sf` or `data.frame`) with columns `PolyID`, `cellSize`
#'   (ha), and `burnyArea` (burnable area, ha).
#' @param firePoints fire points (`sf` or `data.frame`) with a `PolyID` column and a fire size column.
#' @param epochLength numeric. Number of years covered by `firePoints`.
#' @param maxSizeFactor numeric. Factor applied to the largest observed fire to give the maximum
#'   fire size when the Hannon-Dahiya estimate fails.
#' @param fireSizeColumnName character. Name of the fire size column (ha) in `firePoints`.
#' @param targetBurnRate named numeric, by `PolyID`. Burn rate (proportion of `burnyArea` burned
#'   per year) to use instead of the empirical one. `NA` for none.
#'   Partitioned with [ratioPartition2()]; targets below the empirical burn rate are ignored,
#'   with a warning. Takes priority over `fireMultiplier`.
#' @param targetMaxFireSize named numeric, by `PolyID`. Maximum fire size (ha) to use instead of
#'   the estimated one. `NA` for none. Takes priority over `scaleMaxFireSize`.
#' @param fireMultiplier numeric. Factor by which to change the empirical burn rate
#'   (e.g. 2 doubles it); 0 means no fire. Partitioned among ignition rate, escape probability, and
#'   mean fire size by [partitionBurnRateMultiplier()]. The default (1) changes nothing.
#' @param fireMultiplierMethod character. `method` passed to [partitionBurnRateMultiplier()].
#' @param fireMultiplierWeights named numeric. `weights` passed to [partitionBurnRateMultiplier()].
#' @param scaleMaxFireSize logical. If `TRUE`, the maximum fire size (`xMax`, `emfs_ha`) is scaled
#'   by the same factor as mean fire size when `fireMultiplier` is applied.
#'
#' @seealso [partitionBurnRateMultiplier()]
#'
#' @returns list containing the following elements and their values:
#'  `ignitionRate` (ignition rate),
#'  `pEscape` (escape probability),
#'  `xBar` (mean fire size),
#'  `lxBar` (mean log-fire-size),
#'  `xMax` (maximum observed fire size),
#'  `emfs_ha` (estimated maximum fire size in ha),
#'  `empiricalBurnRate` (empirical burn rate)
#'
#' @export
calcZonalRegimePars <- function(polygonID, firePolys, firePoints, epochLength, maxSizeFactor,
                                fireSizeColumnName, targetBurnRate = NULL,
                                targetMaxFireSize = NULL, fireMultiplier = 1,
                                fireMultiplierMethod = "weighted",
                                fireMultiplierWeights = c(ignition = 0.3, escape = 0.4, size = 0.3),
                                scaleMaxFireSize = FALSE) {
  firePoly <- firePolys[firePolys$PolyID == polygonID, ]
  landAttr <- as.data.table(firePoly)
  landAttr <- unique(landAttr[, .SD, .SDcol = setdiff(colnames(firePoly), "geometry")])
  polyPoints <- firePoints[firePoints$PolyID == as.numeric(polygonID), ]

  cellSize <- landAttr[["cellSize"]]
  nFires <- dim(polyPoints)[1]
  if (nFires == 0) {
    return(data.table(PolyID = polygonID)) ## confirm whether NULL values must be added for rbind to work
  }
  ignitionRate <- nFires / (epochLength * landAttr$burnyArea) ## fires per ha per yr
  pEscape <- 0
  xBar <- 0 ## mean fire size
  xMax <- 0
  lxBar <- NA
  emfs_ha <- cellSize ## note that maxFireSize has unit of ha NOT cells!!!
  xVec <- numeric(0)

  ## check for user supplied defaults
  targetBurnRate <- targetBurnRate[polygonID]
  targetMaxFireSize <- targetMaxFireSize[polygonID]

  if (nFires > 0) {
    ## calculate escaped fires; careful to subtract cellSize where appropriate
    xVec <- polyPoints[[fireSizeColumnName]][polyPoints[[fireSizeColumnName]] > cellSize]
    if (length(xVec) > 0) {
      pEscape <- length(xVec) / nFires
      xBar <- mean(xVec)
      lxBar <- mean(log(xVec))
      xMax <- max(xVec)
      xFireSize <- xBar
      zVec <- log(xVec / cellSize)
      if (length(zVec) < 25)
        warning(paste("Less than 25 \"large\" fires in zone", polygonID, ".",
                      "Estimates may be unstable.\n",
                      "\tConsider using a larger area and/or longer epoch.\n"))
      hdList <- HannonDayiha(zVec)
      That <- hdList$That
      if (That == -1) {
        warning(
          sprintf(
            "Hannon-Dahiya convergence failure in zone %s.\n\tUsing sample maximum fire size",
            polygonID
          )
        )
        emfs_ha <- xMax * maxSizeFactor  #just to be safe, re-specify here
      } else {
        emfs_ha <- exp(That) * cellSize
        if (!(emfs_ha > xMax)) {
          warning(
            sprintf("Dodgy maxFireSize estimate in zone %s.\n\tUsing sample maximum fire size.",
                    polygonID)
          )
          emfs_ha <- xMax * maxSizeFactor
        }
        ## missing BEACONS CBFA truncated at 2*xMax. Their reasons don't apply here.
      }
    } else {
      ## there should be a way to pass non-zero defaults but I'm not sure whether we would specify by polygon
      ## and if so, how, given the initial polygons may be modified during sliver removal
      message(paste("no fires larger than cellsize in ", polygonID, "."))
    }
  } else {
    message(paste("Insufficient data for polygon ", polygonID, ". Default values used."))
  }

  ## verify estimation results are reasonable. That=-1 indicates convergence failure.
  ## need to add a name or code for basic verification by Driver module, and time field
  ## to allow for dynamic regeneration of disturbanceDriver pars.
  if (emfs_ha < 1) {
    warning("this can't happen") ## TODO: improve messaging for users
    emfs_ha <- cellSize
  }

  empiricalBurnRate <- sum(polyPoints[[fireSizeColumnName]]) / (epochLength * landAttr$burnyArea)

  if (is.na(targetBurnRate) || is.null(targetBurnRate)) {
    ratio <- 1
  }

  if (!is.na(targetBurnRate) || is.null(targetBurnRate)) {
    ratio <- targetBurnRate / empiricalBurnRate
    if (ratio >= 1) {
      newFireValues <- ratioPartition2(
        targetBurnRate = targetBurnRate,
        empiricalBurnRate = empiricalBurnRate,
        pEscape = pEscape,
        xBar = xBar,
        rate = ignitionRate
      )
      ignitionRate <- newFireValues$rate
      pEscape <- newFireValues$pEscape
      xBar <- newFireValues$xBar
    } else {
      ## TODO: improve messaging for users
      warning("ratio cannot be < 1. Please make sure this does not happen.")
    }
  }

  if (!isTRUE(fireMultiplier == 1)) {
    if (!is.na(targetBurnRate)) {
      message("fireMultiplier is ignored in polygon ", polygonID, " because targetBurnRate is set.")
    } else {
      newFireValues <- partitionBurnRateMultiplier(
        multiplier = fireMultiplier,
        ignitionRate = ignitionRate,
        pEscape = pEscape,
        meanFireSize = xBar,
        method = fireMultiplierMethod,
        weights = fireMultiplierWeights
      )
      ignitionRate <- newFireValues$ignitionRate
      pEscape <- newFireValues$pEscape
      xBar <- newFireValues$meanFireSize
      if (isTRUE(scaleMaxFireSize)) {
        emfs_ha <- emfs_ha * newFireValues$sizeFactor
        xMax <- xMax * newFireValues$sizeFactor
      }
    }
  }

  ## override maximum fire size if user supplied
  if (!is.na(targetMaxFireSize) || is.null(targetMaxFireSize)) {
    emfs_ha <- targetMaxFireSize
    xMax <- targetMaxFireSize
    ## TODO: add check that max is larger than mean, else stop
  }

  paramData <- cbind(ignitionRate, pEscape, xBar, lxBar, xMax, emfs_ha, empiricalBurnRate) |>
    as.data.table()
  paramData$PolyID <- polygonID

  if (pEscape > 1) {
    stop("pEscape exceeds 1, which should not be possible")
  }
  ## rate - per ha/per year ; pEscape; xBar - mean fire size; lxBar - mean log;
  ## xMax - maximum observed size; emfs_ha - Estimated maximum Fire Size in ha
  ## max fire size is returned twice - I think this is a backwards compatibility decision
  return(paramData)
}
