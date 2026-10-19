#' Partition a burn rate multiplier among ignition, escape, and fire size
#'
#' `scfm` represents a fire regime polygon's burn rate (proportion of burnable area burned per year)
#' approximately as the product of three components:
#' \deqn{burn rate \approx ignitionRate \times pEscape \times meanFireSize}
#' To change the burn rate by a factor `multiplier`, this function multiplies each component by its
#' own factor, chosen so that the product of the three factors equals `multiplier`.
#'
#' @section `method = "weighted"`:
#' Each component is multiplied by `multiplier^weight`, using `weights`
#' (a share of the change on the log scale for each component).
#' With the default weights, a doubling (`multiplier = 2`) multiplies ignition rate by 1.23,
#' escape probability by 1.32, and mean fire size by 1.23.
#' Weights below 1 for a component damp its change; a weight of 0 leaves it unchanged.
#'
#' Escape probability cannot exceed 1. If it would, it is set to 1 and the rest of its factor is
#' given to ignition rate and mean fire size, in proportion to their weights, so the product is
#' still exact. If both of those weights are 0, the target cannot be reached and a warning is given.
#'
#' @section `method = "stepwise"`:
#' Uses [ratioPartition2()], as `calcZonalRegimePars()` does for `targetBurnRate`:
#' the components are multiplied in steps of up to 2, in the order escape, size, escape, size,
#' ignition, and any remainder goes to mean fire size. Ignition rate therefore changes only when
#' `multiplier > 16`. Below 1, the whole reduction is applied to escape probability.
#'
#' @section Special cases:
#' - `multiplier = 1` returns the inputs unchanged.
#' - `multiplier = 0` means no fire: `ignitionRate` and `pEscape` are set to 0.
#'   `meanFireSize` is kept, so that diagnostics still describe the historical regime.
#' - `pEscape = 0` (no escaped fires) cannot be scaled through escape or size; a warning is given
#'   and the inputs are returned unchanged.
#'
#' @param multiplier numeric. Factor by which to change the burn rate: a single finite value `>= 0`.
#' @param ignitionRate numeric. Ignition rate (fires per ha per year).
#' @param pEscape numeric. Escape probability (proportion of fires larger than one cell).
#' @param meanFireSize numeric. Mean size of escaped fires (ha).
#' @param method character. `"weighted"` (default) or `"stepwise"`; see the sections below.
#' @param weights named numeric. For `method = "weighted"`, the share of the change given to each
#'   component; names `ignition`, `escape`, and `size`, in any order, non-negative, summing to 1.
#'   The default follows a national-scale reading of the literature that escape contributes
#'   slightly more than ignition and size to changes in area burned.
#'
#' @returns a list with elements `ignitionRate`, `pEscape`, `meanFireSize` (the new values), and
#'   `sizeFactor` (the factor applied to `meanFireSize`; 1 if it is unchanged).
#'
#' @export
#'
#' @examples
#' partitionBurnRateMultiplier(2, ignitionRate = 1e-4, pEscape = 0.2, meanFireSize = 500)
#' partitionBurnRateMultiplier(2, ignitionRate = 1e-4, pEscape = 0.2, meanFireSize = 500,
#'                             method = "stepwise")
partitionBurnRateMultiplier <- function(multiplier, ignitionRate, pEscape, meanFireSize,
                                        method = c("weighted", "stepwise"),
                                        weights = c(ignition = 0.3, escape = 0.4, size = 0.3)) {
  method <- match.arg(method)

  if (!is.numeric(multiplier) || length(multiplier) != 1 || !is.finite(multiplier) ||
        multiplier < 0) {
    stop("`multiplier` must be a single finite number >= 0")
  }
  weights <- .checkMultiplierWeights(weights)

  unchanged <- list(ignitionRate = ignitionRate, pEscape = pEscape, meanFireSize = meanFireSize,
                    sizeFactor = 1)

  if (multiplier == 1) {
    return(unchanged)
  }

  if (multiplier == 0) {
    return(list(ignitionRate = 0, pEscape = 0, meanFireSize = meanFireSize, sizeFactor = 1))
  }

  if (pEscape == 0) {
    warning("There are no escaped fires (pEscape = 0); the burn rate multiplier has no effect.")
    return(unchanged)
  }

  if (method == "stepwise") {
    out <- ratioPartition2(targetBurnRate = multiplier, empiricalBurnRate = 1,
                           pEscape = pEscape, xBar = meanFireSize, rate = ignitionRate)
    return(list(ignitionRate = out$rate, pEscape = out$pEscape, meanFireSize = out$xBar,
                sizeFactor = out$xBar / meanFireSize))
  }

  ## weighted: work on the log scale, where the three factors add up to log(multiplier)
  logM <- log(multiplier)
  logF <- logM * weights

  maxLogEscape <- -log(pEscape) ## pEscape cannot exceed 1
  if (logF[["escape"]] > maxLogEscape) {
    excess <- logF[["escape"]] - maxLogEscape
    logF[["escape"]] <- maxLogEscape
    otherWeights <- weights[["ignition"]] + weights[["size"]]
    if (otherWeights > 0) {
      logF[["ignition"]] <- logF[["ignition"]] + excess * weights[["ignition"]] / otherWeights
      logF[["size"]] <- logF[["size"]] + excess * weights[["size"]] / otherWeights
    } else {
      warning("The burn rate multiplier cannot be reached: pEscape is capped at 1, ",
              "and the ignition and size weights are both 0.")
    }
  }

  f <- exp(logF)
  list(
    ignitionRate = ignitionRate * f[["ignition"]],
    pEscape = if (logF[["escape"]] == maxLogEscape) 1 else pEscape * f[["escape"]],
    meanFireSize = meanFireSize * f[["size"]],
    sizeFactor = f[["size"]]
  )
}

.checkMultiplierWeights <- function(weights) {
  wNames <- c("ignition", "escape", "size")
  if (!is.numeric(weights) || length(weights) != 3 || !setequal(names(weights), wNames)) {
    stop("`weights` must be a numeric vector named ", paste(wNames, collapse = ", "))
  }
  if (anyNA(weights) || any(weights < 0) || abs(sum(weights) - 1) > sqrt(.Machine$double.eps)) {
    stop("`weights` must be non-negative and sum to 1")
  }
  weights[wNames]
}
