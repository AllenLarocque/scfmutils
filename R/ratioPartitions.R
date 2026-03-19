#' Ratio partition
#'
#' @param targetBurnRate target burn rate
#' @param empiricalBurnRate burn rate estimated by the model
#' @param pEscape escape probability
#' @param xBar mean fire size
#' @param rate ignition rate
#'
#' @returns a list containing the following elements:
#'         `rate` (ignition rate),
#'         `pEscape` (escape probability),
#'         `xBar` (mean fire size).
#'
#' @export
#'
#' @rdname ratioPartition
ratioPartition <- function(targetBurnRate, empiricalBurnRate, pEscape, xBar, rate) {
  browser()

  ratio <- targetBurnRate / empiricalBurnRate

  ## How do I partition this 'ratio' among 3 processes: same increase in all processes for now.
  pEscapeRatio <- ratio^(1 / 3)
  # ignitionRatio <- ratio^(1/3)
  # sizeRatio <- ratio^(1/3)
  # ratioPart <-partitions::restrictedparts(pEscapeRatio, 3) ### only works with small numbers
  switch <- FALSE
  if (pEscapeRatio <= 2) {
    pEscapeTemp <- pEscape * 2
    pEscapeRatioNew <- pEscapeRatio / 2
    # Evaluate pEscape for being > 1
    if (pEscapeTemp > 1) {
      warning("careful, pEscape cannot be greater than one")
      # If > 1, keep pEscape at 1 and switch the function
      pEscapeTemp <- 1
      # we switch to 2
      switch <- TRUE
    }
  }
  if (any(switch, all(pEscapeRatio > 2, pEscapeRatio <= 4, pEscape <= 1))) {
    pEscapeTemp <- pEscape * 4
    pEscapeRatioNew <- pEscapeRatio / 4
  }
  if (all(pEscapeRatio > 4, pEscape <= 1)) {
    pEscapeTemp <- pEscape * 4
    xBar <- xBar * 2
    pEscapeRatioNew <- pEscapeRatio / 6
  } else {
    warning("pEscape bigger than 1")
  }
  if (all(pEscapeRatioNew > 4, pEscape <= 1)) {
    pEscapeTemp <- pEscape * 4
    xBar <- xBar * 2
    rate <- rate * 2

    counter <- 0
    while (all(pEscapeRatioNew > 4, pEscape <= 1)) {
      xBar <- xBar * (2 + counter)
      rate <- rate * (2 + counter)
      pEscapeRatioNew <- pEscapeRatio / (8 + counter)
      counter <- counter + 1
    }
  }
  return(list(
    rate = rate,
    pEscape = pEscapeTemp,
    xBar = xBar # mean fire size
  ))
}

#' @export
#' @rdname ratioPartition
#ratioPartition2 <- function(targetBurnRate, empiricalBurnRate, pEscape, xBar, rate) {
#  ratio <-  targetBurnRate / empiricalBurnRate

#  remains <- ratio
  #
  #   step <- function(remains){
  #     step = min(remains, 2)
  #   }
  #
  #   remains <- function(remains, step){
  #     remains <- remains / step
  #     print(remains)
  #   }
  #
  #   while (step > 2){
  #     pEscape <- pEscape * step
  #     remains(remains, step)
  #     step <- step (remains)
  #     if (step > 2){
  #       xBar <- xBar * step
  #       remains(remains, step)}
  #   }
  # return(list(
  #   rate = rate,
  #   pEscape = pEscape,
  #   xBar = xBar  #mean fire size
  # ))
  # }
#   step <- min(remains, 2)
#   pEscape <- pEscape * step
#   remains <- remains / step
#   step <- min(remains, 2)
#   xBar <- xBar * step
#   remains <- remains / step
#   step <- min(remains, 2)
#   pEscape <- pEscape * step
#   remains <- remains/step
#   step <- min(remains, 2)
#   xBar <- xBar * step
#   remains <- remains / step
#   step <- min(remains, 2)
#   rate <- rate  * step
#   remains <- remains / step
#   xBar <- xBar * remains

#   return(list(
#     rate = rate,
#     pEscape = pEscape,
#     xBar = xBar  #mean fire size
#   ))
# }


#' @export
#' @rdname ratioPartition
ratioPartition2 <- function(targetBurnRate, empiricalBurnRate, pEscape, xBar, rate) {
  ratio <- targetBurnRate / empiricalBurnRate
  remains <- ratio
  # This kees the older logic above of assigning to pEscape first.
  # Probably does not reflect error structure wrong when scaling down. Maybe ratioPartion3() is better?
  # When ratio > 1: scale params up. step = min(remains, 2) caps growth per step.
  # When ratio < 1: scale params down. step = max(remains, 0.5) avoids tiny steps.
  # When ratio = 1: remains = 1, step = 1, no change.
  minMax <- if (remains > 1) min else max
  stepBound <- if (remains > 1) 2 else 0.5

  step <- minMax(remains, stepBound)
  pEscape <- pEscape * step
  remains <- remains / step

  step <- minMax(remains, stepBound)
  xBar <- xBar * step
  remains <- remains / step

  step <- minMax(remains, stepBound)
  pEscape <- pEscape * step
  remains <- remains / step

  step <- minMax(remains, stepBound)
  xBar <- xBar * step
  remains <- remains / step

  step <- minMax(remains, stepBound)
  rate <- rate * step
  remains <- remains / step

  # Final remainder goes to xBar
  xBar <- xBar * remains

  # Enforce pEscape <= 1: redistribute excess to rate and xBar
  if (pEscape > 1) {
    scale <- sqrt(pEscape)
    rate <- rate * scale
    xBar <- xBar * scale
    pEscape <- 1
  }

  list(rate = rate, pEscape = pEscape, xBar = xBar)
}

#' Adjust fire regime parameters to match a target burn rate.
#'
#' Burn rate is proportional to rate × pEscape × xBar. To change burn rate by
#' factor r = target/empirical, we scale these three parameters multiplicatively.
#' We partition the change equally across all three, subject to pEscape ≤ 1.
#' This should be tolerant to targetBurnRate=0; empiricalBurnRate=0; pEscape=0; xBar=0; rate=0.
#' 
#' @param targetBurnRate Desired burn rate (area per ha per year).
#' @param empiricalBurnRate Current burn rate from data/model.
#' @param pEscape Escape probability (proportion of fires that grow beyond 1 cell).
#' @param xBar Mean fire size of escaped fires.
#' @param rate Ignition rate (fires per ha per year).
#'
#' @return List with adjusted rate, pEscape, xBar.
#'
#' @export
#' @rdname ratioPartition
ratioPartition3 <- function(targetBurnRate, empiricalBurnRate, pEscape, xBar, rate) {
  # ---- Step 1: Compute target ratio ----
  # r > 1: need higher burn rate → scale params up
  # r < 1: need lower burn rate → scale params down
  # r = 1: no change
  r <- targetBurnRate / empiricalBurnRate

  # ---- Step 2: Equal multiplicative partition ----
  # Ideal: scale each of (rate, pEscape, xBar) by s = r^(1/3)
  # Then product scales by s³ = r, as required.
  s <- r^(1 / 3)

  # ---- Step 3: Enforce pEscape ≤ 1 ----
  # If s * pEscape would exceed 1, cap pEscape at 1 and put the remaining
  # change into rate and xBar. We need:
  #   rate' * pEscape' * xBar' = r * rate * pEscape * xBar
  # With pEscape' = 1:  rate' * xBar' = r * rate * pEscape * xBar
  # Equal partition for rate and xBar: each scales by t = (r * pEscape)^(1/2)
  if (empiricalBurnRate <= 0) {
    stop("empiricalBurnRate must be positive")
  }
  if (s * pEscape <= 1) {
    # No constraint hit: apply s to all three
    pEscapeNew <- pEscape * s
    rateNew    <- rate * s
    xBarNew    <- xBar * s
  } else {
    # pEscape capped at 1; remaining ratio r * pEscape split between rate and xBar
    t <- (r * pEscape)^(1 / 2)
    pEscapeNew <- 1
    rateNew    <- rate * t
    xBarNew    <- xBar * t
  }

  list(rate = rateNew, pEscape = pEscapeNew, xBar = xBarNew)
}