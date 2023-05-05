
#' Selects the \emph{best} template for a given signal based on fitting
#'        values returned by a comparison function
#'
#' @param time.instants The time instants in which the reference signals
#'        and templates are sampled
#'
#' @param signal The reference signal to select the best template
#'
#' @param templates A list of signals from which the "most similar" to the
#'        reference signal will be chosen
#'
#' @param referential.time.instant The time instant to be used as a reference
#'        in the comparison of the signal and the templates
#'        Default value: 0
#'
#' @param delta.time.before.ref The amount of time before the referential
#'        instant to be included in the comparison
#'        Default value: 0
#'
#' @param delta.time.after.ref The amount of time after the referential
#'        instant to be included in the comparison.
#'        Default value: 20 * 0.8 s (20 heart beats approx.)
#'
#' @param comparison.function The function to be used to compare and estimate
#'        the goodness-of-fit between each template and the referential signal
#'        in the period of time of interest.
#'        Default value: get.MSE
#'
#' @param keep.details Whether the answer should keep all comparisons details
#'        Default value: TRUE
#'
#' @param time.tol The minimum amount of time between two time instants.
#'        Default value: one-hundredth of the minimum time difference
#'        between tow time instants
#'
#' @param ... Arguments pass over to comparison.function()
#'
#' @return A list with (at least):
#' \describe{
#'   \item{best.template.index}{The index of the best template in the giving
#'         list of templates}
#'   \item{best.fit.value}{The fitting value scored by the best template}
#' }
#'
#' @importFrom floatUtils are.tolerably.equal
#' @importFrom metricUtils get.MSE
#'
#' @export
#'
get.best.templates <- function(
  time.instants,
  signal,
  templates,
  referential.time.instant = 0,
  delta.time.before.ref = 0,
  delta.time.after.ref = 20 * 0.8,
  comparison.function = get.MSE,
  # it need ranking order: ascendant or descendant
  keep.details = TRUE,
  time.tol = min(diff(time.instants)) / 100,
  ... # Pass over to comparison.function()
)
{
  # Validates lengths
  lengths <- c(length(time.instants), length(signal))
  lengths <- c(lengths, sapply(templates, length))
  lengths <- unique(lengths)
  if(length(lengths) != 1)
    stop("time, signal and templates must have the same length")

  # Initialises detailed answer
  ans <- list()
  ans[["time.instants"]] <- time.instants
  ans[["signal"]] <- signal
  ans[["templates"]] <- templates
  ans[["referential.time.instant"]] <- referential.time.instant

  # Finds referential sample
  i <- which(are.tolerably.equal(
    time.instants,
    referential.time.instant,
    time.tol
  ))
  if(length(i) != 1)
    stop("a unique referential time instant could not be determined")
  ans[["referential.sample"]] <- i

  # Finds initial sample
  ans[["delta.time.before.ref"]] <- delta.time.before.ref
  ans[["initial.time.instant"]] <- referential.time.instant - delta.time.before.ref
  i <- which(are.tolerably.equal(
    time.instants,
    ans[["initial.time.instant"]],
    time.tol
  ))
  if(length(i) != 1)
    stop("initial time instant could not be found in specified time instants")
  ans[["initial.sample"]] <- i

  # Finds final sample
  ans[["delta.time.after.ref"]] <- delta.time.after.ref
  ans[["final.time.instant"]] <-
    referential.time.instant + delta.time.after.ref
  i <- which(are.tolerably.equal(
    time.instants,
    ans[["final.time.instant"]],
    time.tol
  ))
  if(length(i) != 1)
    stop("final time instant could not be found in specified time instants")
  ans[["final.sample"]] <- i

  # Sets relevant interval
  ans[["relevant.samples"]] <-
    ans[["initial.sample"]]:ans[["final.sample"]]

  # Gets relevant segments
  .tmp.fun <- function(s) s[ans[["relevant.samples"]]]
  ans[["relevant.signal.segment"]] <- .tmp.fun(ans[["signal"]])
  ans[["relevant.template.segments"]] <-
    lapply(ans[["templates"]], .tmp.fun)

  # Gets fit values
  .tmp.fun <- function(t)
    comparison.function(ans[["relevant.signal.segment"]], t, ...)
  ans[["fit.values"]] <-
    sapply(ans[["relevant.template.segments"]], .tmp.fun)

  # Orders templates and determines the best one
  ans[["ranking"]] <- order(ans[["fit.values"]])

  # Deletes details if corresponds
  if(!keep.details)
  {
    i <- ans[["ranking"]][1]
    e <- ans[["fit.values"]][i]
    ans <- list(best.template.index = i, best.fit.value = e)
  }

  invisible(ans)
}
