#' Normalises a signal of cerebral blood flow velocity (CBFV)
#'
#' @param time.instants The time instants in which the ABP signal
#'        is sampled
#'
#' @param CBFV.signal The original CBFV signal
#'
#' @param sample.release The index of the sample when thigh cuffs were
#'        realeased
#'
#' @param sampling.time The sampling time used in the CBFV signal
#'
#' @param baseline.initial.time The time instant that marks the beggining
#'        of the signal baseline.
#'        Default value: \code{time.instants[1]}
#'
#' @param baseline.final.time The time instant that marks the end
#'        of the signal baseline.
#'        Default value: \code{time.instants[sample.release]}
#'
#' @param min.CBFV.max.delta.time The amount of time to search for the
#'        minimum CBFV value after the realease of the thigh cuffs.
#'        Default value: 8 * 0.8 s (8 heart beats approx.)
#'
#' @param time.tol The minimum amount of time between two time instants.
#'        Default value: \code{time.instants[sampling.time]} / 100
#'
#'  @return A list with the following:
#'  \describe{
#'    \item{<*details*>}{to be able to normalised the signal}
#'    \item{normalised.ABP.signal}{The normalised ABP signal}
#'  }
#'
#' @importFrom pracma findpeaks
#'
#' @export
#'
normalise.CBFV.signal <- function(
  time.instants,
  CBFV.signal,
  sample.release,
  sampling.time,
  baseline.initial.time = time.instants[1],
  baseline.final.time = time.instants[sample.release],
  min.CBFV.max.delta.time = 8 * 0.8,
  time.tol = sampling.time / 100
)
{
  ans <- list()
  valid <- !is.na(CBFV.signal)
  time.release <- time.instants[sample.release]

  # Adds baseline data to answer
  if(baseline.final.time < baseline.initial.time)
    stop("final time for the CBFV baseline cannot be before its initial time")
  if(baseline.final.time > time.release)
    stop("final time for the CBFV baseline cannot be after the release of the thigh cuffs")

  i <- which(valid & time.instants >= baseline.initial.time - time.tol)
  if(length(i) == 0)
    stop("no time instant for the beginning of the CBFV baseline could be determined")
  i <- i[1]
  ans[["CBFV.baseline.initial.time"]] <- time.instants[i]
  ans[["CBFV.baseline.initial.sample"]] <- i

  i <- which(valid & time.instants <= baseline.final.time + time.tol)
  if(length(i) == 0)
    stop("no time instant for the end of the CBFV baseline could be determined")
  i <- tail(i, 1)
  ans[["CBFV.baseline.final.time"]] <- time.instants[i]
  ans[["CBFV.baseline.final.sample"]] <- i

  ans[["CBFV.baseline.samples"]] <- ans[["CBFV.baseline.initial.sample"]]:ans[["CBFV.baseline.final.sample"]]
  ans[["CBFV.baseline.duration"]] <- ans[["CBFV.baseline.final.time"]] - ans[["CBFV.baseline.initial.time"]]

  # Gets baseline value
  ans[["CBFV.baseline.value"]] <- mean(CBFV.signal[ans[["CBFV.baseline.samples"]]], na.rm = TRUE)

  # Sets min CBFV window
  ans[["min.CBFV.max.delta.time"]] <- min.CBFV.max.delta.time
  i <- time.instants > time.release + time.tol
  j <- time.instants < time.release + ans[["min.CBFV.max.delta.time"]] +
    time.tol
  ans[["min.CBFV.samples"]] <- which(valid & i & j)
  if(length(ans[["min.CBFV.samples"]]) == 0)
    stop("no candidates for min CBFV")

  # Gets minimum CBFV
  ans[["min.CBFV.window"]] <- CBFV.signal[ans[["min.CBFV.samples"]]]
  min.sample.in.window <- which.min(ans[["min.CBFV.window"]])
  min.CBFV.sample <- min.sample.in.window + ans[["min.CBFV.samples"]][1] - 1
  ans[["min.CBFV.time.instant"]] <- time.instants[min.CBFV.sample]
  ans[["min.CBFV.sample"]] <- min.CBFV.sample
  ans[["min.CBFV.value"]] <- CBFV.signal[min.CBFV.sample]

  # Gets min CBFV type
  min.info <- findpeaks(
    -ans[["min.CBFV.window"]],
    zero = "-",
    sortstr = TRUE
  )
  ans[["min.CBFV.type"]] <- "minimum value in window"
  if(!is.null(min.info) && min.info[1, 2] == min.sample.in.window)
    ans[["min.CBFV.type"]] <- "local minimum"

  # Normalises the signal
  ans[["normalised.CBFV.signal"]] <- normalise.signal(
    signal = CBFV.signal,
    signal.baseline.value = ans[["CBFV.baseline.value"]],
    signal.min.value = ans[["min.CBFV.value"]]
  )

  invisible(ans)
}
