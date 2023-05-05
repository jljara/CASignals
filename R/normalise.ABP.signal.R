#' Normalises a signal of arterial blood pressure (ABP)
#'
#' @param time.instants The time instants in which the ABP signal is sampled
#'
#' @param ABP.signal The original ABP signal
#'
#' @param sample.release The index of the sample when the thigh cuffs were
#'        realeased
#'
#' @param sampling.time The sampling time used in the ABP signal
#'
#' @param baseline.initial.time The time instant that marks the beggining
#'        of the signal baseline.
#'        Default value: \code{time.instants[1]}
#'
#' @param baseline.final.time The time instant that marks the end
#'        of the signal baseline.
#'        Default value: \code{time.instants[sample.release]}
#'
#' @param min.ABP.max.delta.time The amount of time to search for the minimum
#'        ABP value after the realease of the thigh cuffs.
#'        Default value: 5 * 0.8 s (5 heart beats approx.)
#'
#' @param time.tol The minimum amount of time between two time instants.
#'        Default value: one-hundredth of the sampling time
#'
#'  @return A list with the following:
#'  \describe{
#'    \item{<*details*>}{to be able to normalised the signal}
#'    \item{normalised.ABP.signal}{The normalised ABP signal}
#'  }
#'
#' When the stimulus used to assess the dynamic cerebral autoregulation is not
#' based on thigh cuffs, \code{sample.release} should indicate the index of the
#' sample in which the ABP signal should start the expected drop.
#' It is important to identified this point in the signal to estimate the
#' signal's baseline value and locate its minimum value.
#'
#' @export
#'
normalise.ABP.signal <- function(
  time.instants,
  ABP.signal,
  sample.release,
  sampling.time,
  baseline.initial.time = time.instants[1],
  baseline.final.time = time.instants[sample.release],
  min.ABP.max.delta.time = 5 * 0.8,
  time.tol = sampling.time / 100
)
{
  ans <- list()
  valid <- !is.na(ABP.signal)
  time.release <- time.instants[sample.release]

  # Adds baseline data to answer
  if(baseline.final.time < baseline.initial.time)
    stop("final time for the ABP baseline cannot be ",
         "before its initial time")
  if(baseline.final.time > time.release)
    stop("final time for the ABP baseline cannot be ",
         "after the release of the thigh cuffs")

  i <- which(valid & time.instants >= baseline.initial.time - time.tol)
  if(length(i) == 0)
    stop("no time instant for the beginning of the ABP baseline could be determined")
  i <- i[1]
  ans[["ABP.baseline.initial.time"]] <- time.instants[i]
  ans[["ABP.baseline.initial.sample"]] <- i

  i <- which(valid & time.instants <= baseline.final.time + time.tol)
  if(length(i) == 0)
    stop("no time instant for the end of the ABP baseline could be determined")
  i <- tail(i, 1)
  ans[["ABP.baseline.final.time"]] <- time.instants[i]
  ans[["ABP.baseline.final.sample"]] <- i

  ans[["ABP.baseline.samples"]] <-
    ans[["ABP.baseline.initial.sample"]]:ans[["ABP.baseline.final.sample"]]
  ans[["ABP.baseline.duration"]] <-
    ans[["ABP.baseline.final.time"]] - ans[["ABP.baseline.initial.time"]]
  ans[["ABP.baseline.value"]] <- mean(
    ABP.signal[ans[["ABP.baseline.samples"]]],
    na.rm = TRUE
  )

  # Sets min ABP window
  ans[["min.ABP.max.delta.time"]] <- min.ABP.max.delta.time
  i <- time.instants > time.release + time.tol
  j <- time.instants < time.release + ans[["min.ABP.max.delta.time"]] +
    time.tol
  ans[["min.ABP.samples"]] <- which(valid & i & j)
  if(length(ans[["min.ABP.samples"]]) == 0)
    stop("no candidates for min ABP")

  # Gets minimum ABP
  ans[["min.ABP.window"]] <- ABP.signal[ans[["min.ABP.samples"]]]
  min.sample.in.window <- which.min(ans[["min.ABP.window"]])
  min.ABP.sample <- min.sample.in.window + ans[["min.ABP.samples"]][1] - 1
  ans[["min.ABP.time.instant"]] <- time.instants[min.ABP.sample]
  ans[["min.ABP.sample"]] <- min.ABP.sample
  ans[["min.ABP.value"]] <- ABP.signal[min.ABP.sample]

  # Gets min ABP type
  min.info <- findpeaks(
    -ans[["min.ABP.window"]],
    zero = "-",
    sortstr = TRUE
  )
  ans[["min.ABP.type"]] <- "minimum value in window"
  if(!is.null(min.info) && min.info[1, 2] == min.sample.in.window)
    ans[["min.ABP.type"]] <- "local minimum"

  # Measures min ABP drop
  ans[["min.ABP.drop"]] <-
    ans[["ABP.baseline.value"]] - ans[["min.ABP.value"]]
  ans[["min.ABP.drop.pc"]] <-
    ans[["min.ABP.drop"]] / ans[["ABP.baseline.value"]]
  ans[["min.ABP.drop.pc"]] <- round(ans[["min.ABP.drop.pc"]] * 100, 2)

  # Normalises the signal
  ans[["normalised.ABP.signal"]] <- normalise.signal(
    signal = ABP.signal,
    signal.baseline.value = ans[["ABP.baseline.value"]],
    signal.min.value = ans[["min.ABP.value"]]
  )

  invisible(ans)
}
