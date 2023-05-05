#' Creates a normalised step decrease signal to represent an arterial blood
#' pressure (ABP) stimulus.
#'
#' This function creates a normalised step decrease signal, with baseline
#' value one that drops to zero, to be used as a (rather perfect) ABP
#' stimulus, similar to the ones observed after the release of thigh cuffs.
#'
#' The stimulus can be 'smoothed' to get 'more realistic' step signals.
#'
#' @param sampling.time Sampling time for the ABP stimulus (in seconds).
#'        Default value: 0.1 s
#'
#' @param time.until.release Time included in the resulting signal until
#'        the time of cuff release (in seconds).
#'        Default value: 10 s
#'
#' @param time.after.release Time included in the resulting signal after
#'        the time of cuff release.
#'        Default value: 20 s
#'
#' @param smooth.step.stimulus Logical, whether the ABP step stimulus
#'        should be smoothed (filtered).
#'        Default value: FALSE
#'
#' @param filter.order The order of the low-pass Butterworth filter used
#'        to smooth the ABP step stimulus.
#'        Default value: 2
#'
#' @param cutoff.frequency The cutoff frequency for the low-pass Butterworth
#'        filter used to smooth the ABP step stimulus (in Hz).
#'        Default value: 0.20 Hz
#'
#' @param left.stabilisation.time The amount of time to be used for the
#'        stabilisation of the decrease in ABP. This time is included in the
#'        step decrease when the filter is applied, but removed after that.
#'        Default value: 30 s when the filter will be applied, 0 s otherwise
#'
#' @param time.rounding.digits The number of decimal places used for
#'        time instants.
#'        Default value: the number of decimals in the sampling time
#'
#' @param time.tol The minimum amount of time between two time instants.
#'        Default value: one-hundredth of the sampling time
#'
#'  @return A list with the following:
#'  \describe{
#'    \item{time.instants}{The time instants in which the signal is sampled}
#'    \item{ABP.normalised}{The normalised ABP stimulus generated}
#'    \item{sampling.time}{The sampling time used}
#'    \item{time.release}{The time instant in which the thigh cuffs are
#'          released. Currently at the second 0.0}
#'  }
#'
#' @importFrom floatUtils is.divisible
#' @importFrom pracma findpeaks
#' @importFrom signal butter filter
#' @importFrom utils tail
#'
#' @export
#'
get.normalised.ABP.stimulus <- function(
  sampling.time = 0.1,
  time.until.release = 10,
  time.after.release = 20,
  smooth.step.stimulus = FALSE,
  filter.order = 2,
  cutoff.frequency = 0.20,
  left.stabilisation.time = ifelse(smooth.step.stimulus, 30, 0),
  time.rounding.digits = format.info(sampling.time)[2],
  time.tol = sampling.time / 100
)
{
  if(!is.divisible(time.until.release, sampling.time, time.tol))
    stop("time until release must be a multiple of the target sampling time")
  if(!is.divisible(time.after.release, sampling.time, time.tol))
    stop("time after release must be a multiple of the target sampling time")

	frequency <- 1 / sampling.time
  nsamples.stabilisation.left <-
    round(left.stabilisation.time / sampling.time)
  nsamples.until.release <-
    round(time.until.release / sampling.time) + 1
  nsamples.left <- nsamples.stabilisation.left + nsamples.until.release
  nsamples.after.release <- round(time.after.release / sampling.time)
  nsamples <- nsamples.until.release + nsamples.after.release

  # ABP step stimulus
  P <- c(rep(1, nsamples.left), rep(0, nsamples.after.release))

  # Smooths ABP step stimulus if corresponds
  if(smooth.step.stimulus)
  {
     wn <- cutoff.frequency / (frequency / 2)
     b <- butter(filter.order, wn)
     P <- as.numeric(filter(b, P))
  }

  if(nsamples.stabilisation.left > 0)
    P <- P[-(1:nsamples.stabilisation.left)]

  tini <- -time.until.release
  time <- seq(tini, length.out = nsamples, by = sampling.time)

  # Creates the answer
  ans <- list()
  ans[["time.instants"]] <- round(time, time.rounding.digits)
  ans[["ABP.normalised"]] <- P
  ans[["sampling.time"]] <- sampling.time
  ans[["time.release"]] <- ans[["time.instants"]][nsamples.until.release]

  invisible(ans)
}

