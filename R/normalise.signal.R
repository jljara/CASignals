#' Normalises a signal.
#'
#' Create a normalised version of a signal.
#'
#' @param signal The original signal to be normalised
#'
#' @param signal.baseline.value The signal's baseline value to be considered
#'     in the normalization operation
#'
#' @param signal.min.value The signal's minimum value to be considered
#'     in the normalization operation
#'
#' @return a normalised version of the given signal (zero at the
#'     selected minimum value, and one at the baseline value)
#'
#' @examples
#' normalise.signal(c(10, 8, 9, 11, 10, 11, 12, 10, 9, 8, 4, 5, 6, 7, 8), 10, 4)
#'
#' @export
#'
normalise.signal <- function(
  signal,
  signal.baseline.value,
  signal.min.value
)
{
  (signal - signal.min.value) / abs(signal.baseline.value - signal.min.value)
}
