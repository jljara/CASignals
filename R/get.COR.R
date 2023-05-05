
#' Gets the correlation coefficient (COR) between two signals
#'
#' @param sim Simulated (estimated) signal
#' @param obs Observed (actual) signal
#' @param ... Arguments pass over to cor()
#'
#' @export
#'
get.COR <-function(sim, obs, ...) UseMethod("get.COR")

#' @describeIn get.COR Default implementation
#'      (for numeric vectors and \code{ts} objects)
#'
#' @param use Pass to \code{use} parameter of the \code{cor} function
#' @param method Pass to \code{method} parameter of the \code{cor} function
#'
#' @importFrom stats cor
#'
get.COR.default <- function(sim, obs,
                            use = "everything",
                            method = c("pearson", "kendall", "spearman"),
                            ...)
{
  valid.classes <- c("integer", "numeric", "ts")
  if(!all(inherits(sim, valid.classes), inherits(obs, valid.classes)))
    stop("Invalid argument type: 'sim' & 'obs' have to be of class ",
         valid.classes)

  COR <- cor(sim, obs, use = use, method = method)

  return(COR)
}
