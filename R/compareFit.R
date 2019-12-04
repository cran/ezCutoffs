
#' Compare two Fit Measure Distributions Using the Wilcoxon-test
#' @description Significane test of the difference between two randomly generated fit index distributions using the Wilcoxon rank sum test.
#' @param x An object of the class \code{ezCutoffs} to use in comparison.
#' @param y A second \code{ezCutoffs} object to compare \code{x} to.
#' @param ...	 Additional arguments to pass to \link[stats]{wilcox.test}.
#' @details Non-overlapping fit measures will be disregarded by the funciton.
#' @return An object of the class \code{wilc_result}, inspectable via \code{summary}.
#' @examples
#' ## model specification examples
#'
#' # simple uni-factorial model
#' model <- "F1 =~ a1 + a2 + a3 + a4 + a5"
#'
#' ## two function calls
#' a <- ezCutoffs(model = model, n_obs = 1000, n_rep = 10, n_cores = 1, normality = "assumed")
#' b <- ezCutoffs(model = model, n_obs = 1000, n_rep = 10, n_cores = 1, normality = "empirical")
#'
#' ## comparison of the fit measure distributions yielded by the simulations
#' w <- compareFit(a, b)
#' summary(w)
#' @seealso \code{\link{ezCutoffs}}

#' @export
compareFit <- function(x, y, ...) {
  pos_x <- which(names(x[["fitDistributions"]]) %in% names(y[["fitDistributions"]]))
  fit_names <- names(x[["fitDistributions"]])[pos_x]
  test_match <- suppressWarnings(any(names(x[["fitDistributions"]]) == names(y[["fitDistributions"]])))
  print_tm <- paste(fit_names, collapse = " ")
  if (test_match == F) {
    warning("Some of the fit measures in the outputs do not overlap. Only the following will be analyzed: ", print_tm)
  }

  wilc_result <- vector("list", length(fit_names))
  for (i in 1:length(fit_names)) {
    wilc_result[[i]] <- stats::wilcox.test(x[["fitDistributions"]][[fit_names[i]]], y[["fitDistributions"]][[fit_names[i]]], ...)
    wilc_result[[i]] <- c(wilc_result[[i]], fit_names[[i]])
  }
  class(wilc_result) <- "wilc_result"
  return(wilc_result)
}