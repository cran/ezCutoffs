#' Fit Measure Cutoffs in SEM
#' @import ggplot2 lavaan moments progress utils
#' @importFrom doSNOW registerDoSNOW
#' @importFrom foreach %dopar%
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom stats sd median quantile wilcox.test
#' @description Calculate cutoff values for model fit measures used in structural equation modeling (SEM) by simulating and testing data sets (cf. Hu & Bentler, 1999 <doi:10.1080/10705519909540118>) with the same parameters (population model, number of observations, etc.) as the model under consideration.
#' @param model \link[lavaan]{lavaan}-style Syntax of a user-specified model.
#' @param data A data frame containing the variables specified in model.
#' @param n_obs Specifies the number of observations. Only needed if no data frame is given. Can be given as a numeric vector representing the exact group sizes in multigroup analyses. In this case, the grouping variable needs to be called \code{"group"}.
#' @param n_rep Number of replications.
#' @param fit_indices Character vector, containing a selection of fit indices for which to calculate cutoff values. Only measures produced by \link[lavaan]{fitMeasures} can be chosen.
#' @param alpha_level Type I-error rate for the generated cutoff values: Between 0 and 1; 0.05 per default.
#' @param normality Specify distributional assumptions for the simulated data: Either \code{"assumed"} for normal distribution, or \code{"empirical"} for distributions based on the skewness and kurtosis values of the empirical data.
#' @param missing_data Specify handling of missing values: Either \code{FALSE} to generate complete data sets, or \code{TRUE} to generate data with the same number of missing values on the observed variables as in the empirical data.
#' @param bootstrapped_ci Specify whether a boostrapped confidence interval for the empirical model fit statistics should be drawn; default = FALSE.
#' @param n_boot Number of replications in bootstrap for confidence intervalls for empirical model fit statistics.
#' @param boot_alpha Type I-error rate choosen for the boostrap-confidence interval: Between 0 and 1; 0.05 per default.
#' @param boot_internal Whether to use the internal boostrap implemented in \code{bootstrapLavaan} or a standard implementation in the \link{boot} package. Defaults to \code{FALSE}
#' @param n_cores The number of cores to use. If \code{NULL} (the default) all available cores will be used.
#' @param ...	Additional arguments to pass to \link[lavaan]{lavaan}.
#'
#' @details
#' \code{model} is expected in standard lavaan nomenclature. The typical pre-multiplication mechanism is supported, with the exception of vectors (see Examples). Multigroup models should instead be specified using the \code{group} argument. \cr\cr
#' If \code{data} is not specified, the program will generate data based on the given \code{model} and \code{n_obs}. A numeric vector would signify multiple groups and \code{group} needs to be set to "group" in this case. Otherwise, \code{n_obs} is disregarded. \cr\cr
#' \code{missing_data = TRUE} assumes that the data is missing completely at random. That, is missings should not be distributed unevenly in multigroup models, for instance.\cr\cr
#' \code{bootstrapped_ci = "TRUE"} Returns a nonparametric bootstrap confidence interval that quantifies the uncertainty within a data set with regard to the empirical fit indices. Larger sample sizes should, under ideal circumstances, have smaller confidence intervals. For more information see, e.g., Efron (1981; 1987). Bootstrapping uses the \code{library(boot)} and (if available) several CPUs to compute the confidence intervals via \code{snow}. \cr\cr
#' \code{...} allows the user to pass lavaan arguments to the model fitting procedure. Options include multigroup, repeated measures, growth curve, and multilevel models.
#'
#' @references Efron, D. (1981). Nonparametric estimates of standard error: The jackknife, the bootstrap and other methods, Biometrika,  68(3), 589-599. doi: 10.1093/biomet/68.3.589 \cr
#' @references Efron, B. (1987). Better bootstrap confidence intervals. Journal of the American statistical Association, 82(397), 171-185.
#' @references Hu, L. T., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance structure analysis: Conventional criteria versus new alternatives. Structural Equation Modeling: A Multidisciplinary Journal, 6(1), 1-55. doi: 10.1080/10705519909540118
#'
#' @return An object of the class ezCutoffs, inspectable via \code{print}, \code{summary}, \code{plot}, and
#' \code{\link{compareFit}}
#'
#' @examples
#' ## model specification examples
#'
#' # simple uni-factorial model
#' model1 <- "F1 =~ a1 + a2 + a3 + a4 + a5"
#'
#' # path model
#' model2 <- "m ~ 0.6*x1
#'            m ~ 0.5*x2
#' 	   m ~ 0.4*x3
#' 	   y ~ 0.7*m"
#'
#' # two-factorial model with some exemplary pre-multiplications
#' model3 <- "F1 =~ NA*a1 + a2 + a3 + 0.8*a4 + a5
#'            F2 =~ b1 + start(0.8)*b2 + b3 + equal('F2 =~ b2')*b4 + b5
#'            F1 ~~ 0*F2"
#'
#' ## function call
#' out <- ezCutoffs(model = model1, n_obs = 1000, n_rep = 10, n_cores = 1)
#' \donttest{
#' out <- ezCutoffs(
#'   model = model1, n_obs = c(300, 400), n_rep = 9999, fit_indices = c("cfi.robust"),
#'   estimator = "MLM", group = "group", group.equal = c("loadings", "intercepts"), n_cores = 1
#' )
#' }
#'
#' ## retrieve output
#' summary(out)
#' plot(out)
#' @seealso \code{\link{compareFit}}

#' @export
ezCutoffs <- function(model = NULL,
                      data = NULL,
                      n_obs = NULL,
                      n_rep = 1000,
                      fit_indices = c("chisq", "cfi", "tli", "rmsea", "srmr"),
                      alpha_level = 0.05,
                      normality = "assumed",
                      missing_data = FALSE,
                      bootstrapped_ci = FALSE,
                      n_boot = 1000,
                      boot_alpha = 0.05,
                      boot_internal = FALSE,
                      n_cores = NULL,
                      ...) {

  #------------------------------------------------------------------------------
  
  # Occurences of c( in model
  if (grepl("\\c\\(", model) == T) {
    stop('Pre-multiplication with vectors is not supported. Please use "group.equal = ..." instead.')
  }
  
  # Create data if none is given
  no_emp_data <- F
  if (is.null(data)) {
    no_emp_data <- T
    data <- simulateData(model, sample.nobs = n_obs)
    if (length(n_obs) > 1) {
      if (!('group'%in%names(list(...)))) {
        stop('Please provide a name for the grouping variable via "group = ...".')
      } else {
        names(data)[names(data) == 'group'] <- list(...)$group
      }
    }
  }
  
  #fit empirical
  arg <- names(formals(empiricalFit))
  arg <- arg[-length(arg)]
  emp <- do.call('empiricalFit', c(mget(arg), list(...)))
  for (i in seq_along(emp)) assign(names(emp)[i], emp[[i]])

  # parallel processing setup ----------------------------------------------------
  if (is.null(n_cores)) {
    n_cores <- parallel::detectCores()
  } else if (!is.numeric(n_cores)) {
    n_cores <- 1
  }

  # empirical fit - bootstrapped CI ----------------------------------------------
  if (bootstrapped_ci) {
    arg <- names(formals(bootstrap))
    arg <- arg[-length(arg)]
    boot_ci <- do.call('bootstrap', c(mget(arg), list(...)))
  }

  # sanity check: fit_indices in empirical fit?-----------------------------------
  length_di <- length(fit_indices)
  for (i in 1:length_di) {
    name_warn <- fit_indices[i]
    if (fit_indices[i] %in% names(empirical_fit) == F) {
      stop(c("The following fit measure is not available with the given parameters: ", name_warn))
    }
  }

  # generate random data----------------------------------------------------------
  arg <- names(formals(dataGeneration))
  data_s_list <- do.call('dataGeneration', c(mget(arg)))

  # add missings if requested
  if (missing_data == T) {
    data_s_list <- lapply(data_s_list, missingData, fit, data, n_rep, missing_data, dots)  
  }

  # fit simulated data------------------------------------------------------------
  arg <- names(formals(simFit))
  arg <- arg[-length(arg)]
  sim <- do.call('simFit', c(mget(arg), list(...)))
  for (i in seq_along(sim)) assign(names(sim)[i], sim[[i]])
  
  # calculate descriptives--------------------------------------------------------
  arg <- names(formals(calcDesc))
  fit_simresults <- do.call('calcDesc', c(mget(arg)))

  # simulation stats--------------------------------------------------------------
  arg <- names(formals(simulationStats))
  simulation_stats <- do.call('simulationStats', c(mget(arg)))
 
  # include bootstrapping CI in ouput --------------------------------------------
  if (bootstrapped_ci == T) {
    fit_simresults <- cbind(fit_simresults, boot_ci)
  }

  # generate output---------------------------------------------------------------
  ezCutoffs_out <- list("simulationParameters" = simulation_stats, "data" = data_s_list, "fitDistributions" = fit_distributions, "summary" = fit_simresults, "empiricalModel" = emp$fit)
  class(ezCutoffs_out) <- "ezCutoffs"
  
  return(ezCutoffs_out)
}
