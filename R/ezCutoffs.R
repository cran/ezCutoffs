#' Fit Measure Cutoffs in SEM
#' @import ggplot2 lavaan moments progress utils
#' @importFrom doSNOW registerDoSNOW
#' @importFrom foreach %dopar%
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom stats sd median quantile wilcox.test
#' @description Calculate cutoff values for model fit measures used in structural equation modeling (SEM) by simulating and testing data sets (cf. Hu & Bentler, 1999 <doi:10.1080/10705519909540118>) with the same parameters (population model, number of observations, etc.) as the model under consideration.
#' @param model \link[lavaan]{lavaan}-style Syntax of a user-specified model.
#' @param data A data frame containing the variables specified in model.
#' @param n_obs Specifies the number of observations. Only needed if no data frame is given. Can be given as a numeric vector representing the exact group sizes in multigroup analyses.
#' @param n_rep Number of replications.
#' @param fit_indices Character vector, containing a selection of fit indices for which to calculate cutoff values. Only measures produced by \link[lavaan]{fitMeasures} can be chosen.
#' @param alpha_level Type I-error rate for the generated cutoff values: Between 0 and 1; 0.05 per default.
#' @param normality Specify distributional assumptions for the simulated data: Either \code{"assumed"} for normal distribution, or \code{"empirical"} for distributions based on the skewness and kurtosis values of the empirical data.
#' @param missing_data Specify handling of missing values: Either \code{"complete"} to generate complete data sets, or \code{"missing"} to generate data with the same number of missing values on the observed variables as in the empirical data. \code{"missing"} should only be run using \code{"MLR"} estimation with \code{"missing = "FIML""}. Other arguments may lead to errors.
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
#' \code{missing_data = "missing"} assumes that the data is missing completely at random. That, is missings should not be distributed unevenly in multigroup models, for instance.\cr\cr
#' \code{bootstrapped_ci = "TRUE"} Returns a nonparametric bootstrap confidence interval that quantifies the uncertainty within a data set with regard to the empirical fit indices. Larger sample sizes should, under ideal circumstances, have smaller confidence intervals. For more information see, e.g., Efron (1981; 1987). Bootstrapping uses the \code{library(boot)} and (if available) several CPUs to compute the confidence intervals via \code{snow}. \cr\cr
#' \code{...} allows the user to pass lavaan arguments to the model fitting procedure. Options include multigroup, repeated measures, growth curve, and multilevel models.
#'
#'
#' @references Efron, D. (1981). Nonparametric estimates of standard error: The jackknife, the bootstrap and other methods, Biometrika,  68(3), 589-599. doi: 10.1093/biomet/68.3.589 \cr
#' @references Efron, B. (1987). Better bootstrap confidence intervals. Journal of the American statistical Association, 82(397), 171-185.
#' @references Hu, L. T., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance structure analysis: Conventional criteria versus new alternatives. Structural Equation Modeling: A Multidisciplinary Journal, 6(1), 1-55. doi: 10.1080/10705519909540118
#'
#' @return An object of the class ezCutoffs, inspectable via \code{print}, \code{summary}, \code{plot}, and
#' \code{\link{compareFit}}
#'
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
                      missing_data = "complete",
                      bootstrapped_ci = FALSE,
                      n_boot = 1000,
                      boot_alpha = 0.05,
                      boot_internal = FALSE,
                      n_cores = NULL,
                      ...) {

  # empirical fit-----------------------------------------------------------------


  # Occurences of c( in model
  if (grepl("\\c\\(", model) == T) {
    stop("Pre-multiplication with vectors is not supported.")
  }

  # Create data if none is given
  if (is.null(data)) {
    if (length(n_obs) == 1) {
      data <- lavaan::simulateData(model, sample.nobs = n_obs)
    } else {
      simgroups <- sample(1:length(n_obs), size = sum(n_obs), replace = TRUE, prob = n_obs)
      data <- lavaan::simulateData(model, sample.nobs = n_obs)
      data[, (ncol(data) + 1)] <- simgroups
      names(data)[ncol(data)] <- "group"
    }
  }

  fit <- lavaan::sem(model = model, data = data, ...)
  fit_measures <- lavaan::fitMeasures(fit)
  empirical_fit <- fit_measures

  # parallel processing setup -----------------------------------------------------------------------
  if (is.null(n_cores)) {
    n_cores <- parallel::detectCores()
  } else if (!is.numeric(n_cores)) {
    n_cores <- 1
  }


  # empirical fit - bootstrapped CI -----------------------------------------------------------------
  if (bootstrapped_ci) {
    message(paste("Bootstrapping Confidence Interval for Model Fit Indices with", n_boot, "Replications and Type I-Error Rate of alpha = ", boot_alpha, "...\n"))

    par_type <- ifelse(n_cores > 1, "snow", "no")
    if (n_cores == 1) message("Only one CPU core detected. Check whether this is valid.")

    if (boot_internal) {
      fitb <- lavaan::sem(model, data)
      bootstrapped_fitind <- list()
      bootstrapped_fitind$t <- lavaan::bootstrapLavaan(fitb,
        R = n_boot,
        FUN = lavaan::fitMeasures, parallel = par_type, ncpus = n_cores,
        fit.measures = fit_indices, ...
      )
    } else {
      fitmeasures_bootstrap <- function(model, data, fit_indices, indices) {
        d <- data[indices, ] # allows boot to select sample
        fitb <- try(lavaan::fitmeasures(lavaan::sem(model, d), fit.measures = fit_indices), silent = T)
        if (!inherits(fitb, "try-error")) { # if does not have issues
          return(fitb) # return fit indices,
        } else { # put NA otherwise
          return(rep(NA, length(fit_indices)))
        }
      }
      bootstrapped_fitind <- boot::boot(
        data = data, model = model, fit_indices = fit_indices, statistic = fitmeasures_bootstrap,
        R = n_boot, ncpus = n_cores, parallel = "snow", ...
      )
    }

    boot_ci <- t(apply(bootstrapped_fitind$t, 2, FUN = function(x) stats::quantile(x = x, probs = c(boot_alpha / 2, 1 - boot_alpha / 2))))

    colnames(boot_ci) <- paste0(c("bootstrapped lb (", "bootstrapped ub ("), "alpha = ", boot_alpha, ") of empirical fit")
  }

  # sanity check: fit_indices in empirical fit?--------------------------------------
  length_di <- length(fit_indices)
  for (i in 1:length_di) {
    name_warn <- fit_indices[i]
    if (fit_indices[i] %in% names(empirical_fit) == F) {
      stop(c("The following fit measure is not available with the given parameters: ", name_warn))
    }
  }

  # input parameters-------------------------------------------------------------
  pop_model <- lavaan::parTable(fit)
  n <- sum(lavaan::lavInspect(fit, what = "nobs"))
  groups_var <- lavaan::lavInspect(fit, what = "group")
  n_groups <- lavaan::lavInspect(fit, what = "ngroups")
  group_labels <- lavaan::lavInspect(fit, what = "group.label")
  # stop if grouping variable wiht only one level has been selected
  if ((length(groups_var) > 0) & n_groups < 2) {
    stop("Less than 2 levels in the defined grouping variable.")
  }

  # generate random data----------------------------------------------------------

  message("Data Generation\n")


  if (normality == "empirical") { # With Skew/Kurt correction
    var_table <- lavaan::varTable(fit)
    skew <- moments::skewness(data[var_table$name])
    kurt <- moments::kurtosis(data[var_table$name])
  } else { # assume normality
    skew <- NULL
    kurt <- NULL
  }

  pb <- progress::progress_bar$new(
    format = "  |:bar| :percent elapsed = :elapsed  ~ :eta",
    total = n_rep, complete = "=", incomplete = " ", current = " ",
    width = 80, clear = F, show_after = 0
  )
  progress <- function(n) {
    pb$tick(tokens = list(trial = (1:n_rep)[n])) # token reported in progress bar
  }

  data_generation <- function() {
    progress()
    lavaan::simulateData(pop_model, sample.nobs = group_sizes, group.label = group_labels, skewness = skew, kurtosis = kurt)
  }

  group_sizes <- lavaan::lavInspect(fit, what = "nobs")

  data_s_list <- vector("list", n_rep)
  data_s_list <- replicate(n_rep, data_generation(), simplify = F)

  if (n_groups > 1) { # Multigroup
    for (i in 1:n_rep) {
      old_names <- names(data_s_list[[i]])
      new_names <- gsub("group", get("groups_var"), old_names)
      names(data_s_list[[i]]) <- new_names
    }
  }

  # add missings if requested
  if (missing_data == "missing") {
    misses <- list()
    var_table <- lavaan::varTable(fit)

    if (n_groups > 1) {
      for (i in 1:n_rep) {
        for (v in 1:nrow(var_table)) {
          n_comp <- var_table[v, "nobs"]
          n_miss <- (sum(group_sizes) - n_comp)
          d_comp <- rep(0, times = n_comp)
          d_miss <- rep(1, times = n_miss)
          d_both <- sample(c(d_comp, d_miss), size = sum(group_sizes), replace = FALSE)
          v_miss <- match(1, d_both)
          data_s_list[[i]][v_miss, v] <- NA
        }
      }
    } else {
      for (i in 1:n_rep) {
        for (v in 1:nrow(var_table)) {
          n_comp <- var_table[v, "nobs"]
          n_miss <- (n - n_comp)
          d_comp <- rep(0, times = n_comp)
          d_miss <- rep(1, times = n_miss)
          d_both <- sample(c(d_comp, d_miss), size = n, replace = FALSE)
          v_miss <- match(1, d_both)
          data_s_list[[i]][v_miss, v] <- NA
        }
      }
    }
  }

  # fit data in lavaan for fitmeasures()----------------------------------------------
  message("\nModel Fitting\n")


  fit_s_list <- vector("list", length = n_rep)
  estimation <- function(i, ...) {
    lavaan::sem(model = model, data = data_s_list[[i]], ...)
  }
  # progress bar
  pb <- progress::progress_bar$new(
    format = "  |:bar| :percent elapsed = :elapsed  ~ :eta",
    total = n_rep, complete = "=", incomplete = " ", current = " ",
    width = 80, clear = F, show_after = 0
  )
  progress <- function(n) {
    pb$tick(tokens = list(trial = (1:n_rep)[n])) # token reported in progress bar
  }

  if (n_cores > 1) {
    cl <- parallel::makeCluster(n_cores)
    doSNOW::registerDoSNOW(cl)
    # foreach loop ------------------------------------------------------------
    fit_s_list <- foreach::foreach(i = 1:n_rep, .options.snow = list(progress = progress)) %dopar% {
      estimation(i)
    }
    parallel::stopCluster(cl)
  } else {
    for (i in 1:n_rep) {
      fit_s <- estimation(i)
      fit_s_list[[i]] <- fit_s
      progress(i)
    }
  }

  # extract fit_measures-----------------------------------------------------------
  fit_measures_s_list <- list()
  for (i in 1:n_rep) {
    fit_measures_s <- try(lavaan::fitmeasures(fit_s_list[[i]], fit.measures = fit_indices), silent = T)
    if (!inherits(fit_measures_s, "try-error")) { # if does not have issues
      fit_measures_s_list[[i]] <- fit_measures_s # save fit indices,
    } else { # put NA otherwise
      fit_measures_s_list[[i]] <- rep(NA, length(fit_indices))
    }
  }

  # get fit distributions------------------------------------------------------------
  ncols <- length(fit_indices)
  m_fit_measures <- matrix(NA, n_rep, ncols)

  for (i in 1:n_rep) {
    m_fit_measures[i, ] <- matrix(unlist(fit_measures_s_list[[i]]), ncol = ncols)
  }

  fit_distributions <- as.data.frame(m_fit_measures)
  names(fit_distributions)[1:ncols] <- fit_indices

  # calculate descriptives----------------------------------------------------
  fit_simresults <- data.frame(matrix(NA, length_di, 5))
  cutoff_name <- paste("Cutoff (alpha = ", alpha_level, ")", collapse = "", sep = "")
  names(fit_simresults) <- c("Empirical fit", "Simulation Mean", "Simulation SD", "Simulation Median", cutoff_name)
  rownames(fit_simresults) <- fit_indices

  high_cut_index <- c(
    "chisq", "chisq.scaled", "fmin", "aic", "bic", "bic2", "rmsea", "rmsea.scaled", "rmsea.ci.upper.scaled", "rmsea.robust",
    "rmsea.ci.upper.robust", "rmsea.ci.upper", "rmr", "rmr_nomean", "srmr", "srmr_bentler", "srmr_bentler_nomean", "crmr",
    "crmr_nomean", "srmr_mplus", "srmr_mplus_nomean", "ecvi"
  )

  low_cut_index <- c(
    "pvalue", "pvalue.scaled", "cfi", "tli", "nnfi", "rfi", "nfi", "pnfi", "ifi", "rni", "cfi.scaled", "tli.scaled", "cfi.robust", "tli.robust",
    "nnfi.scaled", "nnfi.robust", "rfi.scaled", "nfi.scaled", "ifi.scaled", "rni.scaled", "rni.robust", "logl", "unrestricted.logl", "gfi",
    "agfi", "pgfi", "mfi", "rmsea.pvalue", "rmsea.pvalue.scaled", "rmsea.pvalue.robust", "cn_05", "cn_01"
  )


  for (i in 1:length_di) {
    if ((fit_indices[i] %in% high_cut_index) == T) {
      fit_simresults[i, 1] <- empirical_fit[fit_indices[i]]
      fit_simresults[i, 2] <- mean(fit_distributions[, i], na.rm = T)
      fit_simresults[i, 3] <- stats::sd(fit_distributions[, i], na.rm = T)
      fit_simresults[i, 4] <- stats::median(fit_distributions[, i], na.rm = T)
      fit_simresults[i, 5] <- stats::quantile(fit_distributions[, i], probs = (1 - alpha_level), na.rm = T)
    } else if ((fit_indices[i] %in% low_cut_index) == T) {
      fit_simresults[i, 1] <- empirical_fit[fit_indices[i]]
      fit_simresults[i, 2] <- mean(fit_distributions[, i], na.rm = T)
      fit_simresults[i, 3] <- stats::sd(fit_distributions[, i], na.rm = T)
      fit_simresults[i, 4] <- stats::median(fit_distributions[, i], na.rm = T)
      fit_simresults[i, 5] <- stats::quantile(fit_distributions[, i], probs = alpha_level, na.rm = T)
    } else {
      fit_simresults[i, 1] <- empirical_fit[fit_indices[i]]
      fit_simresults[i, 2] <- mean(fit_distributions[, i], na.rm = T)
      fit_simresults[i, 3] <- stats::sd(fit_distributions[, i], na.rm = T)
      fit_simresults[i, 4] <- stats::median(fit_distributions[, i], na.rm = T)
      fit_simresults[i, 5] <- NA
    }
  }

  # simulation stats----------------------------------------------------------

  n_conv <- sum(!is.na(fit_distributions[, 1]))
  s_est <- lavaan::lavInspect(fit, what = "call")$estimator
  if (length(s_est) == 0) {
    s_est <- lavaan::lavInspect(fit, what = "options")$estimator
  }
  simulation_stats <- data.frame(matrix(c(n_rep, n_conv, s_est, alpha_level, n), 1, 5))
  names(simulation_stats) <- c("#Runs", "#Converged", "Estimator", "Alpha", "TotalObservations")
  rownames(simulation_stats) <- ""


  # include bootstrapping CI in ouput -------------------------------------------

  if (bootstrapped_ci == T) {
    fit_simresults <- cbind(fit_simresults, boot_ci)
  }

  # generate output---------------------------------------------------------------
  ezCutoffs_out <- list("simulationParameters" = simulation_stats, "data" = data_s_list, "fitDistributions" = fit_distributions, "summary" = fit_simresults)
  class(ezCutoffs_out) <- "ezCutoffs"

  return(ezCutoffs_out)
}

# summary----------------------------------------------------------------------

#' @rawNamespace S3method(summary,ezCutoffs)

summary.ezCutoffs <- function(object, ...) {
  print(object[["simulationParameters"]])
  cat("\n")
  print(object[["summary"]])
}

# plot---------------------------------------------------------------

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("count"))
}

#' @export
plot.ezCutoffs <- function(x, ...) {
  for (i in 1:nrow(x[["summary"]])) {
    hist_plots <- ggplot2::ggplot(x[["fitDistributions"]], ggplot2::aes_string(x = rownames(x[["summary"]])[i])) +
      ggplot2::geom_histogram(ggplot2::aes(y = stat(count)), color = "black", fill = "white", bins = 30)

    bar_height <- max(ggplot2::ggplot_build(hist_plots)$data[[1]]$count)

    hist_plots <- hist_plots + ggplot2::geom_vline(xintercept = x[["summary"]][i, 1], color = "blue", alpha = 0.8) +
      ggplot2::annotate("text", x = x[["summary"]][i, 1], y = bar_height, angle = 0, label = "Empirical") +
      ggplot2::geom_vline(xintercept = x[["summary"]][i, 5], color = "red", alpha = 0.8) +
      ggplot2::annotate("text", x = x[["summary"]][i, 5], y = bar_height, angle = 0, label = "Cutoff") +
      ggplot2::labs(title = "Simulated Fit Distribution", y = "count") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    (print(hist_plots))
  }
}

# print--------------------------------------------------------------------------------------------
#' @export
print.ezCutoffs <- function(x, ...) {
  filter <- c(grep("Empirical", names(x$summary)), grep("Cutoff", names(x$summary)))
  print(x$summary[, filter])
}
