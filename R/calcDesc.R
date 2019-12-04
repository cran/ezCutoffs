calcDesc <- function(fit_distributions, fit_indices, alpha_level, length_di, empirical_fit, no_emp_data) {
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
    if (no_emp_data==T) {
      fit_simresults[i, 1] <- NA
    }
  }
  return(fit_simresults)
}