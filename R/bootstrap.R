
bootstrap <- function(model, data, missing_data, fit_indices, n_cores,
                      n_boot, boot_alpha, boot_internal, ...) {
  message(paste("Bootstrapping Confidence Interval for Model Fit Indices with",
              n_boot, "Replications and Type I-Error Rate of alpha = ", boot_alpha, "...\n"))                                                

  par_type <- ifelse(n_cores > 1, "snow", "no")
  if (n_cores == 1) message("Only one CPU core used. Check whether this is valid.") 

  if (boot_internal) {
    fitb <- lavaan::sem(model, data, ...)                     
    bootstrapped_fitind <- list()
    bootstrapped_fitind$t <- lavaan::bootstrapLavaan(fitb,
                                                   R = n_boot,
                                                   FUN = lavaan::fitMeasures, parallel = par_type, ncpus = n_cores,
                                                   fit.measures = fit_indices
    )
  } else {
    fitmeasures_bootstrap <- function(model, data, fit_indices, 
                                    indices) { 
      d <- data[indices, ]
      fitb <- try(lavaan::fitmeasures(lavaan::sem(model, d, ...),             
                                    fit.measures = fit_indices), silent = T)
    
      if (!inherits(fitb, "try-error")) {
        return(fitb)
      } else {
        return(rep(NA, length(fit_indices)))
      }
    }
    bootstrapped_fitind <- boot::boot(
      data = data, model = model, fit_indices = fit_indices, statistic = fitmeasures_bootstrap,
      R = n_boot, ncpus = n_cores, parallel = par_type
    )
  }

  boot_ci <- t(apply(bootstrapped_fitind$t, 2, FUN = function(x) stats::quantile(x = x, probs = c(boot_alpha / 2, 1 - boot_alpha / 2))))
  colnames(boot_ci) <- paste0(c("bootstrapped lb (", "bootstrapped ub ("), "alpha = ", boot_alpha, ") of empirical fit")
  return(boot_ci)
}
