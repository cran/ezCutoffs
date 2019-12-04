missingData <- function(simu, fit, data, n_rep, missing_data, dots) {
  
  if (sum(is.na(data))==0) {
    missing_data <- F
  } #check that there actually is missing data, if not switch to complete
  if (missing_data == T) {
    var_table <- lavaan::varTable(fit)
    
    missings <- apply(data, 2, function(x) which(is.na(x)))
    n_var <- nrow(var_table)
    
    if (is.null(dots$missing)==T) {
      dots$missing <- "listwise"
    }
    if (dots$missing == "fiml") { #full misses
      misses <- as.integer(names(which(table(unlist(missings)) == n_var)))
    } else { # listwise, any misses
      misses <- as.integer(names(table(unlist(missings))))
    }
    
    for (i in 1:n_var) {
      pos <- missings[[i]] %in% misses
      missings[[i]] <- missings[[i]][!pos]
    }                                
    
    for (i in 1:n_rep) {
      for (v in 1:nrow(var_table)) {
        simu[missings[[v]], v] <- NA # use same missing template as given data
      }
    }
  }    
  return(simu)
}
