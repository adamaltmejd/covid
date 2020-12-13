
#' Building the covariate matrix for the prior of the time point
#'
#'
#'
#' @param  new_cases - (T x T) new cases each time point
#'
#' @return  X_ones  - (T x T) one matrix
#' @return  X_diag  - (T x T) diagonal entry
#' @return  X_na    - (T x T) was previous entry NA
#' @return  X_t     - (T x T)  linear increase
#' @return  X_first - (T x T) first non na value that is not diagonal
build_X_time <- function(cases_remanin){

  n <- dim(cases_remanin)[1]
  m <- dim(cases_remanin)[2]
  X_ones <- matrix(1, nrow = n, ncol = m)
  X_diag <- matrix(0, nrow = n, ncol = m)
  diag(X_diag) <- 1
  X_na <- matrix(0, nrow = n, ncol = m)
  X_t <-  matrix(0, nrow = n, ncol = m)
  X_first <- matrix(0, nrow = n, ncol = m)
  for(i in 1:(n-1)){
    X_t[i,i:n] <- 0:(n-i)
    for(j in (i+1):(m)){
      X_na[i,j] <- 1*is.na(cases_remanin[i, j-1])
    }
    non_na <-which(is.na(cases_remanin[i,])==F & X_diag[i,]==0)
    if(length(non_na)>0)
      X_first[i, min(non_na)] <- 1
  }

  Days <- weekdays(as.Date(colnames(cases_remanin)))
  Tuesday <- matrix(0, nrow = n, ncol = m)
  Tuesday[Days%in%c("Tuesday")] <- 1
  Wednesday <- matrix(0, nrow = n, ncol = m)
  Wednesday[Days%in%c("Wednesday")] <- 1
  Thursday <- matrix(0, nrow = n, ncol = m)
  Thursday[Days%in%c("Thursday")] <- 1
  Friday <- matrix(0, nrow = n, ncol = m)
  Friday[Days%in%c("Friday")] <- 1
  return(list(X_ones = X_ones,
              X_diag = X_diag,
              X_na   = X_na,
              X_t    = X_t,
              X_first = X_first,
              X_Tuesday = Tuesday,
              X_Wednesday = Wednesday,
              X_Thursday = Thursday))
}
