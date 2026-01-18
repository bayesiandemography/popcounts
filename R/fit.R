#' Fit the TMB model
#'
#' @param data_list A list of data
#' @param parameters_list A list of parameters
#' @export
fit_model <- function(data_list, parameters_list) {
  # ... pre-processing ...
  obj <- TMB::MakeADFun(data = data_list, parameters = parameters_list, DLL = "pkgname")
  # ... optimization, e.g., nlminb(obj$par, obj$fn, obj$gr) ...
  return(obj)
}
