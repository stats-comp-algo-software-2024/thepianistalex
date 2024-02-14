#' Title coef method for hglm object
#'
#' @param hglm_out the hglm object
#' @param ... further arguments passed to or from other methods
#'
#' @export
#' @return the estimated coefficients of the model
coef.hglm <- function(hglm_out, ...) {
  warning("This function is still under development")
  return(hglm_out[["coef"]])
}

#' Title vcov method for hglm object
#'
#' @param hglm_out the hglm object
#' @param ... further arguments passed to or from other methods
#'
#' @export
vcov.hglm <- function(hglm_out, ...) {
  warning("This function is not implemented yet")
}

#' Title print method for hglm object
#'
#' @param hglm_out the hglm object
#' @param ... further arguments passed to or from other methods
#'
#' @export
print.hglm <- function(hglm_out, ...) {
  print("This function is not implemented yet")
}
