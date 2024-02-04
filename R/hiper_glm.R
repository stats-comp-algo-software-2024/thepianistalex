#' Title the core fitting function for hiperglm package
#'
#' @param design design matrix
#' @param outcome outcome vector
#'
#' @export
#' @return an object of hiperres
hiper_glm <- function(design, outcome){
  # To do list: implement model fitting
  warning("This function is not implemented yet")
  hglm_out <- list()
  class(hglm_out) <- "hglm"
  return(hglm_out)
}
