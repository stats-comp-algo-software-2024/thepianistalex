#' Title the core fitting function for hiperglm package
#'
#' @param design design matrix
#' @param outcome outcome vector
#'
#' @export
#' @return an object of hiperres
hiper_glm <- function(design, outcome){
  result <- list()
  class(result) <- "hiperres"
  return(result)
}
