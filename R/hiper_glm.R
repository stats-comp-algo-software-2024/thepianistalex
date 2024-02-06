#' Title the core fitting function for hiperglm package
#'
#' @param design design matrix
#' @param outcome outcome vector
#' @param model model to fit
#'
#' @export
#' @return an object of hiperres
hiper_glm <- function(design, outcome, model){
  # To do list: implement model fitting
  supported_models <- c("linear", "logit")
  if(!(model %in% supported_models)){
    stop(sprintf("Model %s is not supported", model))
  }
  warning("This function is not implemented yet")
  hglm_out <- list()
  class(hglm_out) <- "hglm"
  return(hglm_out)
}
