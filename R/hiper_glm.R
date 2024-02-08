#' Title the core fitting function for hiperglm package
#'
#' @param design design matrix
#' @param outcome outcome vector
#' @param model model to fit
#' @param option list of options
#'
#' @export
#' @return an object of hglm
hiper_glm <- function(design, outcome, model, option = list(mle_solver = 'linear')){
  warning("This function is still under development")

  supported_model <- c("linear", "logit")
  if (!(model %in% supported_model)) {
    stop(sprintf("The model %s is not supported.", model))
  }

  hglm_out <- list()

  hglm_out[["coef"]] <- find_mle(design, outcome, model, option)

  class(hglm_out) <- "hglm"

  return(hglm_out)
}
