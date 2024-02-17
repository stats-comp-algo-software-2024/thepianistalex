#' Title the core fitting function for hiperglm package
#'
#' @param design design matrix
#' @param outcome outcome vector
#' @param model model to fit
#' @param option list of options
#'
#' @export
#' @return an object of hglm
hiper_glm <- function(design, outcome, model, option = NULL) {

  supported_model <- c("linear", "logit")
  if (!(model %in% supported_model)) {
    stop(sprintf("The model %s is not supported.", model))
  }

  if(is.null(option)) {
    if (model == "linear") {
      option <- list(mle_solver = "linear")
    } else if (model == "logit") {
      option <- list(mle_solver = "newton")
    }
  }

  hglm_out <- list()

  if (option[["mle_solver"]] == "linear") {
    hglm_out[["coef"]] <- find_mle_linear(design, outcome)
  } else if(option[["mle_solver"]] == "newton") {
    hglm_out[["coef"]] <- find_mle_newton(design, outcome)
  } else if (option[["mle_solver"]] == "BFGS") {
    hglm_out[["coef"]] <- find_mle_bfgs(design, outcome, model)
  }

  class(hglm_out) <- "hglm"

  return(hglm_out)
}
