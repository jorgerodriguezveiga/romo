
# VarExpression ---------------------------------------------------------------
#' Variable expression.
#'
#' @param independent independent term of the variable expression.
#' @param variables variables coefficients of the variable expression.
#'
#' @return
#' @export
#'
#' @examples
VarExpression <- function(independent=0, variables=c(NA)){
  variables[is.na(variables)] <- 0
  return(
    VarExpressionClass(independent=independent, variables=variables)
  )
}
# --------------------------------------------------------------------------- #


# VarExpressionClass ----------------------------------------------------------
#' Variable expression class.
#'
#' @slot independent numeric. 
#' @slot variables numeric. 
#'
#' @return
#' @export
#'
#' @examples
VarExpressionClass <- setClass(
  # Class name
  "VarExpressionClass",
  
  # Define the slots
  representation = list(
    independent = "numeric",
    variables = "numeric"
  )
)
# --------------------------------------------------------------------------- #

