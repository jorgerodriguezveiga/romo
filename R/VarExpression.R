
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


# VarExpressionClass --------------------------------------------------------------
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


# =============================================================================
# Oparations
# =============================================================================

# + ---------------------------------------------------------------------------
#' Sum elements.
#'
#' @param e1 VarExpressionClass. 
#' @param e2 VarExpressionClass. 
#'
#' @return
#' @export
#'
#' @examples
setMethod(
  "+", 
  signature(e1 = "VarExpressionClass", e2 = "VarExpressionClass"), 
  function(e1, e2){
    independent = e1@independent + e2@independent
    
    len1 = length(e1@variables)
    len2 = length(e2@variables)
    
    max_len = max(len1, len2)
    
    v1 = c(e1@variables, numeric(max_len-len1))
    v2 = c(e2@variables, numeric(max_len-len2))
    VarExpression(independent=independent, variables=v1+v2)
  }
)

#' Sum elements.
#'
#' @param e1 VarExpressionClass. 
#' @param e2 VarElementClass. 
#' 
#' @include VarElement.R
#' @return
#' @export
#'
#' @examples
setMethod(
  "+", 
  signature(e1 = "VarExpressionClass", e2 = "VarElementClass"), 
  function(e1, e2){
    e1 + 1*e2
  }
)

#' Sum elements.
#'
#' @param e1 VarElementClass. 
#' @param e2 VarExpressionClass. 
#'
#' @include VarElement.R
#' @return
#' @export
#'
#' @examples
setMethod(
  "+", 
  signature(e1 = "VarElementClass", e2 = "VarExpressionClass"), 
  function(e1, e2){
    e2+e1
  }
)

#' Sum elements.
#'
#' @param e1 VarExpressionClass. 
#' @param e2 numeric. 
#'
#' @return
#' @export
#'
#' @examples
setMethod(
  "+", 
  signature(e1 = "VarExpressionClass", e2 = "numeric"), 
  function(e1, e2){
    VarExpression(independent=e2, variables=0) + e1 
  }
)

#' Sum elements.
#'
#' @param e1 numeric. 
#' @param e2 VarExpressionClass. 
#'
#' @return
#' @export
#'
#' @examples
setMethod(
  "+", 
  signature(e1 = "numeric", e2 = "VarExpressionClass"), 
  function(e1, e2){
    e2 + e1
  }
)
# -----------------------------------------------------------------------------


# - ---------------------------------------------------------------------------
#' Substrack elements.
#'
#' @param e1 VarExpressionClass. 
#' @param e2 VarExpressionClass. 
#'
#' @return
#' @export
#'
#' @examples
setMethod(
  "-", 
  signature(e1 = "VarExpressionClass", e2 = "VarExpressionClass"), 
  function(e1, e2){
    1*e1 + (-1)*e2
  }
)

setMethod(
  "-", 
  signature(e1 = "ANY", e2 = "VarExpressionClass"), 
  function(e1, e2){
    e1 + VarExpression(independent=-e2@independent, variables=-e2@variables)
  }
)

setMethod(
  "-", 
  signature(e1 = "VarExpressionClass", e2 = "ANY"), 
  function(e1, e2){
    e1 + (-e2)
  }
)
# -----------------------------------------------------------------------------


# * ---------------------------------------------------------------------------
setMethod(
  "*", 
  signature(e1 = "numeric", e2 = "VarExpressionClass"), 
  function(e1, e2){
    VarExpression(independent=e1*e2@independent, variables=e1*e2@variables)
  }
)

setMethod(
  "*", 
  signature(e1 = "VarExpressionClass", e2 = "numeric"), 
  function(e1, e2){
    e2*e1
  }
)
# -----------------------------------------------------------------------------


# / ---------------------------------------------------------------------------
setMethod(
  "/", 
  signature(e1 = "VarExpressionClass", e2 = "numeric"), 
  function(e1, e2){
    (1/e2)*e1
  }
)
# -----------------------------------------------------------------------------

