

# AuxVarElement ---------------------------------------------------------------
AuxVarElement <- function(name, expr, description=""){
  eval_expr <- eval(expr)
  if(class(eval_expr)=="ExpressionClass"){
    eval_expr <- eval(parse(text=eval_expr@expr))
  }
  return(AuxVarElementClass(name=name, expr=eval_expr, description=description))
}
# --------------------------------------------------------------------------- #


# AuxVarElementClass ----------------------------------------------------------
#' Auxiliary variable element class.
#'
#' @slot name character. 
#' @slot expr VarExpressionClass.
#' @slot description character. 
#' 
#' @include VarExpression.R
#' @return
#' @export
#'
#' @examples
AuxVarElementClass <- setClass(
  # Class name
  "AuxVarElementClass",
  
  # Define the slots
  representation = list(
    name = "character",
    expr = "VarExpressionClass",
    description = "character"
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object){
    if(length(object@name)==0) return("Argument 'name' is required.")
    return(TRUE)
  }
)
# --------------------------------------------------------------------------- #


# show ------------------------------------------------------------------------
setMethod(
  "show", 
  "VarElementClass",
  function(object){
    print(object@value)
  }
)
# --------------------------------------------------------------------------- #


# =============================================================================
# Operations
# =============================================================================

# + ---------------------------------------------------------------------------
setMethod(
  "+", 
  signature(e1 = "numeric", e2 = "AuxVarElementClass"), 
  function(e1, e2){
    e1 + e2@expr
  }
)

setMethod(
  "+", 
  signature(e1 = "AuxVarElementClass", e2 = "numeric"), 
  function(e1, e2){
    e2 + e1@expr
  }
)

setMethod(
  "+", 
  signature(e1 = "AuxVarElementClass", e2 = "AuxVarElementClass"), 
  function(e1, e2){
    e1@expr + e2@expr
  }
)
# -----------------------------------------------------------------------------


# - ---------------------------------------------------------------------------
setMethod(
  "-",
  signature(e1 = "AuxVarElementClass", e2 = "AuxVarElementClass"), 
  function(e1, e2){
    e1@expr - e2@expr
  }
)

setMethod(
  "-",
  signature(e1 = "AuxVarElementClass", e2 = "numeric"), 
  function(e1, e2){
    e1@expr - e2
  }
)

setMethod(
  "-",
  signature(e1 = "numeric", e2 = "AuxVarElementClass"), 
  function(e1, e2){
    e1 - e2@expr
  }
)

setMethod(
  "-", 
  signature(e1 = "AuxVarElementClass"), 
  function(e1){
    0 - e1@expr
  }
)
# -----------------------------------------------------------------------------


# * ---------------------------------------------------------------------------
setMethod(
  "*", 
  signature(e1 = "numeric", e2 = "AuxVarElementClass"), 
  function(e1, e2){
    e1*e2@expr
  }
)

setMethod(
  "*", 
  signature(e1 = "AuxVarElementClass", e2 = "numeric"), 
  function(e1, e2){
    e2*e1
  }
)
# -----------------------------------------------------------------------------


# / ---------------------------------------------------------------------------
setMethod(
  "/", 
  signature(e1 = "AuxVarElementClass", e2 = "numeric"), 
  function(e1, e2){
    (1/e2)*e1
  }
)
# -----------------------------------------------------------------------------
