

# AuxVarElement ---------------------------------------------------------------
AuxVarElement <- function(name, expr, description=""){
  eval_expr <- eval.parent(expr, n=2)
  if(class(eval_expr)=="ExpressionClass"){
    eval_expr <- eval.parent(parse(text=eval_expr@expr), n=2)
  }
  return(AuxVarElementClass(
      name=name, expr=eval_expr, description=description))
}
# --------------------------------------------------------------------------- #


# AuxVarElementClass ----------------------------------------------------------
#' Auxiliary variable element class.
#'
#' @slot name character. 
#' @slot expr VarExpressionClass.
#' @slot description character. 
#' 
#' @include var_expression.R
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


# =============================================================================
# Operations
# =============================================================================

# + ---------------------------------------------------------------------------
#' Sum elements.
#'
#' @param e1 ANY. 
#' @param e2 AuxVarElementClass. 
#'
#' @return
#' @export
#'
#' @examples
setMethod(
  "+", 
  signature(e1 = "ANY", e2 = "AuxVarElementClass"), 
  function(e1, e2){
    e1 + e2@expr
  }
)

#' Sum elements.
#'
#' @param e1 AuxVarElementClass. 
#' @param e2 ANY. 
#'
#' @return
#' @export
#'
#' @examples
setMethod(
  "+", 
  signature(e1 = "AuxVarElementClass", e2 = "ANY"), 
  function(e1, e2){
    e1@expr + e2
  }
)

#' Sum elements.
#'
#' @param e1 AuxVarElementClass. 
#' @param e2 AuxVarElementClass. 
#'
#' @return
#' @export
#'
#' @examples
setMethod(
  "+", 
  signature(e1 = "AuxVarElementClass", e2 = "AuxVarElementClass"), 
  function(e1, e2){
    e1@expr + e2@expr
  }
)
# -----------------------------------------------------------------------------


# - ---------------------------------------------------------------------------
#' Substrac element.
#' 
#' @param e2 numeric.
#' @param e1 AuxVarElementClass. 
#'
#' @return
#' @export
#'
#' @examples
setMethod(
  "-", 
  signature(e1 = "AuxVarElementClass", e2 = "AuxVarElementClass"), 
  function(e1, e2){
    e1@expr - e2@expr
  }
)

#' Substrac element.
#'
#' @param e1 ANY.
#' @param e2 AuxVarElementClass.
#'
#' @return
#' @export
#'
#' @examples
setMethod(
  "-", 
  signature(e1 = "ANY", e2 = "AuxVarElementClass"), 
  function(e1, e2){
    e1 - e2@expr
  }
)

#' Substrac element.
#'
#' @param e1 AuxVarElementClass. 
#' @param e2 ANY.
#' 
#' @return
#' @export
#'
#' @examples
setMethod(
  "-", 
  signature(e1 = "AuxVarElementClass", e2 = "ANY"), 
  function(e1, e2){
    e1@expr - e2
  }
)
# -----------------------------------------------------------------------------


# * ---------------------------------------------------------------------------
setMethod(
  "*", 
  signature(e1 = "ANY", e2 = "AuxVarElementClass"), 
  function(e1, e2){
    e1*e2@expr
  }
)

setMethod(
  "*", 
  signature(e1 = "AuxVarElementClass", e2 = "ANY"), 
  function(e1, e2){
    e1@expr*e2
  }
)
# -----------------------------------------------------------------------------


# / ---------------------------------------------------------------------------
setMethod(
  "/", 
  signature(e1 = "AuxVarElementClass", e2 = "ANY"), 
  function(e1, e2){
    e1@expr*(1/e2)
  }
)
# -----------------------------------------------------------------------------
