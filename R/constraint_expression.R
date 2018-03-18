
# ConstraintExpression --------------------------------------------------------
#' Function to build a constraint expression.
#'
#' @param lhs left hand side.
#' @param sense sense. Possibilities c("<=", ">=", "==").
#' @param rhs right hand side.
#'
#' @return
#' @export
#'
#' @examples
ConstraintExpression <- function(lhs, sense, rhs){
  return(ConstraintExpressionClass(lhs = lhs, sense = sense, rhs = rhs))
}
# --------------------------------------------------------------------------- #


# ConstraintExpressionClass ---------------------------------------------------
#' Title
#'
#' @slot lhs numeric. 
#' @slot sense character. 
#' @slot rhs numeric. 
#'
#' @return
#' @export
#'
#' @examples
ConstraintExpressionClass <- setClass(
  # Class name
  "ConstraintExpressionClass",
  
  # Define the slots
  representation = list(
    lhs = "numeric",
    sense = "character",
    rhs = "numeric"
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object){
    return(TRUE)
  }
)
# --------------------------------------------------------------------------- #


# =============================================================================
# Operations
# =============================================================================

# >= --------------------------------------------------------------------------
#' Great or equal method to compare two variable expressions.
#'
#' @param e1 VarExpressionClass.
#' @param e2 VarExpressionClass.
#'
#' @include var_expression.R
#' @return
#' @export
#'
#' @examples
setMethod(
  ">=", 
  signature(e1 = "VarExpressionClass", e2 = "VarExpressionClass"), 
  function(e1, e2){
    expr <- e1 - e2
    ConstraintExpression(lhs = expr@variables, 
                         sense = ">=", 
                         rhs = -expr@independent)
  }
)

setMethod(
  ">=", 
  signature(e1 = "VarExpressionClass", e2 = "numeric"), 
  function(e1, e2){
    e1 >= VarExpression() + e2
  }
)

setMethod(
  ">=", 
  signature(e1 = "numeric", e2 = "VarExpressionClass"), 
  function(e1, e2){
    VarExpression() + e1 >= e2
  }
)

setMethod(
  ">=", 
  signature(e1 = "VarExpressionClass", e2 = "VarElementClass"), 
  function(e1, e2){
    e1 >= VarExpression() + e2
  }
)


setMethod(
  ">=", 
  signature(e1 = "VarElementClass", e2 = "VarExpressionClass"), 
  function(e1, e2){
    VarExpression() + e1 >= e2
  }
)

setMethod(
  ">=", 
  signature(e1 = "VarExpressionClass", e2 = "AuxVarElementClass"), 
  function(e1, e2){
    e1 >= VarExpression() + e2
  }
)


setMethod(
  ">=", 
  signature(e1 = "AuxVarElementClass", e2 = "VarExpressionClass"), 
  function(e1, e2){
    VarExpression() + e1 >= e2
  }
)

setMethod(
  ">=", 
  signature(e1 = "ANY", e2 = "VarElementClass"), 
  function(e1, e2){
    VarExpression() + e2 >= VarExpression() + e1
  }
)

setMethod(
  ">=", 
  signature(e1 = "VarElementClass", e2 = "ANY"), 
  function(e1, e2){
    VarExpression() + e1 >= VarExpression() + e2
  }
)

setMethod(
  ">=", 
  signature(e1 = "AuxVarElementClass", e2 = "ANY"), 
  function(e1, e2){
    VarExpression() + e1 >= VarExpression() + e2
  }
)

setMethod(
  ">=", 
  signature(e1 = "ANY", e2 = "AuxVarElementClass"), 
  function(e1, e2){
    VarExpression() + e1 >= VarExpression() + e2
  }
)
# -----------------------------------------------------------------------------


# <= --------------------------------------------------------------------------
setMethod(
  "<=", 
  signature(e1 = "VarExpressionClass", e2 = "VarExpressionClass"), 
  function(e1, e2){
    expr <- e1 - e2
    ConstraintExpression(lhs = expr@variables, 
                         sense = "<=", 
                         rhs = -expr@independent)
  }
)

setMethod(
  "<=", 
  signature(e1 = "VarExpressionClass", e2 = "numeric"), 
  function(e1, e2){
    e1 <= VarExpression() + e2
  }
)

setMethod(
  "<=", 
  signature(e1 = "numeric", e2 = "VarExpressionClass"), 
  function(e1, e2){
    VarExpression() + e1 <= e2
  }
)

setMethod(
  "<=", 
  signature(e1 = "VarExpressionClass", e2 = "VarElementClass"), 
  function(e1, e2){
    e1 <= VarExpression() + e2
  }
)


setMethod(
  "<=", 
  signature(e1 = "VarElementClass", e2 = "VarExpressionClass"), 
  function(e1, e2){
    VarExpression() + e1 <= e2
  }
)

setMethod(
  "<=", 
  signature(e1 = "VarElementClass", e2 = "VarElementClass"), 
  function(e1, e2){
    VarExpression() + e1 <= VarExpression() + e2
  }
)

setMethod(
  "<=", 
  signature(e1 = "VarExpressionClass", e2 = "AuxVarElementClass"), 
  function(e1, e2){
    e1 <= VarExpression() + e2
  }
)


setMethod(
  "<=", 
  signature(e1 = "AuxVarElementClass", e2 = "VarExpressionClass"), 
  function(e1, e2){
    VarExpression() + e1 <= e2
  }
)

setMethod(
  "<=", 
  signature(e1 = "AuxVarElementClass", e2 = "ExpressionClass"), 
  function(e1, e2){
    VarExpression() + e1 <= VarExpression() + e2
  }
)

setMethod(
  "<=", 
  signature(e1 = "ExpressionClass", e2 = "AuxVarElementClass"), 
  function(e1, e2){
    VarExpression() + e1 <= VarExpression() + e2
  }
)

setMethod(
  "<=", 
  signature(e1 = "ANY", e2 = "VarElementClass"), 
  function(e1, e2){
    VarExpression() + e2 <= VarExpression() + e1
  }
)

setMethod(
  "<=", 
  signature(e1 = "VarElementClass", e2 = "ANY"), 
  function(e1, e2){
    VarExpression() + e1 <= VarExpression() + e2
  }
)

setMethod(
  "<=", 
  signature(e1 = "AuxVarElementClass", e2 = "ANY"), 
  function(e1, e2){
    VarExpression() + e1 <= VarExpression() + e2
  }
)

setMethod(
  "<=", 
  signature(e1 = "ANY", e2 = "AuxVarElementClass"), 
  function(e1, e2){
    VarExpression() + e1 <= VarExpression() + e2
  }
)
# -----------------------------------------------------------------------------


# == --------------------------------------------------------------------------
setMethod(
  "==", 
  signature(e1 = "VarExpressionClass", e2 = "VarExpressionClass"), 
  function(e1, e2){
    expr <- e1 - e2
    ConstraintExpression(lhs = expr@variables, 
                         sense = "==", 
                         rhs = -expr@independent)
  }
)

setMethod(
  "==", 
  signature(e1 = "VarExpressionClass", e2 = "numeric"), 
  function(e1, e2){
    e1 == VarExpression() + e2
  }
)

setMethod(
  "==", 
  signature(e1 = "numeric", e2 = "VarExpressionClass"), 
  function(e1, e2){
    VarExpression() + e1 == e2
  }
)

setMethod(
  "==", 
  signature(e1 = "VarExpressionClass", e2 = "VarElementClass"), 
  function(e1, e2){
    e1 == VarExpression() + e2
  }
)


setMethod(
  "==", 
  signature(e1 = "VarElementClass", e2 = "VarExpressionClass"), 
  function(e1, e2){
    VarExpression() + e1 == e2
  }
)

setMethod(
  "==", 
  signature(e1 = "VarExpressionClass", e2 = "AuxVarElementClass"), 
  function(e1, e2){
    e1 == VarExpression() + e2
  }
)


setMethod(
  "==", 
  signature(e1 = "AuxVarElementClass", e2 = "VarExpressionClass"), 
  function(e1, e2){
    VarExpression() + e1 == e2
  }
)

setMethod(
  "==", 
  signature(e1 = "ANY", e2 = "VarElementClass"), 
  function(e1, e2){
    VarExpression() + e2 == VarExpression() + e1
  }
)

setMethod(
  "==", 
  signature(e1 = "VarElementClass", e2 = "ANY"), 
  function(e1, e2){
    VarExpression() + e1 == VarExpression() + e2
  }
)

setMethod(
  "==", 
  signature(e1 = "AuxVarElementClass", e2 = "ANY"), 
  function(e1, e2){
    VarExpression() + e1 == VarExpression() + e2
  }
)

setMethod(
  "==", 
  signature(e1 = "ANY", e2 = "AuxVarElementClass"), 
  function(e1, e2){
    VarExpression() + e1 == VarExpression() + e2
  }
)
# -----------------------------------------------------------------------------
