
# ConstraintExpression --------------------------------------------------------
ConstraintExpression <- function(lhs, sense, rhs){
  return(.ConstraintExpression(lhs = lhs, sense = sense, rhs = rhs))
}
# --------------------------------------------------------------------------- #


# .ConstraintExpression -------------------------------------------------------
.ConstraintExpression <- setClass(
  # Class name
  ".ConstraintExpression",
  
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
setMethod(
  ">=", 
  signature(e1 = ".VarExpression", e2 = ".VarExpression"), 
  function(e1, e2){
    expr <- e1 - e2
    ConstraintExpression(lhs = expr@variables, 
                         sense = ">=", 
                         rhs = -expr@independent)
  }
)

setMethod(
  ">=", 
  signature(e1 = ".VarExpression", e2 = ".VarElement"), 
  function(e1, e2){
    e1 >= 1*e2
  }
)

setMethod(
  ">=", 
  signature(e1 = ".VarElement", e2 = ".VarExpression"), 
  function(e1, e2){
    1*e1 >= e2
  }
)

setMethod(
  ">=", 
  signature(e1 = ".VarExpression", e2 = "numeric"), 
  function(e1, e2){
    e1 >= VarExpression() + e2
  }
)


setMethod(
  ">=", 
  signature(e1 = "numeric", e2 = ".VarExpression"), 
  function(e1, e2){
    VarExpression() + e1 >= e2
  }
)

setMethod(
  ">=", 
  signature(e1 = ".VarElement", e2 = ".VarElement"), 
  function(e1, e2){
    VarExpression() + e2 >= VarExpression() + e1
  }
)

setMethod(
  ">=", 
  signature(e1 = ".VarElement", e2 = "numeric"), 
  function(e1, e2){
    VarExpression() + e1 >= VarExpression() + e2
  }
)

setMethod(
  ">=", 
  signature(e1 = "numeric", e2 = ".VarElement"), 
  function(e1, e2){
    VarExpression() + e1 >= VarExpression() + e2
  }
)
# -----------------------------------------------------------------------------


# <= --------------------------------------------------------------------------
setMethod(
  "<=", 
  signature(e1 = ".VarExpression", e2 = ".VarExpression"), 
  function(e1, e2){
    expr <- e1 - e2
    ConstraintExpression(lhs = expr@variables, 
                         sense = "<=", 
                         rhs = -expr@independent)
  }
)

setMethod(
  "<=", 
  signature(e1 = ".VarExpression", e2 = ".VarElement"), 
  function(e1, e2){
    e1 <= 1*e2
  }
)

setMethod(
  "<=", 
  signature(e1 = ".VarElement", e2 = ".VarExpression"), 
  function(e1, e2){
    1*e1 <= e2
  }
)

setMethod(
  "<=", 
  signature(e1 = ".VarExpression", e2 = "numeric"), 
  function(e1, e2){
    e1 <= VarExpression() + e2
  }
)


setMethod(
  "<=", 
  signature(e1 = "numeric", e2 = ".VarExpression"), 
  function(e1, e2){
    VarExpression() + e1 <= e2
  }
)

setMethod(
  "<=", 
  signature(e1 = ".VarElement", e2 = ".VarElement"), 
  function(e1, e2){
    VarExpression() + e2 <= VarExpression() + e1
  }
)

setMethod(
  "<=", 
  signature(e1 = ".VarElement", e2 = "numeric"), 
  function(e1, e2){
    VarExpression() + e1 <= VarExpression() + e2
  }
)

setMethod(
  "<=", 
  signature(e1 = "numeric", e2 = ".VarElement"), 
  function(e1, e2){
    VarExpression() + e1 <= VarExpression() + e2
  }
)
# -----------------------------------------------------------------------------


# == --------------------------------------------------------------------------
setMethod(
  "==", 
  signature(e1 = ".VarExpression", e2 = ".VarExpression"), 
  function(e1, e2){
    expr <- e1 - e2
    ConstraintExpression(lhs = expr@variables, 
                         sense = "==", 
                         rhs = -expr@independent)
  }
)

setMethod(
  "==", 
  signature(e1 = ".VarExpression", e2 = ".VarElement"), 
  function(e1, e2){
    e1 == 1*e2
  }
)

setMethod(
  "==", 
  signature(e1 = ".VarElement", e2 = ".VarExpression"), 
  function(e1, e2){
    1*e1 == e2
  }
)

setMethod(
  "==", 
  signature(e1 = ".VarExpression", e2 = "numeric"), 
  function(e1, e2){
    e1 == VarExpression() + e2
  }
)


setMethod(
  "==", 
  signature(e1 = "numeric", e2 = ".VarExpression"), 
  function(e1, e2){
    VarExpression() + e1 == e2
  }
)

setMethod(
  "==", 
  signature(e1 = ".VarElement", e2 = ".VarElement"), 
  function(e1, e2){
    VarExpression() + e2 == VarExpression() + e1
  }
)

setMethod(
  "==", 
  signature(e1 = ".VarElement", e2 = "numeric"), 
  function(e1, e2){
    VarExpression() + e1 == VarExpression() + e2
  }
)

setMethod(
  "==", 
  signature(e1 = "numeric", e2 = ".VarElement"), 
  function(e1, e2){
    VarExpression() + e1 == VarExpression() + e2
  }
)
# -----------------------------------------------------------------------------