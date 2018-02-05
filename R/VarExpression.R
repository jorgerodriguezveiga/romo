
# VarExpression ---------------------------------------------------------------
VarExpression <- function(independent=0, variables=c(NA)){
  variables[is.na(variables)] <- 0
  return(
    .VarExpression(independent=independent, variables=variables)
  )
}
# --------------------------------------------------------------------------- #


# .VarExpression --------------------------------------------------------------
.VarExpression <- setClass(
  # Class name
  ".VarExpression",
  
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
setMethod(
  "+", 
  signature(e1 = ".VarExpression", e2 = ".VarExpression"), 
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

setMethod(
  "+", 
  signature(e1 = ".VarExpression", e2 = ".VarElement"), 
  function(e1, e2){
    e1 + 1*e2
  }
)

setMethod(
  "+", 
  signature(e1 = ".VarElement", e2 = ".VarExpression"), 
  function(e1, e2){
    e2+e1
  }
)

setMethod(
  "+", 
  signature(e1 = ".VarExpression", e2 = "numeric"), 
  function(e1, e2){
    VarExpression(independent=e2, variables=0) + e1 
  }
)

setMethod(
  "+", 
  signature(e1 = "numeric", e2 = ".VarExpression"), 
  function(e1, e2){
    e2+e1
  }
)
# -----------------------------------------------------------------------------


# - ---------------------------------------------------------------------------
setMethod(
  "-", 
  signature(e1 = ".VarExpression", e2 = ".VarExpression"), 
  function(e1, e2){
    e1 + (-e2)
  }
)

setMethod(
  "-", 
  signature(e1 = ".VarElement", e2 = ".VarExpression"), 
  function(e1, e2){
    e1 + (-e2)
  }
)

setMethod(
  "-", 
  signature(e1 = ".VarExpression", e2 = ".VarElement"), 
  function(e1, e2){
    e1 + (-e2)
  }
)

setMethod(
  "-", 
  signature(e1 = "numeric", e2 = ".VarExpression"), 
  function(e1, e2){
    e1 + VarExpression(independent= - e2@independent, variables= - e2@variables)
  }
)

setMethod(
  "-", 
  signature(e1 = ".VarExpression", e2 = "numeric"), 
  function(e1, e2){
    e1 + (-e2)
  }
)

setMethod(
  "-", 
  signature(e1 = ".VarExpression"), 
  function(e1){
    0 - e1
  }
)
# -----------------------------------------------------------------------------


# * ---------------------------------------------------------------------------
setMethod(
  "*", 
  signature(e1 = "numeric", e2 = ".VarExpression"), 
  function(e1, e2){
    VarExpression(independent=e1*e2@independent, variables=e1*e2@variables)
  }
)

setMethod(
  "*", 
  signature(e1 = ".VarExpression", e2 = "numeric"), 
  function(e1, e2){
    e2*e1
  }
)
# -----------------------------------------------------------------------------


# / ---------------------------------------------------------------------------
setMethod(
  "/", 
  signature(e1 = ".VarExpression", e2 = "numeric"), 
  function(e1, e2){
    (1/e2)*e1
  }
)
# -----------------------------------------------------------------------------

