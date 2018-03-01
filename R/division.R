# / ---------------------------------------------------------------------------
# VarElement numeric
setMethod(
  "/", 
  signature(e1 = "VarElementClass", e2 = "numeric"), 
  function(e1, e2){
    (1/e2) * e1
  }
)

# VarExpression numeric
setMethod(
  "/", 
  signature(e1 = "VarExpressionClass", e2 = "numeric"), 
  function(e1, e2){
    (1/e2) * e1
  }
)
