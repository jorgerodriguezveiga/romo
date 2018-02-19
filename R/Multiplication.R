# * ---------------------------------------------------------------------------
# numeric VarElement
setMethod(
  "*", 
  signature(e1 = "numeric", e2 = "VarElementClass"), 
  function(e1, e2){
    variables=numeric(e2@position)
    variables[e2@position] = e1
    VarExpression(independent=0, variables=variables)
  }
)

# VarElement numeric
setMethod(
  "*", 
  signature(e1 = "VarElementClass", e2 = "numeric"), 
  function(e1, e2){
    e2 * e1
  }
)

# VarElement VarExpression
setMethod(
  "*", 
  signature(e1 = "VarElement", e2 = "VarExpressionClass"), 
  function(e1, e2){
    (1*e1)*(1*e2)
  }
)

# VarExpression VarElement
setMethod(
  "*", 
  signature(e1 = "VarExpressionClass", e2 = "VarElement"), 
  function(e1, e2){
    e2 * e1
  }
)

# numeric VarExpression
setMethod(
  "*", 
  signature(e1 = "numeric", e2 = "VarExpressionClass"), 
  function(e1, e2){
    VarExpression(independent=e1*e2@independent, variables=e1*e2@variables)
  }
)

# VarExpression numeric
setMethod(
  "*", 
  signature(e1 = "VarExpressionClass", e2 = "numeric"), 
  function(e1, e2){
    e2 * e1
  }
)


