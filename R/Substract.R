# - ---------------------------------------------------------------------------
# VarExpression VarExpression
#' Substract VarExpression object with VarExpression object.
#'
#' @param e1 missing. 
#' @param e2 VarExpressionClass. 
#'
#' @include var_expression.R
#' @return
#' @export
#'
#' @examples
setMethod(
  "-", 
  signature(e1 = "VarExpressionClass", e2 = "missing"), 
  function(e1, e2){
    VarExpression(independent=-e1@independent, variables=-e1@variables)
  }
)

setMethod(
  "-", 
  signature(e1 = "VarElementClass", e2 = "missing"), 
  function(e1, e2){
    0 - e1
  }
)

setMethod(
  "-", 
  signature(e1 = "AuxVarElementClass", e2 = "missing"), 
  function(e1, e2){
    0 - e1
  }
)

# VarExpression VarExpression
#' Substract VarExpression object with VarExpression object.
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
  "-", 
  signature(e1 = "VarExpressionClass", e2 = "VarExpressionClass"), 
  function(e1, e2){
    len1 = length(e1@variables)
    len2 = length(e2@variables)
    
    max_len = max(len1, len2)
    
    v1 = c(e1@variables, numeric(max_len-len1))
    v2 = c(e2@variables, numeric(max_len-len2))
    VarExpression(independent=e1@independent-e2@independent, variables=v1-v2)
  }
)

# numeric VarElement
#' Substract numeric object with VarElement object.
#'
#' @param e1 numeric. 
#' @param e2 VarElementClass. 
#'
#' @include var_element.R
#' @return
#' @export
#'
#' @examples
setMethod(
  "-", 
  signature(e1 = "numeric", e2 = "VarElementClass"), 
  function(e1, e2){
    variables=numeric(e2@position)
    variables[e2@position] = 1
    VarExpression(independent=e1, variables=-variables)
  }
)

# ANY VarElement
#' Substract ANY object with VarElement object.
#'
#' @param e1 ANY. 
#' @param e2 VarElementClass. 
#'
#' @include var_element.R
#' @return
#' @export
#'
#' @examples
setMethod(
  "-", 
  signature(e1 = "ANY", e2 = "VarElementClass"), 
  function(e1, e2){
    e1 + (0 - e2)
  }
)

# VarElement ANY
#' Substract VarElement object with ANY object.
#'
#' @param e1 VarElementClass. 
#' @param e2 ANY.
#'
#' @include var_element.R
#' @return
#' @export
#'
#' @examples
setMethod(
  "-", 
  signature(e1 = "VarElementClass", e2 = "ANY"), 
  function(e1, e2){
    e1 + (0 - e2)
  }
)

# numeric VarExpression
#' Substract VarElement object with ANY object.
#'
#' @param e1 numeric. 
#' @param e2 VarExpressionClass. 
#'
#' @include var_expression.R
#' @return
#' @export
#'
#' @examples
setMethod(
  "-", 
  signature(e1 = "numeric", e2 = "VarExpressionClass"), 
  function(e1, e2){
    VarExpression(independent=e1-e2@independent, variables=-e2@variables)
  }
)

# ANY VarExpression
#' Substract ANY object with VarExpression object.
#'
#' @param e1 ANY. 
#' @param e2 VarExpressionClass. 
#'
#' @include var_expression.R
#' @return
#' @export
#'
#' @examples
setMethod(
  "-", 
  signature(e1 = "ANY", e2 = "VarExpressionClass"), 
  function(e1, e2){
    e1 + (0 - e2)
  }
)

# VarExpression ANY
#' Substract VarExpression object with ANY object.
#' 
#' @param e1 VarExpressionClass. 
#' @param e2 ANY. 
#'
#' @include var_expression.R
#' @return
#' @export
#'
#' @examples
setMethod(
  "-", 
  signature(e1 = "VarExpressionClass", e2 = "ANY"), 
  function(e1, e2){
    e1 + (0 - e2)
  }
)
