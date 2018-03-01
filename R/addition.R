# + ---------------------------------------------------------------------------
# VarExpression VarExpression
#' Sum VarExpression object with VarExpression object.
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

# numeric VarExpression
#' Sum numeric object with VarExpression object.
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
  "+", 
  signature(e1 = "numeric", e2 = "VarExpressionClass"), 
  function(e1, e2){
    VarExpression(independent=e1+e2@independent, variables=e2@variables)
  }
)

# VarExpression numeric
#' Sum VarExpression object with numeric object.
#'
#' @param e1 VarExpressionClass. 
#' @param e2 numeric. 
#'
#' @include var_expression.R
#' @return
#' @export
#'
#' @examples
setMethod(
  "+", 
  signature(e1 = "VarExpressionClass", e2 = "numeric"), 
  function(e1, e2){
    e2 + e1
  }
)


# VarElement VarExpression
#' Sum VarElement object with VarExpression object.
#'
#' @param e1 VarElementClass. 
#' @param e2 VarExpressionClass. 
#' 
#' @include var_expression.R
#' @include var_element.R
#' @return
#' @export
#'
#' @examples
setMethod(
  "+", 
  signature(e1 = "VarElementClass", e2 = "VarExpressionClass"), 
  function(e1, e2){
    (0 + e1) + (0 + e2)
  }
)

# VarExpression VarElement
#' Sum VarExpression object with VarElement object.
#'
#' @param e1 VarExpressionClass. 
#' @param e2 VarElementClass. 
#' 
#' @include var_expression.R
#' @include var_element.R
#' @return
#' @export
#'
#' @examples
setMethod(
  "+", 
  signature(e1 = "VarExpressionClass", e2 = "VarElementClass"), 
  function(e1, e2){
    (0 + e1) + (0 + e2)
  }
)

# numeric VarElement
#' Sum numeric object with VarElement object.
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
  "+", 
  signature(e1 = "numeric", e2 = "VarElementClass"), 
  function(e1, e2){
    variables=numeric(e2@position)
    variables[e2@position] = 1
    VarExpression(independent=e1, variables=variables)
  }
)

# VarElement numeric
#' Sum VarElement object with numeric object.
#'
#' @param e1 VarElementClass. 
#' @param e2 numeric. 
#'
#' @include var_element.R
#' @return
#' @export
#'
#' @examples
setMethod(
  "+", 
  signature(e1 = "VarElementClass", e2 = "numeric"), 
  function(e1, e2){
    e2 + e1
  }
)

# VarElement VarElement
#' Sum VarElement object with VarElement object.
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
  "+", 
  signature(e1 = "ANY", e2 = "VarElementClass"), 
  function(e1, e2){
    (0 + e1) + (0 + e2)
  }
)



