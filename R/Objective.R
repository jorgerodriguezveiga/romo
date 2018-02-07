
# Objective ---------------------------------------------------------------
#' Objective function.
#'
#' @param name objective functin name.
#' @param sense mathematical expression.
#' @param expr optimization sense. Possibilities c("maximize", "minimize").
#' @param state objective state.  Possibilities c("active", "deactive").
#' @param description objective function description.
#'
#' @return
#' @export
#'
#' @examples
Objective <- function(name, sense, expr, state="active", description=""){
  
  eval_expr <- eval(expr)
  if(class(eval_expr)=="ExpressionClass"){
    eval_expr <- eval(parse(text=eval_expr@expr))
  }
  
  return(
    ObjectiveClass(name=name, sense=sense, expr=eval_expr, state=state, 
                   description=description)
  )
}
# --------------------------------------------------------------------------- #


# ObjectiveClass --------------------------------------------------------------
#' Objective function class.
#'
#' @slot name objective functin name.
#' @slot expr mathematical expression.
#' @slot sense optimization sense. Possibilities c("maximize", "minimize").
#' @slot state objective state.  Possibilities c("active", "deactive").
#' @slot description objective function description.
#'
#' @include VarExpression.R
#' @return
#' @export
#'
#' @examples
ObjectiveClass <- setClass(
  # Class name
  "ObjectiveClass",
  
  # Define the slots
  representation = list(
    name = "character",
    expr = "VarExpressionClass",
    sense = "character",
    state = "character",
    description = "character"
  ),
  
  validity=function(object){
    if(!(object@state %in% c("active", "deactive"))){
      return("'state' value not in c('active', 'deactive').")
    }
    return(TRUE)
  }
)
# --------------------------------------------------------------------------- #

