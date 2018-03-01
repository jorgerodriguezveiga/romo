
# ConstraintElement -----------------------------------------------------------
#' Function to build a constraint element.
#'
#' @param name constraint name.
#' @param expr constraint expression.
#' @param position constraint position.
#' @param state constraint state. Possibilities c("active", "deactive").
#' @param description constraint description.
#'
#' @return object of ConstraintElementClass class.
#' @export
#'
#' @examples 
#' D <- Var("D")
#' ConstraintElement("Demand", D <= 2, description = "Max demand.")
ConstraintElement <- function(name, expr, position=1, state="active", 
                              description = ""){
  eval_expr <- eval.parent(expr, n=2)
  if(class(eval_expr)=="ExpressionClass"){
    eval_expr <- eval.parent(parse(text=eval_expr@expr), n=2)
  }
  return(
    ConstraintElementClass(
      name=name, 
      position=position, 
      expr = eval_expr,
      state = state,
      description = description
     )
  )
}
# --------------------------------------------------------------------------- #


# ConstraintElementClass ------------------------------------------------------
#' Constraint element class.
#'
#' @slot name character. 
#' @slot position numeric. 
#' @slot expr numeric.  
#' @slot state character. 
#' @slot description character. 
#'
#' @include constraint_expression.R
#' @return object of the ConstraintElementClass class.
#' @export
ConstraintElementClass <- setClass(
  # Class name
  "ConstraintElementClass",
  
  # Define the slots
  representation = list(
    name = "character",
    position = "numeric",
    expr = "ConstraintExpressionClass",
    state = "character",
    description = "character"
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object){
    if(!(object@state %in% c("active", "deactive"))){
      return("'state' value not in c('active', 'deactive').")
    }
    return(TRUE)
  }
)
# --------------------------------------------------------------------------- #

