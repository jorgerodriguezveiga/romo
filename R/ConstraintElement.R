
# ConstraintElement -----------------------------------------------------------
ConstraintElement <- function(name, expr, position=1, state="active", 
                              description = ""){
  return(
    .ConstraintElement(name=name, position=position, 
                       lhs = expr@lhs,
                       sense = expr@sense,
                       rhs = expr@rhs,
                       state = state,
                       description = description
                       )
  )
}
# --------------------------------------------------------------------------- #


# .ConstraintElement ----------------------------------------------------------
.ConstraintElement <- setClass(
  # Class name
  ".ConstraintElement",
  
  # Define the slots
  representation = list(
    name = "character",
    position = "numeric",
    lhs = "numeric",
    sense = "character",
    rhs = "numeric",
    state = "character",
    description = "character"
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object){
    if(length(object@name)==0){
      return("Argument 'name' is required.")
    }
    return(TRUE)
  }
)
# --------------------------------------------------------------------------- #

