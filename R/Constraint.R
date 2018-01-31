

# Constraint -----------------------------------------------------------------------
Constraint <- setClass(
  # Class name
  "Constraint",
  
  # Define the slots
  representation = list(
    name = "character",
    sets = "list",
    position = "data.frame",
    value = "arrayORnumeric",
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

setMethod(
  "initialize",
  "Var",
  function(.Object, name, sets, position, value, description){
    dimnames(.Object@value) = dimensionnames(sets)
    .Object@position = indices(sets)
    return(.Object)
  }
)
# --------------------------------------------------------------------------- #


# positions -------------------------------------------------------------------
positions <- function(sets){
  dim <- c()
  for(s in sets){
    dim <- c(dim, length(s@elements))
  }
  return(dim)
}
# --------------------------------------------------------------------------- #


# [] --------------------------------------------------------------------------
setMethod(
  "[", 
  "Param",
  function(x, i, j, ...){
    x@values[i, j, ...]
  }
)
# --------------------------------------------------------------------------- #


# show ------------------------------------------------------------------------
setMethod(
  "show", 
  "Param",
  function(object){
    print(object@values)
  }
)
# --------------------------------------------------------------------------- #


