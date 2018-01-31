

# Param -----------------------------------------------------------------------
Param <- setClass(
  # Class name
  "Param",
  
  # Define the slots
  representation = list(
    name = "character",
    sets = "list",
    value = "arrayORnumeric",
    description = "character"
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object){
    if(length(object@name)==0){
      return("Argument 'name' is required.")
    }else{
      return(TRUE)
    }
  }
)
# --------------------------------------------------------------------------- #


# [] --------------------------------------------------------------------------
setMethod(
  "[", 
  "Param",
  function(x, i, j, ...){
    x@value[i, j, ...]
  }
)
# --------------------------------------------------------------------------- #
