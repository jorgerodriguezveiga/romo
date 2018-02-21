

# ParamElement ----------------------------------------------------------------
ParamElement <- function(name, value, description=""){
  names(value) <- name
  return(ParamElementClass(name=name, value=value, description=description))
}
# --------------------------------------------------------------------------- #


# ParamElementClass -----------------------------------------------------------
ParamElementClass <- setClass(
  # Class name
  "ParamElementClass",
  
  # Define the slots
  representation = list(
    name = "character",
    value = "numeric",
    description = "character"
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object){
    if(length(object@name)==0) return("Argument 'name' is required.")
    if (length(object@value)!=1) return("Length of 'value' must be 1.")
    return(TRUE)
  }
)
# --------------------------------------------------------------------------- #


# show ------------------------------------------------------------------------
setMethod(
  "show", 
  "ParamElementClass",
  function(object){
    print(object@value)
  }
)
# --------------------------------------------------------------------------- #

