library(iterators)

# Param -----------------------------------------------------------------------
Var <- setClass(
  # Class name
  "Var",
  
  # Define the slots
  representation = list(
    name = "character",
    sets = "list",
    start_position = "numeric",
    position = "array",
    value = "arrayORnumeric",
    description = "character"
  ),
  
  # Default values
  prototype = list(
    start_position = 0,
    sets = list(),
    description = ""
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
  function(.Object, name, sets, start_position, position, value, description){
    
    my_dimnames <- dimensionnames(sets)
    
    # Name
    .Object@name = name
    
    # Sets
    .Object@sets = sets
    
    # Positions
    positions = array(dim=dimension(sets), dimnames=my_dimnames)
    ind = indices(sets)
    
    for(i in rownames(ind)){
      positions[as.matrix(ind[i,])] = as.double(i)+.Object@start_position
    }
    .Object@position = positions
    
    # Value dimnames
    .Object@value = array(dim=dimension(sets), 
                          dimnames = my_dimnames)
    
    # Description
    .Object@description = .Object@description
    return(.Object)
  }
)
# --------------------------------------------------------------------------- #


# [] --------------------------------------------------------------------------
setMethod(
  "[", 
  "Var",
  function(x, i, j, ...){
    x@position[i, j, ...]
  }
)
# --------------------------------------------------------------------------- #

