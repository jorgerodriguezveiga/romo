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
<<<<<<< HEAD
    position = "arrayORnumeric",
=======
    position = "array",
>>>>>>> 9b2e88a10ca2f0b7afd3bec3f27de03e4e32c3db
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
  function(.Object, name, sets=list(), start_position, position, value, description){
    # Name
    .Object@name = name
    
    # Sets
    .Object@sets = sets
    
    if(length(sets)==0){
      my_dimnames <- NULL
      # Positions
      .Object@position = .Object@start_position + 1
      
      # Value dimnames
      .Object@value = 0
    }else{
      my_dimnames <- dimensionnames(sets)
      positions = array(dim=dimension(.Object@sets), dimnames=my_dimnames)
      ind = indices(.Object@sets)
      
      # Positions
      for(i in rownames(ind)){
        positions[as.matrix(ind[i,])] = .Object@start_position + as.double(i)
      }
      .Object@position = positions
      
      # Value dimnames
      .Object@value = array(0, 
                            dim=dimension(.Object@sets), 
                            dimnames = my_dimnames)
    }
    
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

