# Set -------------------------------------------------------------------------
Set <- function(name, elements, description=""){
  return(.Set(name=name, elements=elements, description=description))
}
# --------------------------------------------------------------------------- #


# Create Set class ------------------------------------------------------------
.Set <- setClass(
  # Class name
  ".Set",
  
  # Define the slots
  representation = list(
    name = "character",
    elements = "vector",
    description = "character"
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity = function(object){
    if(length(object@name)==0){
      return("Argument 'name' is required.")
    }
    if(length(object@elements)==0){
      return("Argument 'elements' is required.")
    }
    return(TRUE)
  }
)
# --------------------------------------------------------------------------- #


# show ------------------------------------------------------------------------
setMethod(
  "show", 
  "Set",
  function(object){
    cat(object@name, 
        ' = {', 
        paste(object@elements, collapse=", "), 
        "}", 
        sep=""
        )
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


# dimension -------------------------------------------------------------------
dimension <- function(sets){
  dim <- c()
  for(s in sets){
    dim <- c(dim, length(s@elements))
  }
  return(dim)
}
# --------------------------------------------------------------------------- #


# dimnames --------------------------------------------------------------------
dimensionnames <- function(sets){
  dimname <- list()
  for(s in sets){
    dimname[[s@name]] <- s@elements
  }
  return(dimname)
}
# --------------------------------------------------------------------------- #


# %in% ------------------------------------------------------------------------
setMethod(
  "%in%", 
  signature(i = "character", s = ".Set"), 
  function(i, s){
    (1/e2)*e1
  }
)
# --------------------------------------------------------------------------- #
i %in% I