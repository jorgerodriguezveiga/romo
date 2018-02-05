# Set -------------------------------------------------------------------------
#' Function to build a set.
#'
#' @param name set name.
#' @param elements set elements.
#' @param description set description.
#'
#' @return
#' @export
#'
#' @examples
Set <- function(name, elements, description=""){
  return(SetClass(name=name, elements=elements, description=description))
}
# --------------------------------------------------------------------------- #


# SetClass --------------------------------------------------------------------
#' Set class.
#'
#' @slot name character. 
#' @slot elements vector. 
#' @slot description character. 
#'
#' @return
#' @export
#'
#' @examples
SetClass <- setClass(
  # Class name
  "SetClass",
  
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
  "SetClass",
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


# dimension -------------------------------------------------------------------
#' Return ListSet dimension.
#'
#' @param sets ListSet object.
#'
#' @return
#' @export
#'
#' @examples
dimension <- function(sets){
  dim <- c()
  for(s in sets){
    dim <- c(dim, length(s@elements))
  }
  return(dim)
}
# --------------------------------------------------------------------------- #


# dimnames --------------------------------------------------------------------
#' Return dimnames of the ListSet.
#'
#' @param sets 
#'
#' @return
#' @export
#'
#' @examples
dimensionnames <- function(sets){
  dimname <- list()
  for(s in sets){
    dimname[[s@name]] <- s@elements
  }
  return(dimname)
}
# --------------------------------------------------------------------------- #


