# Param -----------------------------------------------------------------------
#' Function to create a parameter.
#'
#' @param name parameter name.
#' @param value parameter value.
#' @param sets set to build the collection of parameters.
#' @param description parameter description.
#'
#' @return Object of ParamClass class.
#' @export
#'
#' @examples
Param <- function(name, value, sets=list(), description=""){
  
  sets <- ListSets(sets)
  
  if(length(sets)==0){
    return(ParamElement(name=name, value=value, description=description))
  }else{
    ind = indices(sets)
    
    parameter <- list()
    position = array(dim=dimension(sets), dimnames=dimensionnames(sets))
    for(i in rownames(ind)){
      # Positions
      pos <- as.double(i)
      sets_elem <- as.matrix(ind[i,])
      position[sets_elem] = pos
      
      # Variables
      # ---------
      # Name
      ele_name <- paste(name, "[", paste(sets_elem, collapse=", "), "]", 
                        sep = "")
      
      parameter[[pos]] <- ParamElement(name=ele_name, value=value[sets_elem], 
                                       description=description)
    }
    return(ParamClass(name=name, sets=sets, position=position, 
                      parameter=parameter, description=description))
  }
}
# --------------------------------------------------------------------------- #


# ParamClass ----------------------------------------------------------------------
#' Parameter class
#'
#' @slot name character. 
#' @slot sets list. 
#' @slot value arrayORnumeric. 
#' @slot description character. 
#'
#' @include new_classes.R
#' @return
#' @export
#'
#' @examples
ParamClass <- setClass(
  # Class name
  "ParamClass",
  
  # Define the slots
  representation = list(
    name = "character",
    sets = "list",
    position = "arrayORnumeric",
    parameter = "list",
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
  c("ParamClass", "ANY", "ANY"),
  function(x, i, j, ..., drop=TRUE){
    index <- matrix(c(i, j, ...), nrow=1)
    pos <- x@position[index]
    x@parameter[[pos]]@value
  }
)

setMethod(
  "[", 
  c("ParamClass", "ANY", "missing"),
  function(x, i, j, ..., drop=TRUE){
    index <- matrix(c(i, ...), nrow=1)
    pos <- x@position[index]
    x@parameter[[pos]]@value
  }
)
# --------------------------------------------------------------------------- #
