

# Var -------------------------------------------------------------------------
#' Function to create variables.
#'
#' @param name variable name.
#' @param sets set to build the collection of variables.
#' @param start_position starting point to enumerate the set of variables.
#' @param value variable value.
#' @param lb variable lower bound.
#' @param ub variable upper bound.
#' @param state variable state.
#' @param type variable type.
#' @param description variable description.
#' 
#' @include VarElement.R
#' @return
#' @export
#'
#' @examples
Var <- function(name, sets=list(), start_position=1, value=NaN, lb=-Inf, ub=Inf, 
                state="unfix", type="continuous", description=""){
  
  sets <- ListSets(sets)
  
  if(length(sets)==0){
    return(VarElement(name=name, position=start_position, value=value, lb=lb, 
                      ub=ub, state=state, type=type, description=description))
  }else{
    ind = indices(sets)
    
    variable <- list()
    position = array(dim=dimension(sets), dimnames=dimensionnames(sets))
    for(i in rownames(ind)){
      # Positions
      pos <- (start_position-1) + as.double(i)
      sets_elem <- as.matrix(ind[i,])
      position[sets_elem] = pos
      
      # Variables
      # ---------
      # Name
      ele_name <- paste(name, "[", paste(sets_elem, collapse=", "), "]", 
                        sep = "")
      
      variable[[pos]] <- VarElement(name=ele_name, position=pos, value=value, 
                                    lb=lb, ub=ub, state=state, type=type, 
                                    description=description)
    }
    
    return(VarClass(name=name, sets=sets, position=position, variable=variable, 
                description=description))
  }
}

# --------------------------------------------------------------------------- #

  
# VarClass --------------------------------------------------------------------
#' Variable class.
#'
#' @slot name character. 
#' @slot sets list. 
#' @slot position arrayORnumeric. 
#' @slot variable list. 
#' @slot description character. 
#'
#' @include NewClasses.R
#' @return
#' @export
#'
#' @examples
VarClass <- setClass(
  # Class name
  "VarClass",
  
  # Define the slots
  representation = list(
    name = "character",
    sets = "list",
    position = "arrayORnumeric",
    variable = "list",
    description = "character"
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object){
    if(length(object@name)==0) return("Argument 'name' is required.")
    return(TRUE)
  }
)
# --------------------------------------------------------------------------- #


# [] --------------------------------------------------------------------------
setMethod(
  "[", 
  c("VarClass", "ANY", "ANY"),
  function(x, i, j, ..., drop=TRUE){
    index <- matrix(as.character(c(i, j, ...)), nrow=1)
    pos <- x@position[index]
    return(x@variable[[pos]])
  }
)

setMethod(
  "[", 
  c("VarClass", "ANY", "missing"),
  function(x, i, j, ..., drop=TRUE){
    index <- matrix(as.character(c(i, ...)), nrow=1)
    pos <- x@position[index]
    return(x@variable[[pos]])
  }
)
# --------------------------------------------------------------------------- #


# show ------------------------------------------------------------------------
setMethod(
  "show", 
  "VarClass",
  function(object){
    print(object@variable)
  }
)
# --------------------------------------------------------------------------- #
