
# AuxVar ----------------------------------------------------------------------
#' Function to create auxiliar variables.
#'
#' @param name variable name.
#' @param expr variable expression.
#' @param iterator iterator to build the collection of variables.
#' @param start_position starting point to enumerate the set of variables.
#' @param description variable description.
#'
#' @include AuxVarElement.R
#' @include Var.R
#' @return
#' @export
#'
#' @examples
AuxVar <- function(name, expr, iterator=list(), description=""){
  
  # Unecessary use the position, only used to link with variable indices.
  start_position <- 1
  
  expr <- as.expression(substitute(expr))
  if(length(iterator)==0){
    return(AuxVarElement(name=name, expr=expr, description=description))
  }else{
    
    sets <- c()
    for(s in iterator){
      sets <- c(sets, s@set)
    }
    
    ind = indices(ListSets(sets))
    
    vars <- list()
    position = array(dim=dimension(sets), dimnames=dimensionnames(sets))
    for(i in rownames(ind)){
      string <- eval(as.character(expr))
      indexed_expr <- parse(text=get_indexed_expr(string, iterator, i, ind))
      
      # Positions
      pos <- (start_position-1) + as.double(i)
      sets_elem <- matrix(sapply(ind[i,], as.character), nrow=1)
      if(length(sets_elem)==1){
        sets_elem <- as.character(sets_elem)
      }
      position[sets_elem] = pos
      
      # Variables
      # ---------
      # Name
      ele_name <- paste(name, "[", paste(sets_elem, collapse=", "), "]", 
                        sep = "")
      
      vars[[pos]] <- AuxVarElement(name=ele_name, expr=indexed_expr, 
                                   description=description)
    }
    return(
      AuxVarClass(
        name=name, 
        sets=sets, 
        position=position, 
        variable=vars, 
        description=description
      )
    )
  }
}

# --------------------------------------------------------------------------- #


# AuxVarClass -----------------------------------------------------------------
#' Auxiliary Variable class.
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
AuxVarClass <- setClass(
  # Class name
  "AuxVarClass",
  
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
  c("AuxVarClass", "ANY", "ANY"),
  function(x, i, j, ..., drop=TRUE){
    index <- matrix(as.character(c(i, j, ...)), nrow=1)
    pos <- x@position[index]
    return(x@variable[[pos]])
  }
)

setMethod(
  "[", 
  c("AuxVarClass", "ANY", "missing"),
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
  "AuxVarClass",
  function(object){
    print(object@variable)
  }
)
# --------------------------------------------------------------------------- #
