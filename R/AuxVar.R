
# AuxVar -------------------------------------------------------------------------
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
      sets_elem <- as.matrix(ind[i,])
      if(length(sets_elem)==1){
        sets_elem <- as.vector(sets_elem)
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
      VarClass(
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

