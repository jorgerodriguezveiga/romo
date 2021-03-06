
#Constraint <- function(sets, expression(expr)){
#  eval(expression(expr), list(j=1))
#}



# Constraint ------------------------------------------------------------------
#' Function to build constraints.
#'
#' @param name constraint name.
#' @param expr mathematical expression to build the constraints.
#' @param iterator iterator to build the collection of constraints.
#' @param description constraints description.
#'
#' @return Object of ConstraintClass or ConstraintElementClass class .
#' @export Constraint
#' 
#' @include constraint_element.R
#' 
#' @examples 
#' D <- Var("D")
#' Constraint("Demand", D <= 2, description = "max demand.")
Constraint <- function(name, expr, iterator=list(), description=""){
  
  expr <- as.expression(substitute(expr))
  if(length(iterator)==0){
    return(ConstraintElement(name, expr, position=1, state="active", 
                             description=description))
  }else{

    sets <- c()
    for(s in iterator){
      sets <- c(sets, s@set)
    }
    
    ind = indices(ListSets(sets))
    
    constr <- list()
    position = array(dim=dimension(sets), dimnames=dimensionnames(sets))
    for(i in rownames(ind)){
      string <- eval(as.character(expr))
      indexed_expr <- parse(text=get_indexed_expr(string, iterator, i, ind))
      
      # Positions
      pos <- as.double(i)
      sets_elem <- as.matrix(as.character(ind[i,]))
      position[sets_elem] <- pos
      
      # Constraints
      # -----------
      # Name
      ele_name <- paste(name, "[", paste(sets_elem, collapse=", "), "]", 
                        sep = "")
      
      constr[[pos]] <- ConstraintElement(name=ele_name, 
                                         expr=indexed_expr, 
                                         position=pos, state="active", 
                                         description=description)
    }
    
    return(ConstraintClass(name=name, iterator=iterator, position=position, 
                           constraint=constr, description=description))
  }
}
# --------------------------------------------------------------------------- #


# ConstraintClass -------------------------------------------------------------
#' Constraint class.
#'
#' @slot name character. 
#' @slot iterator list 
#' @slot position arrayORnumeric. 
#' @slot constraint list. 
#' @slot description character. 
#'
#' @include new_classes.R
#' @include iterator.R
#' @return Object of ConstraintClass class.
#' @export
#'
#' @examples 
ConstraintClass <- setClass(
  # Class name
  "ConstraintClass",
  
  # Define the slots
  representation = list(
    name = "character",
    iterator = "list",
    position = "arrayORnumeric",
    constraint = "list",
    description = "character"
  )
)
# --------------------------------------------------------------------------- #
