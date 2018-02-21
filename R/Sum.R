
# Sum -------------------------------------------------------------------------
#' Sum over a iterator.
#'
#' @param ... list of iterators.
#' 
#' @include expression.R
#' @return
#' @export
#'
#' @examples
Sum <- function(iterator, expr){
  
  expr <- as.expression(substitute(expr))
  
  sum_expr <- Expression(expr = 0)
  
  sets <- c()
  for(s in iterator){
    sets <- c(sets, s@set)
  }
  
  ind = indices(ListSets(sets))
  
  if(nrow(ind)>0){
    constr <- list()
    position = array(dim=dimension(sets), dimnames=dimensionnames(sets))
    for(i in rownames(ind)){
      print(i)
      string <- eval(as.character(expr))
      indexed_expr <- get_indexed_expr(string, iterator, i, ind)
      sum_expr <- sum_expr + NoExpression(indexed_expr)
    }
    
    sum_expr@expr = paste("(", sum_expr@expr, ")")
    
    return(sum_expr)
  }else{
    return(NoExpression("0"))
  }
}
# --------------------------------------------------------------------------- #


# SumClass --------------------------------------------------------------------
#' Sum class.
#'
#' @slot i ANY. 
#' @slot set SetClass. 
#' 
#' @include set.R
#' @return
#' @export
#'
#' @examples
SumClass <- setClass(
  # Class name
  "SumClass",
  
  # Define the slots
  representation = list(
    iterator = "list"
  )
)
# --------------------------------------------------------------------------- #

