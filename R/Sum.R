
# Sum -------------------------------------------------------------------------
#' Sum over a iterator.
#'
#' @param ... list of iterators.
#' 
#' @include Expression.R
#' @return
#' @export
#'
#' @examples
Sum <- function(iterator, expr){
  
  sum_expr <- Expression()
  
  sets <- c()
  for(s in iterator){
    sets <- c(sets, s@set)
  }
  
  ind = indices(ListSets(sets))
  
  constr <- list()
  position = array(dim=dimension(sets), dimnames=dimensionnames(sets))
  for(i in rownames(ind)){
    
    count <- 0
    iterators <- list()
    for(s in iterator){
      count <- count + 1
      iterators[[s@i]] <- as.vector(ind[i,count])
    }
    
    
    new_expression <- eval(as.character(expr))
    do.call(substitute, list(new_expression, iterators))
    eval(new_expression)
    Expression(expr, iterators)
    sum_expr <- sum_expr + NoExpression(new_expression)
  }
  
  return(sum_expr)
}
# --------------------------------------------------------------------------- #


# SumClass --------------------------------------------------------------------
#' Sum class.
#'
#' @slot i ANY. 
#' @slot set SetClass. 
#' 
#' @include Set.R
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

