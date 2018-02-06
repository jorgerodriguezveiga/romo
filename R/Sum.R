
# Sum -------------------------------------------------------------------------
#' Sum over a iterator.
#'
#' @param ... list of iterators.
#'
#' @return
#' @export
#'
#' @examples
Sum <- function(...){
  return(SumClass(iterator=list(...)))
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


# [] --------------------------------------------------------------------------
setMethod(
  "[", 
  c("SumClass", "VarExpressionClass"),
  function(x, i){
    
    sets <- c()
    for(s in x){
      sets <- c(sets, s@set)
    }
    
    ind = indices(ListSets(sets))
    
    constr <- list()
    position = array(dim=dimension(sets), dimnames=dimensionnames(sets))
    for(j in rownames(ind)){
      
      count <- 0
      iterators <- list()
      for(s in iterator){
        count <- count + 1
        iterators[[s@j]] <- as.vector(ind[j,count])
      }
    
      print(eval(i, iterators))
    }
  }
)
# --------------------------------------------------------------------------- #

