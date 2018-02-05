
# Sum -------------------------------------------------------------------------
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




# Sum(t %inset% TP, i %inset% I)
