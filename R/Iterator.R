
# %inset% ---------------------------------------------------------------------
#' Operator that return the considered name of the element and the iterator. 
#'
#' @param i element name.
#' @param s SetClass.
#'
#' @return
#' @export
#'
#' @examples
"%inset%" <- function(i, s){
  return(Iterator(i=deparse(substitute(i)), set=s))
}
# --------------------------------------------------------------------------- #


# Iterator --------------------------------------------------------------------
#' Title
#'
#' @slot i ANY. 
#' @slot set SetClass. 
#' 
#' @include Set.R
#' @return
#' @export
#'
#' @examples
Iterator <- setClass(
  # Class name
  "Iterator",
  
  # Define the slots
  representation = list(
    i = "ANY",
    set = "SetClass"
  )
)
# --------------------------------------------------------------------------- #
