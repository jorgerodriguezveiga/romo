
# Iter -------------------------------------------------------------------------
#' Function to build a list of iterators object.
#'
#' @param ... objects of Iterator class.
#'
#' @return list with iterator objects.
#' @export
#'
#' @examples
Iter <- function(...){
  iterators <- list(...)
  for(i in iterators){
    if(class(i) != "IteratorClass"){
      stop(paste("Argument is not an 'IteratorClass'."))
    }
  }
  return(iterators)
}
# --------------------------------------------------------------------------- #


# For -------------------------------------------------------------------------
#' Title
#'
#' @param ... objects of Iterator class.
#'
#' @return list with iterator objects.
#' @export
#'
#' @examples
For <- function(...){
  return(Iter(...))
}
# --------------------------------------------------------------------------- #


# %inset% ---------------------------------------------------------------------
#' Operator that return the considered name of the element and the iterator. 
#'
#' @param i element name.
#' @param s SetClass.
#' 
#' @include set.R
#' @return
#' @export
#'
#' @examples
"%inset%" <- function(i, s){
  return(IteratorClass(i=deparse(substitute(i)), set=s))
}
# --------------------------------------------------------------------------- #


# IteratorClass --------------------------------------------------------------------
#' Title
#'
#' @slot i ANY. 
#' @slot set SetClass. 
#' 
#' @include set.R
#' @return
#' @export
#'
#' @examples
IteratorClass <- setClass(
  # Class name
  "IteratorClass",
  
  # Define the slots
  representation = list(
    i = "ANY",
    set = "SetClass"
  )
)
# --------------------------------------------------------------------------- #

