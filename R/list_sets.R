
# ListSets --------------------------------------------------------------------
#' Function to obtain a set of sets.
#'
#' @param ... undefined number of set object from the class SetClass.
#'
#' @return
#' @export
#'
#' @examples
ListSets <- function(...){
  sets = c(...)
  if(all(unlist(lapply(sets, function(x) class(x)[[1]]))=='SetClass')){
    return(sets)
  }else{
    stop("Class of list elements must be 'SetClass'")
  }
}
# --------------------------------------------------------------------------- #


# indices ---------------------------------------------------------------------
#' Function to obtain the combination of the elements of a set of sets.
#'
#' @param object 
#'
#' @return
#' @export
#'
#' @examples
setGeneric(
  name="indices",
  def=function(object){standardGeneric("indices")}
)


#' Function to obtain the combination of the elements of a set of sets.
#'
#' @param list 
#'
#' @return
#' @export
#'
#' @examples
setMethod(
  f="indices", 
  signature="list", 
  definition=function(object){
    sets = ListSets(object)
    ind <- list()
    for(s in sets){
      ind[[s@name]] = s@elements
    }
    return(expand.grid(ind))
  }
)
# --------------------------------------------------------------------------- #
