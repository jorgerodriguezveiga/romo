
# ListSets --------------------------------------------------------------------
ListSets <- function(sets = list()){
  if(all(unlist(lapply(sets, function(x) class(x)[[1]]))=='.Set')){
    return(sets)
  }else{
    stop("Class of list elements must be '.Set'")
  }
}
# --------------------------------------------------------------------------- #


# indices ---------------------------------------------------------------------
setGeneric(
  name="indices",
  def=function(object){standardGeneric("indices")}
)


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
