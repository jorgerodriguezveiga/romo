# Iterator --------------------------------------------------------------------
setGeneric(
  name="Iterator",
  def=function(object){standardGeneric("Iterator")}
)


setMethod(
  f="Iterator", 
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