# indices ---------------------------------------------------------------------
setGeneric(
  name="indices",
  def=function(object){standardGeneric("indices")}
)


setMethod(
  f="indices", 
  signature="list", 
  definition=function(object){
    ind <- list()
    for(s in object){
      ind[[s@name]] = seq(length(s@elements))
    }
    return(expand.grid(ind))
  }
)
# --------------------------------------------------------------------------- #


# show ------------------------------------------------------------------------
setMethod(
  "show", 
  "ParamORVar",
  function(object){
    cat(object@name)
    return(object@value)
  }
)
# --------------------------------------------------------------------------- #
