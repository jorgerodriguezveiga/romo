setGeneric("fun", function(x, y) standardGeneric("fun"),
           signature="x")

setMethod("fun", c(x="numeric"), function(x, y) {
  "fun,numeric-method"
})





# Create Set class -----------------------------------------------------------
setGeneric(
  # Class name
  name="ListSets", 
  def=function(x, i, j, ...){
    standardGeneric("ListSets")
  }
)

setMethod(
  "ListSets", 
  "list",
  function(x, i, j, ...){
    for(s in x){
      x[[s@name]] = s@elements 
    }
    x
  }
)
# --------------------------------------------------------------------------- #

o1 = ListSets(a=c(1,2), b=c(3,4))


# show ------------------------------------------------------------------------
setMethod(
  "show", 
  "ListSets",
  function(object){
    name <- c()
    for(s in object){
       name <- c(name, "A")#s@name)
    }
    names(object) <- name
    cat((object)
  }
)
# --------------------------------------------------------------------------- #


setMethod(
  "names", 
  "ParamORVar",
  function(x, i, j, ...){
    x@value[i, j, ...]
  }
)