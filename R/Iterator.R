# Iterator --------------------------------------------------------------------
#' Iterator method.
#'
#' @param object object of SetClass class.
#'
#' @return
#' @export
#'
#' @examples
setGeneric(
  name="Iterator",
  def=function(object){standardGeneric("Iterator")}
)


#' Title
#'
#' @param list 
#'
#' @return
#' @export
#' 
#' @include Set.R
#'
#' @examples
setMethod(
  f="Iterator", 
  signature="SetClass", 
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


.RIterator <- setRefClass("RIterator",
                          contains="abstractiter",
                          methods=list(nextElem=function() stop("not implemented")))

LimitIterator <- setRefClass("SleepIterator",
                             fields=list(times="integer", .curr="integer"),
                             contains="RIterator",
                             methods=list(
                               initialize=function(...) initFields(..., .curr=1L),
                               nextElem=function() {
                                 if (!hasNext())
                                   stop("StopIteration")
                                 .curr <<- .curr + 1L
                                 invisible(NULL)
                               }, hasNext=function() .curr <= times))

unlist(LimitIterator(times=2L))


require(itertools)

it <- ihasNext(1:3)
hasNext(it)
## [1] TRUE

nextElem(it); nextElem(it); nextElem(it)
## [1] 1
## [1] 2
## [1] 3

hasNext(it)

typeof(it)

j=1
j %in% it




