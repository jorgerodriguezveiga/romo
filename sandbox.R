I = Set(name = 'I', elements = 1)
TP = Set(name = 'TP', elements = c("A","B"))
set3 = Set(name = 'C', elements = c(3,4))

sets = ListSets(I, TP, set3)

param = Param(name = "test",
              sets = sets,
              value = array(c(1,2,3,4), 
                             dim = c(1,2,2), 
                             dimnames = list(
                               A=I@elements,
                               B=TP@elements,
                               C=set3@elements
                               )
                             )
              )

set1 = TP
param1 = Param(name = "test",
              sets = set1,
              value = array(c(1,2), 
                            dim = c(2), 
                            dimnames = list(
                              B=TP@elements
                            )
              )
)


x <- Var(name='x', sets=sets)
z <- Var(name='z', sets=ListSets(TP), start_position = 5)
y <- Var(name='y', start_position=7)

(2+3*y)+2 - (-2*x[1,'A',3]+2)/2
y+y+2

y+y
y+y+x[1,'A',3]+y

(-2*(x[1,'A',3]+y))/(2+3)
ConstraintElement(name="asda", 2*x[1,'A',3]+x[1,'B',4] +3 == 10)


param[1,'B',3]




myfunc <- function(v1) {
  deparse(substitute(v1))
}


For <- function(expr){
  iter
  elements <- list()
  for(i in set){
    elements[deparse(substitute(iter))] <- i
  }
}


setGeneric(
  name="in",
  def=function(object){standardGeneric("indices")}
)

setMethod(
  "in", 
  signature(e1 = "ANY", e2 = "SetClass"), 
  function(e1, e2){
    return()
  }
)


Constraint(
  name = "Demand", 
  iterator = list(t %inset% TP, i %inset% I),
  expr=expression(x[i,t,3] <= 4 - z[t])
)







hasNext((t %inset% TP)$set)






unlist(LimitIterator(times=2L))




it <- ihasNext(TP@elements)

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

