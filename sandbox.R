I = Set(name = 'I', elements = 1)
TP = Set(name = 'TP', elements = c("A","B"))
set3 = Set(name = 'C', elements = c(3,4))

param = Param(name = "test",
              sets = c(I, TP, set3),
              value = array(c(1,2,3,4), 
                             dim = c(1,2,2), 
                             dimnames = list(
                               A=I@elements,
                               B=TP@elements,
                               C=set3@elements
                               )
                             )
              )


sets = c(I, TP, set3)
ind=indices(sets)



x <- Var(name='x', sets=c(I, TP, set3))
y <- Var(name='y', sets=list())


constr <- Constraint(
  name = "character",
  sets = "list",
  position = "data.frame",
  value = "arrayORnumeric",
  description = "character"
)


param@values[unlist(indices[1,])]

param['A','B','E']