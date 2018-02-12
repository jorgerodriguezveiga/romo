model <- Model()

model$I <- Set(name = 'I', elements = 1)
model$T <- Set(name = 'T', elements = c("A","B"))
model$C = Set(name = 'C', elements = c(3,4))

model$sets = ListSets(model$I, model$T, model$C)

model$test = Param(name = "test",
              sets = ListSets(model$I, model$T, model$C),
              value = array(c(1,2,3,4), 
                             dim = c(1,2,2), 
                             dimnames = list(
                               A=model$I@elements,
                               B=model$T@elements,
                               C=model$C@elements
                               )
                             )

              )


model$x <- Var(name='x', sets=model$sets)
model$z <- Var(name='z', sets=model$T)
model$y <- Var(name='y')

model$asda <- ConstraintElement(name="asda", 2*model$x[1,'A',3]+ model$x[1,'B',4] +3 == 10)

model$Demand <- Constraint(
  name = "Demand", 
  expr= 2*model$x[i,t,3] <= 4 - model$z[t],
  iterator = For(t %inset% model$T, i %inset% model$I)
)


model$Cost <- Objective(
  name="Cost",
  sense="minimize",
  expr=Sum(iterator=Iter(i %inset% model$I, t %inset% model$T, j %inset% model$C), 2*model$x[i,t,j])
)

model$h <- AuxVar(name='h', 
  iterator=Iter(j %inset% model$C),
  expr=Sum(expr=2*model$x[i,t,j] - 1, iterator=Iter(i %inset% model$I, t %inset% model$T)) 
)


model

for(){
  
}











