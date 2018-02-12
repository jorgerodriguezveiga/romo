model <- Model()

model$I <- Set(name = 'I', elements = 1)
model$T <- Set(name = 'T', elements = c("A","B"))
model$C = Set(name = 'C', elements = c(3,4))

sets = ListSets(model$I, model$T, model$C)

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


model$x <- Var(name='x', sets=sets)
model$z <- Var(name='z', sets=model$T)
model$y <- Var(name='y')

x <- Var(name='x', sets=sets)
model$x
x
2*model$x[1,'A',3] + model$y
model$y + 2  

model$asda <- ConstraintElement(name="asda", 2*model$x[1,'A',3]+ model$x[1,'B',4] +3 == 10)

class(model$asda)

2*model$x[1,'A',3]+ model$x[1,'B',4] +3 == 10

typeof(param[1,'B',3]) 


param1 <- 2

Constraint(
  name = "Demand", 
  expr= param1*x[i,t,3] <= 4 - z[t],
  iterator = For(t %inset% TP, i %inset% IP)
)

Constraint(
  name = "Demand", 
  iterator = For(t %inset% TP),
  expr= Sum(param[i,t,3]*x[i,t,3]-4 +y -z[t1], iterator=Iter(i %inset% IP, t1 %inset% TP)) <= 1 - (y + x[1,t,4])
)


Objective(
  name="Cost",
  sense="minimize",
  expr=Sum(iterator=Iter(i %inset% IP, t %inset% TP, j %inset% set3), 2*x[i,t,j])
)

h <- AuxVar(name='h', 
       iterator=Iter(j %inset% set3),
       expr=Sum(expr=2*x[i,t,j] - 1, iterator=Iter(i %inset% IP, t %inset% TP)) 
       )

2 + h[4]






