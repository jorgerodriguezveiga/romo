IP = Set(name = 'I', elements = 1)
TP = Set(name = 'TP', elements = c("A","B"))
set3 = Set(name = 'C', elements = c(3,4))

sets = ListSets(IP, TP, set3)

param = Param(name = "test",
              sets = sets,
              value = array(c(1,2,3,4), 
                             dim = c(1,2,2), 
                             dimnames = list(
                               A=IP@elements,
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

(-2*(x['1','A','3']+y))/(2+3)
ConstraintElement(name="asda", 2*x[1,'A',3]+x[1,'B',4] +3 == 10)


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










