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




Constraint(
  name = "Demand", 
  iterator = For(t %inset% TP, i %inset% I),
  expr=expression(x[i,t,3] <= 4 - z[t])
)

Constraint(
  name = "Demand", 
  iterator = For(t %inset% TP),
  expr=expression(Sum(parse(x[i,t,3]-4), iterator=list(i %inset% I)) <= 1)
)



expr = expression(x[1,t,3])
substitute(expr, )
Sum(expression(x[1,t,3]), iterator=list(i %inset% I, t %inset% TP))

Demand <- function(s){
  for(t %inset% TP)parse(x[i,t,s]), iterator=list(i %inset% I, t %inset% TP)) <= 2
}


iterator = For(t %inset% TP, i %inset% I)
suma <- function(expr=expression(x[i,t,3]), ...){
  for(n in iterator){
    
  }
}

ssplit <- function(expr, reg){
  new_expr <- expr
  for(r in reg){
    print(new_expr)
    new_expr <- unlist(strsplit(expr, paste("(?<=", r, ")", collapse = ""), fixed=T))
  }
  return(expr)
}


strsplit("ihilili [ix, xi, i,t] +i-i - i", "(?=>\\[|\\]|,| |\\+|\\-|\\*|\\/)", perl=T) <--------

strsplit("ihilili [xi, i,t] +i-i - i", "(?=>\\[|\\]|,| |\\+|\\-|\\*|\\/)", perl=T)

ssplit(expr, c("[", "]"))


get_expr <- function(){
  return(
    ssplit(
      ssplit(
        expr, 
        " "),
      ","),
    "]")
  )
}



gsub('\[ | i ', "1", expr)



res <-0
res <- 0 + for(i in I@elements)for(t in TP@elements){0 + x[i,t,3]}

fun <- function(...){
  for(...)
}
