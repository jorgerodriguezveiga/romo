# Trucos:
# http://adv-r.had.co.nz/Functions.html

x <- 10
f1 <- function(x) {
  function() {
    x + 10
  }
}

f1(1)()


`+`(1, `*`(2, 3))

j <- function(x) {
  y <- 2
  function() {
    c(x, y)
  }
}
k <- j(1)
k()


`(` <- function(e1) {
  if (is.numeric(e1) && runif(1) < 0.1) {
    e1 + 1
  } else {
    e1
  }
}
replicate(50, (1 + 2))
rm("(")


`for`


f <- function(x = ls()) {
  a <- 2
  return(x)
}

# ls() evaluated inside f:
f()
a

`modify<-` <- function(x, position, value) {
  x[position] <- value
}
modify(x, 2) <- 30
x
y
rm(`modify<-`)
