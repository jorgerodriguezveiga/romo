context("Test '+' operation.")


expect_equal(as.numeric('1') + m$CFP['air1'] - m$CRP['air1'] + m$s['air1',1] + NoExpression("0"),
             )


test_that("Addition operation ('+') is well constructed", {
  m <- romo::Model()
  m$x <- romo::Var(name='x')
  m$y <- romo::Var(name='y')
  m$z <- romo::AuxVar(name='z', expr=m$x+m$y)
  
  expect_equal(1+m$x, VarExpression(variables=1, independent=1))
  expect_equal(m$x+1, VarExpression(variables=1, independent=1))
  expect_equal(1+(m$y+1), VarExpression(variables=c(0, 1), independent=2))
  expect_equal((m$y+1)+1, VarExpression(variables=c(0, 1), independent=2))
  expect_equal(m$x+(m$y+1), VarExpression(variables=c(1, 1), independent=1))
  expect_equal((m$y+1)+m$x, VarExpression(variables=c(1, 1), independent=1))
  expect_equal((m$x+1)+(m$y+1), VarExpression(variables=c(1, 1), independent=2))
  expect_equal(1+m$z, VarExpression(variables=c(1,1), independent=1))
  expect_equal(m$z+1, VarExpression(variables=c(1,1), independent=1))
  expect_equal(1+(m$z+1), VarExpression(variables=c(1, 1), independent=2))
  expect_equal((m$z+1)+1, VarExpression(variables=c(1, 1), independent=2))
  expect_equal(m$x+(m$z+1), VarExpression(variables=c(2, 1), independent=1))
  expect_equal((m$z+1)+m$x, VarExpression(variables=c(2, 1), independent=1))
  expect_equal((m$z+1)+(m$y+1), VarExpression(variables=c(1, 2), independent=2))
})


test_that("-", {
  m <- romo::Model()
  m$x <- romo::Var(name='x')
  m$y <- romo::Var(name='y')
  m$z <- romo::AuxVar(name='z', expr=m$x+m$y)
  expect_equal(-m$x, VarExpression(variables=-1, independent=0))
  expect_equal(-(m$x-1), VarExpression(variables=-1, independent=1))
  expect_equal(-(m$z), VarExpression(variables=c(-1, -1), independent=0))
  expect_equal(1-m$x, VarExpression(variables=-1, independent=1))
  expect_equal(m$x-1, VarExpression(variables=1, independent=-1))
  expect_equal(1-(m$y+1), VarExpression(variables=c(0, -1), independent=0))
  expect_equal((m$y+1)-1, VarExpression(variables=c(0, 1), independent=0))
  expect_equal(m$x-(m$y+1), VarExpression(variables=c(1, -1), independent=-1))
  expect_equal((m$y+1)-m$x, VarExpression(variables=c(-1, 1), independent=1))
  expect_equal((m$x+1)-(m$y+1), VarExpression(variables=c(1, -1), independent=0))
  expect_equal(1-m$z, VarExpression(variables=c(-1,-1), independent=1))
  expect_equal(m$z-1, VarExpression(variables=c(1,1), independent=-1))
  expect_equal(1-(m$z+1), VarExpression(variables=c(-1, -1), independent=0))
  expect_equal((m$z+1)-1, VarExpression(variables=c(1, 1), independent=0))
  expect_equal(m$x-(m$z+1), VarExpression(variables=c(0, -1), independent=-1))
  expect_equal((m$z+1)-m$x, VarExpression(variables=c(0, 1), independent=1))
  expect_equal((m$x+1)-(m$z+1), VarExpression(variables=c(0, -1), independent=0))
})


test_that("*", {
  m <- romo::Model()
  m$x <- romo::Var(name='x')
  m$y <- romo::Var(name='y')
  m$z <- romo::AuxVar(name='z', expr=m$x+m$y)
  expect_equal(2*m$x, VarExpression(variables=2, independent=0))
  expect_equal(m$x*2, VarExpression(variables=2, independent=0))
  expect_equal(2*(m$y+1), VarExpression(variables=c(0, 2), independent=2))
  expect_equal((m$y+1)*2, VarExpression(variables=c(0, 2), independent=2))
  expect_equal(2*m$z, VarExpression(variables=c(2,2), independent=0))
  expect_equal(m$z*2, VarExpression(variables=c(2,2), independent=0))
  expect_equal(2*(m$z+1), VarExpression(variables=c(2, 2), independent=2))
  expect_equal((m$z+1)*2, VarExpression(variables=c(2, 2), independent=2))
  expect_equal((3+2)*m$z, VarExpression(variables=c(5, 5), independent=0))
  expect_equal(m$z*(3+2), VarExpression(variables=c(5, 5), independent=0))
})


test_that("/", {
  m <- romo::Model()
  m$x <- romo::Var(name='x')
  m$y <- romo::Var(name='y')
  m$z <- romo::AuxVar(name='z', expr=m$x+m$y)
  expect_equal(m$x/2, VarExpression(variables=0.5, independent=0))
  expect_equal((m$y+1)/2, VarExpression(variables=c(0, 0.5), independent=0.5))
  expect_equal(m$z/2, VarExpression(variables=c(0.5,0.5), independent=0))
  expect_equal((m$z+1)/2, VarExpression(variables=c(0.5, 0.5), independent=0.5))
})


test_that("<=", {
  m <- romo::Model()
  m$x <- romo::Var(name='x')
  m$y <- romo::Var(name='y')
  m$z <- romo::AuxVar(name='z', expr=m$x+m$y)
  expect_equal(m$x <= 1, ConstraintExpression(lhs=1, sense="<=", rhs=1))
  expect_equal(m$z <= 1, ConstraintExpression(lhs=c(1,1), sense="<=", rhs=1))
  expect_equal(m$z <= m$x, ConstraintExpression(lhs=c(0,1), sense="<=", rhs=0))
  expect_equal(m$z+1 <= m$x, ConstraintExpression(lhs=c(0,1), sense="<=", rhs=-1))
  expect_equal(m$z+1 <= m$x+m$y, ConstraintExpression(lhs=c(0,0), sense="<=", rhs=-1))
})


test_that(">=", {
  m <- romo::Model()
  m$x <- romo::Var(name='x')
  m$y <- romo::Var(name='y')
  m$z <- romo::AuxVar(name='z', expr=m$x+m$y)
  expect_equal(m$x >= 1, ConstraintExpression(lhs=1, sense=">=", rhs=1))
  expect_equal(m$z >= 1, ConstraintExpression(lhs=c(1,1), sense=">=", rhs=1))
  expect_equal(m$z >= m$x, ConstraintExpression(lhs=c(0,1), sense=">=", rhs=0))
  expect_equal(m$z+1 >= m$x, ConstraintExpression(lhs=c(0,1), sense=">=", rhs=-1))
  expect_equal(m$z+1 >= m$x+m$y, ConstraintExpression(lhs=c(0,0), sense=">=", rhs=-1))
})


test_that("==", {
  m <- romo::Model()
  m$x <- romo::Var(name='x')
  m$y <- romo::Var(name='y')
  m$z <- romo::AuxVar(name='z', expr=m$x+m$y)
  expect_equal(m$x == 1, ConstraintExpression(lhs=1, sense="==", rhs=1))
  expect_equal(m$z == 1, ConstraintExpression(lhs=c(1,1), sense="==", rhs=1))
  expect_equal(m$z == m$x, ConstraintExpression(lhs=c(0,1), sense="==", rhs=0))
  expect_equal(m$z+1 == m$x, ConstraintExpression(lhs=c(0,1), sense="==", rhs=-1))
  expect_equal(m$z+1 == m$x+m$y, ConstraintExpression(lhs=c(0,0), sense="==", rhs=-1))
})
