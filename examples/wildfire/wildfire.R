# EXACT

m <- Model()

# =============================================================================
# Sets
# =============================================================================

m$np <- 10

m$I <- Set(name="I", elements = c('air1', 'air2'))
m$G <- Set(name="G", elements = c('air'))
m$T <- Set(name="T", elements = seq(1, m$np))
m$T0 <- Set(name="T0", elements = as.character(seq(0, m$np)))

# Corregir para que los sets puedan definirse sobre otros conjunto: SetExpression.
m$G_I <- function(g){
  G_I = list("air"=c('air1', 'air2'))
  return(Set(name="group", elements=G_I[[g]]))
}


# Define empty set
m$T_int <- function(t1, t2){
  t1 <- as.numeric(t1)
  t2 <- as.numeric(t2)
  if(t1<=t2){
    return(Set(name="T_int", 
               elements=as.character(max(1, as.numeric(t1)):min(m$np, as.numeric(t2)))
    )
    )
  }else{
    return(Set(name="T_int", 
               elements=c()
    )
    )
  }

}

# =============================================================================
# Parameters
# =============================================================================

# Resources
# =========
m$C <- c('air1'=100, 'air2'=150)
m$P <- c('air1'=1000, 'air2'=1500)
m$BPR <- c('air1'=100, 'air2'=150)
m$A <- c('air1'=1, 'air2'=2)
m$CFP <- c('air1'=0, 'air2'=0)
m$CRP <- c('air1'=0, 'air2'=0)
m$CTFP <- c('air1'=0, 'air2'=0)
m$FBRP <- c('air1'=1, 'air2'=1)
m$FP <- c('air1'=12, 'air2'=12)
m$RP <- c('air1'=4, 'air2'=4)
m$DFP <- c('air1'=48, 'air2'=48)
m$ITW <- c('air1'=0, 'air2'=0)
m$IOW <- c('air1'=0, 'air2'=0)

# Groups
# ======
m$nMax <- array(2,
                dim = c(1, m$np), 
                dimnames = list(c('air'), m$T@elements))
m$nMin <- array(0, 
                dim = c(1, m$np), 
                dimnames = list(c('air'), m$T@elements))

# Wildfire
# ========
m$PER <- c('1'=100, '2'=50, '3'=50, '4'=50, '5'=50, '6'=50, '7'=50, '8'=50, '9'=50, '10'=100)
m$NVC <- c('1'=1000, '2'=1000, '3'=1000, '4'=1000, '5'=1000, '6'=1000, '7'=1000, '8'=1000, '9'=1000, '10'=1000)
m$EF <- array(1, 
              dim = c(length(m$I@elements), m$np), 
              dimnames = list(m$I@elements, m$T@elements))

# =============================================================================
# Model information 
# =============================================================================

# Auxiliar
# ========
m$PR <- m$BPR * m$EF
m$M_prime <- 100*(sum(m$C) + sum(m$NVC))
m$M <- sum(m$PER) + sum(m$PR)


# =============================================================================
# Variables
# =============================================================================

# Resources
# =========
m$s <- Var(name = "s", sets = ListSets(m$I, m$T), type = "binary")
m$fl <- Var(name = "fl", sets = ListSets(m$I, m$T), type = "binary")
m$r <- Var(name = "r", sets = ListSets(m$I, m$T), type = "binary")
m$er <- Var(name = "er", sets = ListSets(m$I, m$T), type = "binary")
m$e <- Var(name = "e", sets = ListSets(m$I, m$T), type = "binary")

# Wildfire
# ========
m$y <- Var(name = "y", sets = ListSets(m$T0), type = "binary")
m$mu <- Var(name = "mu", sets = ListSets(m$G, m$T), type = "continuous", lb=0)

# Auxiliary
# =========
m$u <- AuxVar(
  name="u", 
  iterator=Iter(i %inset% m$I, t %inset% m$T), 
  expr = (
      Sum(
        iterator = Iter(t1 %inset% m$T_int(1,t)), 
        expr = m$s[i, t1]
      ) 
      - Sum(
        iterator = Iter(t2 %inset% m$T_int(1, as.numeric(t)-1)),
        expr = m$e[i,t2]
      )
  )
)

m$w <- AuxVar(
  name="w", 
  iterator=Iter(i %inset% m$I, t %inset% m$T), 
  expr = m$u[i, t] - m$r[i, t] - m$fl[i, t]
)

m$z <- AuxVar(
  name="z", 
  iterator=Iter(i %inset% m$I), 
  expr = Sum(iterator = Iter(t %inset% m$T), expr = m$e[i, t])
)


# =============================================================================
# Model
# =============================================================================

# Objective function
# ==================

# Auxiliary variables
# -------------------
m$Cost <- AuxVar(
  name="Cost",
  expr = (
      Sum(
        iterator = Iter(i1 %inset% m$I, t1 %inset% m$T), 
        expr = m$C[i1]*m$u[i1, t1]) 
      + Sum(
        iterator = Iter(i2 %inset% m$I), 
        expr = m$P[i2]*m$z[i2]) 
      + Sum(
        iterator = Iter(t2 %inset% m$T), 
        expr = m$NVC[t2]*m$y[as.numeric(t2)-1])
  )
)

m$Penalty <- AuxVar(
  name="Penalty",
  expr = Sum(
          iterator = Iter(g %inset% m$G, t %inset% m$T),
          expr = m$M_prime*m$mu[g, t]
  ) + m$y[m$np]
)
    

# Total Cost
# ----------
m$Total_Cost <- Objective(
  name = "Total_Cost",
  sense = "minimize",
  expr = m$Cost + m$Penalty
)


# Constraints
# ===========

# Wildfire containment
# --------------------
m$cont_1 <- Constraint(
  name = "cont_1",
  expr = (
    Sum(
      iterator = Iter(t1 %inset% m$T), 
      expr = m$PER[t1]*m$y[as.numeric(t1)-1]) 
    <= 
    Sum(
      iterator = Iter(i %inset% m$I, t2 %inset% m$T),
      expr = m$PR[i,t2]*m$w[i,t2]
    )
  )
)

m$cont_2 <- Constraint(
  name = "cont_2",
  iterator = Iter(t %inset% m$T),
  expr = (
    Sum(
      iterator = Iter(t1 %inset% m$T_int(1, t)), 
      expr = m$PER[t1]*m$y[as.numeric(t)-1]) 
    <= 
    Sum(
      iterator = Iter(i %inset% m$I, t2 %inset% m$T_int(1, t)),
      expr = m$PR[i,t2]*m$w[i,t2]
    ) 
    + m$M*m$y[t]
  )
)


# Start of activity
# -----------------
m$start_act_1 <- Constraint(
  name = "start_act_1",
  iterator = Iter(i %inset% m$I, t %inset% m$T),
  expr = (
    m$A[i]*m$w[i,t] <= 
    Sum(
      iterator=Iter(t1 %inset% m$T_int(1, t)), 
      expr= m$fl[i, t1]
    )
  )
)

m$start_act_2 <- Constraint(
  name = "start_act_2",
  iterator = Iter(i %inset% m$I),
  expr = (if(m$ITW[i] == 1){
    m$s[i,1] + Sum(iterator=Iter(t %inset% m$T), expr=(m$np+1)*m$s[i,t]) - m$np*m$z[i]
  }else{
    Sum(iterator=Iter(t %inset% m$T), expr=m$s[i,t]) - m$z[i]
  } <= 0
  )
)


# End of activity
# ---------------
m$end_act <- Constraint(
  name = "end_act",
  iterator = Iter(i %inset% m$I, t %inset% m$T),
  expr = (
    Sum(
      iterator=Iter(t1 %inset% m$T_int(1, as.numeric(t)-m$FBRP[i]+1)),
      expr=m$fl[i,t1]  
    ) 
    >= 
    m$FBRP[i]*m$e[i, t]
  )
)

# Breaks
# ------

# Auxiliary variables
# ···················
m$cr <- AuxVar(
  name="cr", 
  iterator=Iter(i %inset% m$I, t %inset% m$T), 
  expr = (
    if(m$ITW[i] == 0 && m$IOW[i] == 0){
      Sum(
        iterator = Iter(t1 %inset% m$T_int(1, t)),
        expr = ((as.numeric(t)+1-as.numeric(t1))*m$s[i, t1] 
                - (as.numeric(t)-as.numeric(t1))*m$e[i,t1] 
                - m$r[i,t1]
                - m$FP[i]*m$er[i,t1]
        )
      )
    }else{
      (as.numeric(t)+m$CFP[i]-m$CRP[i])*m$s[i,1] + Sum(
        iterator = Iter(t1 %inset% m$T_int(2, t)),
        expr = (as.numeric(t)+1-as.numeric(t1)+m$FP[i])*m$s[i,t1]
      ) + Sum(
        iterator = Iter(t2 %inset% m$T_int(1, t)),
        expr = (
               - (as.numeric(t)-as.numeric(t2))*m$e[i,t2] 
               - m$r[i,t2]
               - m$FP[i]*m$er[i,t2]
        )
      )
    }
  )
)


# Constraints
# ···········
m$breaks_1_lb <- Constraint(
  name = "breaks_1_lb",
  iterator = Iter(i %inset% m$I, t %inset% m$T),
  expr = 0 <= m$cr[i,t]
)

m$breaks_1_ub <- Constraint(
  name = "breaks_1_ub",
  iterator = Iter(i %inset% m$I, t %inset% m$T),
  expr = m$cr[i,t] <= m$FP[i]
)

m$break_2 <- Constraint(
  name = "break_2",
  iterator = Iter(i %inset% m$I, t %inset% m$T),
  expr = (
    if(as.numeric(t)-m$RP[i] >= 0){
      Sum(
        iterator = Iter(t1 %inset% m$T_int(as.numeric(t)-m$RP[i]+1,t)),
        expr = m$r[i,t1]
      ) >= m$RP[i]*m$er[i,t]
    }else{
      m$CRP[i]*m$s[i,1] + Sum(
        iterator = Iter(t1 %inset% m$T_int(1,t)), 
        expr = m$r[i,t1]
      ) >= m$RP[i]*m$er[i,t]
    } 
  )
)

m$break_3 <- Constraint(
  name = "break_3",
  iterator = Iter(i %inset% m$I, t %inset% m$T),
  expr = (
    Sum(
      iterator=Iter(t1 %inset% m$T_int(as.numeric(t)-m$FBRP[i], as.numeric(t)+m$FBRP[i])),
      expr = m$r[i,t1]+m$fl[i,t1]
    )
    >= 
    Sum(
      iterator=Iter(t1 %inset% m$T_int(as.numeric(t)-m$FBRP[i], as.numeric(t)+m$FBRP[i])),
      expr = m$r[i,t]
    )
  )
)

# Maximum number of usage periods in a day
# ----------------------------------------
m$max_num_usage <- Constraint(
  name = "max_num_usage",
  iterator = Iter(i %inset% m$I),
  expr = (
    Sum(
      iterator = Iter(t %inset% m$T),
      expr = m$u[i,t]
    )
    <= m$DFP[i] - m$CTFP[i]
  )
)


# Maximum and minimum number of resources of a group
# --------------------------------------------------
m$min_group <- Constraint(
  name = "min_group",
  iterator = Iter(g %inset% m$G, t %inset% m$T),
  expr = (
    m$nMin[g,t]*m$y[as.numeric(t)-1] <= Sum(
      iterator = Iter(i %inset% m$G_I(g)),
      expr = m$w[i,t] + m$mu[g,t]
    )
  )
)

m$max_group <- Constraint(
  name = "max_group",
  iterator = Iter(g %inset% m$G, t %inset% m$T),
  expr = (
    Sum(
      iterator = Iter(i %inset% m$G_I(g)),
      expr = m$w[i,t]
    )
    <= m$nMax[g,t]*m$y[as.numeric(t)-1] 
  )
)


# Logical
# -------
m$logical_1 <- Constraint(
  name = "logical_1",
  iterator = Iter(i %inset% m$I),
  expr = (
    Sum(
      iterator = Iter(t %inset% m$T),
      expr = as.numeric(t)*m$e[i,t]
    )
    >= 
    Sum(
      iterator = Iter(t %inset% m$T),
      as.numeric(t)*m$s[i,t]
    )
  )
)

m$logical_2 <- Constraint(
  name = "logical_2",
  iterator = Iter(i %inset% m$I),
  expr = (
    Sum(
      iterator = Iter(t %inset% m$T),
      expr = m$e[i,t]
    )
    <= 1
  )
)

m$logical_3 <- Constraint(
  name = "logical_3",
  iterator = Iter(i %inset% m$I, t %inset% m$T),
  expr = (
    m$r[i,t] + m$fl[i,t] <= m$u[i,t]
  )
)

m$logical_4 <- Constraint(
  name = "logical_4",
  expr = (
    m$y[0] == 1
  )
)


# =============================================================================
# Solve model
# =============================================================================

results <- solve(m)
m$y
#sol <- results$x
#
#objects <- get_objects(m)
#
#num_cons <- m@info@ncons
#
#names <- rownames(objects$constraints$A)
#lhs <- objects$constraints$A%*%sol
#rhs <- objects$constraints$rhs
#sense <- objects$constraints$sense
#
#for(i in seq(num_cons)){
#  check_cons <- eval(parse(text=paste(lhs[i], sense[i], rhs[i])))
#  cat(paste("Constraint: ", names[i], "\n", sep=""))
#  cat(paste(check_cons, ":  ",
#            "( ", lhs[i], " ", sense[i], " ", rhs[i], " )", "\n\n", sep=""))
#}


