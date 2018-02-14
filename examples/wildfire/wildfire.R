# EXACT

m <- Model()

# =============================================================================
# Sets
# =============================================================================

np <- 10

m$I <- Set(name="I", elements = c('air1', 'air2'))
m$G <- Set(name="G", elements = c('air'))
m$T <- Set(name="T", elements = seq(1,np))
m$T0 <- Set(name="T0", elements = as.character(seq(0,np)))

# Corregir para que los sets puedan definirse sobre otros conjunto: SetExpression.
m$G_I <- list("air"=c('air1', 'air2'))

# Define empty set
m$T_int <- function(t1, t2){
  t1 <- as.numeric(t1)
  t2 <- as.numeric(t2)
  if(t1<=t2){
    return(Set(name="T_int", 
               elements=as.character(max(1, as.numeric(t1)):min(np,as.numeric(t2)))
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
m$nMax <- array(c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2), 
                dim = c(1, np), 
                dimnames = list(c('air'), m$T@elements))
m$nMin <- array(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                dim = c(1, np), 
                dimnames = list(c('air'), m$T@elements))

# Wildfire
# ========
m$PER <- c('1'=100, '2'=100, '3'=100, '4'=100, '5'=100, '6'=100, '7'=100, '8'=100, '9'=100, '10'=100)
m$NVC <- c('1'=1000, '2'=1000, '3'=1000, '4'=1000, '5'=1000, '6'=1000, '7'=1000, '8'=1000, '9'=1000, '10'=1000)
m$EF <- array(1, 
              dim = c(length(m$I@elements), np), 
              dimnames = list(m$I@elements, m$T@elements))

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
  expr = Sum(
            iterator = Iter(t1 %inset% m$T_int(1,t)), 
            expr = m$s[i, t1]
            ) - Sum(
            iterator = Iter(t2 %inset% m$T_int(1, as.numeric(t)-1)),
            expr = m$e[i,t2]
            )
)

- sum{t2 in T_int[1, t-1]} e[i, t2];
var w {i in I, t in T} = u[i, t] - r[i, t] - fl[i, t];
var z {i in I}         = sum{t in T} e[i, t];


# =============================================================================
# Model
# =============================================================================

# Objective function
# ==================

# Auxiliary variables
# -------------------
var Cost = + sum{i in I, t in T} C[i]*u[i, t] 
+ sum{i in I} P[i]*z[i] 
+ sum{t in T} NVC[t]*y[t-1];

var Penalty = sum{g in G, t in T} M_prime*mu[g, t] + y[m];

# Total Cost
# ----------
minimize Total_Cost: 
  Cost + Penalty
;

# Constraints
# ===========

# Wildfire containment
# --------------------
subject to cont_1:
  sum{t in T} PER[t]*y[t-1] <= sum{i in I, t in T} PR[i,t]*w[i,t]
;

subject to cont_2 {t in T}:
  sum{t1 in T_int[1,t]} PER[t1]*y[t-1] 
<=
  sum{i in I, t1 in T_int[1,t]} PR[i,t1]*w[i,t1] + M*y[t]
;


# Start of activity
# -----------------
subject to start_act_1 {i in I, t in T}:
  A[i]*w[i,t] <= sum{t1 in T_int[1,t]} fl[i,t1]
;

subject to start_act_2 {i in I}:
  if ITW[i] == 1 then
s[i,1] + sum{t in T_int[2,m]} (m+1)*s[i,t] - m*z[i]
else
  sum{t in T} s[i,t] - z[i]
<= 0
;


# End of activity
# ---------------
subject to end_act {i in I, t in T}:
  sum{t1 in T_int[max(1, min(m, t-FBRP[i]+1)),t]} fl[i,t1] >= FBRP[i]*e[i,t]
;


# Breaks
# ------

# Auxiliary variables
# ···················
var cr {i in I, t in T} = 
  if (ITW[i] == 0) and (IOW[i] == 0) then
+ sum{t1 in T_int[1,t]} (t+1-t1)*s[i,t1]
- sum{t2 in T_int[1,t]} (t-t2)*e[i,t2]
- sum{t3 in T_int[1,t]} r[i,t3]
- sum{t4 in T_int[1,t]} FP[i]*er[i,t4]
else
  + (t+CFP[i]-CRP[i])*s[i,1]
+ sum{t1 in T_int[2,t]} (t+1-t1+FP[i])*s[i,t1]
- sum{t2 in T_int[1,t]} (t-t2)*e[i,t2]
- sum{t3 in T_int[1,t]} r[i,t3]
- sum{t4 in T_int[1,t]} FP[i]*er[i,t4]
;


# Constraints
# ···········
subject to breaks_1 {i in I, t in T}:
  0 <= cr[i,t] <= FP[i]
;

subject to break_2 {i in I, t in T}:
  if t-RP[i] >= 0 then
sum{t1 in T_int[max(1, t-RP[i]+1),t]} r[i,t1] 
else
  CRP[i]*s[i,1] + sum{t1 in T_int[1,t]} r[i,t1]

>= RP[i]*er[i,t]
;

subject to break_3 {i in I, t in T}:
  sum{t1 in T_int[max(1,t-FBRP[i]),min(m,t+FBRP[i])]} (r[i,t1]+fl[i,t1])
>= sum{t1 in T_int[max(1,t-FBRP[i]),min(m,t+FBRP[i])]} r[i,t]
;

# Maximum number of usage periods in a day
# ----------------------------------------

subject to max_num_usage {i in I}:
  sum{t in T} u[i,t] <= DFP[i] - CTFP[i]
;


# Maximum and minimum number of resources of a group
# --------------------------------------------------

subject to min_group {g in G, t in T}:
  nMin[g,t]*y[t-1] <= sum{i in G_I[g]} w[i,t] + mu[g,t]
;

subject to max_group {g in G, t in T}:
  sum{i in G_I[g]} w[i,t] <= nMax[g,t]*y[t-1]
;


# Logical
# -------

subject to logical_1 {i in I}:
  sum{t in T} t*e[i,t] >= sum{t in T} t*s[i,t]
;

subject to logical_2 {i in I}:
  sum{t in T} e[i,t] <= 1
;

subject to logical_3 {i in I, t in T}:
  r[i,t] + fl[i,t] <= u[i,t]
;

subject to logical_4:
  y[0] = 1
;
