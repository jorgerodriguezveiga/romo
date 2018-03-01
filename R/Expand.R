Expand <- function(model){
  for(o in model@objects){
    if(class(o)=="ConstraintClass"){
      for(c in o@constraint){
        i_cons <- i_cons + 1
        A[i_cons, ] <- c(c@expr@lhs, numeric(nvars-length(c@expr@lhs)))
        sense <- c(sense, c@expr@sense)
        rhs <- c(rhs, c@expr@rhs)
      }
      
    }else if(class(o)=="ConstraintElementClass"){
      i_cons <- i_cons + 1
      A[i_cons, ] <- c(o@expr@lhs, numeric(nvars-length(o@expr@lhs)))
      sense <- c(sense, o@expr@sense)
      rhs <- c(rhs, o@expr@rhs)
      
    }else if(class(o)=="VarClass"){
      for(v in o@variable){
        i_vars <- i_vars + 1
        type <- c(type, v@type)
        lb <- c(lb, v@lb)
        ub <- c(ub, v@ub)
      }
      
    }else if(class(o)=="VarElementClass"){
      i_vars <- i_vars + 1
      type <- c(type, o@type)
      lb <- c(lb, o@lb)
      ub <- c(ub, o@ub)
      
    }else if(class(o)=="ObjectiveClass"){
      obj <- c(o@expr@variables, numeric(nvars-length(o@expr@variables)))
      obj_sense <- o@sense
      
    }
  }
  
  # Objects
  objects <- list()
  
  # Constraints
  objects$constraints <- list()
  
  objects$constraints$A <- matrix(unlist(A), ncol=nvars, byrow = T)
  objects$constraints$sense <- sense
  objects$constraints$rhs <- rhs
  
  # Variables
  objects$variables <- list()
  objects$variables$type <- type
  objects$variables$lb <- lb
  objects$variables$ub <- ub
  
  # Objective
  objects$objective <- list()
  objects$objective$obj <- obj
  objects$objective$sense <- obj_sense
  
  return(objects)
}
