
# Model -----------------------------------------------------------------------
#' Initialize model object.
#'
#' @return
#' @export
#'
#' @examples
Model <- function(){
  return(
    ModelClass(
      objects=list(),
      info=ModelInfoClass(nvars=0, nobjs=0, ncons=0)
    )
  )
}
# -----------------------------------------------------------------------------


# ModelClass ------------------------------------------------------------------
#' Model class.
#'
#' @slot objects list. 
#' @slot info ModelInfoClass. 
#'
#' @return
#' @export
#' 
#' @include ModelInfo.R
#'
#' @examples
ModelClass <- setClass(
  # Class name
  "ModelClass",
  
  # Define the slots
  slots = c(
    objects = "list",
    info = "ModelInfoClass"
  )
)
# --------------------------------------------------------------------------- #


# =============================================================================
# Operations
# =============================================================================

# $<- -------------------------------------------------------------------------
#' Assign objects to model class.
#'
#' @param ModelClass 
#'
#' @return
#' @export
#'
#' @examples
setReplaceMethod(
  "$", 
  "ModelClass", 
  function(x, name, value){
    # TODO: check types
    # TODO: change VarElements position.
    if(class(value)=="VarClass"){
      
      start_pos <- x@info@nvars
      x@info@nvars <- start_pos + length(value@variable)
      value@name <- name
      
      v_it <- 1
      for(v in value@variable){
        if(class(value@variable[[v_it]]) == "VarElementClass"){
          value@variable[[v_it]]@position <- start_pos + v_it
          v_it <- v_it + 1
        }
      }
      
      x@objects[[name]] <- value
      
    }else if(class(value)=="VarElementClass"){
      
      x@info@nvars <- x@info@nvars + 1
      value@name <- name
      value@position <- x@info@nvars
      x@objects[[value@name]] <- value
      
    }else if(class(value)=="ObjectiveClass"){
      
      x@info@nobjs <- x@info@nobjs + 1
      x@objects[[name]] <- value
      
    }else if(class(value)=="ConstraintClass"){
      
      start_pos <- x@info@ncons
      x@info@ncons <- start_pos + length(value@constraint)
      value@name <- name
      
      c_it <- 1
      for(c in value@constraint){
        value@constraint[[c_it]]@position <- start_pos + c_it
        c_it <- c_it + 1
      }
      
      x@objects[[name]] <- value
      
    }else if(class(value)=="ConstraintElementClass"){
      
      x@info@ncons <- x@info@ncons + 1
      value@name <- name
      value@position <- x@info@ncons
      
      x@objects[[value@name]] <- value
      
    }else{
      x@objects[[name]] <- value
      
    }
    return(x)
  }
)
# --------------------------------------------------------------------------- #


# $ ---------------------------------------------------------------------------
setMethod(
 "$", 
 signature="ModelClass", 
 function(x, name){
   return(x@objects[[name]])
 }
)
# --------------------------------------------------------------------------- #


# =============================================================================
# Functions
# =============================================================================

# get_objects -----------------------------------------------------------------
#' Title
#'
#' @param model mathematical model of ModelClass class. 
#'
#' @return mathematical model objects.
#' @export
#' @import plyr
#'
#' @examples
get_objects <- function(model){
  # model info
  nvars <- model@info@nvars
  ncons <- model@info@ncons
  nobjs <- model@info@nobjs
  
  # Constraints
  i_cons <- 0

  A <- matrix(0, ncol=nvars, nrow = ncons)
  sense <- c()
  rhs <- c()

  # Variables
  i_vars <- 0
  
  type <- c()
  lb <- c()
  ub <- c()
  
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
# --------------------------------------------------------------------------- #


# solve -----------------------------------------------------------------------
#' Solve model.
#'
#' @param model model of ModelClass class.
#' @param solver solver name. Default to 'gurobi'.
#' @param solver_options list with solver options. By default no options.
#'
#' @return
#' @export
#' @import gurobi
#'
#' @examples
solve <- function(model, solver='gurobi', solver_options=list()){
  objects <- get_objects(model)
  
  if(solver=="gurobi"){
    gurobi_model  <- list()
    
    gurobi_model$lb <- objects$variables$lb
    gurobi_model$ub <- objects$variables$ub  
    gurobi_model$vtype  <- rep("C", model@info@nvars)
    gurobi_model$vtype[objects$variables$type == "integer"] <- "I"
    gurobi_model$vtype[objects$variables$type == "binary"]  <- "B"
    
    gurobi_model$A      <- objects$constraints$A
    gurobi_model$sense  <- objects$constraints$sense
    gurobi_model$sense[gurobi_model$sense == "=="] <- "="
    gurobi_model$rhs    <- objects$constraints$rhs
    
    gurobi_model$obj        <- objects$objective$obj
    gurobi_model$modelsense <- objects$objective$sense
    
    result <- gurobi(gurobi_model, solver_options)
    
    print(result$objval)
    print(result$x)
  }
}
