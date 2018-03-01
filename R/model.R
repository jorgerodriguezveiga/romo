
# Model -----------------------------------------------------------------------
#' Initialize model object.
#'
#' @return
#' @export
#'
#' @examples
#' @note Work with envioronments to simplify notation of constraints and to avoid the use of eval.parent
#' 
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
#' @include model_info.R
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
  A         <- matrix(0, ncol=nvars, nrow = ncons)
  sense     <- character(ncons)
  rhs       <- numeric(nvars)
  con_names <- character(ncons)
  
  # Variables
  var_names <- character(nvars)
  type      <- character(nvars)
  value     <- numeric(nvars)
  lb        <- numeric(nvars)
  ub        <- numeric(nvars)
  
  for(o in model@objects){
    if(class(o)=="ConstraintClass"){
      for(c in o@constraint){
        con_names[c@position] <- c@name
        A[c@position, ]       <- c(c@expr@lhs, numeric(nvars-length(c@expr@lhs)))
        sense[c@position]     <- c@expr@sense
        rhs[c@position]       <- c@expr@rhs
      }
      
    }else if(class(o)=="ConstraintElementClass"){
      con_names[o@position] <- o@name
      A[o@position, ]       <- c(o@expr@lhs, numeric(nvars-length(o@expr@lhs)))
      sense[o@position]     <- o@expr@sense
      rhs[o@position]       <- o@expr@rhs
      
    }else if(class(o)=="VarClass"){
      for(v in o@variable){
        var_names[v@position] <- v@name
        type[v@position] <- v@type
        value[v@position] <- v@value
        lb[v@position]   <- v@lb
        ub[v@position]   <- v@ub
      }
      
    }else if(class(o)=="VarElementClass"){
      var_names[o@position] <- o@name
      type[o@position] <- o@type
      value[o@position]   <- o@value
      lb[o@position]   <- o@lb
      ub[o@position]   <- o@ub
      
    }else if(class(o)=="ObjectiveClass"){
      obj <- c(o@expr@variables, numeric(nvars-length(o@expr@variables)))
      obj_sense <- o@sense
      
    }
  }
  
  # Objects
  objects <- list()
  
  # Constraints
  objects$constraints <- list()
  colnames(A) <- var_names
  row.names(A) <- con_names
  names(sense) <- con_names
  names(rhs)   <- con_names
  
  objects$constraints$A <- A
  objects$constraints$sense <- sense
  objects$constraints$rhs <- rhs
  
  # Variables
  names(type)  <- var_names
  names(value) <- var_names
  names(lb)    <- var_names
  names(ub)    <- var_names
  
  objects$variables      <- list()
  objects$variables$type <- type
  objects$variables$value<- value
  objects$variables$lb   <- lb
  objects$variables$ub   <- ub
  
  # Objective
  names(obj)  <- var_names
  
  objects$objective       <- list()
  objects$objective$obj   <- obj
  objects$objective$sense <- obj_sense
  
  return(objects)
}
# --------------------------------------------------------------------------- #
