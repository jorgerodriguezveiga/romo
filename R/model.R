
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
        value@variable[[v_it]]@position <- start_pos + v_it
        print(value@variable[[v_it]]@position)
        v_it <- v_it + 1
      }
      
      x@objects[[name]] <- value
      
    }else if(class(value)=="VarElementClass"){
      
      x@info@nvars <- x@info@nvars + 1
      value@name <- name
      value@position <- x@info@nvars + 1
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
        print(value@constraint[[c_it]]@position)
        c_it <- c_it + 1
      }
      
      x@objects[[name]] <- value
      
    }else if(class(value)=="ConstraintElementClass"){
      
      x@info@ncons <- x@info@ncons + 1
      value@name <- name
      value@position <- x@info@nvars + 1
      
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


