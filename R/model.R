
# Model -----------------------------------------------------------------------
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


# ModelInfoClass --------------------------------------------------------------
ModelInfoClass <- setClass(
  # Class name
  "ModelInfoClass",
  
  # Define the slots
  slots = c(
    nvars = "numeric",
    nobjs = "numeric",
    ncons = "numeric"
  )
)
# --------------------------------------------------------------------------- #


# =============================================================================
# Operations
# =============================================================================
# $ ---------------------------------------------------------------------------
#setMethod(
#  "$", 
#  c("ModelClass"), 
#  function(e1, e2){
#    if(class(e2)=="SetClass"){
#      e1@sets[[e2@name]] <- e2
#    }else if(class(e2)=="ParamClass"){
#      e1@parameters[[e2@name]] <- e2
#    }else if(class(e2)=="ParamElementClass"){
#      e1@parameters[[e2@name]] <- e2
#    }else if(class(e2)=="VarClass"){
#      e1@variables[[e2@name]] <- e2
#    }else if(class(e2)=="VarElementClass"){
#      e1@variables[[e2@name]] <- e2
#    }else if(class(e2)=="ObjectiveClass"){
#      e1@objectives[[e2@name]] <- e2
#    }else if(class(e2)=="ConstraintClass"){
#      e1@constraints[[e2@name]] <- e2
#    }else if(class(e2)=="ConstraintElementClass"){
#      e1@constraints[[e2@name]] <- e2
#    }else{
#      stop("Unknown object class.")
#    }
#  }
#)


# mod <- Model()
# mod$A <- 1
# mod$A <- Set(name='A', elements=c(1,2))



# $ ---------------------------------------------------------------------------
setMethod(
  "$", 
  "ModelClass", 
  function(x){
    # TODO: check types
    # TODO: change VarElements position.
    if(class(value)=="VarClass"){
      start_pos <- x@info@nvars + length(value@variable)
      x@info@nvars <- start_pos
      
      for(v in value@variable)
      x@objects[[value@name]] <- value
    }else if(class(value)=="VarElementClass"){
      x@info@nvars <- x@info@nvars + 1
    }else if(class(value)=="ObjectiveClass"){
      x@info@nobjs <- x@info@nobjs + 1
    }else if(class(value)=="ConstraintClass"){
      x@info@ncons <- x@info@ncons + length(value@constraint)
    }else if(class(value)=="ConstraintElementClass"){
      x@info@ncons <- x@info@ncons + 1
    }else{
      x@objects[[value@name]] <- value
    }
    return(x)
  }
)
# --------------------------------------------------------------------------- #


# $<- -------------------------------------------------------------------------
setReplaceMethod(
  "$", 
  c("ModelClass", "ANY"), 
  function(x, value){
    print(x)
    print(value)
    if(class(value)=="SetClass"){
      print(x@sets)
      x@sets[[value@name]] <- value
      #x@sets[[i]] <- value
    }else{
      x@sets[[value@name]] <- 1
    }
    return(x)
  }
)
# --------------------------------------------------------------------------- #


