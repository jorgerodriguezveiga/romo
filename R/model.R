
# Model -----------------------------------------------------------------------
Model <- function(){
  return(
    ProblemInfoClass(
      sets=list(),
      parameters=list(),
      variables=list(),
      objectives=list(),
      constraints=list()
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
    sets = "list",
    parameters = "list",
    variables = "list",
    objectives = "list",
    constraints = "list"
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object){
    return(TRUE)
  }
)
# --------------------------------------------------------------------------- #


# =============================================================================
# Operations
# =============================================================================
# $ ---------------------------------------------------------------------------
setMethod(
  "$", 
  signature(e1 = "ModelClass", e2 = "ANY"), 
  function(e1, e2){
    if(class(e2)=="SetClass"){
      e1@sets[[e2@name]] <- e2
    }else if(class(e2)=="ParamClass"){
      e1@parameters[[e2@name]] <- e2
    }else if(class(e2)=="ParamElementClass"){
      e1@parameters[[e2@name]] <- e2
    }else if(class(e2)=="VarClass"){
      e1@variables[[e2@name]] <- e2
    }else if(class(e2)=="VarElementClass"){
      e1@variables[[e2@name]] <- e2
    }else if(class(e2)=="ObjectiveClass"){
      e1@objectives[[e2@name]] <- e2
    }else if(class(e2)=="ConstraintClass"){
      e1@constraints[[e2@name]] <- e2
    }else if(class(e2)=="ConstraintElementClass"){
      e1@constraints[[e2@name]] <- e2
    }else{
      stop("Unknown object class.")
    }
  }
)

