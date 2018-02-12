

# VarElement ------------------------------------------------------------------
VarElement <- function(name, position=1, value=NaN, lb=-Inf, ub=Inf, 
                       state="unfix", type="continuous", description=""){
  names(position) <- name
  names(value) <- name
  names(lb) <- name
  names(ub) <- name
  names(state) <- name
  return(
    VarElementClass(name=name, position=position, value=value, lb=lb, ub=ub,
                    state=state, type=type, description=description)
  )
}
# --------------------------------------------------------------------------- #


# VarElementClass -----------------------------------------------------------------
#' Var element class.
#'
#' @slot name character. 
#' @slot position numeric. 
#' @slot value numeric. 
#' @slot lb numeric. 
#' @slot ub numeric. 
#' @slot state character. 
#' @slot type character. 
#' @slot description character. 
#'
#' @return
#' @export
#'
#' @examples
VarElementClass <- setClass(
  # Class name
  "VarElementClass",
  
  # Define the slots
  representation = list(
    name = "character",
    position = "numeric",
    value = "numeric",
    lb = "numeric",
    ub = "numeric",
    state = "character",
    type = "character",
    description = "character"
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object){
    if(length(object@name)==0) return("Argument 'name' is required.")
    if (length(object@value)!=1) return("Length of 'value' must be 1.")
    if (length(object@lb)!=1) return("Length of 'lb' must be 1.")
    if (length(object@ub)!=1) return("Length of 'ub' must be 1.")
    if (length(object@state)!=1) return("Length of 'state' must be 1.")
    if(!(object@state %in% c("fix", "unfix"))){
      return("'state' value not in c('fix', 'unfix').")
    }
    if((!object@type %in% c("binary", "continuous", "integer"))){
      return("'type' value not in c('binary', 'continuous', 'integer').")
    }
    if(object@position <= 0){
      return("'position' must be '>= 1'.")
    }
    return(TRUE)
  }
)
# --------------------------------------------------------------------------- #


# show ------------------------------------------------------------------------
# setMethod(
#   "show", 
#   "VarElementClass",
#   function(object){
#     print(object@value)
#   }
# )
# --------------------------------------------------------------------------- #


# =============================================================================
# Operations
# =============================================================================

# + ---------------------------------------------------------------------------
setMethod(
  "+", 
  signature(e1 = "numeric", e2 = "VarElementClass"), 
  function(e1, e2){
    variables=numeric(e2@position)
    variables[e2@position] = 1
    VarExpression(independent=e1, variables=variables)
  }
)

setMethod(
  "+", 
  signature(e1 = "VarElementClass", e2 = "numeric"), 
  function(e1, e2){
    e2 + e1
  }
)

setMethod(
  "+", 
  signature(e1 = "VarElementClass", e2 = "VarElementClass"), 
  function(e1, e2){
    variables=numeric(max(e1@position, e2@position))
    
    variables[e1@position] = 1
    variables[e2@position] = variables[e2@position] + 1
    VarExpression(independent=0, variables=variables)
  }
)
# -----------------------------------------------------------------------------


# - ---------------------------------------------------------------------------
setMethod(
  "-",
  signature(e1 = "VarElementClass", e2 = "VarElementClass"), 
  function(e1, e2){
    e1 + (-e2)
  }
)

setMethod(
  "-",
  signature(e1 = "VarElementClass", e2 = "numeric"), 
  function(e1, e2){
    e1 + (-e2)
  }
)

setMethod(
  "-",
  signature(e1 = "numeric", e2 = "VarElementClass"), 
  function(e1, e2){
    variables=numeric(e2@position)
    variables[e2@position] = - 1
    VarExpression(independent=e1, variables=variables)
  }
)

setMethod(
  "-", 
  signature(e1 = "VarElementClass"), 
  function(e1){
    0 - e1
  }
)
# -----------------------------------------------------------------------------


# * ---------------------------------------------------------------------------
setMethod(
  "*", 
  signature(e1 = "numeric", e2 = "VarElementClass"), 
  function(e1, e2){
    variables=numeric(e2@position)
    variables[e2@position] = e1
    VarExpression(independent=0, variables=variables)
  }
)

setMethod(
  "*", 
  signature(e1 = "VarElementClass", e2 = "numeric"), 
  function(e1, e2){
    e2*e1
  }
)
# -----------------------------------------------------------------------------


# / ---------------------------------------------------------------------------
setMethod(
  "/", 
  signature(e1 = "VarElementClass", e2 = "numeric"), 
  function(e1, e2){
    (1/e2)*e1
  }
)
# -----------------------------------------------------------------------------
