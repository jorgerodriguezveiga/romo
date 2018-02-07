

# Expression ---------------------------------------------------------------
#' Mathematical expression..
#'
#' @param expr mathematical expression.
#'
#' @return
#' @export
#'
#' @examples
Expression <- function(expr){
  return(
    ExpressionClass(expr=deparse(substitute(expr)))
  )
}
# --------------------------------------------------------------------------- #


# NoExpression ---------------------------------------------------------------
#' Mathematical expression..
#'
#' @param expr mathematical expression.
#'
#' @return
#' @export
#'
#' @examples
NoExpression <- function(expr){
  return(
    ExpressionClass(expr=expr)
  )
}
# --------------------------------------------------------------------------- #

# eval(parse(text= ExpressionClass))

# ExpressionClass --------------------------------------------------------------
#' Expression class.
#'
#' @slot expression character.
#'
#' @return
#' @export
#'
#' @examples
ExpressionClass <- setClass(
  # Class name
  "ExpressionClass",
  
  # Define the slots
  representation = list(
    expr = "character"
  )
)
# --------------------------------------------------------------------------- #


# =============================================================================
# Oparations
# =============================================================================

# + ---------------------------------------------------------------------------
setMethod(
  "+", 
  signature(e1 = "ExpressionClass", e2 = "ExpressionClass"), 
  function(e1, e2){
    ExpressionClass(expr=paste(e1@expr, e2@expr, sep = " + "))
  }
)

setMethod(
  "+", 
  signature(e1 = "ExpressionClass", e2 = "ANY"), 
  function(e1, e2){
    ExpressionClass(expr=paste(e1@expr, deparse(substitute(e2)), sep = " + "))
  }
)

setMethod(
  "+", 
  signature(e1 = "ANY", e2 = "ExpressionClass"), 
  function(e1, e2){
    ExpressionClass(expr=paste(deparse(substitute(e1)), e2@expr, sep = " + "))
  }
)
# -----------------------------------------------------------------------------


# - ---------------------------------------------------------------------------
setMethod(
  "-", 
  signature(e1 = "ExpressionClass", e2 = "ExpressionClass"), 
  function(e1, e2){
    ExpressionClass(expr=paste(e1@expr, e2@expr, sep = " - "))
  }
)

setMethod(
  "-", 
  signature(e1 = "ExpressionClass", e2 = "ANY"), 
  function(e1, e2){
    ExpressionClass(expr=paste(e1@expr, deparse(substitute(e2)), sep = " - "))
  }
)

setMethod(
  "-", 
  signature(e1 = "ANY", e2 = "ExpressionClass"), 
  function(e1, e2){
    ExpressionClass(expr=paste(deparse(substitute(e1)), e2@expr, sep = " - "))
  }
)
# -----------------------------------------------------------------------------


# * ---------------------------------------------------------------------------
setMethod(
  "*", 
  signature(e1 = "ExpressionClass", e2 = "ExpressionClass"), 
  function(e1, e2){
    ExpressionClass(expr=paste(e1@expr, e2@expr, sep = " * "))
  }
)

setMethod(
  "*", 
  signature(e1 = "ExpressionClass", e2 = "ANY"), 
  function(e1, e2){
    ExpressionClass(expr=paste(e1@expr, deparse(substitute(e2)), sep = " * "))
  }
)

setMethod(
  "*", 
  signature(e1 = "ANY", e2 = "ExpressionClass"), 
  function(e1, e2){
    ExpressionClass(expr=paste(deparse(substitute(e1)), e2@expr, sep = " * "))
  }
)
# -----------------------------------------------------------------------------


# / ---------------------------------------------------------------------------
setMethod(
  "/", 
  signature(e1 = "ExpressionClass", e2 = "ExpressionClass"), 
  function(e1, e2){
    ExpressionClass(expr=paste(e1@expr, e2@expr, sep = " / "))
  }
)

setMethod(
  "/", 
  signature(e1 = "ExpressionClass", e2 = "ANY"), 
  function(e1, e2){
    ExpressionClass(expr=paste(e1@expr, deparse(substitute(e2)), sep = " / "))
  }
)

setMethod(
  "/", 
  signature(e1 = "ANY", e2 = "ExpressionClass"), 
  function(e1, e2){
    ExpressionClass(expr=paste(deparse(substitute(e1)), e2@expr, sep = " / "))
  }
)
# -----------------------------------------------------------------------------
