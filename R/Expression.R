

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


# get_indexed_expr ------------------------------------------------------------
#' Substitute index by its value.
#'
#' @param string string with mathematical expression.
#' @param iterator iterator.
#' @param i combination of the indices name.
#' @param ind object with combination indices values.
#'
#' @return
#' @export
#'
#' @examples
get_indexed_expr <- function(string, iterator, i, ind){
  split_elems <- "\\[|\\]|,| |\\+|\\-|\\*|\\/|<=|>=|==|\\(|\\)"
  splited_expr <- unlist(strsplit(string, 
                                  paste("(?=>", split_elems, ")", sep=""), 
                                  perl=T))
  splited_expr <- unlist(strsplit(splited_expr, 
                                  paste("(?<=", split_elems, ")", sep=""), 
                                  perl=T))
  
  count <- 0
  for(s in iterator){
    count <- count + 1
    splited_expr[splited_expr==s@i] <- paste('', as.vector(ind[i,count]), '', 
                                             sep="'")
  }
  
  return(paste(splited_expr, collapse=""))
}
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


# <= --------------------------------------------------------------------------
setMethod(
  "<=", 
  signature(e1 = "ExpressionClass", e2 = "ExpressionClass"), 
  function(e1, e2){
    ExpressionClass(expr=paste(e1@expr, e2@expr, sep = " <= "))
  }
)

setMethod(
  "<=", 
  signature(e1 = "ExpressionClass", e2 = "ANY"), 
  function(e1, e2){
    ExpressionClass(expr=paste(e1@expr, deparse(substitute(e2)), sep = " <= "))
  }
)

setMethod(
  "<=", 
  signature(e1 = "ANY", e2 = "ExpressionClass"), 
  function(e1, e2){
    ExpressionClass(expr=paste(deparse(substitute(e1)), e2@expr, sep = " <= "))
  }
)
# -----------------------------------------------------------------------------


# >= --------------------------------------------------------------------------
setMethod(
  ">=", 
  signature(e1 = "ExpressionClass", e2 = "ExpressionClass"), 
  function(e1, e2){
    ExpressionClass(expr=paste(e1@expr, e2@expr, sep = " >= "))
  }
)

setMethod(
  ">=", 
  signature(e1 = "ExpressionClass", e2 = "ANY"), 
  function(e1, e2){
    ExpressionClass(expr=paste(e1@expr, deparse(substitute(e2)), sep = " >= "))
  }
)

setMethod(
  ">=", 
  signature(e1 = "ANY", e2 = "ExpressionClass"), 
  function(e1, e2){
    ExpressionClass(expr=paste(deparse(substitute(e1)), e2@expr, sep = " >= "))
  }
)
# -----------------------------------------------------------------------------


# == --------------------------------------------------------------------------
setMethod(
  "==", 
  signature(e1 = "ExpressionClass", e2 = "ExpressionClass"), 
  function(e1, e2){
    ExpressionClass(expr=paste(e1@expr, e2@expr, sep = " == "))
  }
)

setMethod(
  "==", 
  signature(e1 = "ExpressionClass", e2 = "ANY"), 
  function(e1, e2){
    ExpressionClass(expr=paste(e1@expr, deparse(substitute(e2)), sep = " == "))
  }
)

setMethod(
  "==", 
  signature(e1 = "ANY", e2 = "ExpressionClass"), 
  function(e1, e2){
    ExpressionClass(expr=paste(deparse(substitute(e1)), e2@expr, sep = " == "))
  }
)
# -----------------------------------------------------------------------------
