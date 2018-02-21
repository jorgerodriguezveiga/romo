
# arrayORnumeric --------------------------------------------------------------
#' @title Array and numeric class.
#' 
#' @description Union of array and numeric classes.
#'  
#' @export
#' 
setClassUnion(
  "vectorORnull", 
  c("vector", "NULL")
)
# --------------------------------------------------------------------------- #


# arrayORnumeric --------------------------------------------------------------
#' @title Array and numeric class.
#' 
#' @description Union of array and numeric classes.
#'  
#' @export
#' 
setClassUnion(
  "arrayORnumeric", 
  c("array", "numeric")
)
# --------------------------------------------------------------------------- #


# arrayORcharacter ------------------------------------------------------------
#' @title Array and character class.
#' 
#' @description Union of array and character classes.
#'  
#' @export
#' 
setClassUnion(
  "arrayORcharacter", 
  c("array", "character")
)
# --------------------------------------------------------------------------- #


# characterORnumeric ----------------------------------------------------------
#' @title Character and numeric class.
#' 
#' @description Union of character and numeric classes.
#'  
#' @export
#' 
setClassUnion(
  "characterORnumeric", 
  c("character", "numeric")
)
# --------------------------------------------------------------------------- #

