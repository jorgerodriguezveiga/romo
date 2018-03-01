# ModelInfoClass --------------------------------------------------------------
#' Model information class.
#'
#' @slot nvars numeric. 
#' @slot nobjs numeric. 
#' @slot ncons numeric. 
#'
#' @return
#' @export
#'
#' @examples
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
