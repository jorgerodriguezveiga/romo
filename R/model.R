




# Create Model class -----------------------------------------------------------
Model <- setClass(
  # Class name
  "Model",
  
  # Define the slots
  slots = c(
    sets = "S4",
    parameters = "list",
    variables = "list",
    objectives = "list",
    constraints = "list"
  ),
  
  # Set the default values for the slots. (optional)
  prototype=list(
    sets = Set(),
    parameters = list(),
    variables = list(),
    objectives = list(),
    constraints = list()
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object){
    return(TRUE)
  }
)
# --------------------------------------------------------------------------- #

