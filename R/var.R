

# Var -------------------------------------------------------------------------
Var <- function(name, sets=list(), start_position=1, value=NaN, lb=-Inf, ub=Inf, 
                state="unfix", type="continuous", description=""){
  
  if(length(sets)==0){
    return(VarElement(name=name, position=start_position, value=value, lb=lb, 
                      ub=ub, state=state, type=type, description=description))
  }else{
    ind = indices(sets)
    
    variable <- list()
    position = array(dim=dimension(sets), dimnames=dimensionnames(sets))
    for(i in rownames(ind)){
      # Positions
      pos <- (start_position-1) + as.double(i)
      sets_elem <- as.matrix(ind[i,])
      position[sets_elem] = pos
      
      # Variables
      # ---------
      # Name
      ele_name <- paste(name, "[", paste(sets_elem, collapse=", "), "]", 
                        sep = "")
      
      variable[[pos]] <- VarElement(name=ele_name, position=pos, value=value, 
                                    lb=lb, ub=ub, state=state, type=type, 
                                    description=description)
    }
    
    return(.Var(name=name, sets=sets, position=position, variable=variable, 
                description=description))
  }
}

# --------------------------------------------------------------------------- #

  
# .Var ------------------------------------------------------------------------
.Var <- setClass(
  # Class name
  ".Var",
  
  # Define the slots
  representation = list(
    name = "character",
    sets = "list",
    position = "arrayORnumeric",
    variable = "list",
    description = "character"
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object){
    if(length(object@name)==0) return("Argument 'name' is required.")
    return(TRUE)
  }
)
# --------------------------------------------------------------------------- #


# [] --------------------------------------------------------------------------
setMethod(
  "[", 
  ".Var",
  function(x, i, j, ...){
    pos <- x@position[matrix(c(i, j, ...), nrow=1)]
    x@variable[[pos]]
  }
)
# --------------------------------------------------------------------------- #


# show ------------------------------------------------------------------------
setMethod(
  "show", 
  ".Var",
  function(object){
    print(object@variable)
  }
)
# --------------------------------------------------------------------------- #
