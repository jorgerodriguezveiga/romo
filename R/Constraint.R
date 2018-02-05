
Constraint <- function(sets, expression(expr)){
  eval(expression(expr), list(j=1))
}



# Constraint ------------------------------------------------------------------
Constraint <- function(name, sets, expr, start_position=1, description=""){
  if(length(sets)==0){
    return(ConstraintElement(name, expr, position=start_position, 
                             state="active", description=description))
  }else{
    ind = indices(sets)
    
    constr <- list()
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
      
      new_expr = eval(expr, list(j=1))
      constr[[pos]] <- ConstraintElement(name=ele_name, expr=expr, 
                                         position=pos, state="active", 
                                         description=description)
    }
    
    return(.Var(name=name, sets=sets, position=position, constraint=constraint, 
                description=description))
  }
}
# --------------------------------------------------------------------------- #


# .Constraint -----------------------------------------------------------------
.Constraint <- setClass(
  # Class name
  ".Constraint",
  
  # Define the slots
  representation = list(
    name = "character",
    sets = "list",
    position = "arrayORnumeric",
    constraint = "list",
    description = "character"
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object){
    if(length(object@name)==0){
      return("Argument 'name' is required.")
    }
    return(TRUE)
  }
)
# --------------------------------------------------------------------------- #
