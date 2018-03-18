
# Solve -----------------------------------------------------------------------
#' Solve model.
#'
#' @param model model of ModelClass class.
#' @param solver solver name. Default to 'gurobi'.
#' @param solver_options list with solver options. By default no options.
#' 
#' @return
#' @export
#'
#' @examples
Solve <- function(model, solver='gurobi', solver_options=list()){
  nameObject <- deparse(substitute(model))
  
  objects <- get_objects(model)
  
  if(solver=="gurobi"){
    gurobi_model  <- list()
    
    gurobi_model$lb <- objects$variables$lb
    gurobi_model$ub <- objects$variables$ub  
    gurobi_model$vtype  <- rep("C", model@info@nvars)
    gurobi_model$vtype[objects$variables$type == "integer"] <- "I"
    gurobi_model$vtype[objects$variables$type == "binary"]  <- "B"
    
    gurobi_model$A      <- objects$constraints$A
    gurobi_model$sense  <- objects$constraints$sense
    gurobi_model$sense[gurobi_model$sense == "=="] <- "="
    gurobi_model$rhs    <- objects$constraints$rhs
    
    gurobi_model$obj        <- objects$objective$obj
    gurobi_model$modelsense <- objects$objective$sense
    
    result <- gurobi::gurobi(gurobi_model, solver_options)
    vars <- result$x
    solver_info = list(
      status=result$status,
      runtime=result$runtime,
      itercount=result$itercount,
      baritercount=result$baritercount,
      nodecount=result$nodecount,
      objval=result$objval,
      objbound=result$objbound
    )
  }
  
  if(solver_info$status == "INFEASIBLE"){
    return(solver_info)
  }else{
    for(o in model@objects){
      if(class(o)=="VarClass"){
        n_vars <- length(o@variable)
        for(v in seq(n_vars)){
          value <- vars[model@objects[[o@name]]@variable[[v]]@position]
          model@objects[[o@name]]@variable[[v]]@value <- value
        }
      }else if(class(o)=="VarElementClass"){
        value <- vars[model@objects[[o@name]]@position]
        model@objects[[o@name]]@value <- value
      }
    }
    
    # Change model in parent frame
    assign(nameObject, model, pos=parent.frame())

    return(solver_info)
  }
}
