# Roxygen documentation

#' A little function that translates the GTA's naming rules into SQL. Here for variable/vector object.
#'
#' Returns renamed vector.
#'
#' @param convert.var Name of the vector you want to convert (in quotes).
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert

gta_r_to_sql_var <- function(convert.var=NULL) {
  
  if(is.null(convert.var)){
    stop("No variables provided for 'convert.var'.")
  }
  return(gsub("\\.","_",convert.var))
}


