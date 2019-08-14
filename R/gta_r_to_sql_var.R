# Roxygen documentation

#' A little function that translates the GTA's naming rules into SQL. Here for variable/vector object.
#'
#' Returns renamed vector.
#'
#' @param convert.var Name of the vector you want to convert (in quotes).
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert

gta_r_to_sql_var <- function(convert.var=NULL,
                             table.prefix=NULL) {
  
  if(is.null(convert.var)){
    stop("No variables provided for 'convert.var'.")
  }
  
  ## add to prefix
  if(is.null(table.prefix)){
    
    return(paste(session.prefix,gsub("\\.","_",convert.var),sep=""))
    
  } else {
    
    translated.vars=character()
    for(i in length(convert.var)){
      
      if(is.na(table.prefix[i])){
        t.p=session.prefix
      }else {
        t.p=table.prefix[i]
      }
      
      translated.vars=c(translated.vars, 
                        paste(t.p,gsub("\\.","_",convert.var[i]),sep=""))
      rm(t.p)
    }
    return(translated.vars)
    
    
  }
  
}


