# Roxygen documentation

#' Kill all open connections to a remote database.
#'
#' Out they go.

#' @references www.globaltradealert.org
#' @author Global Trade Alert


gta_sql_kill_connections <- function(keep.x.first.connections=NULL) {
  ## baed on https://stackoverflow.com/users/2610541/thankgoat at https://stackoverflow.com/questions/32139596
  library(RMySQL)
  
  all_cons <- dbListConnections(MySQL())
  
  
  if(is.null(keep.x.first.connections)){
    for(con in all_cons)
      +  dbDisconnect(con)
    
  } else {
    for(i in keep.x.first.connections:length(all_cons)){
      
      dbDisconnect(dbListConnections(MySQL())[[i]])
      
    }
  }
  
  
  
}

