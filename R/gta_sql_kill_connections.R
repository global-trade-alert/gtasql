# Roxygen documentation

#' Kill all open connections to a remote database.
#'
#' Out they go.

#' @references www.globaltradealert.org
#' @author Global Trade Alert


gta_sql_kill_connections <- function() {
  ## taken from https://stackoverflow.com/users/2610541/thankgoat at https://stackoverflow.com/questions/32139596
  library(RMySQL)
  
  all_cons <- dbListConnections(MySQL())
  
  print(all_cons)
  
  for(con in all_cons)
    +  dbDisconnect(con)
  
  print(paste(length(all_cons), " connections killed."))
  
  
}

