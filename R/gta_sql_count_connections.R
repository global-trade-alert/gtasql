# Roxygen documentation

#' Count all open connections to a remote database.
#'
#' Here you go.

#' @references www.globaltradealert.org
#' @author Global Trade Alert


gta_sql_count_connections <- function() {
  ## based on https://stackoverflow.com/users/2610541/thankgoat at https://stackoverflow.com/questions/32139596
  library(RMySQL)
  
  all_cons <- dbListConnections(MySQL())
  
  return(length(all_cons))
  
}

