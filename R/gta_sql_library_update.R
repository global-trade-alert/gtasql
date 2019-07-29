# Roxygen documentation

#' Get the latest version from GitHub.
#'
#' Syncs your local library with our latest GTA GitHub release.
#'
#' @return Be up to date with our latest functions.
#' @references www.globaltradealert.org
#' @author Global Trade Alert

gta_sql_library_update = function(x){
  devtools::install_github("global-trade-alert/gtasql", force=T)
  library("gtasql")
  
  print("You are up to date.")
}
