# Roxygen documentation

#' Record the classes of Rstudio's data frames when you upload them.
#'
#'  Note that the default connects you to ricardo-dev. Add “ricardo” as db.title’s value if you want to connect to the production database.

#' @param db.title Name of the connection in the GTA's key chain (or as recorded in this function).
#' @param db.name connection info.
#' @param db.name connection info.
#' @param db.user connection info.
#' @param db.password connection info.
#' @param db.host connection info.
#' @param table.prefix Specify the default SQL table prefix for this session. Can be empty (''). All non-empty values have to end in an underscore ('_').
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert


gta_sql_pool_open <- function(db.title=NULL,
                              db.name=NULL,
                              db.user=NULL,
                              db.password=NULL,
                              db.host=NULL,
                              table.prefix=NULL) {
  
  if(is.null(table.prefix)){
    stop("Please set the default table.prefix for this session (e.g. 'hs_').")
  } else {
    
    if(nchar(table.prefix)>0 & stringr::str_detect(table.prefix, "_$", negate=T)){
      
      stop("The table.prefix has to end with an underscore '_'.")
      
    } else {
      
      assign("session.prefix",table.prefix, envir=.GlobalEnv)
    }
    
  }
  
  
  if(is.null(db.title)){
    
    # db.keys<<-gta_pwd("ricardodev")
    # 
    # pool <<- pool::dbPool(
    #   drv = RMySQL::MySQL(),
    #   dbname = 'ricardodev',
    #   host = "gta-ricardo-dev.cp7esvs8xwum.eu-west-1.rds.amazonaws.com",
    #   username = db.keys$user,
    #   password = db.keys$password,
    #   idleTimeout = 3
    # )
    
    source("setup/keys/rdev.R")

    
    
  } else {
    
    if(db.title=="ricardo"){
      
      # 
      # db.keys<<-gta_pwd("ricardomain")
      # 
      # pool <<- pool::dbPool(
      #   drv = RMySQL::MySQL(),
      #   dbname = 'ricardo',
      #   host = "gta-ricardo-main.cp7esvs8xwum.eu-west-1.rds.amazonaws.com",
      #   username = db.keys$user,
      #   password = db.keys$password
      # )
      
      source("setup/keys/ric.R")
      
    }
    
    if(is.null(db.name)){
      
      pool <<- pool::dbPool(
        drv = RMySQL::MySQL(),
        host = db.host,
        username = db.user,
        password = db.password
      )

    } else {
      
      pool <<- pool::dbPool(
        drv = RMySQL::MySQL(),
        host = db.host,
        username = db.user,
        password = db.password,
        dbname=db.name
      )
    
    }
    
    
  }
}


gta_sql_pool_close <- function(){
  poolClose(pool)
}

