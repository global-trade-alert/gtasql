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
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert


gta_sql_pool_open <- function(db.title=NULL,
                              db.name=NULL,
                              db.user=NULL,
                              db.password=NULL,
                              db.host=NULL) {
  
  if(is.null(db.title)){
    
    pool <<- dbPool(
      drv = RMySQL::MySQL(),
      dbname = 'ricardodev',
      host = "gta-rstudio-dev.cp7esvs8xwum.eu-west-1.rds.amazonaws.com",
      username = keyring::key_list("ricardo-dev")[1,2],
      password = keyring::key_get("ricardo-dev","gtaricardodev")
    )
    
    
    
  } else {
    
    if(db.title=="ricardo"){
      
      pool <<- dbPool(
        drv = RMySQL::MySQL(),
        dbname = 'ricardo',
        host = "gta-ricardo.cp7esvs8xwum.eu-west-1.rds.amazonaws.com",
        username = keyring::key_list("ricardo-aws")[1,2],
        password = keyring::key_get("ricardo-aws","gtaricardomaster")
      )
      
    }
    
    if(is.null(db.name)){
      
      pool <<- dbPool(
        drv = RMySQL::MySQL(),
        host = db.host,
        username = db.user,
        password = db.password
      )

    } else {
      
      pool <<- dbPool(
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

