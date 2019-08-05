# Roxygen documentation

#' Remove all data and tables from the existing database.
#'
#' 
#' Handle with great care.
#'
#' @param db.connection Specify the database connection you want to use. Default is 'pool'.
#' @param db.name Name of the database that you want to flush.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert

gta_sql_flush_database <- function(db.connection="pool",
                                    db.name=NULL) {

   if(db.connection=="pool"){
     
     if(is.null(db.name)==F){
       
       are.you.sure <- readline(prompt=paste("To confirm, please re-type the name of the database you want to flush: "))
       
       if(are.you.sure==db.name){
         conn.update=pool::poolCheckout(pool)
         RMySQL::dbSendQuery(conn.update, paste("DROP DATABASE", db.name))
         pool::poolReturn(conn.update)
         
         print("The database is empty now.")
         
       } else {
         print("Flushing aborted because of name difference.")
       }
       
     }
    
    
    
  } else {
    stop("get the connection written up in source code")
  }
  
  
}

