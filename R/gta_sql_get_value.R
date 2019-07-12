# Roxygen documentation

#' Get a value from the SQL database. 
#'
#' Just a wrapper, really. Returns a scalar or a data frame, depending on how many results you have.
#' 
#' @param query Your query in SQL language (incl SQL naming!).
#' @param db.connection Specify the database connection you want to use. Default is 'pool'.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert

gta_sql_get_value <- function(query=NULL,
                              db.connection="pool") {

   if(db.connection=="pool"){
    
    conn.update=poolCheckout(pool)
    sought.value=dbGetQuery(conn.update, query)
    poolReturn(conn.update)
    
    if(nrow(sought.value)==1 & ncol(sought.value)==1){
      return(sought.value[1,1])
    } else {
      names(sought.value)=gsub("_","\\.",names(sought.value))
      return(sought.value)
    }
    
  } else {
    stop("get the connection written up in source code")
  }
}
