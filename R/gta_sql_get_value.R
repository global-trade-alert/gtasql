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
                              db.connection="pool",
                              leak.proof=T) {
  
  nr.connections.start=gta_sql_count_connections()

   if(db.connection=="pool"){
    
    conn.update=poolCheckout(pool)
    sought.value=dbGetQuery(conn.update, query)
    poolReturn(conn.update)
    
    if(nrow(sought.value)==0){
      
      if(ncol(sought.value)==1){
        my.value=NA
      } else {
        names(sought.value)=gsub("_","\\.",names(sought.value))
        my.value=sought.value
      }
      
      
    } else {
      
      if(nrow(sought.value)==1 & ncol(sought.value)==1){
        my.value=sought.value[1,1]
      } else if (ncol(sought.value)==1){
        my.value=as.vector(sought.value[,1])
      } else {
        names(sought.value)=gsub("_","\\.",names(sought.value))
        my.value=sought.value
      }
    }
    
  } else {
    if(leak.proof){
      gta_sql_kill_connections(nr.connections.start)
    }
    stop("get the connection written up in source code")
  }
  
  if(leak.proof){
    gta_sql_kill_connections(nr.connections.start)
  }
  
  return(my.value)
}

