# Roxygen documentation

#' Update a specific portion of an existing table in your SQL database
#'
#' Just a wrapper to make the connection easier. You have to type out the query in SQL, no help here.
#'
#' @param query A string with the SQL query you want to execute.
#' @param db.connection Specify the database connection you want to use. Default is 'pool'.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert

gta_sql_update_table <- function(query=NULL,
                                 db.connection="pool",
                                 leak.proof=T) {
  
  nr.connections.start=gta_sql_count_connections()

   if(db.connection=="pool"){
    
    conn.update=poolCheckout(pool)
    dbSendQuery(conn.update, query)
    poolReturn(conn.update)
    
  } else {
    stop("get the connection written up in source code")
  }
  
  if(leak.proof){
    gta_sql_kill_connections(nr.connections.start)
  }
}

