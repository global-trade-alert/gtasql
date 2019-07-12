# Roxygen documentation

#' Create a table that collects the keytypes
#'
#' @param db.connection Specify the database connection you want to use. Default is 'pool'.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert
#' 

gta_sql_keytype_initialise <- function(db.connection="pool") {
  
  # Construct the fetching query
  init.query="CREATE TABLE r_key_type (
    data_frame text,
    column_name text,
    key_type text,
    increment_style text
);"
  
  ## loading table
  if(db.connection=="pool"){
    db.init.key <<- poolCheckout(pool)
    dbSendQuery(db.init.key,init.query)
    poolReturn(db.init.key)
 
  } else {
    stop("get the connection written up in source code")
  }
}

