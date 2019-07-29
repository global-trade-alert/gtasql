# Roxygen documentation

#' Create a table that collects the classes of Rstudio's data frames when you upload them.
#'
#' The columns will be automatically converted back into those classes when you download them from the SQL database.
#'
#' @param db.connection Specify the database connection you want to use. Default is 'pool'.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert

gta_sql_coltype_initialise <- function(db.connection="pool") {
  
  # Construct the fetching query
  init.query="CREATE TABLE r_column_type (
    data_frame text,
    column_name text,
    column_type text
);"
  
  ## loading table
  if(db.connection=="pool"){
    db.init <<- poolCheckout(pool)
    dbSendQuery(db.init,init.query)
    poolReturn(db.init)

  } else {
    stop("get the connection written up in source code")
  }
}

