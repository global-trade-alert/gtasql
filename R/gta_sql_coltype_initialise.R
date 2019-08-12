# Roxygen documentation

#' Create a table that collects the classes of Rstudio's data frames when you upload them.
#'
#' The columns will be automatically converted back into those classes when you download them from the SQL database.
#'
#' @param db.connection Specify the database connection you want to use. Default is 'pool'.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert

gta_sql_coltype_initialise <- function(db.connection="pool",
                                       table.prefix=NULL) {
  
  if(is.null(table.prefix)){
    
    col.table=paste(session.prefix,"r_column_type",sep="")
    
  } else{
    
    if(nchar(table.prefix)>0 & stringr::str_detect(table.prefix, "_$", negate=T)){
      stop("The table.prefix has to end with an underscore '_'.")
    }
    
    col.table=paste(table.prefix,"r_column_type",sep="")
    
  }
  
  # Construct the fetching query
  init.query=paste("CREATE TABLE ",col.table," (
    data_frame text,
    column_name text,
    column_type text
);", sep="")
  
  ## loading table
  if(db.connection=="pool"){
    db.init <<- poolCheckout(pool)
    dbSendQuery(db.init,init.query)
    poolReturn(db.init)

  } else {
    stop("get the connection written up in source code")
  }
}

