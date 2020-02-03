# Roxygen documentation

#' Create a table that collects the keytypes
#'
#' @param db.connection Specify the database connection you want to use. Default is 'pool'.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert
#' 

gta_sql_keytype_initialise <- function(db.connection="pool",
                                       table.prefix=NULL) {
  
  
  if(is.null(table.prefix)){
    
    key.table=paste(session.prefix,"r_key_type",sep="")
    
  } else{
    
    if(nchar(table.prefix)>0 & stringr::str_detect(table.prefix, "_$", negate=T)){
      stop("The table.prefix has to end with an underscore '_'.")
    }
    
    key.table=paste(table.prefix,"r_key_type",sep="")
    
  }
 
  # Construct the fetching query
  init.query=paste("CREATE TABLE ",key.table," (
                   data_frame text,
                   column_name text,
                   key_type text,
                   increment_style text
  );",sep="")
  
  
  ## loading table
  
  eval(parse(text=paste0("db.init.key=poolCheckout(",db.connection,")"))) 
  
  dbSendQuery(db.init.key,init.query)
  poolReturn(db.init.key)
  

}

