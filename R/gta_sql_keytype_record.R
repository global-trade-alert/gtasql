# Roxygen documentation

#' Record the key in the data frame your uploading
#'
#' The columns will be automatically converted back into those classes when you download them from the SQL database.
#' @param record.frame The name of the data frame (in quotes) whose column classes you want to record.
#' @param db.connection Specify the database connection you want to use. Default is 'pool'.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert



gta_sql_keytype_record <- function(record.frame=NULL,
                                   record.column=NULL,
                                   record.type=NULL,
                                   record.style=NULL,
                                   db.connection="pool") {
  
  sql.df=gsub("\\.","_", record.frame)
  sql.col=gsub("\\.","_", record.column)
  
  
  del.query=paste("DELETE FROM r_key_type WHERE data_frame='",sql.df,"' and column_name='",sql.col,"';",sep="")  
  
  ## loading table
  if(db.connection=="pool"){
    db.keytype <<- poolCheckout(pool)
    dbSendQuery(db.keytype,del.query)
    
    dbWriteTable(conn = db.keytype,
                 name = "r_key_type", 
                 value = data.frame(data_frame=sql.df,
                                    column_name=sql.col,
                                    key_type=record.type,
                                    increment_style=record.style,
                                    stringsAsFactors = F), 
                 row.names=F, 
                 append=T)
    
    poolReturn(db.keytype)
    
  } else {
    stop("get the connection written up in source code")
  }
}

