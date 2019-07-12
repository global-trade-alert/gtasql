# Roxygen documentation

#' Record the classes of Rstudio's data frames when you upload them.
#'
#' The columns will be automatically converted back into those classes when you download them from the SQL database.
#' @param record.frame The name of the data frame (in quotes) whose column classes you want to record.
#' @param db.connection Specify the database connection you want to use. Default is 'pool'.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert


gta_sql_coltype_record <- function(record.frame=NULL,
                                   db.connection="pool") {
  
  sql.df=gsub("\\.","_", record.frame)
  
  
  eval(parse(text=paste("col.types=sapply(",record.frame,", class)",sep="")))
  
  r.column.type<<-data.frame(data_frame=sql.df,
                       column_name=gsub("\\.","_",names(col.types)),
                       column_type=as.character(lapply(col.types, function(x)x[[1]])))
  
  del.query=paste("DELETE FROM r_column_type WHERE data_frame='",sql.df,"';",sep="")
  
  
  ## loading table
  if(db.connection=="pool"){
    db.ct <<- poolCheckout(pool)
    dbSendQuery(db.ct,del.query)
    
    dbWriteTable(conn = db.ct, name = "r_column_type", value = r.column.type, row.names=F, append=T)
    
    poolReturn(db.ct)
    
  } else {
    stop("get the connection written up in source code")
  }
}
