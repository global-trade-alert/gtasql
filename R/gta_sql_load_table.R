# Roxygen documentation

#' Load a table from the SQL database into your local environment.
#'
#' @param load.table The name of the table you want to load. SQL or GTA naming is fine.
#' @param db.connection Specify the database connection you want to use. Default is 'pool'.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert

gta_sql_load_table <- function(load.table=NULL,
                               db.connection="pool") {
  
  
  if(is.null(load.table)){
    stop("Specify a table to load.")
  }
  
  ## adjusting the table name
  sql.table=gsub("\\.","_",load.table)
  
  # Construct the fetching query
  sql <- paste("SELECT * FROM", sql.table)
  query <- sqlInterpolate(pool, sql)
  
  ## flexible version for later
  # sql <- "SELECT * FROM City WHERE ID = ?id1 OR ID = ?id2 OR ID = ?id3;"
  # query <- sqlInterpolate(conn, sql, id1 = input$ID1,
  #                         id2 = input$ID2, id3 = input$ID3)

  ## loading table
  if(db.connection=="pool"){
    column.type <- dbGetQuery(pool, paste("SELECT * FROM r_column_type WHERE data_frame='",sql.table,"';", sep=""))
    sql.data <- dbGetQuery(pool, query)
  } else {
    stop("get the connection written up in source code")
  }
  
  
  ## checking column.types
  
  ## dates
  if(any(grepl("date", column.type$column_type, ignore.case = T))){
    change.cols=unique(column.type$column_name[grepl("date", column.type$column_type, ignore.case = T)])
    
    for(change.it in change.cols){
      eval(parse(text=paste("sql.data$",change.it,"=as.Date(sql.data$",change.it,",'%Y-%m-%d', origin='1970-01-01')",sep="")))
    }
    rm(change.cols, change.it)
  }
  
  if(any(grepl("posix", column.type$column_type, ignore.case = T))){
    
    change.cols=unique(column.type$column_name[grepl("posix", column.type$column_type, ignore.case = T)])
    
    for(change.it in change.cols){
      eval(parse(text=paste("sql.data$",change.it,"=as.POSIXct(sql.data$",change.it,", origin='1970-01-01')",sep="")))
    }
    
    rm(change.cols, change.it)
  }
  
  # logicals
  if(any(grepl("logical", column.type$column_type, ignore.case = T))){
    change.cols=unique(column.type$column_name[grepl("logical", column.type$column_type, ignore.case = T)])
    
    for(change.it in change.cols){
      eval(parse(text=paste("sql.data$",change.it,"=sql.data$",change.it,"==0",sep="")))
    }
    rm(change.cols, change.it)
  }
  
  # character
  if(any(grepl("character", column.type$column_type, ignore.case = T))){
    change.cols=unique(column.type$column_name[grepl("character", column.type$column_type, ignore.case = T)])
    
    for(change.it in change.cols){
      eval(parse(text=paste("sql.data$",change.it,"=as.character(sql.data$",change.it,")",sep="")))
    }
    rm(change.cols, change.it)
  }
  
  # factor
  if(any(grepl("factor", column.type$column_type, ignore.case = T))){
    change.cols=unique(column.type$column_name[grepl("factor", column.type$column_type, ignore.case = T)])
    
    for(change.it in change.cols){
      eval(parse(text=paste("sql.data$",change.it,"=as.factor(as.character(sql.data$",change.it,"))",sep="")))
    }
    rm(change.cols, change.it)
  }
  
  
  # numeric/integer
  if(any(grepl("numeric|integer", column.type$column_type, ignore.case = T))){
    change.cols=unique(column.type$column_name[grepl("numeric|integer", column.type$column_type, ignore.case = T)])
    
    for(change.it in change.cols){
      eval(parse(text=paste("sql.data$",change.it,"=as.numeric(as.character(sql.data$",change.it,"))",sep="")))
    }
    rm(change.cols, change.it)
  }
  
  
  
  ## adjusting table names
  names(sql.data)=gsub("_","\\.",names(sql.data))
  
  return(sql.data)
}

