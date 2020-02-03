# Roxygen documentation

#' Load a table from the SQL database into your local environment.
#'
#' @param load.table The name of the table you want to load. SQL or GTA naming is fine.
#' @param table.prefix Specify the sql-db prefix for the table.
#' @param condition.sql Add SQL code to the default query ('SELECT * FROM table') e.g. "WHERE x = y". No need to add a semicolon at the end. 
#' @param db.connection Specify the database connection you want to use. Default is 'pool'.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert

gta_sql_load_table <- function(load.table=NULL,
                               table.prefix=NULL,
                               condition.sql=NULL,
                               db.connection="pool"){
  

  
  
  
  if(is.null(load.table)){
    stop("Specify a table to load.")
  }
  
  ## adjusting the table name
  
  sql.table=gta_r_to_sql_var(convert.var=load.table,
                             table.prefix=table.prefix)
  
  # Construct the fetching query
  
  if(is.null(condition.sql)){
     
    sql <- paste("SELECT * FROM", sql.table)
    
  } else{
    sql <- paste0("SELECT * FROM ", sql.table," ", condition.sql, ";")
  }
  
  eval(parse(text=paste0("query <- sqlInterpolate(",db.connection,", sql)")))
  
  ## flexible version for later
  # sql <- "SELECT * FROM City WHERE ID = ?id1 OR ID = ?id2 OR ID = ?id3;"
  # query <- sqlInterpolate(conn, sql, id1 = input$ID1,
  #                         id2 = input$ID2, id3 = input$ID3)

  ## loading table
  
  
  if(is.null(table.prefix)){
    
    col.table=paste(session.prefix,"r_column_type",sep="")
    
  } else{
    
    if(nchar(table.prefix)>0 & stringr::str_detect(table.prefix, "_$", negate=T)){
      stop("The table.prefix has to end with an underscore '_'.")
    }
    
    col.table=paste(table.prefix,"r_column_type",sep="")
    
  }
  
  col.type.query=paste("SELECT * FROM ",col.table," WHERE data_frame='",gsub("\\.","_",load.table),"';", sep="")
  
  eval(parse(text=paste0("column.type <- dbGetQuery(",db.connection,", col.type.query)")))
  eval(parse(text=paste0("sql.data <- dbGetQuery(",db.connection,", query)")))

  
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
      eval(parse(text=paste("sql.data$",change.it,"=sql.data$",change.it,"==1",sep="")))
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

