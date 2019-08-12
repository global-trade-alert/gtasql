# Roxygen documentation

#' Append an existing table in your SQL database
#'
#' Upload data frame into your database
#'
#' @param append.table The name of the table you want to append to. Understands GTA and SQL naming convetion (e.g. 'master.frame' or 'master_frame')
#' @param append.by.df A string with the name of the data frame you want to add to the SQL table. It's permissable to enter parts e.g. 'my.df[last.row,]'.
#' @param db.connection Specify the database connection you want to use. Default is 'pool'.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert


gta_sql_append_table <- function(append.table=NULL,
                                 append.by.df=NULL,
                                 table.prefix=NULL,
                                 db.connection="pool") {
  
  ## importing what you want to send
  eval(parse(text=paste("sql.df=",append.by.df,sep="")))
  names(sql.df)=gsub('\\.','_',names(sql.df))
  sql.df=as.data.frame(sql.df)
  
  sql.table=gta_r_to_sql_var(convert.var=append.table,
                             table.prefix=table.prefix)
  
  
  ## checking whether there is an auto-incrementing primary key in the table
  if(db.connection=="pool"){
    
    if(is.null(table.prefix)){
      
      key.table=paste(session.prefix,"r_key_type",sep="")
      
    } else{
      
      if(nchar(table.prefix)>0 & stringr::str_detect(table.prefix, "_$", negate=T)){
        stop("The table.prefix has to end with an underscore '_'.")
      }
      
      key.table=paste(table.prefix,"r_key_type",sep="")
      
    }
    
    
    key.type <- dbGetQuery(pool, paste("SELECT * FROM ",key.table," WHERE data_frame='",sql.table,"';", sep=""))
  } else {
    stop("get the connection written up in source code")
  }
  
  
  ## if so, remove those columns
  if(nrow(subset(key.type, increment_style=="auto"))>0){
    remove.cols=unique(subset(key.type, increment_style=="auto")$column_name)
    
    for(remove.it in remove.cols){
      eval(parse(text=paste("sql.df$",remove.it,"=NULL",sep="")))
    }
    
  } 
  
  if(db.connection=="pool"){
    dbWriteTable(conn = pool, name = sql.table, value = sql.df, row.names=F, append=T)
  } else {
    stop("get the connection written up in source code")
  }
 


}

