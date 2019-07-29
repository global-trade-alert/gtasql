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
                                 db.connection="pool") {
  
  ## importing what you want to send
  eval(parse(text=paste("sql.df=",append.by.df,sep="")))
  names(sql.df)=gsub('\\.','_',names(sql.df))
  
  sql.table=gsub("\\.","_",append.table)
  sql.df=as.data.frame(sql.df)
  
  
  ## checking whether there is an auto-incrementing primary key in the table
  if(db.connection=="pool"){
    key.type <- dbGetQuery(pool, paste("SELECT * FROM r_key_type WHERE data_frame='",sql.table,"';", sep=""))
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

