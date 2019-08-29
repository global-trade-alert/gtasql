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
                                 get.id=NULL,
                                 db.connection="pool") {
  
  ## importing what you want to send
  eval(parse(text=paste("sql.df=",append.by.df,sep="")))
  names(sql.df)=gsub('\\.','_',names(sql.df))
  sql.df=as.data.frame(sql.df)
  
  sql.table=gta_r_to_sql_var(convert.var=append.table,
                             table.prefix=table.prefix)
  
  
  ## checking whether there is an auto-incrementing primary key in the table
  if(db.connection=="pool"){
    append.a.table=poolCheckout(pool)

  } else {
    append.a.table=db.connection
  }
  
  
  if(is.null(table.prefix)){
      
      key.table=paste(session.prefix,"r_key_type",sep="")
      
    } else{
      
      if(nchar(table.prefix)>0 & stringr::str_detect(table.prefix, "_$", negate=T)){
        stop("The table.prefix has to end with an underscore '_'.")
      }
      
      key.table=paste(table.prefix,"r_key_type",sep="")
      
    }
    
    
  key.type <- dbGetQuery(append.a.table, paste("SELECT * FROM ",key.table," WHERE data_frame='",sql.table,"';", sep=""))
   
  
  
  ## if so, remove those columns
  if(nrow(subset(key.type, increment_style=="auto"))>0){
    remove.cols=unique(subset(key.type, increment_style=="auto")$column_name)
    
    for(remove.it in remove.cols){
      eval(parse(text=paste("sql.df$",remove.it,"=NULL",sep="")))
    }
   
    
  } 
  
  ## inverting logical columns
  col.types=sapply(sql.df, class)
  
  logi.cols=names(col.types)[col.types=="logical"]
  
  if(length(logi.cols)>0){
    
    for(l.col in logi.cols){
      
      eval(parse(text=paste("sql.df$",l.col,"=as.numeric(sql.df$",l.col,")",sep="")))
      
    }
    
  }
  rm(col.types, logi.cols)

  dbWriteTable(conn = append.a.table, name = sql.table, value = sql.df, row.names=F, append=T)
  if(!is.null(get.id)){
    
    
    query <- paste("SELECT MAX(",gsub("\\.","_",get.id),") FROM ",sql.table,";",sep="")
    
    sought.value=dbGetQuery(append.a.table, query)
    
    if(nrow(sought.value)==1 & ncol(sought.value)==1){
      return(sought.value[1,1])
    } else {
      names(sought.value)=gsub("_","\\.",names(sought.value))
      return(sought.value)
    }
    
    
    
  }
  
  if(db.connection=="pool"){
    
    poolReturn(append.a.table)
  } 
}

