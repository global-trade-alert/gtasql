# Roxygen documentation

#' Create a new table in your SQL database.
#'
#' Creates a data table with the name of your dataframe. Uploads the data frame thereafter.
#' @param write.df The name of the data frame (in quotes) you want to add.
#' @param create.primary.key Name of the column that is the primary key. Can only be one. GTA or SQL naming is fine.
#' @param create.primary.auto.incr Do you want the primary key to be auto incrementing? Default is TRUE.
#' @param create.foreign.key Vector with names of columns that are foreign keys. Can use GTA or SQL naming.
#' @param create.foreign.key.parent On what table is the foreign key the primary key?
#' @param create.foreign.key.del.cascade What happens to the lines of foreign key if a value is deleted from the primary key is deleted. Default is that they are removed as well. 
#' @param db.connection Specify the database connection you want to use. Default is 'pool'.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert

gta_sql_create_table <- function(write.df=NULL,
                                    create.primary.key=NULL,
                                    create.primary.auto.incr=F,
                                    create.foreign.key=NULL,
                                    create.foreign.key.parent=NULL,
                                    create.foreign.key.del.cascade=T,
                                    append.existing=T,
                                    db.connection="pool") {
  
  if(is.null(write.df)){
    stop("No data frame provided for 'write.df'.")
  }
  
  eval(parse(text=paste("sql.df=",write.df,sep="")))
  names(sql.df)=gsub('\\.','_',names(sql.df))
  sql.name=gsub("\\.","_",write.df)
  sql.df=as.data.frame(sql.df)
  
  if(db.connection=="pool"){
    dbWriteTable(conn = pool, name = sql.name, value = sql.df, row.names=F, append=append.existing)
  } else {
    stop("get the connection written up in source code")
  }
  
  gta_sql_coltype_record(write.df)
  
  if((is.null(create.primary.key)==F | is.null(create.foreign.key)==F)){
    
    db.keys <<- poolCheckout(pool)
    dbSendQuery(db.keys,paste("TRUNCATE TABLE ",sql.name, sep=""))
    
    gta_sql_set_table_keys(table.name=sql.name,
                              primary.key=create.primary.key,
                              primary.auto.incr=create.primary.auto.incr,
                              foreign.key=create.foreign.key,
                              foreign.key.parent=create.foreign.key.parent,
                              foreign.key.del.cascade=create.foreign.key.del.cascade,
                              db.connection="db.keys")
    
    gta_sql_create_table(write.df=write.df, append.existing=T,
                            create.primary.key=NULL,
                            create.foreign.key=NULL)
    poolReturn(db.keys)
    
  }


}

