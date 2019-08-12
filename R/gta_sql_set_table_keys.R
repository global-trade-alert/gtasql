# Roxygen documentation

#' A wrapper function that sets the key types in your SQL table.
#' 
#' @param db.connection Specify the database connection you want to use. Default is 'pool'.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert
#' 

gta_sql_set_table_keys <- function(table.name=NULL,
                                   primary.key=NULL,
                                   primary.auto.incr=F,
                                   foreign.key=NULL,
                                   foreign.key.parent=NULL,
                                   foreign.key.del.cascade=T,
                                   db.connection="pool",
                                   table.prefix=NULL) {
  

  
  ## opening a connection
  if(db.connection=="pool"){
    eval(parse(text=paste("conn=poolCheckout(",db.connection,")",sep="")))
  } else {
    eval(parse(text=paste("conn=",db.connection,"",sep="")))
  }
  
  
  # ALTER TABLE Orders
  # ADD PRIMARY KEY (OrderID)
  # ADD FOREIGN KEY (PersonID) REFERENCES Persons(PersonID);

  ## alter table
  if(is.null(table.name)){
    stop("Please specify a table.")
  } else {
    table.name=gta_r_to_sql_var(table.name,
                                table.prefix=table.prefix)
    
    alter.table=paste("ALTER TABLE ", table.name,sep="")
  }
  
 
    ## primary key
  if(is.null(primary.key)==F){
    primary.key=gta_r_to_sql_var(primary.key,
                                 table.prefix="")
    
    
    primary.part=paste(paste(" CHANGE COLUMN",
                      primary.key,
                      primary.key,
                      "INT,",
                      sep=" "),
                      paste(" ADD PRIMARY KEY (", 
                      paste(primary.key, collapse=", "),
                      ")",
                      sep=""), collapse="")
    
    query=paste(paste(alter.table, primary.part, sep=""), sep="; ")
    dbSendQuery(conn,query)
    rm(query)
    
    
    ## AUTO INCREMENT
    if(primary.auto.incr){
      
      auto.increment=alter.table
      
      if(primary.auto.incr){
        auto.increment=paste(auto.increment,
                             "CHANGE COLUMN",
                             primary.key,
                             primary.key,
                             "INT AUTO_INCREMENT", sep=" ")
      }
      
      
      auto.increment=paste(auto.increment, ";",sep="")
      
      dbSendQuery(conn,auto.increment)
      
      ## record key type
      gta_sql_keytype_record(record.frame=table.name,
                             record.column=primary.key,
                             record.type="primary",
                             record.style="auto",
                             table.prefix=table.prefix)
      
    } else {
      
      gta_sql_keytype_record(record.frame=table.name,
                             record.column=primary.key,
                             record.type="primary",
                             record.style="none",
                             table.prefix=table.prefix)
    } 

  }
      
  
  ## foreign key
  if(is.null(foreign.key)==F){
    foreign.key=gta_r_to_sql_var(foreign.key,
                                 table.prefix="")
    foreign.key.parent=gta_r_to_sql_var(foreign.key.parent,
                                        table.prefix="")
    
    for(i in 1:length(foreign.key)){
      
      set.to.int=paste(" CHANGE COLUMN",
                       foreign.key[i],
                       foreign.key[i],
                       "INT",
                       sep=" ")
      
      query=paste(alter.table, set.to.int,";", sep="")
      dbSendQuery(conn,query)
      
      if(foreign.key.del.cascade){
        foreign.part=paste(" ADD FOREIGN KEY (",foreign.key[i],") REFERENCES ", foreign.key.parent[i],"(",foreign.key[i],") ON DELETE CASCADE", sep="")
      } else {
        foreign.part=paste(" ADD FOREIGN KEY (",foreign.key[i],") REFERENCES ", foreign.key.parent[i],"(",foreign.key[i],") ON DELETE SET NULL", sep="")
      }
      
      
      query=paste(alter.table, foreign.part,";", sep="")
      dbSendQuery(conn,query)
      rm(query)
      
      gta_sql_keytype_record(record.frame=table.name,
                             record.column=foreign.key[i],
                             record.type="foreign",
                             record.style="none",
                             table.prefix=table.prefix)
      
    }

    
  } 

 
  
  
  ## closing connection
  if(db.connection=="pool"){
    poolReturn(conn)
    }
}

