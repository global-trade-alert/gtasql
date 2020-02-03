# Roxygen documentation

#' Run an SQL script or multiple SQL queries
#'
#' Run an entire SQL script or multiple SQL queries from within R 
#'
#' @param query.string Specify the input query
#' @param output.queries Specify in a vector, which queries contain outputs to be extracted, i.e. c(1,5) will return the results from queries 1 and 5.
#' @param db.connection Specify the database connection you want to use. Default is 'pool'.
#' @param show.time Show execution duration per query.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert

gta_sql_multiple_queries=function(query.string, output.queries, db.connection="pool", show.time=F){
  library(stringr)
  
  queries=gsub('\n',' ',unlist(strsplit(query.string,';')))
  queries=queries[str_trim(queries)!='']
  queries=gsub("/\\*.*/\\**", "", queries)
  
  if(!is.character(query.string)){stop('The query must be provided as a string')}
  if (!all(output.queries %in% 1:length(queries) | is.na(output.queries))){stop('The desired output is not in bounds of the queries provided')}
  
  
  if (length(output.queries)>1){output=list()}
  
  for(i in 1:length(queries)){
    if(show.time){
      t=Sys.time()
      print("______________________________________________________")
      print(paste("Began query",i,"of",length(queries)))
      print(paste("Starts with ", substr(queries[i],1,100),"...",sep=""))
      }
    result=gta_sql_get_value(query=paste0(queries[i],';'),db.connection = db.connection)
    
    if(show.time){
      
      print(paste("Finished query",i))
      print(round(Sys.time()-t,2))
      print("______________________________________________________")
    }
    
    if (length(output.queries)>1 & i %in% output.queries){
      output[[length(output)+1]]=result
    } else if (length(output.queries)==1 & i==output.queries) {
      output=result
    }
  }
  
  return(output)
}

