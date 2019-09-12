# Roxygen documentation

#' Run an SQL script or multiple SQL queries
#'
#' Run an entire SQL script or multiple SQL queries from within R 
#'
#' @param query.string Specify the input query
#' @param db.connection Specify the database connection you want to use. Default is 'pool'.
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert

gta_sql_multiple_queries=function(query.string, db.connection='pool'){
  library(stringr)
  
  queries=str_trim(gsub('\n','',unlist(strsplit(query.string,';'))))
  queries=queries[queries!='']
  
  output=list()
  for(i in 1:length(queries)){
    result=gta_sql_get_value(query=paste0(queries[i],';'),db.connection = db.connection)
    if (nrow(result)>0 | length(result)>0){
      output[[length(output)+1]]=result
    }
  }
  
  return(output)
}

