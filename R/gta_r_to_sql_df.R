# Roxygen documentation

#' A little function that translates the GTA's naming rules into SQL. Here for data frame (names and column names).
#'
#' Returns renamed dataframe.
#'
#' @param convert.df Name of the data frame you want to convert (in quotes).
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert

gta_r_to_sql_df <- function(convert.df=NULL,
                            table.prefix=NULL) {
  
  if(is.null(convert.df)){
    stop("No data frame provided for 'convert.df'.")
  }
  
  eval(parse(text=paste("names(convert.df)=gsub('\\.','_',names(convert.df))",sep="")))
  
  if(is.null(table.prefix)){
    
    eval(parse(text=paste(paste(session.prefix,gsub("\\.","_",convert.df),sep=""),"=convert.df",sep="")))
    
    return(eval(parse(text=paste(paste(session.prefix,gsub("\\.","_",convert.df),sep=""),sep=""))))
    
  } else {
    
    if(nchar(table.prefix)>0 & stringr::str_detect(table.prefix, "_$", negate=T)){
      stop("The table.prefix has to end with an underscore '_'.")
    }
   
    eval(parse(text=paste(paste(table.prefix,gsub("\\.","_",convert.df),sep=""),"=convert.df",sep="")))
    
    return(eval(parse(text=paste(paste(table.prefix,gsub("\\.","_",convert.df),sep=""),sep=""))))
     
  }
  
}

