# Roxygen documentation

#' A little function that translates the GTA's naming rules into SQL. Here for data frame (names and column names).
#'
#' Returns renamed dataframe.
#'
#' @param convert.df Name of the data frame you want to convert (in quotes).
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert

gta_r_to_sql_df <- function(convert.df=NULL) {
  
  if(is.null(convert.df)){
    stop("No data frame provided for 'convert.df'.")
  }
  
  eval(parse(text=paste("names(convert.df)=gsub('\\.','_',names(convert.df))",sep="")))
  eval(parse(text=paste(gsub("\\.","_",convert.df),"=convert.df",sep="")))
  
  return(eval(parse(text=paste(gsub("\\.","_",convert.df),sep=""))))
}

