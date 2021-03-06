% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gta_sql_append_table.R
\name{gta_sql_append_table}
\alias{gta_sql_append_table}
\title{Append an existing table in your SQL database}
\usage{
gta_sql_append_table(append.table = NULL, append.by.df = NULL,
  db.connection = "pool")
}
\arguments{
\item{append.table}{The name of the table you want to append to. Understands GTA and SQL naming convetion (e.g. 'master.frame' or 'master_frame')}

\item{append.by.df}{A string with the name of the data frame you want to add to the SQL table. It's permissable to enter parts e.g. 'my.df[last.row,]'.}

\item{db.connection}{Specify the database connection you want to use. Default is 'pool'.}
}
\description{
Upload data frame into your database
}
\references{
www.globaltradealert.org
}
\author{
Global Trade Alert
}
