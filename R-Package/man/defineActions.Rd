% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/status.R
\name{defineActions}
\alias{defineActions}
\title{defineActions returns a list of a partition in 5 sets of all
packages with status information}
\usage{
defineActions(status = getOption("c2o.status"))
}
\arguments{
\item{status}{dataframe read from datafile or constructed}
}
\value{
list with compoents "retired", "uptodate", "update" and
"totry" , "tried" holding the names of respective packages
}
\description{
defineActions returns a list of a partition in 5 sets of all
packages with status information
}
