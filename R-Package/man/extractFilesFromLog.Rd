% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/specs.R
\name{extractFilesFromLog}
\alias{extractFilesFromLog}
\title{extractFilesFromLog takes the lines of an osc build log and
extrats the list of unpackaged files. Times must be stripped
already.}
\usage{
extractFilesFromLog(buildlog, packname, systemRlib = "/usr/lib64/R/library/")
}
\arguments{
\item{buildlog}{Character vector containing the lines of an osc
build log}
}
\value{
Character vector containing entries for the files section,
}
\description{
extractFilesFromLog takes the lines of an osc build log and
extrats the list of unpackaged files. Times must be stripped
already.
}
