% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/desc2spec.R
\name{dropFileSection}
\alias{dropFileSection}
\title{dropFileSection removes the filelist from a character array
representing a spec file. Keep the first line of the file list.}
\usage{
dropFileSection(speclines)
}
\arguments{
\item{speclines}{character vector with lines holding a specfile}
}
\value{
character vector like the input minus file list
}
\description{
dropFileSection removes the filelist from a character array
representing a spec file. Keep the first line of the file list.
}