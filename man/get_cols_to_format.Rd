% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{get_cols_to_format}
\alias{get_cols_to_format}
\title{Find columns matching a pattern}
\usage{
get_cols_to_format(data, pattern)
}
\arguments{
\item{data}{a data frame}

\item{pattern}{a regular expression specifying column names to match}
}
\value{
a vector of column names
}
\description{
Find columns matching a pattern
}
\examples{
get_cols_to_format(iris, pattern = "Length|Width")
}
