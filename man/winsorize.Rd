% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/winsorize.r
\name{winsorize}
\alias{winsorize}
\title{Winsorize one vector}
\usage{
winsorize(x, trim = 0.01, na.rm = FALSE)
}
\arguments{
\item{x}{Vector to winsorize (numeric)}

\item{trim}{Fraction of data to trim (if length == 1, same at top and bottom,
if length == 2, trim different fractions)}

\item{na.rm}{Remove NAs? Fails if NAs are present and this argument is FALSE}
}
\value{
x, but winsorized

(this version deals with point masses!)
}
\description{
Winsorize one vector
}
