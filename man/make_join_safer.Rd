% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/joins.r
\name{make_join_safer}
\alias{make_join_safer}
\title{DEPRECATED: Make a join function safer.}
\usage{
make_join_safer(join_fn, fast = TRUE)
}
\arguments{
\item{join_fn}{Original join function to wrap}

\item{fast}{Boolean, should the check be fast or thorough (defaults to fast)}
}
\value{
A new join function

The new join function will have arguments \code{x}, \code{y}, \code{by}, \code{...} and
\code{allow_cartesian = FALSE}. The goal here is to emulate \code{\link[=merge]{merge()}}, which will
raise an error if rows aren't uniquely identified. Instead, dplyr follows SQL
and does a cartesian join if rows aren't unique.

Additionally, the \code{by} variables are required. They cannot be implicit.
The \code{...} arguments are passed to the dplyr join function.

If fast == TRUE, do the join, then check that the number of rows is not
greater than the sum of the row counts of the input tables.
If fast == FALSE, make sure that the by variables uniquely identify rows in
at least one of the tables before doing the join.
}
\description{
DEPRECATED: Make a join function safer.
}
\examples{
\dontrun{
nrow(mtcars)  # 32
nrow(dplyr::inner_join(mtcars, mtcars, by = 'cyl'))  # 366
inner_join <- make_join_safer(dplyr::inner_join)
inner_join(mtcars, mtcars, by = 'cyl')  # error
}
}
\seealso{
\code{\link[=merge]{merge()}}, \code{\link[dplyr:mutate-joins]{dplyr::inner_join()}}
}
