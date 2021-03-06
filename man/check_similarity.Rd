% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_similarity.R
\name{check_similarity}
\alias{check_similarity}
\title{Check similarity}
\usage{
check_similarity(partition, similarity)
}
\arguments{
\item{partition}{two-dimensional \code{matrix} of mode \code{numeric}; 
contains relative abundance of types}

\item{similarity}{two-dimensinal \code{matrix} of mode \code{numeric}; 
contains pair-wise similarity between types}
}
\value{
Returns a two-dimensions \code{matrix} of mode \code{numeric}. If 
the similarity matrix was valid, this should be identical to that which was
input as an argument.
}
\description{
\code{check_similarity()} is used to validate similarity matrices.
}
\examples{
population <- data.frame(a = 1:2, b = 3:4)
zmatrix <- matrix(rep(-1,4), 2)
\dontrun{check_similarity(population, zmatrix)}

}

