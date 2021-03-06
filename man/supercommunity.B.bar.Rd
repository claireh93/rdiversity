% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diversity-measures.R
\name{supercommunity.B.bar}
\alias{supercommunity.B.bar}
\title{Similarity-sensitive normalised supercommunity.B diversity}
\usage{
supercommunity.B.bar(super, qs)
}
\arguments{
\item{super}{object of class \code{supercommunity}}

\item{qs}{\code{vector} of \emph{q} values}
}
\value{
Returns a two-dimensional \code{matrix} of diversities, with columns  
representing subcommunities and rows representing values of \emph{q}.
}
\description{
\code{supercommunity.B.bar()} is used to calculate the 
effective number of distinct subcommunities.
}
\details{
\code{supercommunity.B.bar()} calculates the total normalised supercommunity 
beta diversity of a series of columns representing independent subcommunity 
counts relative to the supercommunity as a whole (by default the sum of the 
subcommunities). This measure may be calculated for a series of orders, 
repesented as a vector of \code{qs}.
}
\examples{
pop <- sample(1:50, 5)
super <- supercommunity(pop)

# Calculate normalised supercommunity beta diversity (takes the relative 
# entropy)
a <- betabar(super)
superdiv(a, 0:2)

}

