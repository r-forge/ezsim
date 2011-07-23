\name{createFormula}
\title{Create Formula From a Vector of Character.}
\usage{createFormula(x, right=TRUE)}
\description{Elements of the vector is evenly distributed to both of the formula. Each element in the formula is seperated by \code{+}.}
\alias{createFormula}
\value{Formula}
\author{TszKin Julian Chan \email{ctszkin@gmail.com}}
\note{For internal use of ezsim. It aims at creating formula for facets in \pkg{ggplot2}.}
\seealso{\code{\link{formula}}}
\arguments{\item{x}{A vector of character}
\item{right}{If there is only one element in \code{x}, should it appear in the left or right hand side of the formula.}}
\examples{createFormula(letters[1])  ## . ~ a
createFormula(letters[1],right=FALSE)  ## a ~ .
createFormula(letters[1:3])  ## c ~ a + b
createFormula(letters[1:4])  ## c + d ~ a + b
createFormula(letters[1:4],right=FALSE) ## a + b ~ c + d}
