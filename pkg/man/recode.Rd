\name{recode}
\title{Recode the value of a vector}
\usage{recode(x, value)}
\description{Recode the value of a vector.}
\alias{recode}
\value{a vector}
\author{TszKin Julian Chan \email{ctszkin@gmail.com}}
\note{For internal use of ezsim.}
\arguments{\item{x}{a vector}
\item{value}{recode name and value}}
\examples{x=rep(1:5,each=2)
value=5:1
names(value)=1:5
recode(x,value)}
