\name{add2DataFrame}
\title{Merge a Vector to \code{data.frame}}
\usage{add2DataFrame(x, y, name=names(y))}
\description{Merge a vector to data.frame. Each element of the vector will be a new column. Each rows will be filled up with the same value.}
\alias{add2DataFrame}
\value{a \code{data.frame} that contains all columns in x and all element of y as different columns.}
\author{TszKin Julian Chan \email{ctszkin@gmail.com}}
\note{For internal use of ezsim.}
\arguments{\item{x}{\code{data.frame}}
\item{y}{\code{vector} to be merged to the \code{data.frame}}
\item{name}{Name of new columns in the \code{data.frame}. (Default = names(y))}}
\examples{d=data.frame(x=1:5,y=11:15)
v=c(a=100,b=200,c=300)
add2DataFrame(d,v)}
