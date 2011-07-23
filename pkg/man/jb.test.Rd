\name{jb.test}
\title{p Value of Jarque Bera test}
\usage{jb.test(x)}
\description{Return the p Value of Jarque Bera test. The Jarque Bera test  test the null hypothesis that the data are from a normal distribution.}
\alias{jb.test}
\value{p Value of Jarque Bera test}
\author{TszKin Julian Chan \email{ctszkin@gmail.com}}
\arguments{\item{x}{data}}
\examples{jb.test(rnorm(50))
jb.test(rt(50,3))

n=100
# size
mean(replicate(n,jb.test(rnorm(100)))<0.05)

# power
mean(replicate(n,jb.test(rt(100,3)))<0.05)}
