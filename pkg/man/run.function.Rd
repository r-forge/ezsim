\name{run.function}
\title{Evaluate Function Under Local Variables}
\usage{\method{run}{function} (x, variables=list, ...)}
\description{This function evaluates a function \code{x} under an environment which is created by a list. All elements of the list is local to the function; other words all elements of the list can be accessed directly by the function. 
In this function, a new environment is created and each element of \code{variables} is assigned to the new environment. Then the environment associated with the \code{x} is updated with the new environment. Finally \code{x(...)} is evaluated and return the result.
The main usage of \code{run.function} is to evaluate \code{dgp} or \code{true_value} under a list of parameter. See \code{\link{ezsim}} for details.}
\alias{run.function}
\value{Return value of the \code{x}.}
\author{TszKin Julian Chan \email{ctszkin@gmail.com}}
\note{For internal use of ezsim.}
\seealso{\code{\link{environment}}, \code{\link{ezsim}}}
\arguments{\item{x}{A function to be called}
\item{variables}{A list to be converted to an environment}
\item{\dots}{Further arguments to \code{x}}}
\examples{run(function() rnorm(n,mean,sd),list(n=5,mean=5,sd=1))}
