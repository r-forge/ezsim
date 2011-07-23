\name{getScalarsName.ezsim}
\title{Get Names of scalars Parameters.}
\usage{\method{getScalarsName}{ezsim}(x,simple=FALSE,parameters_priority,...)}
\description{Get names of scalars parameters from an ezsim object.}
\alias{getScalarsName.ezsim}
\value{\item{scalar_length_greater_one}{ Name of scalar parameters with more than one elements} \item{scalar_length_one}{Name of scalar parameters with only one element} \item{subtitle}{subtitle for fixed scalar parameters}}
\author{TszKin Julian Chan \email{ctszkin@gmail.com}}
\note{For internal use of ezsim.}
\seealso{\code{\link{getScalarsName.summary.ezsim}}}
\arguments{\item{x}{an ezsim object}
\item{simple}{If true, return only the name of scalar parameters. If False, split the scalar into two groups, one with fixed value, one with varied value. Also, subtitle is returned.}
\item{parameters_priority}{Priority in sorting parameters.}
\item{\dots}{unused}}
\examples{\dontrun{
ezsim_basic<-ezsim(
m             = 100,
run           = TRUE,
core          = 1,
display_name  = c(mean_hat="hat(mu)",sd_mean_hat="hat(sigma[hat(mu)])"),
parameter_def = createParDef(list(n=seq(20,80,20),mu=c(0,2),sigma=c(1,3,5))),
dgp           = function() rnorm(n,mu,sigma),
estimator     = function(x) c(mean_hat = mean(x), 
sd_mean_hat=sd(x)/sqrt(length(x)-1)),
true_value    = function() c(mu, sigma / sqrt(n-1))
)

getScalarsName(ezsim_basic)
getScalarsName(summary(ezsim_basic))
}}
