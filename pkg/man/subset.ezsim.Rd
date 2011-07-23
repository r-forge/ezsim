\name{subset.ezsim}
\title{Return of the Simulation}
\usage{\method{subset}{ezsim} (x, subset, ...)}
\description{Return a subset of the simulation result of an ezsim object.}
\alias{subset.ezsim}
\value{sim of ezsim}
\author{TszKin Julian Chan \email{ctszkin@gmail.com}}
\seealso{\code{\link{ezsim}}}
\note{For internal use of ezsim.}
\arguments{\item{x}{An ezsim Object}
\item{subset}{A list contains the subset of estimators and parameters. To select a subset of estimators: \code{list(estimator=c('name of estimator1','name of estimator2'))}. To select a subset of parameters: \code{list(mean=1:3,sd=4:5)}. Or both.}
\item{\dots}{unused}}
\examples{\dontrun{
ezsim_basic<-ezsim(
m             = 100,
run           = TRUE,
display_name  = c(mean_hat="hat(mu)",sd_mean_hat="hat(sigma[hat(mu)])"),
parameter_def = createParDef(list(n=seq(20,80,20),mu=c(0,2),sigma=c(1,3,5))),
dgp           = function() rnorm(n,mu,sigma),
estimator     = function(x) c(mean_hat = mean(x), 
sd_mean_hat=sd(x)/sqrt(length(x)-1)),
true_value    = function() c(mu, sigma / sqrt(n-1))
)
subset(ezsim_basic,subset=list(estimator='mean_hat',mu=0,n=c(20,40)))$sim
}}
