\name{test.ezsim}
\title{Perform a Test for an ezsim Object}
\usage{\method{test}{ezsim} (x, return_name=TRUE, print_result=FALSE, ...)}
\description{For each set of parameters, the simulation is ran once to obtain the value of estimator and true value to make sure everything in ezsim is properly defined. The test results will be shown in the console. The test will be ran automatically when you create an ezsim object.}
\alias{test.ezsim}
\value{Optional: names of estimator.}
\author{TszKin Julian Chan \email{ctszkin@gmail.com}}
\seealso{\code{\link{ezsim}}}
\arguments{\item{x}{An ezsim Object}
\item{return_name}{Whehter to return the name of estimator}
\item{print_result}{Whehter to print the return}
\item{\dots}{unused}}
\examples{ezsim_basic<-ezsim(
m             = 100,
run           = FALSE,
display_name  = c(mean_hat="hat(mu)",sd_mean_hat="hat(sigma[hat(mu)])"),
parameter_def = createParDef(list(n=seq(20,80,20),mu=c(0,2),sigma=c(1,3,5))),
dgp           = function() rnorm(n,mu,sigma),
estimator     = function(x) c(mean_hat = mean(x), 
sd_mean_hat=sd(x)/sqrt(length(x)-1)),
true_value    = function() c(mu, sigma / sqrt(n-1))
)

test(ezsim_basic,print_result=TRUE)}
