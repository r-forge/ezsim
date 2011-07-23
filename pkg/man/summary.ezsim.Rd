\name{summary.ezsim}
\title{Summarize an ezsim Object}
\usage{\method{summary}{ezsim}(object,stat=c('mean','tv','bias',
'biaspercentage','sd','rmse','min','q25','median',
'q75','max','jb_test'),simple=TRUE,subset,...)}
\description{A quick summary to the simulation. Summary statistics included mean, true value (tv), bias, bias percentage (mean/tv-1), sd, rmse (root mean square error), min, q25 (first quarter), median, q75 (third quarter), max, p value of jb-test. See \code{\link{ezsim}} and \code{\link{plot.summary.ezsim}} for details and examples.}
\alias{summary.ezsim}
\value{A summary.ezsim object}
\author{TszKin Julian Chan \email{ctszkin@gmail.com}}
\seealso{\code{\link{ezsim}}, \code{\link{plot.summary.ezsim}}, \code{\link{getScalarsName.summary.ezsim}}}
\keyword{post-simulation}
\arguments{\item{object}{An ezsim object}
\item{stat}{Some preset summary statistics. Included,  \code{c('mean','tv','bias','biaspercentage','sd','rmse','min','q25','median','q75','max','jb_test')}}
\item{simple}{If True, shows only mean, true value, bias, sd and rmse of the estimator. If False, shows all statistics in stat.}
\item{subset}{subset of estimators or parameters. See \code{\link{subset.ezsim}} for details.}
\item{\dots}{Furhter summary statistics. Given in the form stat_name=stat. For example, Mean=mean}}
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


## Subset of an ezsim object.
subset(ezsim_basic,subset=list(estimator='mean_hat',mu=0,n=c(20,40)))

## Summary of an ezsim object
summary(ezsim_basic)

## Summary of a subset of ezsim object
summary(ezsim_basic,subset=list(estimator='mean_hat',n=c(20,40),sigma=c(1,3)))

## More Summary Statistics
summary(ezsim_basic,simple=FALSE,subset=list(estimator='mean_hat',n=c(20,40),sigma=c(1,3)))

## Customize the Summary Statistics
summary(ezsim_basic,stat=c("q25","median","q75"),Q025=quantile(value_of_estimator,0.025),Q975=quantile(value_of_estimator,0.975),subset=list(estimator='mean_hat',n=c(20,40),sigma=c(1,3)))
}}
