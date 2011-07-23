\name{plot.summary.ezsim}
\title{Plot an summary.ezsim Object}
\usage{\method{plot}{summary.ezsim}(x,parameters_priority,ylab='Summary Statistics',title,pdf_option,...)}
\description{Plot the summary statistics for several estimators in the same plot. Summary statistics abd estimators are separated by colour and linetype.
The longest scalars parameter will be the x-variable of the plot. The rest of the scalars parameters will be become the facets of the plot (see \pkg{ggplot2}).
others parameters will not be shown in the graph.}
\alias{plot.summary.ezsim}
\value{Optional: a ggplot2 object}
\author{TszKin Julian Chan \email{ctszkin@gmail.com}}
\seealso{\code{\link{ezsim}},\code{\link{summary.ezsim}}, \code{\link{plot.summary.ezsim}},}
\keyword{post-simulation}
\arguments{\item{x}{An summary.ezsim Object}
\item{parameters_priority}{Display priority of parameter. Any missed parameters will be sorted by length.}
\item{ylab}{Label of y-axis}
\item{title}{Title of the plot}
\item{pdf_option}{A list of option pass to \code{\link{pdf}}. If it is not missing, the plot will export to a pdf file}
\item{\dots}{unused}}
\examples{ezsim_basic<-ezsim(
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
## Plot the summary ezsim
plot(summary(ezsim_basic,c("q25","q75")))
plot(summary(ezsim_basic,c("q25","q75"),subset=list(estimator='mean_hat')))
plot(summary(ezsim_basic,c("median"),subset=list(estimator='sd_mean_hat')))}
