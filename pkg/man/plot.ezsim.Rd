\name{plot.ezsim}
\title{Plot an ezsim Object}
\usage{\method{plot}{ezsim} (x, type=c("summary", "density", "powerfun"),
    subset, parameters_priority, return_print=FALSE, ylab, title,
    pdf_option, null_hypothesis, benchmark, ...)}
\description{There are 3 different modes to plot an ezsim object. \code{'summary'}, \code{'density'} and \code{'powerfun'} plot the summary statistics,density function and power function of an ezsim object respectively.\cr\cr
\code{'summary'}: The y-variable of the plot are summary statistics of the estimator. Two confidence bounds will be shaded in the plot. 25\% and 75\% percentile will form a 50\% confidence bound. Similarly, 2.5\% and 97.5\% percentile will form a 95\% confidence bound.  Each plot have only one estimator. The scalars parameter has the longest length will be the x-variable of the plot. The rest of the scalars parameters will be become the facets of the plot (see \pkg{ggplot2}). \cr\cr \code{density} : Density plot of the estimator. Each plot have only one estimator. scalars parameter will appear as different colour and in different facets.\cr\cr \code{powerfun} : Plot the power function of test(s). Estimators have to be a test (value = 1 if rejecting the null hypothesis, value = 0 if fail to reject the null hypothesis) others parameters will not be shown in the graph.}
\alias{plot.ezsim}
\value{Optional: a list of ggplot2 object}
\author{TszKin Julian Chan \email{ctszkin@gmail.com}}
\seealso{\code{\link{ezsim}},\code{\link{summary.ezsim}}, \code{\link{plot.summary.ezsim}},}
\keyword{post-simulation}
\arguments{\item{x}{An ezsim object}
\item{type}{Type of plot}
\item{subset}{subset of estimators or parameters. See \code{\link{subset.ezsim}} for details.}
\item{parameters_priority}{Display priority of parameter. If any parameter is missing here, they will be sorted by length.}
\item{return_print}{If TRUE, return a list of ggplot2 object. If FALSE(default), all of the plot will be printed out.}
\item{ylab}{Label of y-axis}
\item{title}{Title of the plot}
\item{pdf_option}{A list of option pass to \code{\link{pdf}}. If it is not missing, the plot will export to a pdf file}
\item{null_hypothesis}{Null hypothesis of the test. For \code{type=='powerfun'} only.}
\item{benchmark}{Benchmark distribution. For \code{type=='density'} only.}
\item{\dots}{unused}}
\examples{\dontrun{
## example 1
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
## Plot an ezsim object
plot(ezsim_basic)
## Subet of the Plot
plot(ezsim_basic,subset=list(estimator="sd_mean_hat",mu=0))
plot(ezsim_basic,subset=list(estimator="mean_hat",sigma=3))
## Parameters Priority of the Plot
plot(ezsim_basic,subset=list(estimator="sd_mean_hat",mu=0),parameters_priority=c("sigma","n"))
plot(ezsim_basic,subset=list(estimator="mean_hat",sigma=c(1,3)),parameters_priority="mu")

## Density Plot
plot(ezsim_basic,'density')
plot(ezsim_basic,"density",subset=list(estimator="mean_hat",sigma=3),parameters_priority="n",benchmark=dnorm)
plot(ezsim_basic,"density",subset=list(estimator="mean_hat",mu=0),parameters_priority="n" ,benchmark=dnorm)

## example 2
ezsim_ols<-ezsim(
m             = 100,    
run           = TRUE,
core          = 1,
display_name  = c(beta_hat='hat(beta)',es='sigma[e]^2',xs='sigma[x]^2',sd_beta_hat='hat(sigma)[hat(beta)]'),
parameter_def = createParDef(scalars=list(xs=c(1,3),beta=c(0,2),n=seq(20,80,20),es=c(1,3))),
dgp           = function(){
x<-rnorm(n,0,xs)
e<-rnorm(n,0,es)
y<-beta * x + e
data.frame(y,x)
},
estimator     = function(d){
r<-summary(lm(y~x-1,data=d))
out<-r$coef[1,1:2]
names(out)<-c('beta_hat','sd_beta_hat')
out
},
true_value    = function() c(beta, es/sqrt(n)/xs) 
)
plot(ezsim_ols)
plot(ezsim_ols,subset=list(beta=0))

plot(ezsim_ols,'density')
plot(ezsim_ols,'density',subset=list(es=1,xs=1))


## example 3
ezsim_powerfun<-ezsim(
run           = TRUE,   
m             = 100,
parameter_def = createParDef(scalars=list(xs=1,n=50,es=c(1,5),b=seq(-1,1,0.1))),
display_name  = c(b='beta',es='sigma[e]^2',xs='sigma[x]^2'),
dgp           = function(){
x<-rnorm(n,0,xs)
e<-rnorm(n,0,es)
y<-b * x + e
data.frame(y,x)
},
estimator     = function(d){
r<-summary(lm(y~x-1,data=d))
stat<-r$coef[,1]/r$coef[,2]

# test whether b > 0
# level of significance : 5%
out <- stat > c(qnorm(.95), qt(0.95,df=r$df[2]))
names(out)<-c("z-test","t-test")
out
}
)
plot(ezsim_powerfun,'powerfun') plot(ezsim_basic,'powerfun')
}}
