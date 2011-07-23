\name{setOthers.parameterDef}
\title{Set a parameterDef Object.}
\usage{\method{setOthers}{parameterDef}(x,...)}
\description{setOthers sets the scalar parameters of a parameterDef object. setOthers are "call by reference", so assignment is not needed to update the parameterDef object. In other words, they will overwrite the value of its argument(parameterDef object).
parameterDef is a short hand of "parameter definition". It defines parameters used by the \code{\link{dgp}} which is the most important part of a simulation. For each simulation,There is a particular set of parameter. parameterDef allow us to define several parameters for different simulation at once. There are two types of parameter in parameterDef, scalar parameters and other parameters.
Scalar parameters must be a scalar. Any vectors or matrix is regarded as a sequence of scalar parameters. For example, n=seq(10,50,10), first simulation  takes n=10, second simulation takes n=20 and so on.
Other parameters can be anything and it is others over the scalar parameters.
For example, we would like to know how would the sample size affect the variance of the sample mean of normally distributed variable. We can set n=seq(10,50,10), mean=1 and sd=2.  (see example)}
\alias{setOthers.parameterDef}
\value{A parameterDef object}
\author{TszKin Julian Chan \email{ctszkin@gmail.com}}
\seealso{\code{\link{setScalars.parameterDef}},\code{\link{createParDef}},\code{\link{test.parameterDef}},\code{\link{generate.parameterDef}}}
\keyword{parameterDef}
\arguments{\item{x}{A parameterDef object}
\item{\dots}{Variables to be added to a parameterDef object}}
\examples{par_def1<-createParDef(scalar=list(mean=1,sd=2,n=seq(10,50,10)))

par_def2<-createParDef()
setScalars(par_def2,mean=1,sd=2,n=seq(10,50,10))

identical(par_def1,par_def2)

test(par_def1, function() rnorm(n,mean,sd) )  # 10 random number
test(par_def1, function() rnorm(n,mean,sd), index=3)  # 30 random number

generate(par_def1)

# More than 1 scalars parameters 
par_def3<-createParDef(scalars=list(sd=2,mean=1:3,n=seq(10,50,10)))

generate(par_def3)

# 
par_def4<-createParDef(scalar=list(mean=1,sd=2,n=seq(10,50,10)))
setOthers(par_def4,some_matrix=matrix(1:4,nrow=2),some_vector=1:6)
par_def4
generate(par_def4)}
