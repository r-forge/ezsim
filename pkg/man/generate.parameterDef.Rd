\name{generate.parameterDef}
\title{Generate Parameter}
\usage{\method{generate}{parameterDef}(x,...)}
\description{Generate Parameters from a parameterDef Object.
The scalars parameters in parameterDef is expanded and concatenated with the others parameters.}
\alias{generate.parameterDef}
\value{\item{other_parameters}{A list of other_parameters} \item{scalar_parameters}{A data.frame of scalar_parameters} \item{parameter_list}{A list of all parameters}}
\author{TszKin Julian Chan \email{ctszkin@gmail.com}}
\seealso{\code{\link{setOthers.parameterDef}}, \code{\link{setScalars.parameterDef}}, \code{\link{test.parameterDef}}, \code{\link{generate.parameterDef}}}
\arguments{\item{x}{A parameterDef Object}
\item{\dots}{unused}}
\examples{par_def1<-createParDef(scalars=list(mean=1,sd=2,n=seq(10,50,10)))
generate(par_def1)
par_def2<-createParDef(scalars=list(sd=2,mean=1:3,n=seq(10,50,10)))
generate(par_def2)}
