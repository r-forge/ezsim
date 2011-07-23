rm(list=ls(all=TRUE))
source("C:\\Users\\julian\\Dropbox\\R project\\ezsim\\ezsim.r")

# library(doSNOW)
# library(foreach)
# library(plyr)
# library(reshape)
# library(ggplot2)
library(ezsim)


############# example OLS #############

ez_ols<-ezsim(
    m             = 100,
    display_name  = c(beta_hat='hat(beta)',es='sigma[e]^2',xs='sigma[x]^2'),
    parameter_def = createParDef(scalars=list(xs=1,beta=1,n=seq(20,100,20),es=c(1,10))),
    dgp           = function(){
                        x<-rnorm(n,0,xs)
                        e<-rnorm(n,0,es)
                        y<-beta * x + e
                        data.frame(y,x)
                    },
    estimator     = function(d){
                        r<-summary(lm(y~x-1,data=d))
                        out<-c(r$coef[,1],(r$coef[,1]-1)/r$coef[,2] )
                        names(out)<-c('beta_hat','t-test')
                        out
                    },
    true_value    = function() c(beta ,(beta -1)/(es/sqrt(n)/xs)) 
)

par_def<-createParDef(scalars=list(xs=1,beta=1,n=seq(20,100,20),es=c(1,10)))

dgp<-function(){
	x<-rnorm(n,0,xs)
	e<-rnorm(n,0,es)
	y<-beta * x + e
	data.frame(y,x)
}
estimator<-function(d){
	r<-summary(lm(y~x-1,data=d))
	out<-c(r$coef[,1],(r$coef[,1]-1)/r$coef[,2] )
	names(out)<-c('beta_hat','t-test')
	out
}

true<-function(){
	c(beta ,(beta -1)/(es/sqrt(n)/xs)) 
}
display_name<-c(beta_hat='hat(beta)',es='sigma[e]^2',xs='sigma[x]^2')

test(par_def,dgp)
estimator(test(par_def,dgp))
test(par_def,true)

ez<-ezsim(20,estimator,dgp,par_def,true,run=TRUE,core=1,display_name=display_name)
summary(ez)
plot(ez)
plot(ez,'density')



############# example powerfun #############
par_def<-createParDef(scalars=list(xs=1,n=50,es=5,b=seq(-1,1,0.1)))
# setScalars(par_def,xs=1,n=50,es=5,b=seq(-1,1,0.1))

dgp<-function(){
	x<-rnorm(n,0,xs)
	e<-rnorm(n,0,es)
	y<-b * x + e
	data.frame(y,x)
}
estimator<-function(d){
	r<-summary(lm(y~x-1,data=d))
	stat<-r$coef[,1]/r$coef[,2]

	# test whether b > 0
	out <- stat > c(qnorm(.95), qt(0.95,df=r$df[2]))
	names(out)<-c('z-test','t-test')
	out
}

true<-function(){
	c(NA,NA ) 
}

ez<-ezsim(50,estimator,dgp,par_def,true,run=TRUE,core=1)

summary(ez)
plot(ez,'powerfun',null_hypothesis=0)



########################

source("C:\\Users\\julian\\Dropbox\\R project\\ezsim\\ezsim.r")


par_def<-createParDef(scalars=list(n=1:5*20,mu=1:3,sigma=c(3,6,9)))
# setScalars(par_def,n=1:5*20,mu=1:3,sigma=c(3,6,9))

dgp<-function(){
	rnorm(n,mu,sigma)
}
estimator<-function(x){
    c(mean_hat = mean(x), sd_mean_hat=sd(x)/sqrt(length(x)-1))
}

true<-function(){
    c(mu, sigma / sqrt(n-1))
}
display_name<-c(mean_hat='hat(mu)',sd_mean_hat='hat(sigma[mu])')
ez<-ezsim(50,estimator,dgp,par_def,true,run=TRUE,core=1,display_name=display_name)

## Basic plot
plot(ez)
plot(ez,'density')
plot(summary(ez,c('mean','median')))

## estimators subset (Default: all estimator)
plot(ez,subset=list(estimator='sd_mean_hat'))
plot(ez,subset=list(estimator='mean_hat'))

## parameters subset (Default: all parameters)
plot(ez,subset=list(mu=c(1,3)))
plot(ez,subset=list(mu=c(1)))

## estimators and parameters subset
plot(ez,subset=list(estimator='sd_mean_hat',mu=c(1,3)))
plot(ez,subset=list(estimator='sd_mean_hat',mu=2))
plot(ez,subset=list(estimator='sd_mean_hat',sigma=6))
plot(ez,subset=list(estimator='sd_mean_hat',n=20))

## parameters_priority (Default: parameter with the largest in number)
plot(ez,subset=list(estimator='sd_mean_hat'),parameters_priority='mu' )
plot(ez,subset=list(estimator='sd_mean_hat'),parameters_priority='sigma' )
plot(ez,subset=list(estimator='sd_mean_hat'),parameters_priority=c('mu','sigma') )

## estimators subset, parameters subset and parameters_priority can apply to other plot function.
plot(ez,'density',subset=list(estimator='mean_hat',mu=1,n=20) )
plot(ez,'density',subset=list(estimator='mean_hat',sigma=3,n=20) )
plot(ez,'density',subset=list(estimator='mean_hat',sigma=3,mu=1) )

plot(ez,'density',subset=list(estimator='sd_mean_hat',mu=1,n=20) )
plot(ez,'density',subset=list(estimator='sd_mean_hat',sigma=3,n=20) )
plot(ez,'density',subset=list(estimator='sd_mean_hat',sigma=3,mu=1) )


summary_ez <- summary(ez,c('q25','q75'),subset=list(estimator='mean_hat'))
plot(summary_ez ,parameters_priority=c('sigma','n'))

summary_ez <- summary(ez,c('q25','q75'),subset=list(estimator='mean_hat',mu=2))
plot(summary_ez ,parameters_priority=c('sigma','n'))


####################################################
# vignette:
ez<-ezsim(
    m             = 100,
    display_name  = c(mean_hat="hat(mu)",sd_mean_hat="sigma[hat(mu)]"),
    parameter_def = createParDef(list(n=seq(20,80,20),mu=c(0,2),sigma=c(1,3,5))),
    dgp           = function() rnorm(n,mu,sigma),
    estimator     = function(x) c(mean_hat = mean(x), 
                                 sd_mean_hat=sd(x)/sqrt(length(x)-1)),
    true_value    = function() c(mu, sigma / sqrt(n-1))
)

ez_powerfun<-ezsim(
    run           = TRUE,   
    m             = 100,
    parameter_def = createParDef(scalars=list(xs=1,n=50,es=5,b=seq(-1,1,0.1))),
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


#################################################### 

##ezsim 
#############

## Example 1
ezsim_basic<-ezsim(
    m             = 100,
    run           = TRUE,
    core          = 1,
    display_name  = c(mean_hat="hat(mu)",sd_mean_hat="sigma[hat(mu)]"),
    parameter_def = createParDef(list(n=seq(20,80,20),mu=c(0,2),sigma=c(1,3,5))),
    dgp           = function() rnorm(n,mu,sigma),
    estimator     = function(x) c(mean_hat = mean(x), 
                                 sd_mean_hat=sd(x)/sqrt(length(x)-1)),
    true_value    = function() c(mu, sigma / sqrt(n-1))
)

## Test whether an ezsim object is valid. Print the result of the test and dont return the name of estimator.
test(ezsim_basic,print_result=TRUE,return_name=FALSE)

## Subset of an ezsim object.
subset(ezsim_basic,subset=list(estimator='mean_hat',mu=0,n=c(20,40)))$sim

## Summary of an ezsim object
summary(ezsim_basic)

## Summary of a subset of ezsim object
summary(ezsim_basic,subset=list(estimator='mean_hat',n=c(20,40),sigma=c(1,3)))

## More Summary Statistics
summary(ezsim_basic,simple=FALSE,subset=list(estimator='mean_hat',n=c(20,40),sigma=c(1,3)))

## Customize the Summary Statistics
summary(ezsim_basic,stat=c("q25","median","q75"),Q025=quantile(value_of_estimator,0.025),Q975=quantile(value_of_estimator,0.975),subset=list(estimator='mean_hat',n=c(20,40),sigma=c(1,3)))

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

## Plot the summary ezsim
plot(summary(ezsim_basic,c("q25","q75")))
plot(summary(ezsim_basic,c("q25","q75"),subset=list(estimator='mean_hat')))
plot(summary(ezsim_basic,c("median"),subset=list(estimator='sd_mean_hat')))

## Example 2
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
summary(ezsim_ols)
plot(ezsim_ols)
plot(ezsim_ols,subset=list(beta=0))

plot(ezsim_ols,'density')
plot(ezsim_ols,'density',subset=list(es=1,xs=1))


## example 3
ezsim_powerfun<-ezsim(
    run           = TRUE,   
    m             = 100,
    display_name  = c(b='beta',es='sigma[e]^2',xs='sigma[x]^2'),
    parameter_def = createParDef(scalars=list(xs=1,n=50,es=c(1,5),b=seq(-1,1,0.1))),
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
plot(ezsim_powerfun,'powerfun')


############# 
# run.ezsim
############# 
\dontrun{
ezsim_basic<-ezsim(
    m             = 100,
    run           = FALSE,
    display_name  = c(mean_hat="hat(mu)",sd_mean_hat="sigma[hat(mu)]"),
    parameter_def = createParDef(list(n=seq(20,80,20),mu=c(0,2),sigma=c(1,3,5))),
    dgp           = function() rnorm(n,mu,sigma),
    estimator     = function(x) c(mean_hat = mean(x), 
                                 sd_mean_hat=sd(x)/sqrt(length(x)-1)),
    true_value    = function() c(mu, sigma / sqrt(n-1))
)
    run(ezsim_basic)
}

#############
# test.ezsim
#############
ezsim_basic<-ezsim(
    m             = 100,
    run           = FALSE,
    display_name  = c(mean_hat="hat(mu)",sd_mean_hat="sigma[hat(mu)]"),
    parameter_def = createParDef(list(n=seq(20,80,20),mu=c(0,2),sigma=c(1,3,5))),
    dgp           = function() rnorm(n,mu,sigma),
    estimator     = function(x) c(mean_hat = mean(x), 
                                 sd_mean_hat=sd(x)/sqrt(length(x)-1)),
    true_value    = function() c(mu, sigma / sqrt(n-1))
)

test(ezsim_basic,print_result=TRUE)


# subset.ezsim
\dontrun{
ezsim_basic<-ezsim(
    m             = 100,
    run           = TRUE,
    display_name  = c(mean_hat="hat(mu)",sd_mean_hat="sigma[hat(mu)]"),
    parameter_def = createParDef(list(n=seq(20,80,20),mu=c(0,2),sigma=c(1,3,5))),
    dgp           = function() rnorm(n,mu,sigma),
    estimator     = function(x) c(mean_hat = mean(x), 
                                 sd_mean_hat=sd(x)/sqrt(length(x)-1)),
    true_value    = function() c(mu, sigma / sqrt(n-1))
)
subset(ezsim_basic,subset=list(estimator='mean_hat',mu=0,n=c(20,40)))$sim
}

################### plot.ezsim
\dontrun{
## example 1
ezsim_basic<-ezsim(
    m             = 100,
    run           = TRUE,
    core          = 1,
    display_name  = c(mean_hat="hat(mu)",sd_mean_hat="sigma[hat(mu)]"),
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
    display_name  = c(b='beta',es='sigma[e]^2',xs='sigma[x]^2'),
    parameter_def = createParDef(scalars=list(xs=1,n=50,es=c(1,5),b=seq(-1,1,0.1))),
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
plot(ezsim_powerfun,'powerfun') 
}



################### summary.ezsim
\dontrun{
ezsim_basic<-ezsim(
    m             = 100,
    run           = TRUE,
    core          = 1,
    display_name  = c(mean_hat="hat(mu)",sd_mean_hat="sigma[hat(mu)]"),
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
}

################## plot.summary.ezsim
ezsim_basic<-ezsim(
    m             = 100,
    run           = TRUE,
    core          = 1,
    display_name  = c(mean_hat="hat(mu)",sd_mean_hat="sigma[hat(mu)]"),
    parameter_def = createParDef(list(n=seq(20,80,20),mu=c(0,2),sigma=c(1,3,5))),
    dgp           = function() rnorm(n,mu,sigma),
    estimator     = function(x) c(mean_hat = mean(x), 
                                 sd_mean_hat=sd(x)/sqrt(length(x)-1)),
    true_value    = function() c(mu, sigma / sqrt(n-1))
)
## Plot the summary ezsim
plot(summary(ezsim_basic,c("q25","q75")))
plot(summary(ezsim_basic,c("q25","q75"),subset=list(estimator='mean_hat')))
plot(summary(ezsim_basic,c("median"),subset=list(estimator='sd_mean_hat')))



################### getScalarsName.summary.ezsim
\dontrun{
ezsim_basic<-ezsim(
    m             = 100,
    run           = TRUE,
    core          = 1,
    display_name  = c(mean_hat="hat(mu)",sd_mean_hat="sigma[hat(mu)]"),
    parameter_def = createParDef(list(n=seq(20,80,20),mu=c(0,2),sigma=c(1,3,5))),
    dgp           = function() rnorm(n,mu,sigma),
    estimator     = function(x) c(mean_hat = mean(x), 
                                 sd_mean_hat=sd(x)/sqrt(length(x)-1)),
    true_value    = function() c(mu, sigma / sqrt(n-1))
)

getScalarsName(ezsim_basic)
getScalarsName(summary(ezsim_basic))
}


################## getScalarsName.ezsim
ezsim_basic<-ezsim(
    m             = 100,
    run           = TRUE,
    core          = 1,
    display_name  = c(mean_hat="hat(mu)",sd_mean_hat="sigma[hat(mu)]"),
    parameter_def = createParDef(list(n=seq(20,80,20),mu=c(0,2),sigma=c(1,3,5))),
    dgp           = function() rnorm(n,mu,sigma),
    estimator     = function(x) c(mean_hat = mean(x), 
                                 sd_mean_hat=sd(x)/sqrt(length(x)-1)),
    true_value    = function() c(mu, sigma / sqrt(n-1))
)

getScalarsName(ezsim_basic)
getScalarsName(summary(ezsim_basic))
}



######## print.summary.ezsim
######################### createParDef
 par_def1<-createParDef(scalar=list(mean=1,sd=2,n=seq(10,50,10)))
 
 par_def2<-createParDef()
 setScalars(par_def2,mean=1,sd=2,n=seq(10,50,10))
 
 identical(par_def1,par_def2)
 
 test(par_def1, function() rnorm(n,mean,sd) )  # 10 random number
 test(par_def1, function() rnorm(n,mean,sd), index=3)  # 30 random number
 
 generate(par_def1)
 
 # More than 1 scalars parameters 
 par_def3<-createParDef(scalars=list(sd=2,mean=1:3,n=seq(10,50,10)))
 
 generate(par_def3)


######################### setOthers.parameterDef
 par_def1<-createParDef(scalar=list(mean=1,sd=2,n=seq(10,50,10)))
 
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
 generate(par_def4)

######################### setScalars.parameterDef
 par_def1<-createParDef(scalar=list(mean=1,sd=2,n=seq(10,50,10)))
 
 par_def2<-createParDef()
 setScalars(par_def2,mean=1,sd=2,n=seq(10,50,10))
 
 identical(par_def1,par_def2)
 
 test(par_def1, function() rnorm(n,mean,sd) )  # 10 random number
 test(par_def1, function() rnorm(n,mean,sd), index=3)  # 30 random number
 
 generate(par_def1)
 
 # More than 1 scalars parameters 
 par_def3<-createParDef(scalars=list(sd=2,mean=1:3,n=seq(10,50,10)))
 
 generate(par_def3)


######################### generate.parameterDef
 par_def1<-createParDef(scalars=list(mean=1,sd=2,n=seq(10,50,10)))
 generate(par_def1)
 par_def2<-createParDef(scalars=list(sd=2,mean=1:3,n=seq(10,50,10)))
 generate(par_def2)

######################### test.parameterDef
 par_def<-createParDef()
 setScalars(par_def,mean=1,sd=2,n=seq(10,50,10))
 
 test(par_def, function() rnorm(n,mean,sd) )  # 10 random number
 test(par_def, function() rnorm(n,mean,sd), index=3)  # 30 random number
 
 ## Example 2
 par_def<-createParDef()
 setOthers(par_def,xs=1,b=1)
 setScalars(par_def,n=seq(20,100,20),es=c(1,10))
 
 dgp<-function(){
 x<-rnorm(n,0,xs)
 e<-rnorm(n,0,es)
 y<-b * x + e
 data.frame(y,x)
 }
 estimator<-function(d){
 r<-summary(lm(y~x-1,data=d))
 c(b=r$coef[,1],t=(r$coef[,1]-1)/r$coef[,2] )
 }
 
 true<-function(){
 c(b,(b-1)/(es/sqrt(n)/xs)) 
 }
 test(par_def,dgp)
 estimator(test(par_def,dgp))
 test(par_def,true)


# print.parameters
# print.parameterDef
########################## add2DataFrame
 d=data.frame(x=1:5,y=11:15)
 v=c(a=100,b=200,c=300)
 add2DataFrame(d,v)

######################### recode
 x=rep(1:5,each=2)
 value=5:1
 names(value)=1:5
 recode(x,value)


######################### run.function
 run(function() rnorm(n,mean,sd),list(n=5,mean=5,sd=1))

######################### createFormula
 createFormula(letters[1])  ## . ~ a
 createFormula(letters[1],right=FALSE)  ## a ~ .
 createFormula(letters[1:3])  ## c ~ a + b
 createFormula(letters[1:4])  ## c + d ~ a + b
 createFormula(letters[1:4],right=FALSE) ## a + b ~ c + d

######################### jb.test
 jb.test(rnorm(50))
 jb.test(rt(50,3))
 
 n=100
 # size
 mean(replicate(n,jb.test(rnorm(100)))<0.05)
 
 # power
 mean(replicate(n,jb.test(rt(100,3)))<0.05)

# label_both_parsed_recode



