################### SIMULATION ##########################

#'rnorm'  random numbers
# 'dnorm' density
#'pnorm' cumulative distrib function CDF (integral of dnorm)
#'qnorm' inverse of the CDF, also called the quantile function

#Suppose we wish to simulate a large number of normal random variables
#with mean 10 and standard deviation 3
z <- rnorm(200000, mean=10, sd=3)  #population
hist(z,freq=FALSE,nclass=100)
x <- seq(min(z),max(z),len=200)  #sample
lines(x,dnorm(x, mean=10, sd=3),col=2, lty=2, lwd=3) #true parameters
lines(x,dnorm(x, mean=mean(z),sd=sqrt(var(z)))) #estimated parameters

# what proportion of the deviates lie outside 3 standard deviations from the true mean
sum(abs((z-10)/3)>3)/length(z) #observes % of deviates > +- 3 sd
2*pnorm(-3) #theoretical %


pnorm(1:3)-pnorm(-(1:3)) #probability to find extreme values
qnorm(c(.05,.95)) #ci al 90%
qnorm(c(.025,.975)) #ci al 95%

############# Algoritmo EM ################
grb <- read.table ("http://astrostatistics.psu.edu/datasets/GRB_afterglow.dat", 
                   header=T, skip=1)
#   grb <- read.table ("GRB_afterglow.dat", header=T, skip=1)
flux <- grb[,2]
cflux <- flux
cflux[flux>=60] <- 60 #creo delle obs right censored
n <- length(cflux)
yy <- (1:n)/n
plot(sort(cflux),log(1-yy+1/n))

#The complete dataset is a set of n observations from an exponential distribution with unknown mean mu, say, X1, ..., Xn. 
#What we observe is Z1, ..., Zn, where Zi is defined as min(Xi, 60).
#Loglikelihood of observed data
m <- sum(flux>=60)
s <- sum(cflux)
loglik <- function(mu) -(n-m)*log(mu)-s/mu
#stima di max verosimiglianza
mle <- s/(n-m)
