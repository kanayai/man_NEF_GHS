 test <- function(y,lambda,N=10000){
  res <- (lambda-2)*log(2)-log(pi)-lgamma(lambda) + 2*lgamma(lambda/2)-sum(log(1+(y^2)/((lambda+2*(0:N))^2)))
  exp(res)
}

test <- Vectorize(test,vectorize.args = "y")


test2 <- function(y,lambda,N=10000){
  res <- (lambda-2)*log(2)-log(pi)-lgamma(lambda) 
  if (lambda%%1 == 0){
      if (lambda%%2 == 0){
        lambda   <- lambda/2
        if (lambda == 1){
          res  <- res + log(pi/2)+log(abs(y))-log(sinh(pi*abs(y)/2))
        }else{
          res <- res + log(pi/2)+log(abs(y))-log(sinh(pi*abs(y)/2))+sum(log(((1:(lambda-1))^2+(y/2)^2)))
        }
      }else{
        if (lambda==1){
          res <- res + log(pi)-log(cosh(pi*y/2))
        }else{
          lambda<-(lambda-1)/2
          res <- res + log(pi)-log(cosh(pi*y/2))+sum(log(((1:lambda)-1/2)^2+(y/2)^2))
        }
      }
    }else{
        res <- res + 2*lgamma(lambda/2)-sum(log(1+(y^2)/((lambda+2*(0:N))^2)))  
    }
  exp(res)
}



test2 <- Vectorize(test2,vectorize.args = "y") 

integrate(test,lower=-Inf,upper=Inf,lambda=1,N=10000)
integrate(test,lower=-Inf,upper=Inf,lambda=2,N=10000)
integrate(test,lower=-Inf,upper=Inf,lambda=3,N=10000)
integrate(test,lower=-Inf,upper=Inf,lambda=pi,N=10000)

integrate(function(y,lambda,N) y*test(y,lambda,N), -Inf, Inf,lambda=1,N=10000)
integrate(function(y,lambda,N) y*test(y,lambda,N), -Inf, Inf,lambda=2,N=10000)
integrate(function(y,lambda,N) y*test(y,lambda,N), -Inf, Inf,lambda=3,N=10000)
integrate(function(y,lambda,N) y*test(y,lambda,N), -Inf, Inf,lambda=pi,N=10000)

integrate(test2,lower=-Inf,upper=Inf,lambda=1 ,N=10000)
integrate(test2,lower=-Inf,upper=Inf,lambda=2 ,N=10000)
integrate(test2,lower=-Inf,upper=Inf,lambda=3 ,N=10000)
integrate(test2,lower=-Inf,upper=Inf,lambda=pi,N=10000)


integrate(function(y,lambda,N) y*test2(y,lambda,N), -Inf, Inf,lambda=1, N=10000)
integrate(function(y,lambda,N) y*test2(y,lambda,N), -Inf, Inf,lambda=2, N=10000)
integrate(function(y,lambda,N) y*test2(y,lambda,N), -Inf, Inf,lambda=3, N=10000)
integrate(function(y,lambda,N) y*test2(y,lambda,N), -Inf, Inf,lambda=pi,N=10000)


test(pi,1,10000)
test2(pi,1)

test(-pi,1,10000)
test2(-pi,1)


test(pi,2,100000)
test2(pi,2)

test(-pi,2,100000)
test2(-pi,2)


test(pi,3,10000)
test2(pi,3)

test(-pi,3,10000)
test2(-pi,3)

test(pi,14.2,10000)
test2(pi,14)
test2(pi,15)



# New stuff


library(pracma)
gammaz(c(-2-2i, -1-1i, 0, 1+1i, 2+2i))

library(hypergeo)
complex_gamma(c(-2-2i, -1-1i, 0, 1+1i, 2+2i))


test  <- function(y,lambda,log=F){
  z   <- (lambda+y*1i)/2
  aux <- exp(Re(complex_gamma(z,log=T)))
  aux  <- (lambda-2)*log(2)-log(pi)-lgamma(lambda) + 2*Re(complex_gamma((lambda+y*1i)/2,log=T))
 if (log){
  res<-aux
 }else{
   res<-exp(aux) 
 }
  res
}

dghs  <- function(y,lambda,log=F){
  logf  <- (lambda-2)*log(2)-log(pi)-lgamma(lambda) + 2*Re(complex_gamma((lambda+y*1i)/2,log=T))
  if (log){
    res<-logf
  }else{
    res<-exp(logf) 
  }
  res
}

lambda<-seq(2,20,length=1000)




ll<-rep(NA,1000)
for (i in 1:1000){
  ll[i]<-test(y=2.34,lambda[i],log=T)
}

plot(lambda,ll,type="l")


# Digamma function for a complex number PRACMA
complex_digamma <- psi(1+2i)
print(complex_digamma)

dghs1 <- function(y,lambda,arg=1){
  (y^arg)*dghs(y,lambda,log=F)
}

complex_gamma(3.24+1.4*1i,log = T)
complex_gamma(3.24-1.4*1i,log=T)
psi(3.24+1.4*1i)
psi(3.24-1.4*1i)

integrate(dghs,lower=-Inf,upper=Inf,lambda=1,log=F)
integrate(dghs,lower=-Inf,upper=Inf,lambda=2,log=F)
integrate(dghs,lower=-Inf,upper=Inf,lambda=3,log=F)
integrate(dghs,lower=-Inf,upper=Inf,lambda=pi,log=F)


integrate(dghs1,lower=-Inf,upper=Inf,lambda=1,arg=0)
integrate(dghs1,lower=-Inf,upper=Inf,lambda=1,arg=1)
integrate(dghs1,lower=-Inf,upper=Inf,lambda=5,arg=2)
