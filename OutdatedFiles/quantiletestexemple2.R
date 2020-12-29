pre <- c(9, 9, 10, 10, 14, 14, 14, 15, 16, 20)
post <- c(6, 10, 13, 15, 18, 21, 22, 23, 30, 37)

m <- length(pre)
n <- length(post)

library(EnvStats)
qtest <- quantileTest(pre,post,target.quantile = 0.8,
  alternative = "greater")
qtest$parameters
qtest$statistic
qtest$p.value



sort(c(pre,post))
(ri <- rank(c(pre,post),ties.method="average")[c((m+1):(m+n))])
b <- 0.8

c <- ceiling(m+n-((m+n+1)*b))
x <- sum(ifelse(ri/(m+n+1) < 0.8,0,1))
(choose(m+n-c,n-x)*choose(c,x))/choose(m+n,n)

sum(exp(qnorm(ri/(m+n+1))-1/2)-1)
choose(20,10)
