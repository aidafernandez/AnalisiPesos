library(readxl)
# https://www.rdocumentation.org/packages/EnvStats/versions/2.3.1/topics/quantileTest

pesInd <- read_excel("G://GEA4//PRACTIQUES//pesosIndividuales.xlsx",                      range = "A1:D949")

pesInd$Box <- as.factor(pesInd$Box)
pesInd$Treat <- as.factor(pesInd$Treat)

pesInd$difBW41_28 <- pesInd$BW41-pesInd$BW28
pesInd$indexBW41_28 <- 100*pesInd$BW41/pesInd$BW28                   
pesInd$taxBW41_28 <- 100*(pesInd$BW41-pesInd$BW28)/pesInd$BW28    

library(EnvStats)

qtest <- quantileTest(
  subset(pesInd,Treat == 3)$difBW41_28,
  subset(pesInd,Treat == 1)$difBW41_28,
  target.quantile = 0.2,
  alternative = "greater")


obs <- as.data.frame(na.omit(rbind(subset(pesInd,Treat == 3)[,c("Treat","difBW41_28")],subset(pesInd,Treat == 1)[,c("Treat","difBW41_28")])
))

(m <- nrow(subset(obs,Treat==3)))
(n <- nrow(subset(obs,Treat==1)))

qtest$parameters

rangs <- rank(obs$difBW41_28,ties.method="average")
obs$rangs <- rank(obs$difBW41_28,ties.method="average")

(r <- min(rangs[rangs/(nrow(obs)+1) > 0.2]))
r_ <- m+n-r
(k <- sum(subset(obs,Treat==3)$rangs >= r))

qtest$statistic
  
muk <- (m*r_)/(m+n)
sigma2k <- (m*n*r_*(m+n-r_))/((m+n)^2*(m+n-1))

(papprox <- 1 - pnorm((k-muk-(1/2))/sqrt(sigma2k),0,1))
  
qtest$p.value
  
vect <- as.data.frame(c(k:r_))
func <- function(i){
  (choose(m+n-r_,m-i)*choose(r_,i))/choose(m+n,n)
}

apply(vect,1,func)
(p <- sum(apply(vect,1,func)))
