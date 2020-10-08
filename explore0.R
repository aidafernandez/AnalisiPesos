library(readxl)
pes.ind <- read_excel("C:/Users/farre/Desktop/PINSO/Nedra_pesades/pesos individuales.xlsx",
                      range = "A1:D949")
# View(pes_ind)
# names(pes.ind)
# dim(pes.ind)

pes.ind$difBW41_28   <- pes.ind$BW41-pes.ind$BW28                       # guany de pes
pes.ind$indexBW41_28 <- 100*pes.ind$BW41/pes.ind$BW28                   # index increm de pes
pes.ind$taxBW41_28   <- 100*(pes.ind$BW41-pes.ind$BW28)/pes.ind$BW28    # taxa increm de pes

boxplot(BW28~Treat,data=pes.ind,main="BW28 by Treatment")
boxplot(BW41~Treat,data=pes.ind,main="BW41 by Treatment")
boxplot(difBW41_28~Treat,data=pes.ind,main="Difference BW41 w.r. BW28 by Treatment")
boxplot(indexBW41_28~Treat,data=pes.ind,main="Index BW41 w.r. BW28 by Treatment")
boxplot(taxBW41_28~Treat,data=pes.ind,main="Tax BW41 w.r. BW28 by Treatment")

## detectem i eliminem outlier:
out<-which.max(pes.ind$taxBW41_28)
pes.ind<-pes.ind[-out,]
# dim(pes.ind)
## repetim plots sense outlier

boxplot(BW28~Treat,data=pes.ind,main="BW28 by Treatment")
boxplot(BW41~Treat,data=pes.ind,main="BW41 by Treatment")
boxplot(difBW41_28~Treat,data=pes.ind,main="Difference BW41 w.r. BW28 by Treatment")
boxplot(indexBW41_28~Treat,data=pes.ind,main="Index BW41 w.r. BW28 by Treatment")
boxplot(taxBW41_28~Treat,data=pes.ind,main="Tax BW41 w.r. BW28 by Treatment")

## METHODS to be applied
# quantile evaluation, quantiles test
# R package: snpar 
# skew-normal distribution:  alpha <0 long left tail, alpha=0 Gaussian, alpha >0 right tail
# skew-t distribution 
# symmetry
# kurtosis (heavy tails)
# risks: probabilities of animals under given thresholds, comparison or attributable risks

## New idea: cost-effectiveness  !!!
## simulations ?? 

## layout d'histogrames
# treat 1 over treat j, forall j: 2,...,8 






