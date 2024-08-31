#######Packages#################################################
install.packages("readxl")
install.packages("corrplot")
install.packages("aplpack")
install.packages("fmsb")
install.packages("MVar.pt")
install.packages("tidyverse")
#######Load the data###########################################
library(readxl)
DD <- read_excel("DDMomentos.xlsx")
#######Scatter plot###########################################
Med_X1<-mean(DD$X1)
Med_X2<-mean(DD$X2)
Med_Y1<-mean(DD$Y1)
Med_Y2<-mean(DD$Y1)
par(mfrow = c(1,2))
plot(DD$X2~DD$X1, main="A", xlab="X1", ylab="X2")
points(Med_X2~Med_X1, col="blue", pch=18, cex=2)
plot(DD$Y2~DD$Y1, main="B", xlab="Y1", ylab="Y2")
points(Med_Y2~Med_Y1, col="blue", pch=18, cex=2)
#######Covariance Matrix###########################################
