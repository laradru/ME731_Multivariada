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
#######Covariance Matrix########################################################
MDDX<-as.matrix(cbind(DD$X1,DD$X2)) #matriz com colunas X1 e X2, somente
colnames(MDDX) <- c("X1", "X2") #renomeando as colunas
DDCovX<-cov(MDDX) #Matriz de variâncias e covariâncias entre X1 e X2
DDCovX
round(DDCovX, 2)
#######Total and Generalized Variances###########################################
#total:
sum(diag(DDCovX)) #traço da matriz de var-cov é a soma das diagonais da matriz (no caso, as variâncias de X1, X2)
#generalized:
det(DDCovX)
#######Correlation Matrix#######################################################
DDCor<-cor(MDDX)
round(DDCor,2)




