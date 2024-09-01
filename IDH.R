#######Load the data###########################################
library(readxl)
idh <- read_excel("IDH.xlsx")

#######Mahalanobis distance###########################################
idh.matrix <- as.matrix(cbind(idh$IFHR, idh$IDHL, idh$IDHE))
DM <- mahalanobis(idh.matrix, colMeans(idh.matrix), var(idh.matrix))
PD_M <- 1-pchisq(DM, dim(idh.matrix)[2])
#######Euclidian distance###########################################
DE <- mahalanobis(idh.matrix, colMeans(idh.matrix), diag(dim(idh.matrix)[2]))

Tab<- cbind(sqrt(DM), PD_M, sqrt(DE))
Tab<- cbind(idh$Nome, Tab)
colnames(Tab) <- c("Municipio", "DM", "Prob", "DE")
Tab

#### FUNCTION: OUTLIER MULT ########################################
#Determina as raizes das dm e de. Calcula também a prob de que a dm >= valor observado, admitindo normalidade multivariada e n grande:

OutlierMult <-function(x){
  d <- dim(x)
  dm <- matrix(0, d[1])
  Pv <- matrix(0, d[1])
  de <- matrix(0, d[1])
  Ident <- diag(d[2])
  
  for (i in 1:d[1]){
    dm[i] <- mahalanobis(t(x[i,]), colMeans(x[-i,]), var(x[-i,]))
    Pv[i] <- 1-pchisq(dm[i], d[2])
    de[i] <- mahalanobis(t(x[i,]), colMeans(x[-i,]), Ident)
  }
  
  de <- sqrt(de)
  dm <- sqrt(dm)
  observacao <- 1:d[1]
  
  Mteste <- as.data.frame(cbind(observacao, dm, Pv, de))
  names(Mteste) <- c("Obs", "Mahalanobis", "Probabilidade", "Euclidiana")
  
  Mteste
}

saida <- OutlierMult(idh.matrix)
ID <- as.vector(cbind(idh$Nome))
resultado <- cbind(ID, saida)
resultado
ordem<- order(resultado$Mahalanobis, decreasing = TRUE)
resultado[ordem,]

#### Biplot ########################################
library(MVar.pt)
crime.matrix
crime.df <- as.data.frame(crime.matrix)
Biplot(crime.df, linlab = crime$Regiao)














