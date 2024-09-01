#######Load the data###########################################
library(readxl)
crime <- read_excel("DadosCrime.xlsx")

#######Observation matrix######################################
crime.matrix<-as.matrix(cbind(crime$HD, crime$F,crime$R, crime$RF))
colnames(crime.matrix) <- c("HD", "F", "R", "RF")

#######Vector of means######################################
media_HD <- mean(crime$HD)
media_F <- mean(crime$F)
media_R <- mean(crime$R)
media_RF <- mean(crime$RF)

media<- as.vector(c(media_HD, media_F, media_R, media_RF))
round(media,2)

##############Covariance Matrix##########################################
cov.crime<-cov(crime.matrix) 
round(cov.crime, 2)

#######Total and Generalized Variances###########################################
#total:
sum(diag(cov.crime))
#generalized:
det(cov.crime)

#######Correlation Matrix#######################################################
cor.crime<-cor(crime.matrix)
round(cor.crime,2)

#######Plots#######################################################
#Gráfico de calor
library(corrplot)
corrplot(cor.crime, method = "ellipse")

#Faces de Chernoff (com data.frame - a ordem das variáveis importa: está relacionada as características da face)
library(aplpack)
crime1 <- crime[, 2:5]
faces(crime1, labels = crime$Regiao)

crime2 <- as.data.frame(cbind(crime$F, crime$RF, crime$HD, crime$R))
colnames(crime2) <- c("F", "RF", "HD", "R")
row.names(crime$Regiao)

faces(crime2, labels = crime$Regiao)

#Gráfico de radar
stars(crime1, key.loc = c(9,6), flip.labels = FALSE, labels = crime$Regiao)




