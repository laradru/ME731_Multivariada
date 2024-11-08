##############################################################################################################
# Instalação de pacotes
##############################################################################################################
install.packages("tidyverse")
install.packages("dplyr")
install.packages("knitr")
install.packages("GGally")
install.packages("biotools")
install.packages("mvoutlier")
install.packages("reshape")
install.packages("rrcov")
install.packages("robustbase")
##############################################################################################################
# Rodando os pacotes
##############################################################################################################
library(tidyverse)
library(knitr)
library(GGally)
library(biotools)
library(mvoutlier)
library(dplyr)
library(reshape)
library(multcomp)
library(rrcov)
library(robustbase)
##############################################################################################################
# Importação dos dados 
##############################################################################################################
psychology <- data.frame(group = as.factor(c(rep(0, times = 11), rep(1, times = 16), rep(2, times = 13))), 
                      exper = c(4,10,6,6,4,7,3,8,6,3,5,6,6,5,2,8,6,5,5,8,4,5,4,7,4,7,6,5,10,7,6,5,9,10,6,6,9,5,9,4),
                      stats = c(12,15,5,6,5,8,2,9,7,6,8,10,6,11,4,7,6,8,10,10,9,10,9,10,7,14,8,11,4,13,8,12,11,9,9,14,12,14,13,6),
                      social = c(12,14,7,9,9,10,7,14,10,14,8,8,8,13,7,5,5,7,6,12,10,11,10,7,8,14,6,6,8,8,10,10,11,9,7,8,9,8,12,8),
                      develop = c(12,14,8,8,13,9,9,15,10,14,9,10,8,10,7,9,7,8,8,12,8,9,9,7,9,13,8,9,5,8,8,14,12,12,9,8,9,6,11,3),
                      person = c(11,13,12,6,13,10,6,15,9,15,7,12,8,11,7,7,7,6,10,10,7,10,8,8,5,11,8,10,9,12,9,10,10,9,7,4,5,7,11,6))
levels(psychology$group) <- c("Year1", "Year2", "Year3")
psychology

##############################################################################################################
# Análise Exploratória e Descritiva dos dados
##############################################################################################################
#Medidas de resumo
medidas.resumo <- psychology %>% dplyr::select(exper, stats, social, develop, person, group) %>% group_by(group) %>%
  summarise(across(c(exper, stats, social, develop, person), 
            list(media = ~ mean(.x, na.rm = TRUE),
                 variancia = ~ var(.x, na.rm = TRUE),
                 desvio.padrao = ~ sd(.x, na.rm = TRUE),
                 mediana = ~ median(.x, na.rm = TRUE),
                 minimo = ~ min(.x, na.rm = TRUE),
                 maximo = ~ max(.x, na.rm = TRUE),
                 coef.variacao = ~ (sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE)) * 100),
            .names = "{.col}_{.fn}"))
medidas.resumo

#ou

medidas.resumo.expert <- psychology %>% dplyr::select(exper, group) %>% 
  group_by(group) %>%
  summarise(across(c(exper), 
                   list(n = ~ n(),
                        media = ~ mean(.x, na.rm = TRUE),
                        variancia = ~ var(.x, na.rm = TRUE),
                        desvio.padrao = ~ sd(.x, na.rm = TRUE),
                        mediana = ~ median(.x, na.rm = TRUE),
                        minimo = ~ min(.x, na.rm = TRUE),
                        maximo = ~ max(.x, na.rm = TRUE),
                        coef.variacao = ~ (sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE)) * 100),
                   .names = "{.col}_{.fn}"))
medidas.resumo.expert

medidas.resumo.stats <- psychology %>% dplyr::select(stats, group) %>% 
  group_by(group) %>%
  summarise(across(c(stats), 
                   list(n = ~ n(),
                        media = ~ mean(.x, na.rm = TRUE),
                        variancia = ~ var(.x, na.rm = TRUE),
                        desvio.padrao = ~ sd(.x, na.rm = TRUE),
                        mediana = ~ median(.x, na.rm = TRUE),
                        minimo = ~ min(.x, na.rm = TRUE),
                        maximo = ~ max(.x, na.rm = TRUE),
                        coef.variacao = ~ (sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE)) * 100),
                   .names = "{.col}_{.fn}"))
medidas.resumo.stats

medidas.resumo.social <- psychology %>% dplyr::select(social, group) %>% 
  group_by(group) %>%
  summarise(across(c(social), 
                   list(n = ~ n(),
                        media = ~ mean(.x, na.rm = TRUE),
                        variancia = ~ var(.x, na.rm = TRUE),
                        desvio.padrao = ~ sd(.x, na.rm = TRUE),
                        mediana = ~ median(.x, na.rm = TRUE),
                        minimo = ~ min(.x, na.rm = TRUE),
                        maximo = ~ max(.x, na.rm = TRUE),
                        coef.variacao = ~ (sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE)) * 100),
                   .names = "{.col}_{.fn}"))
medidas.resumo.social

medidas.resumo.develop <- psychology %>% dplyr::select(develop, group) %>% 
  group_by(group) %>%
  summarise(across(c(develop), 
                   list(n = ~ n(),
                        media = ~ mean(.x, na.rm = TRUE),
                        variancia = ~ var(.x, na.rm = TRUE),
                        desvio.padrao = ~ sd(.x, na.rm = TRUE),
                        mediana = ~ median(.x, na.rm = TRUE),
                        minimo = ~ min(.x, na.rm = TRUE),
                        maximo = ~ max(.x, na.rm = TRUE),
                        coef.variacao = ~ (sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE)) * 100),
                   .names = "{.col}_{.fn}"))
medidas.resumo.develop

medidas.resumo.person <- psychology %>% dplyr::select(person, group) %>% 
  group_by(group) %>%
  summarise(across(c(person), 
                   list(n = ~ n(),
                        media = ~ mean(.x, na.rm = TRUE),
                        variancia = ~ var(.x, na.rm = TRUE),
                        desvio.padrao = ~ sd(.x, na.rm = TRUE),
                        mediana = ~ median(.x, na.rm = TRUE),
                        minimo = ~ min(.x, na.rm = TRUE),
                        maximo = ~ max(.x, na.rm = TRUE),
                        coef.variacao = ~ (sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE)) * 100),
                   .names = "{.col}_{.fn}"))
medidas.resumo.person



#Boxplots:
psychology %>% ggplot(aes(x = group, y = exper)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x = "Grupo", y = "Psicologia Experimental") +
  scale_x_discrete(labels = c("Year1" = "Estudantes 1ano", "Year2" = "Estudantes 2ano", "Year3" = "Estudantes 3ano")) +
  theme_bw()
psychology %>% ggplot(aes(x = group, y = stats)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x = "Grupo", y = "Estatística em Psicologia") +
  scale_x_discrete(labels = c("Year1" = "Estudantes 1ano", "Year2" = "Estudantes 2ano", "Year3" = "Estudantes 3ano")) +
  theme_bw()
psychology %>% ggplot(aes(x = group, y = social)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x = "Grupo", y = "Psicologia Social") +
  scale_x_discrete(labels = c("Year1" = "Estudantes 1ano", "Year2" = "Estudantes 2ano", "Year3" = "Estudantes 3ano")) +
  theme_bw()
psychology %>% ggplot(aes(x = group, y = develop)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x = "Grupo", y = "Psicologia do Desenvolvimento") +
  scale_x_discrete(labels = c("Year1" = "Estudantes 1ano", "Year2" = "Estudantes 2ano", "Year3" = "Estudantes 3ano")) +
  theme_bw()
psychology %>% ggplot(aes(x = group, y = person)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x = "Grupo", y = "Personalidade") +
  scale_x_discrete(labels = c("Year1" = "Estudantes 1ano", "Year2" = "Estudantes 2ano", "Year3" = "Estudantes 3ano")) +
  theme_bw()

#Explorando a relação entre as variáveis respostas por grupo:
ggpairs(psychology, columns = 2:6, aes(color = group, alpha = 0.6)) +
  theme_minimal() +
  labs(title = "Gráficos de Dispersão entre Variáveis de interesse por Grupo")


#Explorando a existência de outliers na análise multivariada
aq.plot(psychology[, 2:6])

##############################################################################################################
# Pressupostos
##############################################################################################################
##############Obtenção da Covariance Matrix##########################################
by(psychology[,2:6], psychology$group, cov)
matrizes.cov <- by(round(psychology[, 2:6], 2), psychology$group, function(x) round(cov(x), 2)) #deixando com 2 casas decimais
matrizes.cov

##############Determinante da Covariance Matrix##########################################
by(round(psychology[, 2:6], 2), psychology$group, function(x) round(det(cov(x)), 2))

##############Box's M-Test para igualdade das matrizes de variâncias e covariâncias##############################
#H0:as matrizes são iguais
#H1:as matrizes são diferentes
#Nível de significância adotado: 5%
result <- boxM(data = psychology[, 2:6], grouping = psychology$group)
print(result)

qchisq(p = 0.95, df = 30) # chiquadrado crítico


##############Avaliação da normalidade multivariada##############################
psychology.g1 <- psychology %>% filter(group == "Year1") 
psychology.g1 <-as.data.frame(cbind(psychology.g1$exper,psychology.g1$stats, psychology.g1$social, psychology.g1$develop, psychology.g1$person))
media.psychology.g1 <- colMeans(psychology.g1)
var.psychology.g1 <- var(psychology.g1)

g1.mah <- as.data.frame(mahalanobis(psychology.g1, media.psychology.g1, var.psychology.g1))
names(g1.mah) <- c("Mahalabonis")
g1.mah
ordem.g1 <- order(g1.mah$Mahalabonis, decreasing = FALSE)
g1.mah <- g1.mah[ordem.g1,]
g1.mah
n1 <- 11
Perc.1 <- seq((1 - 0.5) / n1, (n1 - 0.5) / n1, length.out = n1)
QQ1 <-qchisq(Perc.1, dim(psychology.g1)[2])
TabQQ1 <-as.data.frame(cbind(g1.mah, Perc.1, QQ1))
plot(TabQQ1$QQ1 ~ TabQQ1$g1.mah, pch=16, xlab = "Distância de Mahalanobis ao quadrado", ylab = "Q")
abline(0,1)





psychology.g2 <- psychology %>% filter(group == "Year2") 
psychology.g2 <-as.data.frame(cbind(psychology.g2$exper,psychology.g2$stats, psychology.g2$social, psychology.g2$develop, psychology.g2$person))
media.psychology.g2 <- colMeans(psychology.g2)
var.psychology.g2 <- var(psychology.g2)

g2.mah <- as.data.frame(mahalanobis(psychology.g2, media.psychology.g2, var.psychology.g2))
names(g2.mah) <- c("Mahalabonis")
g2.mah
ordem.g2 <- order(g2.mah$Mahalabonis, decreasing = FALSE)
g2.mah <- g2.mah[ordem.g2,]
g2.mah
n2 <- 16
Perc.2 <- seq((1 - 0.5) / n2, (n2 - 0.5) / n2, length.out = n2)
QQ2 <-qchisq(Perc.2, dim(psychology.g2)[2])
TabQQ2 <-as.data.frame(cbind(g2.mah, Perc.2, QQ2))
plot(TabQQ2$QQ2 ~ TabQQ2$g2.mah, pch=16, xlab = "Distância de Mahalanobis ao quadrado", ylab = "Q")
abline(0,1)




psychology.g3 <- psychology %>% filter(group == "Year3") 
psychology.g3 <-as.data.frame(cbind(psychology.g3$exper,psychology.g3$stats, psychology.g3$social, psychology.g3$develop, psychology.g3$person))
media.psychology.g3 <- colMeans(psychology.g3)
var.psychology.g3 <- var(psychology.g3)

g3.mah <- as.data.frame(mahalanobis(psychology.g3, media.psychology.g3, var.psychology.g3))
names(g3.mah) <- c("Mahalabonis")
g3.mah
ordem.g3 <- order(g3.mah$Mahalabonis, decreasing = FALSE)
g3.mah <- g3.mah[ordem.g3,]
g3.mah
n3 <- 13
Perc.3 <- seq((1 - 0.5) / n3, (n3 - 0.5) / n3, length.out = n3)
QQ3 <-qchisq(Perc.3, dim(psychology.g3)[2])
TabQQ3 <-as.data.frame(cbind(g3.mah, Perc.3, QQ3))
plot(TabQQ3$QQ3 ~ TabQQ3$g3.mah, pch=16, xlab = "Distância de Mahalanobis ao quadrado", ylab = "Q")
abline(0,1)

##############################################################################################################
# MANOVA ROBUSTA
##############################################################################################################
# Reorganizando os dados
psychology$row <- rep(c(1:11, 1:16, 1:13))
psychology.long <- psychology %>% pivot_longer(cols = 2:6, 
                                               names_to = "variaveis", 
                                               values_to = "scores")
psychology.robusta <- cast(psychology.long, row ~ group + variaveis, value = "scores")
psychology.robusta$row <- NULL

##################
# The cmanova()function in R (Wilcox, 2005) Código disponível em: https://osf.io/spvzc:
##################


cmanova<-function(J,K,x,grp=c(1:JK),JK=J*K){
  #
  # Perform the Choi and Marden
  # multivariate one-way rank-based ANOVA
  # (Choi and Marden, JASA, 1997, 92, 1581-1590.
  #
  # x can be a matrix with columns corresponding to groups
  # or it can have list mode.
  #
  # Have a J by K design with J independent levels and K dependent
  # measures
  #
  #
  x=drop_na(x)
  if(is.matrix(x))x<-listm(x)
  xx<-list()
  nvec<-NA
  jk<-0
  for(j in 1:J){
    for(k in 1:K){
      jk<-jk+1
      xx[[jk]]<-x[[grp[jk]]]
      if(k==1)nvec[j]<-length(xx[[jk]])
    }}
  N<-sum(nvec)
  RVALL<-matrix(0,nrow=N,K)
  x<-xx
  jk<-0
  rmean<-matrix(NA,nrow=J,ncol=K)
  for(j in 1:J){
    RV<-matrix(0,nrow=nvec[j],ncol=K)
    jk<-jk+1
    temp1<-matrix(x[[jk]],ncol=1)
    for(k in 2:K){
      jk<-jk+1
      temp1<-cbind(temp1,x[[jk]])
    }
    X<-temp1
    if(j==1)XALL<-X
    if(j>1)XALL<-rbind(XALL,X)
    n<-nvec[j]
    for(i in 1:n){
      for (ii in 1:n){
        temp3<-sqrt(sum((X[i,]-X[ii,])^2))
        if(temp3 != 0)RV[i,]<-RV[i,]+(X[i,]-X[ii,])/temp3
      }
      RV[i,]<-RV[i,]/nvec[j]
      if(j==1 && i==1)sighat<-RV[i,]%*%t(RV[i,])
      if(j>1 || i>1)sighat<-sighat+RV[i,]%*%t(RV[i,])
    }
  }
  # Assign ranks to pooled data and compute R bar for each group
  for(i in 1:N){
    for (ii in 1:N){
      temp3<-sqrt(sum((XALL[i,]-XALL[ii,])^2))
      if(temp3 != 0)RVALL[i,]<-RVALL[i,]+(XALL[i,]-XALL[ii,])/temp3
    }
    RVALL[i,]<-RVALL[i,]/N
  }
  bot<-1-nvec[1]
  top<-0
  for(j in 1:J){
    bot<-bot+nvec[j]
    top<-top+nvec[j]
    flag<-c(bot:top)
    rmean[j,]<-apply(RVALL[flag,],2,mean)
  }
  sighat<-sighat/(N-J)
  shatinv<-solve(sighat)
  KW<-0
  for(j in 1:J){
    KW<-KW+nvec[j]*t(rmean[j,])%*%shatinv%*%rmean[j,]
  }
  df<-K*(J-1)
  sig.level<-1-pchisq(KW,df)
  list(test.stat=KW[1,1],df=df,p.value=sig.level)
}

cmanova(3, 5, psychology.robusta)


##################
# Alternativa: The mulrank()function in R (Wilcox, 2005). Código disponível em: https://osf.io/spvzc:
##################

mulrank<-function(J,K,x,grp=c(1:p),p=J*K){
  #
  # Perform the Munzel and Brunner
  # multivariate one-way rank-based ANOVA
  # (Munzel and Brunner, Biometrical J., 2000, 42, 837--854
  #
  # x can be a matrix with columns corresponding to groups
  #
  # Have a J by K design with J independent levels and K dependent
  # measures
  #
  # or it can have list mode.
  #
  newx=list()
  GV=matrix(c(1:p),ncol=K,byrow=TRUE)
  if(is.list(x)){
    temp=NA
    jk=0
    for(j in 1:J){
      temp=elimna(matl(x[GV[j,]]))
      for(k in 1:K){
        jk=jk+1
        newx[[jk]]=temp[,k]
      }}
    x=NA
    x=newx
  }
  if(is.matrix(x)){
    x=elimna(x)
    x<-listm(x)
  }
  xx<-list()
  nvec<-NA
  for(j in 1:p){
    xx[[j]]<-x[[grp[j]]]
    nvec[j]<-length(xx[[j]])
  }
  Nrow=nvec[GV[,1]]
  v<-matrix(0,p,p)
  Ja<-matrix(1,J,J)
  Ia<-diag(1,J)
  Pa<-Ia-Ja/J
  Jb<-matrix(1,K,K)
  Ib<-diag(1,K)
  Pb<-Ib-Jb/K
  cona<-kron(Pa,Ib)
  xr<-list()
  N<-0
  jj=0
  for(k in 1:K){
    temp<-x[[k]]
    jk<-k
    for (j in 2:J){
      jj=jj+1
      jk<-jk+K
      temp<-c(temp,x[[jk]])
    }
    N<-length(temp)
    pr<-rank(temp)
    xr[[k]]<-pr[1:nvec[k]] #Put ranks of pooled data for first
    #                       variable in xr
    top<-nvec[k]
    jk<-k
    bot<-1
    for (j in 2:J){
      jk<-jk+K
      bot<-bot+nvec[jk]
      top<-top+nvec[jk]
      xr[[jk]]<-pr[bot:top] # Put midranks in xr
    }}
  phat<-NA
  botk<-0
  for(j in 1:J){
    for(k in 1:K){
      botk<-botk+1
      phat[botk]<-(mean(xr[[botk]])-.5)/N
    }}
  klow<-1-K
  kup<-0
  for(j in 1:J){
    klow<-klow+K
    kup<-kup+K
    sel<-c(klow:kup)
    v[sel,sel]<-covmtrim(xr[klow:kup],tr=0)/N
  }
  qhat<-matrix(phat,J,K,byrow=TRUE)
  test<-N*t(phat)%*%cona%*%phat/sum(diag(cona%*%v))
  nu1<-sum(diag(cona%*%v))^2/sum(diag(cona%*%v%*%cona%*%v))
  sig.level<-1-pf(test,nu1,1000000)
  list(test.stat=test[1,1],nu1=nu1,p.value=sig.level,N=N,q.hat=qhat)
}
mulrank(3, 5, psychology.robusta)
