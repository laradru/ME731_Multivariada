##############################################################################################################
# Instalação de pacotes
##############################################################################################################
install.packages("tidyverse")
install.packages("knitr")
install.packages("GGally")
install.packages("biotools")

##############################################################################################################
# Rodando os pacotes
##############################################################################################################
library(tidyverse)
library(knitr)
library(GGally)
library(biotools)
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
medidas.resumo <- psychology %>% select(c(exper, stats, social, develop, person, group)) %>% 
  group_by(group) %>%
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
medidas.resumo.expert <- psychology %>% select(c(exper, group)) %>% 
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

medidas.resumo.stats <- psychology %>% select(c(stats, group)) %>% 
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

medidas.resumo.social <- psychology %>% select(c(social, group)) %>% 
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

medidas.resumo.develop <- psychology %>% select(c(develop, group)) %>% 
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

medidas.resumo.person <- psychology %>% select(c(person, group)) %>% 
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



#######Total and Generalized Variances###########################################
#total:
sum(diag(cov.crime))
#generalized:
det(cov.crime)

#######Correlation Matrix#######################################################
cor.crime<-cor(crime.matrix)
round(cor.crime,2)



















#Matriz de covariâncias




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







#Os helicpteros de seda , independente da presenca de clipes, tiveram os tempos mais variados em relacao aos helicopteros de outros papeis:
psychology %>% ggplot(aes(x = papel, y = tempo, fill = papel)) + 
  geom_boxplot(show.legend = FALSE) +
  facet_wrap(~ clips) +
  labs(x = "Tipo de papel", y = "Tempo médio de queda (em segundos)") +
  theme_bw()

#Gráfico de dispersão:
dados %>% ggplot(aes(x = papel, y = tempo, color = papel)) + 
  geom_point(show.legend = FALSE) +
  facet_wrap(~ clips) +
  labs(x = "Tipo de papel", y = "Tempo médio de queda (em segundos)") +
  theme_bw()

# Gráfico de interação:
medias <- dados %>% group_by(clips, papel) %>% summarise(media_tempo = mean(tempo))

dados %>% ggplot(aes(x = papel, y = tempo)) +
  geom_point() +
  geom_line(data = medias, aes(x = papel, y = media_tempo, group = clips, color = clips)) +
  labs(x = "Tipo de papel", y = "Tempo médio de queda (em segundos)", color = "") +
  theme_bw()