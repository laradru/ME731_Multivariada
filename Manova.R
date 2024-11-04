##############################################################################################################
# Instalação de pacotes
##############################################################################################################
install.packages("tidyverse")
install.packages("knitr")
install.packages("GGally")

##############################################################################################################
# Rodando os pacotes
##############################################################################################################
library(tidyverse)
library(knitr)
library(GGally)
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
# Análise Exploratória dos dados
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
  theme_bw()
psychology %>% ggplot(aes(x = group, y = stats)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x = "Grupo", y = "Estatística em Psicologia") +
  theme_bw()
psychology %>% ggplot(aes(x = group, y = social)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x = "Grupo", y = "Psicologia Social") +
  theme_bw()
psychology %>% ggplot(aes(x = group, y = develop)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x = "Grupo", y = "Psicologia do Desenvolvimento") +
  theme_bw()
psychology %>% ggplot(aes(x = group, y = person)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(x = "Grupo", y = "Personalidade") +
  theme_bw()



#Explorando a relação entre as variáveis respostas por grupo:
ggpairs(psychology, columns = 2:6, aes(color = group, alpha = 0.6)) +
  theme_minimal() +
  labs(title = "Gráficos de Dispersão entre Variáveis por Grupo")














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