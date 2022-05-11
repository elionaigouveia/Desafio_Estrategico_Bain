################################################################################
############################ Importando pacotes ################################
################################################################################
rm(list=ls())
library(forecast)    # fazer previsões de series temporais
library(lubridate)   # converter datas
#library("mlbench")
#library('RSNNS')
#library("corpcor")
#library("kernlab")
library(readxl)      # ler arquivo excel
library(plyr)        # ordenar a coluna do dataframe
library(xts)
library(dplyr)
library(ggplot2)
#library(plyr)        # ordenar a coluna do dataframe
#library(xts)
#library(ggplot2)


################################################################################
############################ Importando os dados ###############################
################################################################################
agriculture <- read_excel("agriculture.xlsx") #Ler o arquivo do excel
head(agriculture) #visualizar alguns dados
str(agriculture) # vizualizar como os dados estão sendo armazenados
#aqui os dados NaN foram excluídos
agriculture <- data.frame(agriculture[complete.cases(agriculture),]) #limpar NA e transformar em data frame
colnames(agriculture) <-c("Ano", "Cidade", "Tipo_de_Produto", "Produto", "Area") #atribuicao de nomes das colunas
agriculture <- agriculture[order(agriculture$Ano, agriculture$Cidade, agriculture$Tipo_de_Produto, agriculture$Produto, decreasing=c(FALSE, FALSE, TRUE,TRUE)), ]


#pontos a melhorar
#1- não retirar os dados NaN, mas substituí-los pela média 

################################################################################
################### filtro para cidades, produtos e tipos ######################
################################################################################
table(agriculture$Cidade)
(cidades <- unique(agriculture$Cidade))

table(agriculture$Produto)
(produtos <- unique(agriculture$Produto))

table(agriculture$`Tipo_de_Produto`)
(tipo <- unique(agriculture$`Tipo_de_Produto`))

x <- nrow(subset(agriculture, agriculture$Cidade==cidades[1] & 
              agriculture$Produto==produtos[1] &
              agriculture$`Tipo_de_Produto`==tipo[1]))

################################################################################
############################## Seperação dos dados ############################# 
################################################################################
# Estrutura de repeticao para separar todas as series. São armazenada se for > 1
for(j in 1:length(cidades)){
  for(i in 1:length(produtos)){
    for(p in 1:length(tipo)){
      if(nrow(subset(agriculture, agriculture$Cidade==cidades[j] & 
                     agriculture$Produto==produtos[i] &
                     agriculture$`Tipo_de_Produto`==tipo[p])) >= 1){
        assign(paste0("Cidade", cidades[j],"Tipo",tipo[p], "Produto", produtos[i]),
               subset(agriculture, agriculture$Cidade==cidades[j] & 
                        agriculture$Produto==produtos[i] &
                        agriculture$`Tipo_de_Produto`== tipo[p]))
      }
    }
  }
}



#plot de uma serie qualquer
plot(Cidade07ce2e79e19977e9TipotemporaryProdutoOthers[,5],type='l', ylab="Area")
boxplot(Cidade0307883b5d063703TipotemporaryProdutoOthers[,5])

################################################################################
############################# Modelos Preditivos ###############################
################################################################################
dat <- Cidadebe4424aa54f8d2aeTipotemporaryProdutoSoy
ord.dat <- dat[order(dat$Ano),]
ts_dat = ts(ord.dat$Area, start=2010, end=2017)
treino = window(ts_dat, start=2010, end=2015)
teste = as.data.frame(window(ts_dat, start=2016, end=2017))

modelo_reg_mult = tslm(treino ~ trend, data = treino)

Prev1 = forecast(modelo_reg_mult, h = 2)
summary(Prev1)
#compara os dois
plot(ts_dat)
lines(Prev1$mean, col="red")
###################### mesma tabela - 2016 - 2017 ##############################
dat <- Cidadebe4424aa54f8d2aeTipotemporaryProdutoSoy
dat <- dat[order(dat$Ano, decreasing = T),][1:4,]
ord.dat1 <- dat1[order(dat1$Ano),]
ord.dat <- dat[order(dat$Ano),]
ts_dat = ts(ord.dat$Area, start=2010, end=2017)
treino = window(ts_dat, start=2010, end=2015)
teste = as.data.frame(window(ts_dat, start=2016, end=2017))

modelo_reg_mult = tslm(treino ~ trend, data = treino)

Prev1 = forecast(modelo_reg_mult, h = 2)
summary(Prev1)
#compara os dois
plot(ts_dat)
lines(Prev1$mean, col="red")
################################################################################
library(data.table)
dat1 <- Cidadeab984377b2dc0284TipotemporaryProdutoCassava
dat1 <- dat1[order(dat1$Ano, decreasing = T),][1:4,]
ord.dat1 <- dat1[order(dat1$Ano),]


ts_dat1 = ts(ord.dat1$Area, start=2014,  end= 2017)
treino = window(ts_dat1, end=2015)
teste = as.data.frame(window(ts_dat1, start=2016))


mod_reg_mult_dats = tslm(treino ~ trend , data = treino)
Prev1 = forecast(modelo_reg_mult_1, h = 2)
summary(Prev1)
plot(ts_dat1)
lines(Prev1$mean, col="red")

#modelo de ets
dats <- Cidadeab984377b2dc0284TipotemporaryProdutoCassava
dats <- dats[order(dats$Ano, decreasing = T),][1:4,]
dats <- dats[order(dats$Ano, decreasing = F),][1:2,]
#ord.dats <- dats[order(dats$Ano),]

ets_dats = ts(dats$Area, start=2014,  end= 2015)
m_ets_dats = ets(ets_dats ) #modelo de ets
p_ets_dats = forecast(m_ets_dats, h=2) # previsão para 2016,2017
summary(p_ets_dats)

#modelo de suavização exponencial simples
dats <- Cidadeab984377b2dc0284TipotemporaryProdutoCassava
dats <- dats[order(dats$Ano, decreasing = T),][1:4,]
dats <- dats[order(dats$Ano, decreasing = F),][1:2,]
#ord.dats <- dats[order(dats$Ano),]

es_dats = ts(dats$Area, start=2014,  end= 2015)
m_es_dats = window(es_dats, start= 2014)
p_es_dats = ses(es_dats, h = 2) # Previsao para os próximos 2 anos
summary(p_es_dats)

#modelo de Holt e holt com damp
dats <- Cidadeab984377b2dc0284TipotemporaryProdutoCassava
dats <- dats[order(dats$Ano, decreasing = T),][1:4,]
dats <- dats[order(dats$Ano, decreasing = F),][1:2,]
#ord.dats <- dats[order(dats$Ano),]

holt_dats = ts(dats$Area, start=2014,  end= 2015)
m_holt_dats = window(holt_dats, start= 2014)
p_holt_dats = holt(m_holt_dats, h = 2) 
summary(p_holt_dats)

p_holt_1 = holt(m_holt_dats, h = 2, PI = F)
summary(p_holt_dats)

p_holt_damp = holt(m_holt_dats, h = 2, damped = T, PI = F)
summary(p_holt_dats)

#modelo naive
dat1 <- Cidadeab984377b2dc0284TipotemporaryProdutoCassava
dat1 <- dat1[order(dat1$Ano, decreasing = T),][1:4,]
ord.dat1 <- dat1[order(dat1$Ano),]


ts_dat1 = ts(ord.dat1$Area, start=2014,  end= 2017)
treino = window(ts_dat1, end=2015)
teste = as.data.frame(window(ts_dat1, start=2016))
previsao_naive = naive(treino,h = 2)
summary(previsao_naive)

autoplot(previsao_naive) + 
  ylab("Área destina ao agronegócio") + 
  autolayer(teste, series = "Dados de Teste")

accuracy(previsao_naive, teste) #qual pacote

################################################################################
############################### Exportando dados ###############################
################################################################################



# fazer uma fç que analisa tres modelos prevê o menor erro e aplica





