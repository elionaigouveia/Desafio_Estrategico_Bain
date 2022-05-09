################################################################################
############################ Importando pacotes ################################
################################################################################
rm(list=ls()) # para que serve??
library(forecast)  # fazer previsões de series temporais
library(lubridate)   #converter datas
#library("mlbench")
#library('RSNNS')
#library("corpcor")
#library("kernlab")
library(readxl) # ler arquivo excel
library(plyr)   # ordenar a coluna do dataframe
library(xts)
library(dplyr)

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

nrow(subset(agriculture, agriculture$Cidade==cidades[1] & 
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
#modelo preditivo para o primeiro dataframe
#modelo preditivo usando regressão linear e arvore de decisão
c01 <- Cidade0307883b5d063703TipopastureProdutoLivestock

train_data = c01[c01$Ano <= 2015-01-01,];
test_data = c01[c01$Ano > 2015-01-01,];

#Use the Area column to check the quality of the prediction against actual values
actual_area <- test_data$Ano;

#Model 1: Use lm to create a linear regression model, trained with the training data set
model_lm <- lm(c01$Area ~  c01$Ano , data = train_data);

#Model 2: Use rpart to create a decision tree model, trained with the training data set
library(rpart);
model_rpart  <- rpart(c01$Area ~ c01$Ano , data = train_data);
model_rpart  <- rpart(c01$Area ~ c01$Ano + $Cidade + c01$Tipo_de_Produto + c01$Produto, data = train_data);

#Use both models to make predictions using the test data set.
predit_lm <- predict(model_lm, test_data)
predit_lm <- data.frame(Ano = test_data$Ano, 
                        Cidade = test_data$Cidade, 
                        Tipo_de_Produto = test_data$Tipo_de_Produto,
                        Produto = test_data$Produto,
                        Area = test_data$Area,
                        Area_Pred = predit_lm)

predit_rpart  <- predict(model_rpart,  test_data)
predit_rpart <- data.frame(Ano = test_data$Ano, 
                           Cidade = test_data$Cidade, 
                           Tipo_de_Produto = test_data$Tipo_de_Produto,
                           Produto = test_data$Produto,
                           Area = test_data$Area,
                           Area_Pred = predit_rpart)

#To verify it worked, look at the top rows of the two prediction data sets.
head(predit_lm);
head(predit_rpart);

#Use the plotting functionality in R to visualize the results from the predictions
par(mfrow = c(1, 1));
plot_lm <- plot(predit_lm$Area_Pred - predit_lm$Area, main = "Difference between actual and predicted. lm")
plot_rp <- plot(predit_rpart$Area_Pred  - predit_rpart$Area,  main = "Difference between actual and predicted. rpart")

#modelo usando séries temporais
dat <- Cidadebe4424aa54f8d2aeTipotemporaryProdutoSoy
ord.dat <- dat[order(dat$Ano),]
ts_dat = ts(ord.dat$Area, start=2010)


ts_dat = ts(ord.dat$Area, start=2010)
#ts_dat = ts(ord.dat$Area, start=c(2010,1),frequency=1) #início 2010, frequência de 1 em 1 anos
plot(ts_dat) #gráfico da area destinada para essa tipo de produto especifico

library(forecast) #biblioteca de forecast
m_ets_dats = ets(ts_dat) #modelo de ets
f_ets_dats = forecast(m_ets_dats, h=2) # previsão para 2018,2019
plot(f_ets_dats) #gráfico de forecast básico
summary(f_ets_dats) #resumo das informações do forecast
autoplot(f_ets_dats, ts.colour = 'blue',predict.colour = 'red',predict.linetype = 'dashed', conf.int = TRUE,conf.int.fill = 'lightblue') # aperfeiçoamento do gráfico de forecast 
library(ggplot2)
# autoplot(f_ets_dats, ts.colour = 'blue',predict.colour = 'red',predict.linetype = 'dashed', conf.int = TRUE,conf.int.fill = 'lightblue') + ggtitle("Projeção da area para 2018 e 2019") + labs(x="Ano",y="Area") + annotate("text",x=2010,y=-1,label="Previsão para 2019: x área",color="red")
packs <- c("png","grid") #lendo bibliotecas
lapply(packs, require, character.only = TRUE)


autoplot(f_ets_dats, ts.colour = 'blue',predict.colour = 'red',predict.linetype = 'dashed', conf.int = TRUE,conf.int.fill = 'lightblue') + ggtitle("Projeção da area para 2018 e 2019") + labs(x="Ano",y="Área") 
autoplot(f_ets_dats, ts.colour = 'red',predict.colour = 'black',predict.linetype = 'dashed', conf.int = TRUE,conf.int.fill = 'yellow') + ggtitle("Projeção da area para 2018 e 2019") + labs(x="Ano",y="Área") 
autoplot(f_ets_dats,  ts.colour = 'darkgreen',predict.colour = 'blue',predict.linetype = 'dashed', conf.int = TRUE,conf.int.fill = 'yellow') + ggtitle("Projeção da area para 2018 e 2019") + labs(x="Ano",y="Área") 

############################# segunda tentativa ################################
dat <- Cidadebe4424aa54f8d2aeTipotemporaryProdutoSoy
ord.dat <- dat[order(dat$Ano),]
ts_dat = ts(ord.dat$Area, start=2010)
treino = window(ts_dat, start=2010, end=2015)
teste = as.data.frame(window(ts_dat, start=2016, end=2017))

modelo_reg_mult = tslm(treino ~ trend, data = treino)

Prev1 = forecast(modelo_reg_mult, h = 2)

#compara os dois
plot(ts_dat)
lines(Prev1$mean, col="red")

library(data.table)
dat1 <- Cidadeab984377b2dc0284TipotemporaryProdutoCassava
dat1 <- dat1[order(dat1$Ano, decreasing = T),][1:4,]
ord.dat1 <- dat1[order(dat1$Ano),]


ts_dat1 = ts(ord.dat1$Area, start=(2014))
treino = window(ts_dat1, start=2014, end=2015)
teste = as.data.frame(window(ts_dat1, start=2016, end=2017))


modelo_reg_mult_1 = tslm(treino ~ trend, data = treino)

Prev1 = forecast(modelo_reg_mult_1, h = 2)
plot(ts_dat1)
lines(Prev1$mean, col="red")


## problemas: transformar um dataframe em série temporal sem perder as colunas
## olhar para os modelos de regressão logistica/arvore de decisão/regressão multivariada e entender como as colunas são usadas sozinha 

################################################################################
############################### Exportando dados ###############################
################################################################################



# fazer uma fç que analisa tres modelos prevê o menor erro e aplica





