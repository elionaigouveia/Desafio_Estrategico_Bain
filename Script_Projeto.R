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
################################################################################
############################ Importando os dados ###############################
################################################################################
#dados <- read.csv2(file = "historical-database.csv", header = TRUE, sep = ";")
agriculture <- read_excel("agriculture.xlsx") #Ler o arquivo do excel
head(agriculture) #visualizar alguns dados
str(agriculture) # vizualizar como os dados estão sendo armazenados
#aqui os dados NaN foram excluídos
agriculture <- data.frame(agriculture[complete.cases(agriculture),]) #limpar NA e transformar em data frame
colnames(agriculture) <-c("Ano", "Cidade", "Tipo_de_Produto", "Produto", "Area") #atribuicao de nomes das colunas
#agriculture <- order(rank(agriculture$Ano),) 
#agriculture <- dmy(agriculture$Ano)
agriculture[order(rank(agriculture$Ano))]  #colocando as datas em ordem crescente

################################################################################
################### filtro para cidades, produtos e tipos ######################
################################################################################
table(agriculture$Cidade)
(cidades <- unique(agriculture$Cidade))

table(agriculture$Produto)
(produtos <- unique(agriculture$Produto))

table(agriculture$`Tipo de Produto`)
(tipo <- unique(agriculture$`Tipo de Produto`))

nrow(subset(agriculture, agriculture$Cidade==cidades[1] & 
              agriculture$Produto==produtos[1] &
              agriculture$`Tipo de Produto`==tipo[1]))

################################################################################
############################## Seperação dos dados ############################# 
################################################################################
# Estrutura de repeticao para separar todas as series. São armazenada se for > 1
for(j in 1:length(cidades)){
  for(i in 1:length(produtos)){
    for(p in 1:length(tipo)){
      if(nrow(subset(agriculture, agriculture$Cidade==cidades[j] & 
                     agriculture$Produto==produtos[i] &
                     agriculture$`Tipo de Produto`==tipo[p])) >= 1){
        assign(paste0("Cidade", cidades[j],"Tipo",tipo[p], "Produto", produtos[i]),
               subset(agriculture, agriculture$Cidade==cidades[j] & 
                        agriculture$Produto==produtos[i] &
                        agriculture$`Tipo de Produto`== tipo[p]))
      }
    }
  }
}

#plot de uma serie qualquer
plot(Cidade07ce2e79e19977e9TipotemporaryProdutoOthers[,5],type='l', ylab="Area")
boxplot(Cidade0307883b5d063703TipotemporaryProdutoOthers[,5])

################################################################################
############################## Modelo Preditivo ################################
################################################################################
#modelo preditivo para uma tabela

c01 <- Cidade0307883b5d063703TipopastureProdutoLivestock

train_data = c01[c01$Ano <= 2015-01-01,];
test_data = c01[c01$Ano > 2015-01-01,];

#Use the Area column to check the quality of the prediction against actual values
actual_area <- test_data$Ano;

#Model 1: Use lm to create a linear regression model, trained with the training data set
model_lm <- lm(c01$Area ~ c01$Cidade + c01$Tipo_de_Produto + c01$Produto, data = train_data);


#Model 2: Use rpart to create a decision tree model, trained with the training data set
library(rpart);
model_rpart  <- rpart(c01$Area ~ c01$Cidade + c01$Tipo_de_Produto + c01$Produto, data = train_data);

#Use both models to make predictions using the test data set.
predit_lm <- predict(model_lm, test_data)
predit_lm <- data.frame(Ano = test_data$Ano, 
                        Cidade = agriculture$Cidade, 
                        Tipo_de_Produto = agriculture$Tipo_de_Produto,
                        Produto = agriculture$Produto,
                        Area = test_data$Area,
                        Area_Pred = predict_lm)

predit_rpart  <- predict(model_rpart,  test_data)
predit_rpart <- data.frame(Ano = test_data$Ano, 
                           Cidade = agriculture$Cidade, 
                           Tipo_de_Produto = agriculture$Tipo_de_Produto,
                           Produto = agriculture$Produto,
                           Area = test_data$Area,
                           Area_Pred = predict_lm)

#To verify it worked, look at the top rows of the two prediction data sets.
head(predit_lm);
head(predit_rpart);

#Use the plotting functionality in R to visualize the results from the predictions
par(mfrow = c(1, 1));
plot_lm <- plot(predit_lm$Area_Pred - predit_lm$Area, main = "Difference between actual and predicted. lm")
plot_rp <- plot(predit_rpart$Area_Pred  - predit_rpart$Area,  main = "Difference between actual and predicted. rpart")
################################################################################
############################### Exportando dados ###############################
################################################################################









