################################################################################
############################ Importando pacotes ################################
################################################################################
rm(list=ls())
library(lubridate)   #converter datas
#library("mlbench")
#library('RSNNS')
#library("corpcor")
#library("kernlab")
library(readxl)





################################################################################
############################ Importando os dados ###############################
################################################################################
# dados <- read.csv2(file = "historical-database.csv", header = TRUE, sep = ";")
agriculture <- read_excel("agriculture.xlsx") #Ler o arquivo do excel
head(agriculture) #visualizar alguns dados
str(agriculture) # vizualizar como os dados estão sendo armazenados
agriculture <- data.frame(agriculture[complete.cases(agriculture),]) #limpar NA e transformar em data frame
colnames(agriculture) <-c("Ano", "Cidade", "Tipo de Produto", "Produto", "Area") #atribuicao de nomes das colunas
#agriculture <- order(rank(agriculture$Ano),)




################################################################################
############################## Seperação dos dados ############################# 
################################################################################
# dados_c1_1415_pasture <- subset(dados, city_code == "0307883b5d063703" & product_type =="pasture" & year>= "2014-01-01" & year <= "2015-01-01")






################################################################################
############################## Modelo Preditivo ################################
################################################################################






################################################################################
############################### Exportando dados ###############################
################################################################################