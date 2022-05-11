# assistir novamente a palestra e verificar se é pra usar somente os dados de 2016-2017 para prever 2018-2019
# melhorar os dados de treino e teste
# adicionar outros modelos 
# comparar os modelos pelo WMAPE


# Q1)set.seed()
# Q2)as bibliotecas precisam vir antes do bloco de códigos ou precisam ser carregadas qdo forem usadas?
# Q3)a fç table pertence a uma biblioteca? Qual é exatamente sua funcionalidade?

# limpar váriaveis do ambiente e carregar as bibliotecas que utilizaremos
rm(list=ls())
library(readxl)      # ler arquivo excel


#Ler o arquivo do excel e vizualisar dados
agriculture <-read_excel("agriculture.xlsx")
head(agriculture)

#substituindo 0 e NA pela média total
agriculture[agriculture==0] <- NA
a <- data.frame(agriculture[complete.cases(agriculture),])
mean <- mean(a$destinated_area)
agriculture[is.na(agriculture)] <- mean
agriculture <- data.frame(agriculture[complete.cases(agriculture),])
            
#atribuicao de nomes das colunas
colnames(agriculture) <-c("Ano", "Cidade", "Tipo_de_Produto", "Produto", "Area") 

#ordenando o dataframe pelas datas
agriculture[order(as.Date(agriculture$ano, format="%Y-%m-%d")),]
agriculture <- agriculture[order(agriculture$Ano, agriculture$Cidade, agriculture$Tipo_de_Produto, agriculture$Produto, decreasing=c(FALSE, FALSE, TRUE,TRUE)), ]

#obtendo todos abaixo de 2015
library(dplyr)
agriculture <- agriculture %>% filter(Ano < "2015-12-31")

#filtro para cidades, produtos e tipos
table(agriculture$Cidade)
(cidades <- unique(agriculture$Cidade))

table(agriculture$Produto)
(produtos <- unique(agriculture$Produto))

table(agriculture$`Tipo_de_Produto`)
(tipo <- unique(agriculture$`Tipo_de_Produto`))


################################################################################
#exemplo para uma cidade:

model <- nnetar(Cidade8683715784c40f0fTipopastureProdutoLivestock[,5],)
Cidade8683715784c40f0fTipopastureProdutoLivestock

fcst <- data.frame(forecast(model, h = 2))

p1 <- c("2018-01-01",cidades[j],tipo[p],produtos[i],fcst[1,1])
p2 <- c("2019-01-01",cidades[j],tipo[p],produtos[i],fcst[2,1])

teste <- rbind(Cidade8683715784c40f0fTipopastureProdutoLivestock,p1,p2)
plot(teste[,5],type='l')


################################################################################
#prevendo os modelos na medida em que sao gerados
library(zoo)
library(forecast)

previsoes <- data.frame()
#set.seed()
for(j in 1:length(cidades)){
  for(i in 1:length(produtos)){
    for(p in 1:length(tipo)){
      if(nrow(subset(agriculture, agriculture$Cidade==cidades[j] & 
                     agriculture$Produto==produtos[i] &
                     agriculture$`Tipo_de_Produto`==tipo[p])) > 2){
        
        t <- subset(agriculture, agriculture$Cidade==cidades[j] & 
                      agriculture$Produto==produtos[i] &
                      agriculture$`Tipo_de_Produto`== tipo[p])
        
        model <- nnetar(t[,5],)
        
        fcst <- data.frame(forecast(model, h = 2))
        
        p1 <- c("2016-01-01",cidades[j],tipo[p],produtos[i],fcst[1,1])
        p2 <- c("2017-01-01",cidades[j],tipo[p],produtos[i],fcst[2,1])
        previsoes <- data.frame(rbind(previsoes,p1,p2))
        colnames(previsoes) = c("Ano","Cidade", "Tipo_de_Produto", "Produto", "Area")
        print(previsoes)
      }
    }
  }
}

tail(previsoes)
previsoes
dados <- agriculture %>% filter(Ano > "2015-12-31")

dados[dados$Cidade=="8683715784c40f0f",]
previsoes[previsoes$Cidade=="8683715784c40f0f",]
################################################################################
#modelo de regressão linear
library(data.table)

previsoes2 <- data.frame()
for(j in 1:length(cidades)){
  for(i in 1:length(produtos)){
    for(p in 1:length(tipo)){
      if(nrow(subset(agriculture, agriculture$Cidade==cidades[j] & 
                     agriculture$Produto==produtos[i] &
                     agriculture$`Tipo_de_Produto`==tipo[p])) > 2){
        
        t <- subset(agriculture, agriculture$Cidade==cidades[j] & 
                      agriculture$Produto==produtos[i] &
                      agriculture$`Tipo_de_Produto`== tipo[p])
        
        t <- t[order(t$Ano, decreasing = T),][1:4,]
        ord.t <- t[order(t$Ano),]
       
        ts_t = ts(ord.t$Area, start=2014,  end= 2017)
        treino = window(ts_t, end=2015)
        teste = as.data.frame(window(ts_t, start=2016))
        
        
        m_rl = tslm(treino ~ trend , data = treino)
        p_rl = data.frame(forecast(m_rl, h = 2))
        
        p1 <- c("2016-01-01",cidades[j],tipo[p],produtos[i],p_rl[1,1])
        p2 <- c("2017-01-01",cidades[j],tipo[p],produtos[i],p_rl[2,1])
        previsoes2 <- data.frame(rbind(previsoes2,p1,p2))
        colnames(previsoes2) = c("Ano","Cidade", "Tipo_de_Produto", "Produto", "Area")
        print(previsoes2)
      }
    }
  }
}
################################################################################
# Modelo de Suavização Exponencial - ets
previsoes3 <- data.frame()
for(j in 1:length(cidades)){
  for(i in 1:length(produtos)){
    for(p in 1:length(tipo)){
      if(nrow(subset(agriculture, agriculture$Cidade==cidades[j] & 
                     agriculture$Produto==produtos[i] &
                     agriculture$`Tipo_de_Produto`==tipo[p])) > 2){
        
        t <- subset(agriculture, agriculture$Cidade==cidades[j] & 
                      agriculture$Produto==produtos[i] &
                      agriculture$`Tipo_de_Produto`== tipo[p])
        
        t <- t[order(t$Ano, decreasing = T),][1:4,]
        t <- t[order(t$Ano, decreasing = F),][1:2,]
       
        ts_t = ts(t$Area, start=2014,  end= 2015)
        m_ets = ets(ts_t) 
        p_ets = data.frame(forecast(m_ets, h=2)) 
        
        p1 <- c("2016-01-01",cidades[j],tipo[p],produtos[i],p_ets[1,1])
        p2 <- c("2017-01-01",cidades[j],tipo[p],produtos[i],p_ets[2,1])
        previsoes3 <- data.frame(rbind(previsoes3,p1,p2))
        colnames(previsoes3) = c("Ano","Cidade", "Tipo_de_Produto", "Produto", "Area")
        print(previsoes3)
      }
    }
  }
}
################################################################################
#Modelo de Suavização Exponencial com Tendência Linear - Holt
previsoes4 <- data.frame()
for(j in 1:length(cidades)){
  for(i in 1:length(produtos)){
    for(p in 1:length(tipo)){
      if(nrow(subset(agriculture, agriculture$Cidade==cidades[j] & 
                     agriculture$Produto==produtos[i] &
                     agriculture$`Tipo_de_Produto`==tipo[p])) > 2){
        
        t <- subset(agriculture, agriculture$Cidade==cidades[j] & 
                      agriculture$Produto==produtos[i] &
                      agriculture$`Tipo_de_Produto`== tipo[p])
        
        t <- t[order(t$Ano, decreasing = T),][1:4,]
        t <- t[order(t$Ano, decreasing = F),][1:2,]

        holt_t = ts(t$Area, start=2014,  end= 2015)
        m_holt_t = window(holt_t, start= 2014)
        p_holt_t = data.frame(holt(m_holt_t, h = 2))
        
        p1 <- c("2016-01-01",cidades[j],tipo[p],produtos[i],p_holt_t[1,1])
        p2 <- c("2017-01-01",cidades[j],tipo[p],produtos[i],p_holt_t[2,1])
        previsoes4 <- data.frame(rbind(previsoes4,p1,p2))
        colnames(previsoes4) = c("Ano","Cidade", "Tipo_de_Produto", "Produto", "Area")
        print(previsoes4)
      }
    }
  }
}

################################################################################
#modelo naive
previsoes5 <- data.frame()
for(j in 1:length(cidades)){
  for(i in 1:length(produtos)){
    for(p in 1:length(tipo)){
      if(nrow(subset(agriculture, agriculture$Cidade==cidades[j] & 
                     agriculture$Produto==produtos[i] &
                     agriculture$`Tipo_de_Produto`==tipo[p])) > 2){
        
        t <- subset(agriculture, agriculture$Cidade==cidades[j] & 
                      agriculture$Produto==produtos[i] &
                      agriculture$`Tipo_de_Produto`== tipo[p])
        
        t <- t[order(t$Ano, decreasing = T),][1:4,]
        t <- t[order(t$Ano, decreasing = F),][1:2,]
        
        
        t <- t[order(t$Ano, decreasing = T),][1:4,]
        ord.t <- t[order(t$Ano),]
        
        
        ts_t = ts(ord.t$Area, start=2014,  end= 2017)
        treino = window(ts_t, end=2015)
        teste = as.data.frame(window(ts_t, start=2016))
        p_naive_t = data.frame(naive(treino,h = 2))
        
        p1 <- c("2016-01-01",cidades[j],tipo[p],produtos[i],p_naive_t[1,1])
        p2 <- c("2017-01-01",cidades[j],tipo[p],produtos[i],p_naive_t[2,1])
        previsoes5 <- data.frame(rbind(previsoes5,p1,p2))
        colnames(previsoes5) = c("Ano","Cidade", "Tipo_de_Produto", "Produto", "Area")
        print(previsoes5)
      }
    }
  }
}
################################################################################
#Comparando os modelos

library(dplyr)
b <- agriculture %>% filter(Ano > "2015-12-31")


#transforacao da primeira coluna em data
previsoes[,1] <- as.Date(previsoes[,1],format="%Y-%m-%d")
previsoes2[,1] <- as.Date(previsoes2[,1],format="%Y-%m-%d")
previsoes3[,1] <- as.Date(previsoes3[,1],format="%Y-%m-%d")
previsoes4[,1] <- as.Date(previsoes4[,1],format="%Y-%m-%d")
previsoes5[,1] <- as.Date(previsoes5[,1],format="%Y-%m-%d")
b[,1] <- as.Date(b[,1],format="%Y-%m-%d")

e <- 0 #inicializacao do erro
e2 <- 0 #inicializacao do erro
e3 <- 0 #inicializacao do erro
e4 <- 0 #inicializacao do erro
e5 <- 0 #inicializacao do erro




for(i in 1:dim(previsoes)[1]){
  for(j in 1:dim(b)[1]){
    tr <- previsoes[i,1:4]==b[j,1:4] #verifica se sao cidade,tipo e produto corretos
    if(sum(tr)==4){ #soma dos valores l?gicos deve ser igual a 4 
      e <- e + (abs(b[j,5] - as.numeric(previsoes[i,5]))/b[j,5]) #calcula o erro de previsao
      print(e) #printa o erro
      print(i) #printa o numero da previsao
    }
  }
}

for(i in 1:dim(previsoes2)[1]){
  for(j in 1:dim(b)[1]){
    tr <- previsoes2[i,1:4]==b[j,1:4] #verifica se sao cidade,tipo e produto corretos
    if(sum(tr)==4){ #soma dos valores l?gicos deve ser igual a 4 
      e2 <- e2 + (abs(b[j,5] - as.numeric(previsoes2[i,5]))/b[j,5]) #calcula o erro de previsao
      print(e2) #printa o erro
      print(i) #printa o numero da previsao
    }
  }
}

for(i in 1:dim(previsoes3)[1]){
  for(j in 1:dim(b)[1]){
    tr <- previsoes3[i,1:4]==b[j,1:4] #verifica se sao cidade,tipo e produto corretos
    if(sum(tr)==4){ #soma dos valores l?gicos deve ser igual a 4 
      e3 <- e3 + (abs(b[j,5] - as.numeric(previsoes3[i,5]))/b[j,5]) #calcula o erro de previsao
      print(e3) #printa o erro
      print(i) #printa o numero da previsao
    }
  }
}

for(i in 1:dim(previsoes4)[1]){
  for(j in 1:dim(b)[1]){
    tr <- previsoes4[i,1:4]==b[j,1:4] #verifica se sao cidade,tipo e produto corretos
    if(sum(tr)==4){ #soma dos valores l?gicos deve ser igual a 4 
      e4 <- e4 + (abs(b[j,5] - as.numeric(previsoes4[i,5]))/b[j,5]) #calcula o erro de previsao
      print(e4) #printa o erro
      print(i) #printa o numero da previsao
    }
  }
}

for(i in 1:dim(previsoes5)[1]){
  for(j in 1:dim(b)[1]){
    tr <- previsoes5[i,1:4]==b[j,1:4] #verifica se sao cidade,tipo e produto corretos
    if(sum(tr)==4){ #soma dos valores l?gicos deve ser igual a 4 
      e5 <- e5 + (abs(b[j,5] - as.numeric(previsoes5[i,5]))/b[j,5]) #calcula o erro de previsao
      print(e5) #printa o erro
      print(i) #printa o numero da previsao
    }
  }
}

vector <- c(e,e2,e3,e4,e4)
menor_erro <- min(vector)
if (menor_erro == e){
  print("O modelo de redes neurais é o melhor")
} else if(menor_erro == e2){
  print("O modelo de regressão linear é o melhor")
} else if(menor_erro == e3){
  print("O modelo de suavização exponencial é o melhor")
} else if(menor_erro == e4){
  print("O modelo de Holt é o melhor")
} else(menor_erro == e5){
  print("O modelo de Naive é o melhor")
}
  