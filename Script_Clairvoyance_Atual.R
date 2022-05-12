#Modelo Naive
previsoes5 <- data.frame()
for(j in 1:length(cidades)){
  for(i in 1:length(produtos)){
    for(p in 1:length(tipo)){
      if(nrow(subset(agriculture, agriculture$Cidade==cidades[j] & 
                     agriculture$Produto==produtos[i] &
                     agriculture$`Tipo de Produto`==tipo[p])) > 2){
        
        t <- subset(agriculture, agriculture$Cidade==cidades[j] & 
                      agriculture$Produto==produtos[i] &
                      agriculture$`Tipo de Produto`== tipo[p])
        
        t0 <- t %>% filter(Ano < "2015-12-31")
        t0 <- t0[order(t0$Ano),]
        t1 <- t %>% filter(Ano > "2015-12-31")
        t1 <- t1[order(t1$Ano),]
        
        tam0 <- dim(t0)[1]
        inicio0 <- format(as.Date(t0[1,1], format="%Y-%m-%d"),"%Y")
        fim0 <- format(t0[tam0,1],format="%Y")
        
        tam1 <- dim(t1)[1]
        inicio1 <- format(as.Date(t1[1,1], format="%Y-%m-%d"),"%Y")
        fim1 <- format(t1[tam1,1],format="%Y")
        
        ts_t0 <- ts(t0$Area, start=inicio0, end=fim0)
        ts_t1 <- ts(t1$Area, start=inicio1, end=fim1)
        
        treino <- window(ts_t0, start=time(ts_t0)[1], end=time(ts_t0)[tam0])
        teste = as.data.frame(window(ts_t1, start=time(ts_t1)[1], end=time(ts_t1)[tam1]))
        p_naive_t <- naive(treino,h = 2)
        fcst <- data.frame(p_naive_t)       
        p1 <- c("2016-01-01",cidades[j],tipo[p],produtos[i],fcst[1,1])
        p2 <- c("2017-01-01",cidades[j],tipo[p],produtos[i],fcst[2,1])
        previsoes5 <- data.frame(rbind(previsoes5,p1,p2))
        colnames(previsoes5) = c("Ano","Cidade", "Tipo de Produto", "Produto", "Area")
        print(previsoes5)
      }
    }
  }
}

#Modelo de regressão linear - tendencia
previsoes2 <- data.frame()
for(j in 1:length(cidades)){
  for(i in 1:length(produtos)){
    for(p in 1:length(tipo)){
      if(nrow(subset(agriculture, agriculture$Cidade==cidades[j] & 
                     agriculture$Produto==produtos[i] &
                     agriculture$`Tipo de Produto`==tipo[p])) > 2){
        
        t <- subset(agriculture, agriculture$Cidade==cidades[j] & 
                      agriculture$Produto==produtos[i] &
                      agriculture$`Tipo de Produto`== tipo[p])
        
        t0 <- t %>% filter(Ano < "2015-12-31")
        t0 <- t0[order(t0$Ano),]
        t1 <- t %>% filter(Ano > "2015-12-31")
        t1 <- t1[order(t1$Ano),]
        
        tam0 <- dim(t0)[1]
        inicio0 <- format(as.Date(t0[1,1], format="%Y-%m-%d"),"%Y")
        fim0 <- format(t0[tam0,1],format="%Y")
        
        tam1 <- dim(t1)[1]
        inicio1 <- format(as.Date(t1[1,1], format="%Y-%m-%d"),"%Y")
        fim1 <- format(t1[tam1,1],format="%Y")
        
        ts_t0 <- ts0(t0$Area, start=inicio0, end=fim0)
        ts_t1 <- ts1(t1$Area, start=inicio1, end=fim1)
        
        treino <- window(ts_t0, start=time(ts_t0)[1], end=time(ts_t0)[tam0])
        teste = as.data.frame(window(ts_t1, start=time(ts_t1)[1], end=time(ts_t1)[tam1]))
        
        m_rl <-  tslm(treino ~ trend, data = treino)
        p_rl <-  forecast(m_rl, h = 2)
        fcst <- data.frame(p_rl)
        p1 <- c("2016-01-01",cidades[j],tipo[p],produtos[i],fcst[1,1])
        p2 <- c("2017-01-01",cidades[j],tipo[p],produtos[i],fcst[2,1])
        previsoes2 <- data.frame(rbind(previsoes2,p1,p2))
        colnames(previsoes2) = c("Ano","Cidade", "Tipo de Produto", "Produto", "?rea")
        print(previsoes2)
      }
    }
  }
}
