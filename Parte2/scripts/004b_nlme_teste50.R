

#### 0.0 - Inicio do Código Automatizado para Executar os 105 Dias####

##### 0.1 - Definição do Caminho "raiz" do script ####
local_pasta <- "D:\\Solar2\\Parte2\\"

##### 0.2 - Dependências do Script #####

# Para executar esse script é necessário executar antes o scritp:
#     - 001a_nlme_treino21 

cod_treino <- paste(local_pasta, "scripts\\004a_nlme_treino50.R", sep = "")
source(cod_treino)


##### 1.0 - Leitura dos dados ##### 
dados1 <- read.csv("D:\\Solar2\\Parte2\\datasets\\csv\\1_21.csv") 
dados2 <- read.csv("D:\\Solar2\\Parte2\\datasets\\csv\\22_42.csv") 
dados3 <- read.csv("D:\\Solar2\\Parte2\\datasets\\csv\\43_63.csv") 
dados4 <- read.csv("D:\\Solar2\\Parte2\\datasets\\csv\\64_84.csv") 
dados5 <- read.csv("D:\\Solar2\\Parte2\\datasets\\csv\\85_105.csv") 


###### 1.1 - Ajuste do Dataset ######


####### 1.1.1 - Agrupar os Datasets ####### 
dados2$DIA <- dados2$DIA + 21 
dados3$DIA <- dados3$DIA + 42 
dados4$DIA <- dados4$DIA + 63 
dados5$DIA <- dados5$DIA + 84 
dadosTotal <- c()
dadosTotal <- rbind(dados1, dados2, dados3, dados4, dados5)


####### 1.1.2 - Remoção da Primeira Coluna #######
dadosTotal$X <- NULL

####### 1.1.4 - Remoção dos PDC's = 0 ####### 
dadosTotal <- na.omit(dadosTotal)
dadosTotal <- dadosTotal[!(dadosTotal$PDC == 0),]



####### 1.1.3 - Selecionar os 50 Primeiros Dias ####### 
dadosTreino <- dadosTotal[!(dadosTotal$DIA > 50),]

dadosTeste <- dadosTotal[!(dadosTotal$DIA > 100 ),]
dadosTeste <- dadosTeste[!(dadosTeste$DIA < 51 ),]

xi <- numeric()
xf <- numeric()
Dip_list <- numeric()
Dio_list <- numeric()
Dim <- numeric()
EQMi <- numeric()

##### 2.0 - Início dos Testes #####
### Laço para pegar os dois datasets 
for(j in 1:2){

  if( j == 1){
      dados_teste <- dadosTreino
  }  
  if( j == 2){
    dados_teste <- dadosTeste
    dados_teste$DIA <- dados_teste$DIA - 50
  }
  
  
  ###### 2.1 - Ajustes no Dataset para o Modelo ######
  str(dados_teste)
  ND <- data.frame(dados_teste)
  
  ##### 2.2 - Remoção da Primeira Coluna (contador de amostras) #####
  ND$X <- NULL     
  
  
  ##### 2.3 - Remoção dos PDC's = 0 #####
  ND <- ND[!(ND$PDC == 0),]
  nr <- nrow(ND)
  
  
  ##### 2.4 - Cálculo de IRR e PDC acumulados #####
  id.fi <- cumsum(as.numeric(ftable(ND[,1]))) 
  id.in <- c(1,id.fi+1)
  AIRR <- 0
  APDC <- 0
  
  
  for(d in 1:length(id.fi)){
    AIRR <- c(AIRR, cumsum(ND[id.in[d]:id.fi[d],3]))
    APDC <- c(APDC, cumsum(ND[id.in[d]:id.fi[d],6]))
  }
  
  NDA <- ND
  NDA[,3] <- AIRR[-1]     
  NDA[,6] <- APDC[-1]
  
  
  ##### 2.5 - Transformação Log #####
  NDA[,1] <- factor(NDA[,1])     
  NDA[,2] <- NDA[,2] - 1
  NDA[,3] <- log(NDA[,3])
  NDA[,4] <- log(NDA[,4])
  NDA[,5] <- log(NDA[,5])
  NDA[,6] <- log(NDA[,6])
  NDA <- data.frame(NDA)
  
  newd <- data.frame(NDA[,1:5])  
  newd <- na.exclude(newd)
  
  ##### 2.6 - Valores Obtidos pelo preditor #####
  pred9 <- as.numeric(predict(M9, newd))
  
  
  #### 3.0 - Gráficos dos Modelos ####
  id.fi <- cumsum(as.numeric(ftable(NDA[,1]))) 
  id.in <- c(1,id.fi+1)
  
  
  ###### 3.1 - Tempo x PDC (Observado e Esperado) #####
  for(i in 1:length(id.fi)){
    
    # i = dia
    # j = é o arquivo
    
    nome_arquivo <- paste ("dia50_", (i+(j-1)*50), ".png", sep="")
    pathDest <- paste(local_pasta, "graphs\\1 - Tempo x PDC\\", sep = "")
    fileDest <- paste(pathDest, nome_arquivo, sep = "")
    png(filename = fileDest, width = 720, height = 480, units = 'px')
    
    dia <- i
    xi[dia] <- id.in[dia] 
    xf[dia] <- id.fi[dia]
    
    ######### 3.1.1 - Cálculo do PDC Observado  #######  
    Dio <- NDA[xi[dia]:xf[dia],6]
    plot(NDA[xi[dia]:xf[dia],2], Dio, pch=19, lwd=2, ylab="Valores Y", xlab="Tempo", xlim=c(0,length(Dio)), ylim=c(2,13))
    points(NDA[xi[dia]:xf[dia],2], Dio, col="black", type="l", lwd=2)
    
    
    ######### 3.1.2 - Cálculo do PDC Predito ##### 
    Dip <- as.numeric(pred9[xi[dia]:xf[dia]])
    points(NDA[xi[dia]:xf[dia],2], Dip, col="red", type="l", lwd=2)
    
    legend("topleft", legend=c("Observado","Esperado"), col=c("black","red"), lty=1, pch=c(1, NA), lwd=2)
    
    Dip_list[ (i+(j-1)*50) ] <- sum(Dip)
    Dio_list[ (i+(j-1)*50) ] <- sum(Dio)
    
    
    ######### 3.2 - Cálculo do Erro Quadrático Médio Diário #####
    EQMi[ (i+(j-1)*50) ] <- round(mean((Dio - Dip)^2), 6)
    
    
    ######### 3.4 - Cálculo da Diferença Percentual Média  ######    
    Dim[ (i+(j-1)*50) ] <- round(mean(( (exp(Dio) - exp(Dip) ) / exp(Dip) ) * 100), 6)
    
    out <- (i+(j-1)*50)
    print(out)
    
    textoEQM <- paste("MSE:", EQMi[ (i+(j-1)*50) ])
    text(68, 11, textoEQM)
    dev.off()  
  }
  
  
  
  # summary(pred9)     # Coef. de Correlação: Phi =  0.98317 
  # summary(M9)
  # Média dos Erros Quadráticos Médios Diários: 0.00275
  # Mean_EQM <- round( mean(EQMi) , 5)
  
  
  
  ######### 3.5 - Gráfico: DIP x DIO #####
  nome_arquivo <- paste ("energia50_", (i+(j-1)*50), ".png", sep="")
  pathDest <- paste(local_pasta, "graphs\\2 - DIP x DIO\\", sep = "")
  fileDest <- paste(pathDest, nome_arquivo, sep = "")
  png(filename = fileDest, width = 720, height = 480, units = 'px')
  
  sm <- seq(1,length(id.fi)*j)
  p = ggplot() + theme_bw() + 
    geom_line(aes(x = sm, y = Dip_list, group=1), color = "blue", size = 0.5) +
    geom_line(aes(x = sm, y = Dio_list, group=2), color = "red", size = 0.5) +
    geom_point(aes(x = sm, y = Dip_list, group=1), color = "blue", size = 1) +
    geom_point(aes(x = sm, y = Dio_list, group=2), color = "red", size = 1) +
    xlab('Dias') +
    ylab('Potência') + 
    scale_x_continuous(breaks=seq(0, 100, 5))  +
    scale_fill_manual(values=c("#a6cee3","#1f78b4","#b2df8a"))
  
  print(p)
  dev.off()
  
  
  ####### 3.6 - Gráfico: EQM/MSE #####
  nome_arquivo <- paste ("eqm50_1_", (i+(j-1)*50), ".png", sep="")
  pathDest <- paste(local_pasta, "graphs\\3 - EQM\\", sep = "")
  fileDest <- paste(pathDest, nome_arquivo, sep = "")
  png(filename = fileDest, width = 720, height = 480, units = 'px')
  
  sm <- seq(1,length(id.fi)*j)
  plot(sm, EQMi, type="l", xlim=c(0,length(id.fi)*j),  ylim=c(0,max(EQMi)), ylab="MSE", xlab="Day")
  points(sm, EQMi, pch=19)
  
  dev.off()
  
  ####### 3.7 - Gráfico: Dif. Perc. Média #####
  nome_arquivo <- paste ("difperc50_1_", (i+(j-1)*50), ".png", sep="")
  pathDest <- paste(local_pasta, "graphs\\4 - DPM\\", sep = "")
  fileDest <- paste(pathDest, nome_arquivo, sep = "")
  png(filename = fileDest, width = 720, height = 480, units = 'px')
  
  
  data_Bar2 <- data.frame (ListaDias_Bar = seq(1,length(id.fi)*j),
                           value = round(Dim, 4))
  
  p = ggplot(data_Bar2, aes(x=ListaDias_Bar, y=value)) + 
    #  geom_bar(stat = "identity", colour="dodgerblue", fill="white") +
    geom_bar(stat = "identity", colour="black", fill="grey") + theme_bw() + 
    #  geom_text(aes(x = ListaDias_Bar, y = Dim, label = round(Dim, 4)), vjust=1.0,) +  
    #  ggtitle("Differences (percentage): Observed and Estimated") + 
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(y= "Diferenca Percentual Media", x = "Dias") + scale_y_continuous(limits = c(min(Dim)-.03,max(Dim)+.03))
  
  print(p)
  dev.off()
  
}
