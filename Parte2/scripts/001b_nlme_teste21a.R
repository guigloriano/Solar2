



########################################################################
####                                                                ####
####      SCRIPT DESATUALIZADO - UTILIZAR A VERS�O b (abaixo)       ####
####                                                                ####
########################################################################



#### 0.0 - Inicio do C�digo Automatizado para Executar os 105 Dias####

##### 0.1 - Defini��o do Caminho "raiz" do script ####
local_pasta <- "D:\\Solar2\\Parte2\\"

##### 0.2 - Depend�ncias do Script #####

# Para executar esse script � necess�rio executar antes o scritp:
#     - 001a_nlme_treino21 

cod_treino <- paste(local_pasta, "scripts\\001a_nlme_treino21.R", sep = "")
source(cod_treino)


#### 1.0 - Leitura dos Caminhos dos Datasets ####
caminho_dia01a21 <- paste("D:\\Solar2\\Parte2\\datasets\\csv\\1_21.csv")
caminho_dia22a42 <- paste("D:\\Solar2\\Parte2\\datasets\\csv\\22_42.csv")
caminho_dia43a63 <- paste("D:\\Solar2\\Parte2\\datasets\\csv\\43_63.csv")
caminho_dia64a84 <- paste("D:\\Solar2\\Parte2\\datasets\\csv\\64_84.csv")
caminho_dia85a105 <- paste("D:\\Solar2\\Parte2\\datasets\\csv\\85_105.csv")

##### 1.1 - Cria��o da Lista de Caminhos #####
TodosCaminhos <- c(caminho_dia01a21, caminho_dia22a42, caminho_dia43a63,
                   caminho_dia64a84, caminho_dia85a105)



for(j in 1:length(TodosCaminhos)){
  # Mostrar Qual � o Caminho Atual do Arquivo a Ser Lido
  out1 <- paste0("Arquivo Lido: ", TodosCaminhos[j])
  print(out1)
  
  # Indicar os �ndices dos Dias do dataset 
  out2 <- paste0("Dias: ", (j+(j-1)*20), " ao dia ", (j*21) )
  print(out2)
}



xi <- numeric()
xf <- numeric()
Dip_list <- numeric()
Dio_list <- numeric()
Dim <- numeric()
EQMi <- numeric()

for(j in 1:length(TodosCaminhos)){
  
  # Mostrar Qual � o Caminho Atual do Arquivo a Ser Lido
  out1 <- paste0("Arquivo Lido: ", TodosCaminhos[j])
  print(out1)
  
  # Indicar os �ndices dos Dias do dataset 
  out2 <- paste0("Dias: ", (j+(j-1)*20), " ao dia ", (j*21) )
  print(out2)
  
  ###### 1.2 - Leitura do Dataset a ser manipulado #####
  dados_teste <- read.csv(TodosCaminhos[j])
  
  
  ##### 1.3 - Ajustes no Dataset para o Modelo #####
  str(dados_teste)
  ND <- data.frame(dados_teste)
  
  ##### 1.4 - Remo��o da Primeira Coluna (contador de amostras) #####
  ND$X <- NULL     
  
  
  ##### 1.5 - Remo��o dos PDC's = 0 #####
  ND <- ND[!(ND$PDC == 0),]
  nr <- nrow(ND)
  
  
  ##### 1.6 - C�lculo de IRR e PDC acumulados #####
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
  
  
  ##### 1.7 - Transforma��o Log #####
  NDA[,1] <- factor(NDA[,1])     
  NDA[,2] <- NDA[,2] - 1
  NDA[,3] <- log(NDA[,3])
  NDA[,4] <- log(NDA[,4])
  NDA[,5] <- log(NDA[,5])
  NDA[,6] <- log(NDA[,6])
  NDA <- data.frame(NDA)
  
  newd <- data.frame(NDA[,1:5])  
  newd <- na.exclude(newd)
  
  
  #### 2.0 - Valores Obtidos pelo preditor ####
  pred9 <- as.numeric(predict(M9, newd))
  
  
  #### 3.0 - Gr�ficos dos Modelos ####
  id.fi <- cumsum(as.numeric(ftable(NDA[,1]))) 
  id.in <- c(1,id.fi+1)
  
  
  ###### 3.1 - Tempo x PDC (Observado e Esperado) #####
  for(i in 1:length(id.fi)){
    
    # i = dia
    # j = � o arquivo
    
    nome_arquivo <- paste ("dia_", (i+(j-1)+(j-1)*20), ".png", sep="")
    pathDest <- paste(local_pasta, "graphs\\1 - Tempo x PDC\\", sep = "")
    fileDest <- paste(pathDest, nome_arquivo, sep = "")
    png(filename = fileDest, width = 720, height = 480, units = 'px')
    
    dia <- i
    xi[dia] <- id.in[dia] 
    xf[dia] <- id.fi[dia]
    
    ######### 3.1.1 - C�lculo do PDC Observado  #######  
    Dio <- NDA[xi[dia]:xf[dia],6]
    plot(NDA[xi[dia]:xf[dia],2], Dio, pch=19, lwd=2, ylab="Valores Y", xlab="Tempo", xlim=c(0,length(Dio)), ylim=c(2,13))
    points(NDA[xi[dia]:xf[dia],2], Dio, col="black", type="l", lwd=2)
    
    
    ######### 3.1.2 - C�lculo do PDC Predito ##### 
    Dip <- as.numeric(pred9[xi[dia]:xf[dia]])
    points(NDA[xi[dia]:xf[dia],2], Dip, col="red", type="l", lwd=2)
    
    legend("topleft", legend=c("Observado","Esperado"), col=c("black","red"), lty=1, pch=c(1, NA), lwd=2)
    
    
    
#    Dip_list[ (i+(j-1)+(j-1)*20) ] <- sum(Dip)
#    Dio_list[ (i+(j-1)+(j-1)*20) ] <- sum(Dio)
    Dip_list[ (i+(j-1)+(j-1)*20) ] <- Dip[length(Dip)]
    Dio_list[ (i+(j-1)+(j-1)*20) ] <- Dio[length(Dio)]
    
    
    ######### 3.2 - C�lculo do Erro Quadr�tico M�dio Di�rio #####
    EQMi[ (i+(j-1)+(j-1)*20) ] <- round(mean((Dio - Dip)^2), 6)
#    EQMi[ (i+(j-1)+(j-1)*20) ] <- round(mean((Dio[length(Dio)] - Dip[length(Dip)])^2), 6)
    
    
    ######### 3.4 - C�lculo da Diferen�a Percentual M�dia  ######    
    Dim[ (i+(j-1)+(j-1)*20) ] <- round(mean(( (exp(Dio) - exp(Dip) ) / exp(Dip) ) * 100), 6)
    
    out <- (i+(j-1)+(j-1)*20)
    print(out)
    
    textoEQM <- paste("MSE:", EQMi[ (i+(j-1)+(j-1)*20) ])
    text(68, 11, textoEQM)
    dev.off()  
  }
  
  
  
  # summary(pred9)     # Coef. de Correla��o: Phi =  0.98317 
  # summary(M9)
  # M�dia dos Erros Quadr�ticos M�dios Di�rios: 0.00275
  # Mean_EQM <- round( mean(EQMi) , 5)
  
  
  
  ######### 3.5 - Gr�fico: DIP x DIO #####
  nome_arquivo <- paste ("energia_", (i+(j-1)+(j-1)*20), ".png", sep="")
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
    ylab('Pot�ncia') + 
    scale_x_continuous(breaks=seq(0, 105, 5))  +
    scale_fill_manual(values=c("#a6cee3","#1f78b4","#b2df8a"))
  
  print(p)
  dev.off()
  
  
  ####### 3.6 - Gr�fico: EQM/MSE #####
  nome_arquivo <- paste ("eqm_1_", (j*21), ".png", sep="")
  pathDest <- paste(local_pasta, "graphs\\3 - EQM\\", sep = "")
  fileDest <- paste(pathDest, nome_arquivo, sep = "")
  png(filename = fileDest, width = 720, height = 480, units = 'px')
  
  sm <- seq(1,length(id.fi)*j)
  plot(sm, EQMi, type="l", xlim=c(0,length(id.fi)*j),  ylim=c(0,max(EQMi)), ylab="MSE", xlab="Day")
  points(sm, EQMi, pch=19)
  
  dev.off()
  
  ####### 3.7 - Gr�fico: Dif. Perc. M�dia #####
  nome_arquivo <- paste ("difperc_1_", (j*21), ".png", sep="")
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
