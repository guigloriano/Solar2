

#### 0.0 - Inicio do Segundo Código para Teste : Dia 22 ao 42 ####


#### 1.0 - Leitura do Segundo Dataset ####
caminho_dia22a42 <- paste(local_pasta, "datasets\\csv\\22_42.csv", sep = "")
dados_teste <- read.csv(caminho_dia22a42)


##### 1.1 - Ajustes no Dataset para o Modelo #####
str(dados_teste)
ND <- data.frame(dados_teste)

##### 1.2 - Remoção da Primeira Coluna (contador de amostras) #####
ND$X <- NULL     


##### 1.3 - Remoção dos PDC's = 0 #####
ND <- ND[!(ND$PDC == 0),]
nr <- nrow(ND)


##### 1.4 - Cálculo de IRR e PDC acumulados #####
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


##### 1.5 - Transformação Log #####
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


#### 3.0 - Gráficos dos Modelos ####
id.fi <- cumsum(as.numeric(ftable(NDA[,1]))) 
id.in <- c(1,id.fi+1)


##### 3.1 - Tempo x PDC (Observado e Esperado) #####
for(i in 1:length(id.fi)){
  nome_arquivo <- paste ("dia_", i+21, ".png", sep="")
  pathDest <- paste(local_pasta, "graphs\\Tempo x PDC\\", sep = "")
  fileDest <- paste(pathDest, nome_arquivo, sep = "")
  png(filename = fileDest, width = 720, height = 480, units = 'px')
  
  dia <- i
  xi[dia] <- id.in[dia] 
  xf[dia] <- id.fi[dia]
  
  ######  3.1.1 - Cálculo do PDC Observado  ######  
  Dio <- NDA[xi[dia]:xf[dia],6]
  plot(NDA[xi[dia]:xf[dia],2], Dio, pch=19, lwd=2, ylab="Valores Y", xlab="Tempo", xlim=c(0,length(Dio)), ylim=c(2,13))
  points(NDA[xi[dia]:xf[dia],2], Dio, col="black", type="l", lwd=2)
  
  #####  3.1.2 - Cálculo do PDC Predito ##### 
  Dip <- pred9[xi[dia]:xf[dia]]
  points(NDA[xi[dia]:xf[dia],2], Dip, col="red", type="l", lwd=2)
  
  legend("topleft", legend=c("Observado","Esperado"), col=c("black","red"), lty=1, pch=c(1, NA), lwd=2)
  
  Dip_list[i+21] <- sum(Dip)
  Dio_list[i+21] <- sum(Dio)
  
  EQMi[i+21] <- round(mean((Dio - Dip)^2), 6)
  
  textoEQM <- paste("MSE:", EQMi[i+21])
  text(68, 11, textoEQM)
  dev.off()  
  
}


##### 3.2 - DIP x DIO (p/ 42 dias) #####
nome_arquivo <- paste ("energia_", i+21, ".png", sep="")
pathDest <- paste(local_pasta, "graphs\\DIP x DIO\\", sep = "")
fileDest <- paste(pathDest, nome_arquivo, sep = "")
png(filename = fileDest, width = 720, height = 480, units = 'px')

sm <- seq(1,length(id.fi)*2)
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


##### 3.3 - Erro quadrático Médio Diário (p/ 42 dias) #####
for(i in 1:(length(id.fi))){         
  dia <- i
  xi <- id.in[dia] 
  xf <- id.fi[dia]
  Dio <- NDA[xi:xf,6]
  Dip <- pred9[xi:xf]
  eqm[i+21] <- mean((Dio - Dip)^2)
}


nome_arquivo <- paste ("eqm1_", i+21, ".png", sep="")
pathDest <- paste(local_pasta, "graphs\\EQM\\", sep = "")
fileDest <- paste(pathDest, nome_arquivo, sep = "")
png(filename = fileDest, width = 720, height = 480, units = 'px')


sm <- seq(1,length(id.fi)+21)
plot(sm, eqm, type="l", xlim=c(0,43),  ylim=c(0,max(eqm)), ylab="MSE", xlab="Day")
points(sm, eqm, pch=19)

dev.off()



##### 3.4 - Diferença Percentual Média (p/ 42 dias) ######    
for(i in 1:length(id.fi)){
  Dio <- NDA[id.in[i]:id.fi[i],6]
  Dip <- as.numeric(pred9[id.in[i]:id.fi[i]])
  Dim[i+21] <- round(mean(( (exp(Dio) - exp(Dip) ) / exp(Dip) ) * 100), 6)
} 

###### 3.4.1 - Grafico em Barras das Diferenças Percentuais Medias #####

#### Informações para Salvar a imagem 
nome_arquivo <- paste ("dif_perc1_", i+21, ".png", sep="")
pathDest <- paste(local_pasta, "graphs\\DPM\\", sep = "")
fileDest <- paste(pathDest, nome_arquivo, sep = "")
png(filename = fileDest, width = 720, height = 480, units = 'px')


data_Bar2 <- data.frame (ListaDias_Bar = seq(1,length(id.fi)+21),
                         value = round(Dim, 4))

ggplot(data_Bar2, aes(x=ListaDias_Bar, y=value)) + 
  #  geom_bar(stat = "identity", colour="dodgerblue", fill="white") +
  geom_bar(stat = "identity", colour="black", fill="grey") + theme_bw() + 
#  geom_text(aes(x = ListaDias_Bar, y = Dim, label = round(Dim, 4)), vjust=1.0,) +  
#  ggtitle("Differences (percentage): Observed and Estimated") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y= "Diferenca Percentual Media", x = "Dias") + scale_y_continuous(limits = c(min(Dim)-.03,max(Dim)+.03))
dev.off()

