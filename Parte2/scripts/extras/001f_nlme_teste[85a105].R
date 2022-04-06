

#### 0.0 - Inicio do Primeiro Código: Dia 1 ao 21 ####

#### 1.0 - Leitura do Terceiro Dataset ####
caminho_dia85a105 <- paste(local_pasta, "datasets\\csv\\85_105.csv", sep = "")
dados_teste <- read.csv(caminho_dia85a105)


##### 1.1 - Ajustes no Dataset para o Modelo #####
str(dados_teste)
ND <- data.frame(dados_teste)


##### 1.2 - Remoção da Primeira Coluna #####
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


##### Inicialização das Variáveis
#dia <- 97
#i <- 1
#xi <- numeric()
#xf <- numeric()
#Dip_list <- numeric()
#Dio_list <- numeric()
#EQMi <- numeric()



##### 3.1 - Tempo x PDC (Observado e Esperado) #####
for(i in 1:length(id.fi)){
  nome_arquivo <- paste ("dia_", i+84, ".png", sep="")
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
  
  ##### tempo x irradiancia
#  d1 <- NDA[xi[dia]:xf[dia],2]
#  d2 <- NDA[xi[dia]:xf[dia],3]
#  d3 <- NDA[xi[dia]:xf[dia],5]
  
  #####  3.1.2 - Cálculo do PDC Predito ##### 
  Dip <- pred9[xi[dia]:xf[dia]]
  points(NDA[xi[dia]:xf[dia],2], Dip, col="red", type="l", lwd=2)
  
  legend("topleft", legend=c("Observado","Esperado"), col=c("black","red"), lty=1, pch=c(1, NA), lwd=2)
  
  Dip_list[i+84] <- sum(Dip)
  Dio_list[i+84] <- sum(Dio)
  
  EQMi[i+84] <- round(mean((Dio - Dip)^2), 6)
  
  textoEQM <- paste("EQM = ", EQMi[i+84])
  text(68, 11, textoEQM)
  dev.off()  
  
}


##### 3.2 - DIP x DIO (p/ 105 dias) #####
nome_arquivo <- paste ("energia2_", i+84, ".png", sep="")
pathDest <- paste(local_pasta, "graphs\\DIP x DIO\\", sep = "")
fileDest <- paste(pathDest, nome_arquivo, sep = "")
png(filename = fileDest, width = 720, height = 480, units = 'px')

sm <- seq(1,length(id.fi)+84)
p <- ggplot() + theme_bw() + 
  geom_line(aes(x = sm, y = Dip_list, group=1), color = "blue", size = 0.6) +
  geom_line(aes(x = sm, y = Dio_list, group=2), color = "red", size = 0.6) +
  geom_point(aes(x = sm, y = Dip_list, group=1), color = "blue", size = 1) +
  geom_point(aes(x = sm, y = Dio_list, group=2), color = "red", size = 1) +
  xlab('Experiment Days') +   
  ylab('DC Power') + 
  scale_x_continuous(breaks=seq(0, 105, 5)) +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=45, hjust=1)) 

print(p)
dev.off()


#### ........................................................................... ####.
  teste_data <- c()
  teste_data <- data.frame(cbind(sm, Dip_list, Dio_list))
  teste_data <- cbind(teste_data, "T1" = c(1-(Dip_list/Dio_list))*100)
  
  teste_data2 <- cbind(teste_data, "T2" = c( round( ( (Dio_list - Dip_list) / (Dip_list) )* 100, 6) ) )
#  perc1 <- round(((exp(Do1[nd1]) - exp(De1[nd1]))/exp(De1[nd1]))*100, 5)

#  gp <- c()
  # Predito   Predicted     Observado    Observed
  gp <- ggplot(data = teste_data, aes(x = sm)) + theme_bw() +
    geom_line(aes(y= Dip_list, group = 1, color = "Predito"), size = 0.6, lty = "81") +
    geom_point(aes(y = Dip_list, color = "Predito"), size= 1.0) + 
    
    geom_line(aes(y= Dio_list, group = 2, color = "Observado"), size = 0.6) +
    geom_point(aes(y = Dio_list, color = "Observado"), size= 1.0) +

    xlab('Dias') +   ylab('Potência DC') +
    scale_x_continuous(breaks=seq(0, 105, 5))+ guides(fill=guide_legend(title=NULL)) + 
    theme(text = element_text(size=16),
        axis.text.x = element_text(angle=45, hjust=1),
        legend.justification=c(1,0), legend.position=c(1,0),
        legend.background = element_rect(fill = "lightgray"),
        ) + 
    scale_colour_manual(name=NULL ,
        values=c("Predito" = "blue", "Observado" ="red")) 
  gp 
  print(gp)
  
dev.off()
### ...........................................................................................



##### 3.3 - Erro quadrático Médio Diário (p/ 105 dias) #####
#eqm <- numeric()
for(i in 1:(length(id.fi))){         
  dia <- i
  xi <- id.in[dia] 
  xf <- id.fi[dia]
  Dio <- NDA[xi:xf,6]
  Dip <- pred9[xi:xf]
  eqm[i+84] <- mean((Dio - Dip)^2)
}


nome_arquivo <- paste ("eqm1_", i+84, ".png", sep="")
pathDest <- paste(local_pasta, "graphs\\EQM\\", sep = "")
fileDest <- paste(pathDest, nome_arquivo, sep = "")
png(filename = fileDest, width = 720, height = 480, units = 'px')

sm <- seq(1,length(id.fi)+84)
plot(sm, eqm, type="l", xlim=c(0,106),  ylim=c(0,max(eqm)), ylab="EQM", xlab="Dia")
points(sm, eqm, pch=19)

media_eqm_graf <- mean(eqm)

xa <- seq(1, 105, 0.1)
ya <- rep(media_eqm_graf, length(xa))
lines(xa, ya, lty=2)

dev.off()


##### 3.4 - Diferença Percentual Média (p/ 105 dias) ######   
#i <- 1 
#Dim <- numeric()
#Dim2 <- numeric()
for(i in 1:length(id.fi)){
  dia <- i
  xi <- id.in[dia] 
  xf <- id.fi[dia]
  Dio <- NDA[xi:xf,6]
  Dip <- as.numeric(pred9[xi:xf])
  
#  Dim[i+84] <- round(mean(((Dio - Dip)/Dio)*100), 4)
#  Dim2[i+84] <- round(mean(((Dio - Dip)/Dip)*100), 4)
  
  Dim[i+84] <- round(mean(( (exp(Dio) - exp(Dip) ) / exp(Dip) ) * 100), 6)
} 


###### 3.4.1 - Grafico em Barras das Diferenças Percentuais Medias #####

#### Informações para Salvar a imagem 
nome_arquivo <- paste ("dif_perc1_", i+84, ".png", sep="")
pathDest <- paste(local_pasta, "graphs\\DPM\\", sep = "")
fileDest <- paste(pathDest, nome_arquivo, sep = "")
png(filename = fileDest, width = 720, height = 480, units = 'px')


data_Bar2 <- data.frame (ListaDias_Bar = seq(1,length(id.fi)+84),
                         value = round(Dim, 4))


ggplot(data_Bar2, aes(x=ListaDias_Bar, y=value)) + 
  #  geom_bar(stat = "identity", colour="dodgerblue", fill="white") +
  geom_bar(stat = "identity", colour="black", fill="grey") + theme_bw() + 
  #  geom_text(aes(x = ListaDias_Bar, y = Dim, label = round(Dim, 4)), vjust=1.0,) +  
  #  ggtitle("Differences (percentage): Observed and Estimated") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y= "Diferenca Percentual Media", x = "Dias") + scale_y_continuous(limits = c(min(Dim)-.03,max(Dim)+.03))

dev.off()
