
#### 0.0 - Inicio do Código Automatizado para Executar os 105 Dias####

##### 0.1 - Definição do Caminho "raiz" do script ####
local_pasta <- "D:\\Solar2\\Parte2\\"

##### 0.2 - Dependências do Script #####

# Para executar esse script é necessário executar antes o scritp:
#     - 001a_nlme_treino21 

cod_treino <- paste(local_pasta, "scripts\\001a_nlme_treino21.R", sep = "")
source(cod_treino)


#### 1.0 - Leitura dos Caminhos dos Datasets ####
caminho_dia01a21 <- paste("D:\\Solar2\\Parte2\\datasets\\csv\\1_21.csv")
caminho_dia22a42 <- paste("D:\\Solar2\\Parte2\\datasets\\csv\\22_42.csv")
caminho_dia43a63 <- paste("D:\\Solar2\\Parte2\\datasets\\csv\\43_63.csv")
caminho_dia64a84 <- paste("D:\\Solar2\\Parte2\\datasets\\csv\\64_84.csv")
caminho_dia85a105 <- paste("D:\\Solar2\\Parte2\\datasets\\csv\\85_105.csv")

##### 1.1 - Criação da Lista de Caminhos #####
TodosCaminhos <- c(caminho_dia01a21, caminho_dia22a42, caminho_dia43a63,
                   caminho_dia64a84, caminho_dia85a105)



for(j in 1:length(TodosCaminhos)){
  # Mostrar Qual é o Caminho Atual do Arquivo a Ser Lido
  out1 <- paste0("Arquivo Lido: ", TodosCaminhos[j])
  print(out1)
  
  # Indicar os índices dos Dias do dataset 
  out2 <- paste0("Dias: ", (j+(j-1)*20), " ao dia ", (j*21) )
  print(out2)
}



xi <- numeric()
xf <- numeric()

Dip_list <- numeric()
Dio_list <- numeric()

ADIO_O_list <- numeric()
ADIP_O_list <- numeric()

Dim <- numeric()
Dim2 <- numeric()
EQMi <- numeric()


PDC_list_treino <- numeric() 
pred9_list_treino <- numeric()
PDC_list_teste <- numeric()
pred9_list_teste <- numeric()


j = 1

for(j in 1:length(TodosCaminhos)){
  
  # Mostrar Qual é o Caminho Atual do Arquivo a Ser Lido
  out1 <- paste0("Arquivo Lido: ", TodosCaminhos[j])
  print(out1)
  
  # Indicar os índices dos Dias do dataset 
  out2 <- paste0("Dias: ", (j+(j-1)*20), " ao dia ", (j*21) )
  print(out2)
  
  ###### 1.2 - Leitura do Dataset a ser manipulado #####
  dados_teste <- read.csv(TodosCaminhos[j])
  
  
  ##### 1.3 - Ajustes no Dataset para o Modelo #####
  str(dados_teste)
  ND <- data.frame(dados_teste)
  
  ##### 1.4 - Remoção da Primeira Coluna (contador de amostras) #####
  ND$X <- NULL     
  
  
  ##### 1.5 - Remoção dos PDC's = 0 #####
  ND <- ND[!(ND$PDC == 0),]
  nr <- nrow(ND)
  
  
  ##### 1.6 - Cálculo de IRR e PDC acumulados #####
  id.fi <- cumsum(as.numeric(ftable(ND[,1]))) 
  id.in <- c(1,id.fi+1)
  AIRR <- 0
  APDC <- 0
  
  for(d in 1:length(id.fi)){
    AIRR <- c(AIRR, cumsum(ND[id.in[d]:id.fi[d],3]))
    APDC <- c(APDC, cumsum(ND[id.in[d]:id.fi[d],6]))
  }
  
  NDA <- ND
  
  
  
  ##### 1.7 - Manter os Valores Originais #####
  NDA$IRR_O <- NDA[,3]      
  NDA$PDC_O <- NDA[,6]      
  NDA$AIRR_O <- AIRR[-1]     
  NDA$APDC_O <- APDC[-1]
  
  
  
  ##### 1.8 - Troca para IRR e PDC acumulados #####
  NDA[,3] <- AIRR[-1]     
  NDA[,6] <- APDC[-1]
  
  
  
  ##### 1.9 - Transformação Log #####
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
  
  ###### 3.1 - Tempo x PDC (Observado e Esperado) #####
  i = 1
  for(i in 1:length(id.fi)){
    
    # i = dia
    # j = é o arquivo
    
    nome_arquivo <- paste ("dia_", (i+(j-1)+(j-1)*20), ".png", sep="")
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
    
    
    
#    Dip_list[ (i+(j-1)+(j-1)*20) ] <- sum(Dip)
#    Dio_list[ (i+(j-1)+(j-1)*20) ] <- sum(Dio)
    Dip_list[ (i+(j-1)+(j-1)*20) ] <- Dip[length(Dip)]
    Dio_list[ (i+(j-1)+(j-1)*20) ] <- Dio[length(Dio)]
    
    
    
    
    ######### 3.1.3 - Testes Auxiliares para Cálculo da POT. REAL ##### 
    ADIO_O_list[ (i+(j-1)+(j-1)*20) ] <- NDA$APDC_O[xf[dia]]
    Dip_O <- exp(Dip)
    ADIP_O_list[ (i+(j-1)+(j-1)*20) ] <- Dip_O[length(Dip_O)]
 
    ######### 3.2 - Cálculo do Erro Quadrático Médio Diário #####
    EQMi[ (i+(j-1)+(j-1)*20) ] <- round(mean((Dio - Dip)^2), 6)
    #    EQMi[ (i+(j-1)+(j-1)*20) ] <- round(mean((Dio[length(Dio)] - Dip[length(Dip)])^2), 6)
    
    ######### 3.4 - Cálculo da Diferença Percentual Média  ######    
    Dim[ (i+(j-1)+(j-1)*20) ] <- round(mean(( (exp(Dio) - exp(Dip) ) / exp(Dip) ) * 100), 6)
    Dim2[ (i+(j-1)+(j-1)*20) ] <- round(mean(( (Dio - Dip) / Dip ) * 100), 6)
    
    
    out <- (i+(j-1)+(j-1)*20)
    print(out)
    
    textoEQM <- paste("MSE:", EQMi[ (i+(j-1)+(j-1)*20) ])
    text(68, 11, textoEQM)
    dev.off()  
    
  }
  
  
  # summary(pred9)     # Coef. de Correlação: Phi =  0.98317 
  # summary(M9)
  # Média dos Erros Quadráticos Médios Diários: 0.00275
  # Mean_EQM <- round( mean(EQMi) , 5)
  

  
  ######### 3.5 - Gráfico: DIP x DIO #####
  nome_arquivo <- paste ("energiaTACC_", (i+(j-1)+(j-1)*20), ".png", sep="")
  pathDest <- paste(local_pasta, "graphs\\2 - DIP x DIO\\", sep = "")
  fileDest <- paste(pathDest, nome_arquivo, sep = "")
  png(filename = fileDest, width = 720, height = 480, units = 'px')
  
  sm <- seq(1,length(id.fi)*j)
  p = ggplot() + theme_bw() + 
    geom_line(aes(x = sm, y = Dip_list, group=1), color = "blue", size = 0.5) +
    geom_line(aes(x = sm, y = Dio_list, group=2), color = "red", size = 0.5) +
    geom_point(aes(x = sm, y = Dip_list, group=1), color = "blue", size = 1) +
    geom_point(aes(x = sm, y = Dio_list, group=2), color = "red", size = 1) +
    
#    geom_line(aes(x = sm, y = ADIP_O_list, group=1), color = "blue", size = 0.5) +
#    geom_line(aes(x = sm, y = ADIO_O_list, group=2), color = "red", size = 0.5) +
#    geom_point(aes(x = sm, y = ADIP_O_list, group=1), color = "blue", size = 1) +
#    geom_point(aes(x = sm, y = ADIO_O_list, group=2), color = "red", size = 1) +
    
    xlab('Dias') +
    ylab('Potência') + 
    scale_x_continuous(breaks=seq(0, 105, 5))  +
    scale_fill_manual(values=c("#a6cee3","#1f78b4","#b2df8a"))
  
  print(p)
  dev.off()
  
  
  ####### 3.6 - Gráfico: EQM/MSE #####
  nome_arquivo <- paste ("eqm_1_", (j*21), ".png", sep="")
  pathDest <- paste(local_pasta, "graphs\\3 - EQM\\", sep = "")
  fileDest <- paste(pathDest, nome_arquivo, sep = "")
  png(filename = fileDest, width = 720, height = 480, units = 'px')
  
  sm <- seq(1,length(id.fi)*j)
  plot(sm, EQMi, type="l", xlim=c(0,length(id.fi)*j),  ylim=c(0,max(EQMi)), ylab="MSE", xlab="Day")
  points(sm, EQMi, pch=19)
  
  dev.off()
  
  ####### 3.7 - Gráfico: Dif. Perc. Média #####
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
  
  #### 4.0 - Separação dos Valores entre Treino/Teste para avaliação ####
  if (j == 1){
    PDC_list_treino <- NDA$PDC
    pred9_list_treino <- pred9
    
  }
  if (j > 1){
    PDC_list_teste <- append(PDC_list_teste, NDA$PDC)
    pred9_list_teste <- append(pred9_list_teste, pred9)
    
  }
  
  
}



#### 5.1 - Métricas para o Treino ####


# https://stackoverflow.com/questions/40901445/function-to-calculate-r2-r-squared-in-r
rss_treino <- sum((pred9_list_treino - PDC_list_treino) ^ 2)  ## residual sum of squares
tss_treino <- sum((PDC_list_treino - mean(PDC_list_treino)) ^ 2)  ## total sum of squares
rsq_treino <- 1 - rss_treino/tss_treino

# 2. R2 Score components
# 2.1. Average of actual data
avr_y_actual <- mean(PDC_list_treino)
# 2.2. Total sum of squares
ss_total <- sum((PDC_list_treino - avr_y_actual)^2)
# 2.3. Regression sum of squares
ss_regression <- sum((pred9_list_treino - avr_y_actual)^2)
# 2.4. Residual sum of squares
ss_residuals <- sum((PDC_list_treino - pred9_list_treino)^2)


###### 5.1.1 - R2 Score (Treino) ######
r2 <- 1 - ss_residuals / ss_total


###### 5.1.2 - EQM/MSE Treino ######
mean(EQMi[1:21])


# https://www.statology.org/adjusted-r-squared-in-r/
AdjustedR2_treino = 1 - ( (1-rsq_treino)*(1625-1)/(1625-2-1) )

# Adjusted R2 = 1 - [(1-R2)*(n-1)/(n-k-1)]
# where:

# R2: The R2 of the model
# n: The number of observations         = 1625
# k: The number of predictor variables  = 5


###### 5.1.3 - Coef. de Correlação (Treino) ######
A_treino = PDC_list_treino - mean(PDC_list_treino)
B_treino = pred9_list_treino - mean(pred9_list_treino)

numerador_treino = sum(A_treino * B_treino)

C_treino = sum (A_treino^2)
D_treino = sum (B_treino^2)

denominador_treino = sqrt( C_treino * D_treino )

r_treino = numerador_treino/denominador_treino



#### 5.2 - Métricas para o Teste ####

PDC_list_teste
pred9_list_teste


rss_test <- sum((pred9_list_teste - PDC_list_teste) ^ 2)  ## residual sum of squares
tss_test <- sum((PDC_list_teste - mean(PDC_list_teste)) ^ 2)  ## total sum of squares


###### 5.2.1 - R2 Score (Teste) ######
rsq_teste <- 1 - rss_test/tss_test



# https://www.statology.org/adjusted-r-squared-in-r/
AdjustedR2_teste = 1 - ( (1-rsq_teste)*(7970-1)/(7970-2-1) )

# Adjusted R2 = 1 - [(1-R2)*(n-1)/(n-k-1)]
# where:

# R2: The R2 of the model
# n: The number of observations         = 7970
# k: The number of predictor variables  = 5

# x_i     :     amostra
# x_barra :     média amostral

# y_i     :     valor predito
# y_barra :     média predita 


###### 5.2.2 - EQM/MSE Teste ######
mean(EQMi[22:105])


###### 5.2.3 - Coef. de correlação (teste) ######
A_teste = PDC_list_teste - mean(PDC_list_teste)
B_teste = pred9_list_teste - mean(pred9_list_teste)

numerador_teste = sum(A_teste * B_teste)

C_teste = sum (A_teste^2)
D_teste = sum (B_teste^2)

denominador_teste = sqrt( C_teste * D_teste )

r_teste = numerador_teste/denominador_teste


##### 5.3 - Métricas Globais #####




PDC_full <- numeric()
pred9_full <- numeric()

PDC_full <- append(PDC_list_treino, PDC_list_teste)
pred9_full <- append(pred9_list_treino, pred9_list_teste)



rss_full <- sum((pred9_full - PDC_full) ^ 2)  ## residual sum of squares
tss_full <- sum((PDC_full - mean(PDC_full)) ^ 2)  ## total sum of squares


###### 5.3.1 - R2 Score (Teste) ######
rsq_global <- 1 - rss_full/tss_full



###### 5.3.2 - MSE Global ######
mean(EQMi)



###### 5.3.3 - Coef. de Relação para Todos os Dados #######

A_full = PDC_full - mean(PDC_full)
B_full = pred9_full - mean(pred9_full)

numerador_full = sum(A_full * B_full)

C_full = sum (A_full^2)
D_full = sum (B_full^2)

denominador_full = sqrt( C_full * D_full )

r_full = numerador_full/denominador_full








