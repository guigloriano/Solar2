
#### 0.0 - Cabeçalho: Declaração das Bibliotecas ####

# rm(list=ls(all=TRUE))
set.seed(19)
library(compiler)
enableJIT(3)

library(lme4)
library(tidyverse)  
library(gdata)
library(nlme)
library(RVAideMemoire)
library(readODS)
library(car)
require(MASS)
library(predictmeans)
library(ggplot2)
library(ggpubr)
library(nlraa)



####  0.1 - Caminho dos Arquivos ####  
# >>>> INSIRA A PASTA RAIZ NESTE TRECHO <<<
local_pasta <- "D:\\Solar2\\Parte2\\"

#### 1.0 - Leitura do Primeiro Dataset ####
caminho_dados <- paste(local_pasta, "datasets\\csv\\1_21.csv", sep = "")
dados_treino <- read.csv(caminho_dados)

##### 1.1 - Ajustes no Dataset para o Modelo #####
str(dados_treino)
D <- data.frame(dados_treino)

##### 1.2 - Remoção da Primeira Coluna #####
D$X <- NULL     


##### 1.3 - Remoção dos PDC's = 0 #####
D <- D[!(D$PDC == 0),]
n <- nrow(D)


##### 1.4 - Cálculo de IRR e PDC acumulados #####
id.fi <- cumsum(as.numeric(ftable(D[,1]))) 
id.in <- c(1,id.fi+1)
AIRR <- 0
APDC <- 0


for(d in 1:length(id.fi)){
  AIRR <- c(AIRR, cumsum(D[id.in[d]:id.fi[d],3]))
  APDC <- c(APDC, cumsum(D[id.in[d]:id.fi[d],6]))
}

DA <- D
DA[,3] <- AIRR[-1]     
DA[,6] <- APDC[-1]



##### 1.5 - Transformação Log #####
DA[,1] <- factor(DA[,1])     
DA[,2] <- DA[,2] - 1
DA[,3] <- log(DA[,3])
DA[,4] <- log(DA[,4])
DA[,5] <- log(DA[,5])     # Coluna da Massa Acumulada
DA[,6] <- log(DA[,6])
DA <- data.frame(DA)



#### 2.0 - Treino do Modelo para Aplicação nos 105 dias ###### 

f.logi <- function(x1, alpha0, beta0, beta1){
  (alpha0) - log(1 + exp(beta0 - beta1*x1))
} 

#####  2.1 - Modelo M9 (nlme: alpha0 e beta0 | corr: IRR e MA) #####
v9 <- c(1,1,1,1,1,1,1) 
M9 <- nlme(PDC ~ f.logi(TIME, alpha0, beta0, beta1), 
           data = DA, 
           ### inserção destas linhas para convergência do modelo ###
           #                  control = nlmeControl(maxIter=1000,
           #                                 opt = c("nlminb"),
           #                                msMaxIter = 1000,
           #                  ),
           ### .................................................. ###
           fixed=list(alpha0 ~ IRR + MA, beta0 ~ IRR + MA, beta1 ~ 1),
           random = list(alpha0 ~ 1, beta0 ~ 1),
           correlation = corAR1(0.2, form = ~ TIME|DIA),
           weights = varPower(),   
           start=v9, groups = ~ DIA)

pred9 <- predict(M9)
EQM9 <- round(mean((pred9 - DA[,6])^2), 5)

summary(M9)





# https://stackoverflow.com/questions/40901445/function-to-calculate-r2-r-squared-in-r
R2_treino = cor(pred9, DA$PDC) ^ 2




rss_treino <- sum((pred9 - DA$PDC) ^ 2)  ## residual sum of squares
tss_treino <- sum((DA$PDC - mean(DA$PDC)) ^ 2)  ## total sum of squares
rsq_treino <- 1 - rss_treino/tss_treino





# 2. R2 Score components
# 2.1. Average of actual data
avr_y_actual <- mean(DA$PDC)

# 2.2. Total sum of squares
ss_total <- sum((DA$PDC - avr_y_actual)^2)

# 2.3. Regression sum of squares
ss_regression <- sum((pred9- avr_y_actual)^2)

# 2.4. Residual sum of squares
ss_residuals <- sum((DA$PDC - pred9)^2)

# 3. R2 Score
r2 <- 1 - ss_residuals / ss_total





MSE_treino = (sum(DA$PDC - pred9)^2)/length(pred9)



# https://www.statology.org/adjusted-r-squared-in-r/
AdjustedR2_treino = 1 - ( (1-rsq_treino)*(1625-1)/(1625-2-1) )

# Adjusted R2 = 1 - [(1-R2)*(n-1)/(n-k-1)]
# where:

# R2: The R2 of the model
# n: The number of observations         = 1625
# k: The number of predictor variables  = 5












# sigma(M9) # MSE: 38.16482

AIC(M9)   # -15395.29
BIC(M9)   # -15325.18
EQM9      # 7e-04

# Model_M9 <- c(AIC(M9), BIC(M9), EQM9)



#### 3.0 - Graficos por dia ####
id.fi <- cumsum(as.numeric(ftable(D[,1]))) 
id.in <- c(1,id.fi+1)



##### 3.1 - Graficos do Dia 1 #####
dia <- 2
xi <- id.in[1] 
xf <- id.fi[1]


###### 3.1.1 - Tempo x PCD (Observado) ###### 
Dio <- DA[xi:xf,6]
plot(DA[xi:xf,2], Dio, type="l", lwd=2, ylab="Values Y", xlab="Tempo", xlim=c(0,length(Dio)), col="black")
points(DA[xi:xf,2], Dio, pch=1, lwd=2, col="black")


###### 3.1.2 - Tempo x PDC (Predito) ##### 
# d1 <- DA[xi:xf,2]
# d2 <- DA[xi:xf,3]
# d3 <- DA[xi:xf,5]

Dip <- as.numeric(pred9[xi:xf])
points(DA[xi:xf,2], Dip, col="red", type="l", lwd=2)
ni <- length(Dip)
legend("topleft", legend=c("Observado","Esperado"), col=c("black","red"), lty=1, pch=c(1, NA), lwd=2)


###### 3.1.3 - Erro Qudratico médio do dia ######
EQM <- mean((Dio - Dip)^2)
EQM 


#### 4.0 - Diferença Percentual Média (p/ 21 dias) #####   
id.fi <- cumsum(as.numeric(ftable(D[,1]))) 
id.in <- c(1,id.fi+1)

perc_list <- c()
MSE_list <- c()

for(i in 1:length(id.fi)){
  dia <- i
  xi <- id.in[dia] 
  xf <- id.fi[dia]
  Dio <- DA[xi:xf,6]
  Dip <- as.numeric(pred9[xi:xf])
  nd_l <- length(Dio)
  
  perc_list <- c(perc_list, round(mean(((exp(Dio) - exp(Dip)) / exp(Dip))*100), 6))
  MSE_list <- c(MSE_list, round(mean((Dio - Dip)^2), 6))
} 


##### 4.1 - Gráfico de Barras da Dif. Percentual Média (p/ 21 dias) #####
nome_arquivo <- paste ("percentage_ObsPred.png", sep="")
pathDest <- paste(local_pasta, "graphs\\", sep = "")
fileDest <- paste(pathDest, nome_arquivo, sep = "")
png(filename = fileDest, width = 900, height = 500, units = 'px')

    data_Bar <- data.frame (ListaDias_Bar = seq(1,length(id.fi)),
                             value = round(perc_list, 4))
    
    p = ggplot(data_Bar, aes(x=ListaDias_Bar, y=value)) + 
        #  geom_bar(stat = "identity", colour="dodgerblue", fill="white") +
        geom_bar(stat = "identity", colour="black", fill="grey") + 
        geom_text(aes(x = ListaDias_Bar, y = perc_list, label = round(perc_list, 4)),
                  vjust=1.0,) +  theme_bw() + 
        ggtitle("Differences (percentage): Observed and Estimated") + theme(plot.title = element_text(hjust = 0.5)) +
        labs(y= "Percentage", x = "Days") + scale_y_continuous(limits = c(-1.4,1.5))

print(p)
dev.off()






