#### Script para Avaliação dos Modelos para 21 Dias ####

#####  1.0 - Declaração das Bibliotecas #####
# rm(list=ls(all=TRUE))
# set.seed(19)

library(compiler)
enableJIT(3)
    
library(lme4)
    
#remove.packages("rlang")
#install.packages("rlang")
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


##### 2.0 - Leitura dos dados ##### 
dados <- read.csv("D:\\Solar2\\Parte2\\datasets\\csv\\1_21.csv")
str(dados)
D <- data.frame(dados)

###### 2.1 - Ajuste do Dataset ######
####### 2.1.1 - Remoção da Primeira Coluna #######
D$X <- NULL     


####### 2.1.2 - Remoção dos PDC's = 0 ####### 
D <- D[!(D$PDC == 0),]
n <- nrow(D)


####### 2.1.5 - Análise Descritiva das Variáveis D = 21 ####### 
D[,1] <- factor(D[,1]) 
summary(D)
sd(D[,3])     # Desvio Padrão da IRR
sd(D[,4])     # Desvio Padrão da TEMP
sd(D[,5])     # Desvio Padrão da MA
sd(D[,6])     # Desvio Padrão do PDC


####### 2.1.4 - Verificação da Correlação das Variáveis ####### 
cor(D[, 3:5])


####### 2.1.5 - Cálculo da IRR, PDC, e TEMP acumuladas ####### 
id.fi <- cumsum(as.numeric(ftable(D[,1]))) 
id.in <- c(1,id.fi+1)
AIRR <- 0
APDC <- 0
ATEMP <- 0 

for(i in 1:length(id.fi)){
    AIRR <- c(AIRR, cumsum(D[id.in[i]:id.fi[i],3]))
    APDC <- c(APDC, cumsum(D[id.in[i]:id.fi[i],6]))
    ATEMP <- c(ATEMP, cumsum(D[id.in[i]:id.fi[i],4]))
}
            
DA <- D 
DA[,3] <- AIRR[-1]     
DA[,6] <- APDC[-1]

            
####### 2.1.6 - Transformação Log ####### 
DA[,1] <- factor(DA[,1])     
DA[,2] <- DA[,2] - 1
DA[,3] <- log(DA[,3])
DA[,4] <- log(DA[,4])
DA[,5] <- log(DA[,5])
DA[,6] <- log(DA[,6])
            
DA <- data.frame(DA)




##### 3.0 - Gráficos dos dias 1 e 2 #####

fileDest <- paste("D:\\Solar2\\Parte2\\graphs\\", "Fig5a3_AccSolarPower.png", sep = "")
png(filename = fileDest, width = 625, height = 405, units = 'px')

df_dia1 <- numeric()
df_dia1 <- data.frame("Time"= DA[id.in[1]:id.fi[1],2], 
                      "Value" = exp(DA[id.in[1]:id.fi[1],6]))

plot_dia1 <- ggplot(df_dia1, aes(x = Time, y = Value)) +
                geom_line(size=1) + geom_point(size=3) + theme_bw() +
                scale_x_continuous(name = "Time") + 
                scale_y_continuous(name = "Accumulated Solar Power (W)",
                                   labels = function(l) ifelse(l <= 9999, l, comma(l))) +
                theme(
                  axis.text.y = element_text(hjust = 0.5,angle=90, size=18),
                  axis.text.x = element_text(size=18),
                  axis.title.x = element_text(hjust = 0.5, size=18),
                  axis.title.y = element_text(hjust = 0.5, size=18),
                  plot.title = element_text(hjust = 0.5, size=26)
                )

plot(plot_dia1)
dev.off()


#plot(DA[id.in[1]:id.fi[1],2], exp(DA[id.in[1]:id.fi[1],6]), type="l", 
#     lwd=2, ylab="Accumulated Solar Power (W)", xlab="Time", xlim=c(0,80), 
#     ylim=c(0,330000))
#points(DA[id.in[1]:id.fi[1],2], exp(DA[id.in[1]:id.fi[1],6]), pch=19)
#grid(nx = NULL, ny = NULL,
#     lty = 2,      # Grid line type
#     col = "gray", # Grid line color
#     lwd = 1)      # Grid line width


fileDest <- paste("D:\\Solar2\\Parte2\\graphs\\", "Fig5b3_LogAccSolarPower.png", sep = "")
png(filename = fileDest, width = 625, height = 405, units = 'px')

df_dia1 <- numeric()
df_dia1 <- data.frame("Time"= DA[id.in[1]:id.fi[1],2], 
                      "Value" = DA[id.in[1]:id.fi[1],6])

plot_dia1_log <- ggplot(df_dia1, aes(x = Time, y = Value)) +
  geom_line(size=1) + geom_point(size=3) + theme_bw() +
  scale_x_continuous(name = "Time") + 
  scale_y_continuous(name = "Log of the Accumulated Solar Power (W)",
                     labels = function(l) ifelse(l <= 9999, l, comma(l))) +
  theme(
    axis.text.y = element_text(hjust = 0.5,angle=90, size=18),
    axis.text.x = element_text(size=18),
    axis.title.x = element_text(hjust = 0.5, size=18),
    axis.title.y = element_text(hjust = 0.5, size=18),
    plot.title = element_text(hjust = 0.5, size=26)
  )

plot(plot_dia1_log)
dev.off()


#plot(DA[id.in[1]:id.fi[1],2], DA[id.in[1]:id.fi[1],6], type="l", lwd=2, 
#     ylab="Log of the Accumulated Solar Power (W)", xlab="Time", xlim=c(0,80), ylim=c(4,12.7))
#points(DA[id.in[1]:id.fi[1],2], DA[id.in[1]:id.fi[1],6], pch=19)
#grid(nx = NULL, ny = NULL,
#     lty = 2,      # Grid line type
#     col = "gray", # Grid line color
#     lwd = 1)      # Grid line width




plot(DA[id.in[1]:id.fi[1],2], exp(DA[id.in[1]:id.fi[1],6]), type="l", 
     lwd=2, ylab="Power Generated", xlab="Time")
points(DA[id.in[1]:id.fi[1],2], exp(DA[id.in[1]:id.fi[1],6]), pch=19)



plot(DA[id.in[2]:id.fi[2],2], DA[id.in[2]:id.fi[2],6], type="l", lwd=2, 
     ylab="Log-measures", xlab="Time")
points(DA[id.in[2]:id.fi[2],2], DA[id.in[2]:id.fi[2],6], pch=19)


##### 4.0 - Modelos Preditivos ##### 

###### 4.1 - Inicialização da Tabela de Métricas ######
Metrics <- c("AIC", "BIC", "MSE")

f.logi <- function(x1, alpha0, beta0, beta1){
              (alpha0) - log(1 + exp(beta0 - beta1*x1))
              } 

####### 4.2 - Modelo M0 (Linear) ####### 

M0 <- lm(formula = PDC ~ IRR + MA,
         data = DA,)
pred0 <- predict(M0)

EQM0 <- round(mean((pred0 - DA[,6])^2), 4)

summary (M0)

AIC(M0) # -4495.575
BIC(M0) # -4474.002
EQM0    # 0.0037

Model_M0 <- c(AIC(M0), BIC(M0), EQM0)

####### 4.3 - Modelo M1 (LogLogistico) ####### 
v1 <- c(1,1,0.1)    
M1 <- nls(PDC ~ f.logi(TIME, alpha0, beta0, beta1), 
                data = DA, 
                start=c(alpha0=1, beta0=1, beta1=0.1)
          )    

pred1 <- predict(M1)
EQM1 <- round(mean((pred1 - DA[,6])^2), 4)

summary(M1)

AIC(M1) # 2834.606
BIC(M1) # 2856.179
EQM1    # 0.3334

Model_M1 <- c(AIC(M1), BIC(M1), EQM1)


           
####### 4.4 - Modelo M2 (nlme: alpha0) ####### 
v2 <- c(1, 1, 0.2)    
M2 <- nlme(PDC ~ f.logi(TIME, alpha0, beta0, beta1), 
                 data = DA, 
                 fixed=list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
                 random = list(alpha0 ~ 1),
                 start=v2,
                 groups = ~ DIA
           )

pred2 <- predict(M2)
EQM2 <- round(mean((pred2 - DA[,6])^2), 4)

summary(M2)

AIC(M2) # 2245.463
BIC(M2) # 2272.429
EQM2    # 0.2181

Model_M2 <- c(AIC(M2), BIC(M2), EQM2)
           


####### 4.5 - Modelo M3 (nlme: beta0) ####### 
v3 <- c(1, 1, 0.1)    
M3 <- nlme(PDC ~ f.logi(TIME, alpha0, beta0, beta1), 
                 data = DA, 
                 fixed=list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
                 random = list(beta0 ~ 1),
                 start=v3, 
                 groups = ~ DIA
           )

pred3 <- predict(M3)
EQM3 <- round(mean((pred3 - DA[,6])^2), 4)

AIC(M3) # 2730.587       
BIC(M3) # 2757.554
EQM3    # 0.2998

Model_M3 <- c(AIC(M3), BIC(M3), EQM3)


           
####### 4.6 - Modelo M4 (nlme: beta1) ####### 
v4 <- c(1, 1, 0.1)    
M4 <- nlme(PDC ~ f.logi(TIME, alpha0, beta0, beta1), 
                 data = DA, 
                 fixed=list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
                 random = list(beta1 ~ 1),
                 start=v4, 
                 groups = ~ DIA
           )

pred4 <- predict(M4)
EQM4 <- round(mean((pred4 - DA[,6])^2), 4)
           
AIC(M4)  # 2351.5   
BIC(M4)  # 2378.466
EQM4     # 0.2338

Model_M4 <- c(AIC(M4), BIC(M4), EQM4)
           


####### 4.7 - Modelo M5 (nlme: alpha0 e beta0) ####### 
v5 <- c(1, 1, 0.1)    
M5 <- nlme(PDC ~ f.logi(TIME, alpha0, beta0, beta1), 
                 data = DA, 
                 fixed=list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
                 random = list(alpha0 ~ 1, beta0 ~ 1),
                 start=v5, groups = ~ DIA
           )

pred5 <- predict(M5)
EQM5 <- round(mean((pred5 - DA[,6])^2), 4)

AIC(M5) #  2191.774 
BIC(M5) #  2229.527
EQM5    #  0.2056 
           
Model_M5 <- c(AIC(M5), BIC(M5), EQM5)



####### 4.8 - Modelo M6 (nlme: alpha0 e beta1) #######
v6 <- c(1, 1, 0.1)    
M6 <- nlme(PDC ~ f.logi(TIME, alpha0, beta0, beta1), 
                 data = DA, 
                 fixed=list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
                 random = list(alpha0 ~ 1, beta1 ~ 1),
                 start=v6, 
                 groups = ~ DIA
           )

pred6 <- predict(M6)
EQM6 <- round(mean((pred6 - DA[,6])^2), 4)
           
AIC(M6)  # 2248.816   
BIC(M6)  # 2286.569   
EQM6     # 0.218

Model_M6 <- c(AIC(M6), BIC(M6), EQM6)


           
####### 4.9 - Modelo M7 (nlme: beta0 e beta1) #######  
v7 <- c(1, 1, 0.1)    
M7 <- nlme(PDC ~ f.logi(TIME, alpha0, beta0, beta1), 
                 data = DA, 
                 fixed=list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
                 random = list(beta0 ~ 1, beta1 ~ 1),
                 start=v3, groups = ~ DIA
           )

pred7 <- predict(M7)
EQM7 <- round(mean((pred7 - DA[,6])^2), 4)

summary(M7)

AIC(M7)  # 2234.82   
BIC(M7)  # 2272.573        
EQM7     # 0.2119 

Model_M7 <- c(AIC(M7), BIC(M7), EQM7)


           
####### 4.10 - Modelo M8 (nlme: alpha0, beta0 e beta1) ####### 
v8 <- c(1, 1, 0.1)    
M8 <- nlme(PDC ~ f.logi(TIME, alpha0, beta0, beta1), 
            data = DA, 
            ### inserção destas linhas para convergência do modelo ###
            control = nlmeControl(maxIter=100000,
                                  opt = c("nlminb"), #nlminb
                                  msMaxIter = 100000,
            ),
            ### .................................................. ###
            fixed=list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
            random = list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
            start=v8, 
            groups = ~ DIA
)

pred8 <- predict(M8)
EQM8 <- round(mean((pred8 - DA[,6])^2), 4)

summary(M8)

AIC(M8)  # 2090.743  
BIC(M8)  # 2144.676
EQM8     # 0.1889

Model_M8 <- c(AIC(M8), BIC(M8), EQM8)



####### 4.11 - Modelo M9 (nlme: alpha0 e beta0 | corr: IRR e MA) ####### 
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
EQM9 <- round(mean((pred9 - DA[,6])^2), 4)

summary(M9)

AIC(M9)   # -15395.29
BIC(M9)   # -15325.18
EQM9      # 7e-04

Model_M9 <- c(AIC(M9), BIC(M9), EQM9)

       
       
####### 4.12 - Modelo M10 (nlme: alpha0 e beta0 | corr: IRR) ####### 
v10 <- c(1,1,1,1,1) 
M10 <- nlme(PDC ~ f.logi(TIME, alpha0, beta0, beta1), 
                      data = DA, 
                      fixed=list(alpha0 ~ IRR, beta0 ~ IRR, beta1 ~ 1),
                      random = (list(alpha0 ~ 1, beta0 ~ 1)),
                      correlation = corAR1(0.2, form = ~ TIME|DIA),
                      weights = varPower(),   
                      start=v10, groups = ~ DIA)

pred10 <- predict(M10)
EQM10 <- round(mean((pred10 - DA[,6])^2), 4)

summary(M10)
fixef(M10)      # Extract Fixed Effects
#VarCorr(M10)
#getVarCov(M10)


AIC(M10)
BIC(M10)
EQM10

Model_M10 <- c(AIC(M10), BIC(M10), EQM10)
         
TableMetrics <- data.frame(Metrics, Model_M0, Model_M1, Model_M2, Model_M3,
                           Model_M4, Model_M5, Model_M6, Model_M7, Model_M8,
                           Model_M9, Model_M10)             
       

###### 5.0 - Gráficos dos Modelos ###### 
id.fi <- cumsum(as.numeric(ftable(D[,1]))) 
id.in <- c(1,id.fi+1)


####### 5.1 - Grafico dos Valores Preditos x Resíduos ####### 
par(mai=c(1,1,0.3,0.3))
res <- as.numeric(residuals(M9))
plot(pred9, res, ylim=c(-1.5,1.5), ylab="Residuals",xlab="Predicted values")
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "gray", # Grid line color
     lwd = 1)      # Grid line width

####### 5.2 - Grafico dos valores registrados x preditos ####### 
plot(DA[,6], pred9, ylab="Predicted values", xlab="Observed values")
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "gray", # Grid line color
     lwd = 1)      # Grid line width         


####### 5.3 - Graficos dos 2 Primeiros Dias #######

######## 5.3.1 - Graficos do Dia 1 ########
xi1 <- id.in[1] 
xf1 <- id.fi[1]


######## 5.3.1.1 - Tempo x PDC Acumulado (Escala Original) ######## 
Di1 <- cumsum(D[xi1:xf1,6])
plot(D[xi1:xf1,2], Di1, pch=19, lwd=2, ylab="X", xlab="Tempo", xlim=c(0,length(Di1)))
points(D[xi1:xf1,2], Di1, col="black", type="l", lwd=2)
          

######## 5.3.1.2 - Tempo x Valores Preditos e Observados ######## 
Do1 <- DA[xi1:xf1,6]
De1 <- as.numeric(pred9[xi1:xf1])
nd1 <- length(Do1)
        
## Log-scale
plot(D[xi1:xf1,2], Do1, col="grey70", pch=19, lwd=2, ylab="Y's values", xlab="Time", xlim=c(0, 77), ylim=c(3,13))
points(D[xi1:xf1,2], De1, col="black", type="l", lwd=2)
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "gray", # Grid line color
     lwd = 1)      # Grid line width         

## origina;-scale
plot(D[xi1:xf1,2], exp(Do1), pch=19, col="grey70", lwd=2, ylab="X", xlab="Tempo", xlim=c(0,length(Do1)))
points(D[xi1:xf1,2], exp(De1), col="black", type="l", lwd=2)
          

perc1 <- round(((exp(Do1[nd1]) - exp(De1[nd1]))/exp(De1[nd1]))*100, 5)
perc1

MSE1 <- round(mean((Do1 - De1)^2), 5)
MSE1






######## 5.3.2 - Graficos do Dia 2 ########
xi2 <- id.in[2] 
xf2 <- id.fi[2]

######## 5.3.2.1 - Tempo x PDC Acumulado (Escala Original) ######## 
Di2 <- cumsum(D[xi2:xf2,6])
plot(D[xi2:xf2,2], Di2, pch=19, lwd=2, ylab="X", xlab="Tempo", xlim=c(0,length(Di2)))
points(D[xi2:xf2,2], Di2, col="black", type="l", lwd=2)


######## 5.3.2.2 - Tempo x Valores Preditos e Observados ######## 
Do2 <- DA[xi2:xf2,6]
De2 <- as.numeric(pred9[xi2:xf2])
nd2 <- length(Do2)

## Log-scale
plot(D[xi2:xf2,2], Do2, col="grey70", pch=19, lwd=2, ylab="Y's values", xlab="Time", xlim=c(0, 77), ylim=c(3,13))
points(D[xi2:xf2,2], De2, col="black", type="l", lwd=2)
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "gray", # Grid line color
     lwd = 1)      # Grid line width         


## origina;-scale
plot(D[xi2:xf2,2], exp(Do2), pch=19, col="grey70", lwd=2, ylab="X", xlab="Tempo", xlim=c(0,length(Do2)))
points(D[xi2:xf2,2], exp(De2), col="black", type="l", lwd=2)

#### Percentuais em relaÃ§Ã£o
perc2 <- round(((exp(Do2[nd2]) - exp(De2[nd2]))/exp(De2[nd2]))*100, 4)
perc2

MSE2 <- round(mean((Do2 - De2)^2), 5)
MSE2
          




######## 5.3.2 - Diferença Percentual Média (p/ 21 dias) ########

PE_List <- c()
id.fi <- cumsum(as.numeric(ftable(D[,1]))) 
id.in <- c(1,id.fi+1)

#perc_i <- c()
#MSE_i <- c()
perc_i2 <- c()
MSE_i2 <- c()

for(i in 1:length(id.fi)){
  
  Do_i <- DA[id.in[i]:id.fi[i],6]
  De_i <- as.numeric(pred9[id.in[i]:id.fi[i]])
  nd_i <- length(Do_i)
  
# perc_i <- c(perc_i, round(((exp(Do_i[nd_i]) - exp(De_i[nd_i]))/exp(De_i[nd_i]))*100, 6))
#  MSE_i <- c(MSE_i, round(mean((Do_i[nd_i] - De_i[nd_i])^2), 8))
  
  perc_i2 <- c(perc_i2, round(mean(((exp(Do_i) - exp(De_i)) / exp(De_i))*100), 6))
  MSE_i2 <- c(MSE_i2, round(mean((Do_i - De_i)^2), 6))
  
}

#### Trecho removido de um código original para conferir como está sendo
#### calculada a Dif. Percentual (perce_i2) e o Erro Quadrático Médio (MSE_i2)

##### DiferenÃ§a Percentual mÃ©dia
# for(i in 1:21){
#   dia <- i
#   xi <- id.in[dia] 
#   xf <- id.fi[dia]
#   Dio <- DA[xi:xf,6]
#   Dip <- as.numeric(pred3[xi:xf])
#   Dim[i] <- round(mean(((Dio - Dip)/Dio)*100), 4)
# } 

##### EQM
# d1 <- NDA[xi:xf,2]
# d2 <- NDA[xi:xf,3]
# d3 <- NDA[xi,xf,5]
# Dip <- pred3[xi:xf]
# points(NDA[xi:xf,2], Dip, col="red", type="l", lwd=2)
# legend("topleft", legend=c("Observado","Esperado"), col=c("black","red"), lty=1, pch=c(1, NA), lwd=2)
# EQMi <- mean((Dio - Dip)^2)
# EQMi




######## 5.3.3 -  - Gráfico de Barras ########
data_Bar <- data.frame (ListaDias_Bar = c("01","02","03","04","05", "06", "07",
                                           "08","09","10","11","12", "13", "14",
                                           "15","16","17","18","19", "20", "21"),
                        value = round(perc_i2, 4))


ggplot(data_Bar, aes(x=ListaDias_Bar, y=value)) + 
#  geom_bar(stat = "identity", colour="dodgerblue", fill="white") +
  geom_bar(stat = "identity", colour="black", fill="grey") + 
  geom_text(aes(x = ListaDias_Bar, y = perc_i2, label = round(perc_i2, 4)),
            vjust=1.0,) +  theme_bw() + 
  ggtitle("Differences (percentage): Observed and Estimated") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(y= "Percentage", x = "Days") + scale_y_continuous(limits = c(-1.4,1.5))


