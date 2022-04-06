#### Script para Avaliação dos Modelos para 21 Dias ####

#####  1.0 - Declaração das Bibliotecas #####

if (1 == 1){
  rm(list=ls(all=TRUE))
  set.seed(19)
  
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
  
}


##### 2.0 - Leitura dos dados ##### 
dados1 <- read.csv("D:\\Solar2\\Parte2\\datasets\\csv\\1_21.csv") 
dados2 <- read.csv("D:\\Solar2\\Parte2\\datasets\\csv\\22_42.csv") 
dados3 <- read.csv("D:\\Solar2\\Parte2\\datasets\\csv\\43_63.csv") 
dados4 <- read.csv("D:\\Solar2\\Parte2\\datasets\\csv\\64_84.csv") 
dados5 <- read.csv("D:\\Solar2\\Parte2\\datasets\\csv\\85_105.csv") 


###### 2.1 - Ajuste do Dataset ######


####### 2.1.1 - Agrupar os Datasets ####### 
dados2$DIA <- dados2$DIA + 21 
dados3$DIA <- dados3$DIA + 42 
dados4$DIA <- dados4$DIA + 63 
dados5$DIA <- dados5$DIA + 84 
dadosTotal <- c()
dadosTotal <- rbind(dados1, dados2, dados3, dados4, dados5)


####### 2.1.2 - Remoção da Primeira Coluna #######
dadosTotal$X <- NULL


####### 2.1.3 - Selecionar os 50 Primeiros Dias ####### 
dados50 <- dadosTotal[!(dadosTotal$DIA > 50),]
# dados50 <- dados1
# dados50 <- dadosTotal


####### 2.1.4 - Remoção dos PDC's = 0 ####### 
dados50 <- na.omit(dados50)
dados50 <- dados50[!(dados50$PDC == 0),]


####### 2.1.5 - Finalização dos Ajustes ####### 
str(dados50)
D2 <- data.frame(dados50)
n2 <- nrow(D2)


####### 2.1.6 - Cálculo da IRR, PDC, e TEMP acumuladas ####### 
id.fi2 <- cumsum(as.numeric(ftable(D2[,1]))) 
id.in2 <- c(1,id.fi2+1)
AIRR2 <- 0
APDC2 <- 0
ATEMP2 <- 0 

for(d in 1:length(id.fi2)){
  AIRR2 <- c(AIRR2, cumsum(D2[id.in2[d]:id.fi2[d],3]))
  APDC2 <- c(APDC2, cumsum(D2[id.in2[d]:id.fi2[d],6]))
  ATEMP2 <- c(ATEMP2, cumsum(D2[id.in2[d]:id.fi2[d],4]))
}


DA2 <- D2
DA2[,3] <- AIRR2[-1]     
DA2[,6] <- APDC2[-1]



####### 2.1.7 - Transformação Log ####### 
DA2[,1] <- factor(DA2[,1])     
DA2[,2] <- DA2[,2] - 1
DA2[,3] <- log(DA2[,3])
DA2[,4] <- log(DA2[,4])
DA2[,5] <- log(DA2[,5])
DA2[,6] <- log(DA2[,6])

DA2 <- data.frame(DA2)

####### 2.1.8 - Verificação da Correlação das Variáveis ####### 
cor(DA2[, 3:6])

####### 2.1.9 - Salvar Dataset Ajustado ####### 
# write.table(DA2, file = "C:\\Users\\Guilherme\\Desktop\\data.csv", sep = ",", row.names = F)



##### 3.0 - Gráficos dos dias 1 e 2 #####
plot(DA2[id.in2[1]:id.fi2[1],2], exp(DA2[id.in2[1]:id.fi2[1],6]), type="l", 
     lwd=2, ylab="Power Generated", xlab="Time")
points(DA2[id.in2[1]:id.fi2[1],2], exp(DA2[id.in2[1]:id.fi2[1],6]), pch=19)

plot(DA2[id.in2[2]:id.fi2[2],2], exp(DA2[id.in2[2]:id.fi2[2],6]), type="l", 
     lwd=2, ylab="Power Generated", xlab="Time")
points(DA2[id.in2[2]:id.fi2[2],2], exp(DA2[id.in2[2]:id.fi2[2],6]), pch=19)

plot(DA2[id.in2[1]:id.fi2[1],2], DA2[id.in2[1]:id.fi2[1],6], type="l", lwd=2, 
     ylab="Log-measures", xlab="Time")
points(DA2[id.in2[1]:id.fi2[1],2], DA2[id.in2[1]:id.fi2[1],6], pch=19)

plot(DA2[id.in2[2]:id.fi2[2],2], DA2[id.in2[2]:id.fi2[2],6], type="l", lwd=2, 
     ylab="Log-measures", xlab="Time")
points(DA2[id.in2[2]:id.fi2[2],2], DA2[id.in2[2]:id.fi2[2],6], pch=19)


##### 4.0 - Modelos Preditivos ##### 

###### 4.1 - Inicialização da Tabela de Métricas ######
Metrics <- c("AIC", "BIC", "MSE")


f.logia <- function(x1, alpha0, beta0, beta1){
  (alpha0) - log(1 + exp(beta0 - beta1*x1))
} 

####### 4.2 - Modelo M0 (Linear) ####### 

M0a <- lm(formula = PDC ~ IRR + MA,
         data = DA2,)
pred0a <- predict(M0a)

EQM0a <- round(mean((pred0a - DA2[,6])^2), 4)

summary (M0a)

AIC(M0a) # -4495.575
BIC(M0a) # -4474.002
EQM0a    # 0.0037

Model_M0a <- c(AIC(M0a), BIC(M0a), EQM0a)



####### 4.3 - Modelo M1 (LogLogistico) ####### 
v1a <- c(1,1,0.1)    

M1a <- nls(PDC ~ f.logia(TIME, alpha0, beta0, beta1), 
           data = DA2, 
           start=c(alpha0=1, beta0=1, beta1=0.1) #, start = v1,
)    

pred1a <- predict(M1a)
EQM1a <- round(mean((pred1a - DA2[,6])^2), 4)

summary(M1a)

AIC(M1a)
BIC(M1a)
EQM1a

Model_M1a <- c(AIC(M1a), BIC(M1a), EQM1a)



####### 4.4 - Modelo M2 (nlme: alpha0) ####### 
v2a <- c(1, 1, 0.2)    
M2a <- nlme(PDC ~ f.logia(TIME, alpha0, beta0, beta1), 
            data = DA2, 
            fixed=list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
            random = list(alpha0 ~ 1),
            start=v2a,
            groups = ~ DIA
)

pred2a <- predict(M2a)
EQM2a <- round(mean((pred2a - DA2[,6])^2), 4)

summary(M2a)

AIC(M2a)
BIC(M2a)
EQM2a

Model_M2a <- c(AIC(M2a), BIC(M2a), EQM2a)



####### 4.5 - Modelo M3 (nlme: beta0) ####### 
v3a <- c(1, 1, 0.1)    
M3a <- nlme(PDC ~ f.logia(TIME, alpha0, beta0, beta1), 
            data = DA2, 
            fixed=list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
            random = list(beta0 ~ 1),
            start=v3a, 
            groups = ~ DIA
)

pred3a <- predict(M3a)
EQM3a <- round(mean((pred3a - DA2[,6])^2), 4)

summary(M3a)

AIC(M3a)           
BIC(M3a)
EQM3a

Model_M3a <- c(AIC(M3a), BIC(M3a), EQM3a)



####### 4.6 - Modelo M4 (nlme: beta1) ####### 
v4a <- c(1, 1, 0.1)    
M4a <- nlme(PDC ~ f.logia(TIME, alpha0, beta0, beta1), 
            data = DA2, 
            fixed=list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
            random = list(beta1 ~ 1),
            start=v4a, 
            groups = ~ DIA
)

pred4a <- predict(M4a)
EQM4a <- round(mean((pred4a - DA2[,6])^2), 4)

summary(M4a)

AIC(M4a)     
BIC(M4a)
EQM4a

Model_M4a <- c(AIC(M4a), BIC(M4a), EQM4a)



####### 4.7 - Modelo M5 (nlme: alpha0 e beta0) ####### 
v5a <- c(1, 1, 0.1)    
M5a <- nlme(PDC ~ f.logia(TIME, alpha0, beta0, beta1), 
            data = DA2, 
            fixed=list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
            random = list(alpha0 ~ 1, beta0 ~ 1),
            start=v5a, groups = ~ DIA
)

pred5a <- predict(M5a)
EQM5a <- round(mean((pred5a - DA2[,6])^2), 4)

summary(M5a)

AIC(M5a)     
BIC(M5a)
EQM5a

Model_M5a <- c(AIC(M5a), BIC(M5a), EQM5a)



####### 4.8 - Modelo M6 (nlme: alpha0 e beta1) #######
v6a <- c(1, 1, 0.1)    
M6a <- nlme(PDC ~ f.logia(TIME, alpha0, beta0, beta1), 
            data = DA2, 
            fixed=list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
            random = list(alpha0 ~ 1, beta1 ~ 1),
            start=v6a, 
            groups = ~ DIA
)

pred6a <- predict(M6a)
EQM6a <- round(mean((pred6a - DA2[,6])^2), 4)

summary(M6a)

AIC(M6a)     
BIC(M6a)           
EQM6a

Model_M6a <- c(AIC(M6a), BIC(M6a), EQM6a)



####### 4.9 - Modelo M7 (nlme: beta0 e beta1) #######  
v7a <- c(1, 1, 0.1)    
M7a <- nlme(PDC ~ f.logia(TIME, alpha0, beta0, beta1), 
            data = DA2, 
            fixed=list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
            random = list(beta0 ~ 1, beta1 ~ 1),
            start=v7a, groups = ~ DIA
)

pred7a <- predict(M7a)
EQM7a <- round(mean((pred7a - DA2[,6])^2), 4)

summary(M7a)

AIC(M7a)     
BIC(M7a)           
EQM7a


Model_M7a <- c(AIC(M7a), BIC(M7a), EQM7a)



####### 4.10 - Modelo M8 (nlme: alpha0, beta0 e beta1) ####### 
v8a <- c(1, 1, 0.1)    
M8a <- nlme(PDC ~ f.logia(TIME, alpha0, beta0, beta1), 
            data = DA2, 
            ### inserção destas linhas para convergência do modelo ###
            control = nlmeControl(maxIter=100000,
                                  opt = c("nlminb"), #nlminb
                                  msMaxIter = 100000,
                                  ),
            ### .................................................. ###
            fixed=list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
            random = list(alpha0 ~ 1, beta0 ~ 1, beta1 ~ 1),
            start=v8a, 
            groups = ~ DIA
)

pred8a <- predict(M8a)
EQM8a <- round(mean((pred8a - DA2[,6])^2), 4)

summary(M8a)

AIC(M8a)     
BIC(M8a)  
EQM8a

Model_M8a <- c(AIC(M8a), BIC(M8a), EQM8a)



####### 4.11 - Modelo M9 (nlme: alpha0 e beta0 | corr: IRR e MA) ####### 
v9a <- c(1,1,1,1,1,1,1) 
M9a <- nlme(PDC ~ f.logia(TIME, alpha0, beta0, beta1), 
            data = DA2, 
            ### inserção destas linhas para convergência do modelo ###
            control = nlmeControl(maxIter=100000,
                                  opt = c("nlminb"), #nlminb
                                  msMaxIter = 100000,
                                  minScale = 1e-8
                                  ),
            ### .................................................. ###
            fixed=list(alpha0 ~ IRR + MA, beta0 ~ IRR + MA, beta1 ~ 1),
            random = list(alpha0 ~ 1, beta0 ~ 1),
            correlation = corAR1(0.2, form = ~ TIME|DIA),
            weights = varPower(),   
            start=v9a, groups = ~ DIA
            )

pred9a <- predict(M9a)
EQM9a <- round(mean((pred9a - DA2[,6])^2), 4)

summary(M9a)

AIC(M9a)
BIC(M9a)
EQM9a


Model_M9a <- c(AIC(M9a), BIC(M9a), EQM9a)

TableMetrics2 <- c()
TableMetrics2 <- data.frame(Metrics, Model_M0a, Model_M1a, Model_M2a, Model_M3a,
                            Model_M4a, Model_M5a, Model_M6a, Model_M7a, Model_M8a,
                            Model_M9a)  



####### 4.12 - Modelo M10 (nlme: alpha0 e beta0 | corr: IRR) ####### 
v10a <- c(1,1,1,1,1) 
M10a <- nlme(PDC ~ f.logia(TIME, alpha0, beta0, beta1), 
             data = DA2,            
             ### inserção destas linhas para convergência do modelo ###
             control = nlmeControl(maxIter=100000,
                                   opt = c("nlminb"), #nlminb
                                   msMaxIter = 100000,
                                   pnlsMaxIter = 20,
                                   minScale = 1e-5,
                                   pnlsTol = 0.0001,
                                   nlsTols = 0.1,
                                   tolerance=100
                                   ),
             ### .................................................. ###
             fixed=list(alpha0 ~ IRR, beta0 ~ IRR, beta1 ~ 1),          
             random = (list(alpha0 ~ 1, beta0 ~ 1)),
             correlation = corAR1(0.2, form = ~ TIME|DIA),
             weights = varPower(),   
             start=v10a, 
             groups = ~ DIA
             )
  
pred10a <- predict(M10a)
EQM10a <- round(mean((pred10a - DA2[,6])^2), 4)

summary(M10a)

AIC(M10a)
BIC(M10a)
EQM10a


Model_M10a <- c(AIC(M10a), BIC(M10a), EQM10a)

TableMetrics2 <- data.frame(Metrics, Model_M0a, Model_M1a, Model_M2a, Model_M3a,
                            Model_M4a, Model_M5a, Model_M6a, Model_M7a, Model_M8a,
                            Model_M9a, Model_M10a)  




break






#### GrÃ¡fico dos valores preditos x resÃduos

par(mai=c(1,1,0.3,0.3))
res <- as.numeric(residuals(M9))
plot(pred9, res, ylim=c(-1.5,1.5), ylab="Residuals",xlab="Predicted values")

#### GrÃ¡fico dos valores registrados x preditos

plot(DA[,6], pred9, ylab="Predicted values", xlab="Registered values")


##### Graficos dos dias #####

id.fi <- cumsum(as.numeric(ftable(D[,1]))) 
id.in <- c(1,id.fi+1)

dia <- 1

##### tempo x PCD - Acumulados

xi <- id.in[dia] 
xf <- id.fi[dia]
Di <- cumsum(D[xi:xf,6])
plot(D[xi:xf,2], Di, pch=19, lwd=2, ylab="X", xlab="Tempo", xlim=c(0,length(Di)))
points(D[xi:xf,2], Di, col="black", type="l", lwd=2)

##### tempo x PCD - Acumulados

xi <- id.in[dia] 
xf <- id.fi[dia]
Do <- DA[xi:xf,6]
De <- as.numeric(pred9[xi:xf])
nd <- length(Do)

## Log-scale

plot(D[xi:xf,2], Do, col="grey70", pch=19, lwd=2, ylab="Y's values", xlab="Time", xlim=c(0, 77), ylim=c(3,13))
points(D[xi:xf,2], De, col="black", type="l", lwd=2)

## origina;-scale

plot(D[xi:xf,2], exp(Do), pch=19, col="grey70", lwd=2, ylab="X", xlab="Tempo", xlim=c(0,length(Do)))
points(D[xi:xf,2], exp(De), col="black", type="l", lwd=2)

###########################          
#### Percentuais em relaÃ§Ã£o
###########################

perc <- round(((exp(Do[nd]) - exp(De[nd]))/exp(De[nd]))*100, 4)
perc

EQM <- round(mean((Do - De)^2), 4)
EQM

PE <- c(-0.0697, 0.6212, 0.8907, -0.5001, -0.2452, 0.3554, -0.4009, -0.0659, 0.2027, 0.2923, 0.0370, -0.0278)
barplot(PE, ylim=c(-1,1), xlab="Days", ylab="Percentage")

text(0.8, -0.95, "Day 1", cex=0.7)
text(1.9, -0.95, "Day 2", cex=0.7)
text(3.1, -0.95, "Day 3", cex=0.7)
text(4.3, -0.95, "Day 4", cex=0.7)
text(5.5, -0.95, "Day 5", cex=0.7)
text(6.7, -0.95, "Day 6", cex=0.7)
text(7.9, -0.95, "Day 7", cex=0.7)
text(9.1, -0.95, "Day 8", cex=0.7)
text(10.3, -0.95, "Day 9", cex=0.7)
text(11.5, -0.95, "Day 10", cex=0.7)
text(12.7, -0.95, "Day 11", cex=0.7)
text(13.9, -0.95, "Day 12", cex=0.7)

text(0.8, -0.0997, "-0.0697", cex=0.6)
text(1.9, 0.6612, "0.6212", cex=0.6)
text(3.1, 0.9307, "0.8907", cex=0.6)
text(4.3, -0.5401, "-0.5001", cex=0.6)
text(5.5, -0.2852, "-0.2452", cex=0.6)
text(6.7, 0.3954, "0.3554", cex=0.6)
text(7.9, -0.4409, "-0.4009", cex=0.6)
text(9.1, -0.1059, "-0.0659", cex=0.6)
text(10.3, 0.2427, "0.2027", cex=0.6)
text(11.5, 0.3323, "0.2923", cex=0.6)
text(12.7, 0.0770, "0.0370", cex=0.6)
text(13.9, -0.0678, "-0.0278", cex=0.6)
