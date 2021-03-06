#####################################################
#         Lista 3
#####################################################
library(xtable)

#   +-----------------+
#   |   EXERC�CIO 1   |
#   +-----------------+
restaurante = read.csv2(file.choose(),header = F, sep = ";",
                        dec = ".")
names(restaurante) = c("Fat","Gasto")

#dispers�o dos dados
par(mfrow=c(1,1))
plot(restaurante$Gasto,restaurante$Fat, pch=15,
     main="Dispers�o dos Dados",
     xlab = "Gasto com propaganda (mil USD)",
     ylab = "Faturamento anual")

#ajustando um modelo linear geral
modelo1 = glm(Fat ~ Gasto, family=gaussian(link="identity"), data = restaurante)
summary(modelo1)
AIC_model1 = data.frame("Modelo 1",round(modelo1$aic,2))
names(AIC_model1)=c("Modelo","AIC")

#diagn�stico do modelo1
par(mfrow=c(2,2))
plot(restaurante$Gasto,rstudent(modelo1),pch=16,
     xlab = "Preditora (Gasto)",
     ylab = "Res�duos", main="Res�duos vs Preditoras", bty="l")
abline(h=c(-2,2),lty=2)

hist(modelo1$residuals, density = 50, main="Histograma dos Erros",
     probability = T, xlab = "Res�duos", ylab="Probabilidade",
     col="grey11")

qqnorm(modelo1$residuals, pch=16, main="Normal QQ-Plot",
       xlab="Percentis te�ricos",
       ylab="Percentis amostrais", lwd=2, bty="l")
qqline(modelo1$residuals,col=2)

plot(rstudent(modelo1),pch=16, main="Res�duos Studentizados",
     xlab = "Ordem das Observa��es", ylab = "Res. Studentizados",
     bty="l")
par(mfrow=c(1,1))

fit.model = modelo1
source("C:\\Users\\Cl�zio Lopes\\Desktop\\Cl�zio Lopes\\Curso Estat�stica\\8� Semestre\\MLG\\envelope_normal.R")


#Outros ajustes
#--- Gama - log
modelo2 = glm(Fat ~ Gasto, family=Gamma(link="log"), data = restaurante)
summary(modelo2)
AIC_model2 = data.frame("Modelo 2",round(modelo2$aic,2))
names(AIC_model2)=c("Modelo","AIC")

#--- Gama - identidade
modelo22 = glm(Fat ~ Gasto, family=Gamma(link="identity"), data = restaurante)
summary(modelo22)
AIC_model22 = data.frame("Modelo 2.2",round(modelo22$aic,2))
names(AIC_model22)=c("Modelo","AIC")

fit.model = modelo22
source("C:\\Users\\Cl�zio Lopes\\Desktop\\Cl�zio Lopes\\Curso Estat�stica\\8� Semestre\\MLG\\envelope_gama_ident.R")


#--- Gaussiana Inversa - log
modelo3 = glm(Fat ~ Gasto, family=inverse.gaussian(link="log"), data = restaurante)
summary(modelo3)
AIC_model3 = data.frame("Modelo 3",round(modelo3$aic,2))
names(AIC_model3)=c("Modelo","AIC")

#--- Gaussiana Inversa - identidade
modelo33 = glm(Fat ~ Gasto, family=inverse.gaussian(link="identity"), data = restaurante)
summary(modelo33)
AIC_model33 = data.frame("Modelo 3.3",round(modelo33$aic,2))
names(AIC_model33)=c("Modelo","AIC")

fit.model = modelo3
source("C:\\Users\\Cl�zio Lopes\\Desktop\\Cl�zio Lopes\\Curso Estat�stica\\8� Semestre\\MLG\\envelope_ginv_ident.R")



#--- AIC dos MOdelos
Modelos_AIC = rbind(AIC_model1,AIC_model2,AIC_model22,AIC_model3,AIC_model33)
Modelos_AIC
xtable(Modelos_AIC)

#Pelo AIC escolhemos o modelo 22, com erros Gamma, e liga��o identidade
#Diagnostico do modelo 22

#diagn�stico do modelo22
par(mfrow=c(2,2))
plot(restaurante$Gasto,rstudent(modelo22),pch=16,
     xlab = "Preditora (Gasto)",
     ylab = "Res�duos", main="Res�duos vs Preditoras", bty="l")
abline(h=c(-2,2),lty=2)

plot(modelo22$fitted.values,rstudent(modelo22),pch=16,
     xlab = "Valores Preditos",
     ylab = "Res�duos Studentizados",
     main="Res�duos vs Preditos", bty="l")

hist(modelo22$residuals, density = 50, main="Histograma dos Erros",
     probability = T, xlab = "Res�duos", ylab="Probabilidade",
     col="grey11")

plot(rstudent(modelo22),pch=16, main="Res�duos Studentizados",
     xlab = "Ordem das Observa��es", ylab = "Res. Studentizados",
     bty="l")
par(mfrow=c(1,1))


#diagn�stico do modelo33
par(mfrow=c(2,2))
plot(restaurante$Gasto,rstudent(modelo33),pch=16,
     xlab = "Preditora (Gasto)",
     ylab = "Res�duos", main="Res�duos vs Preditoras", bty="l")
abline(h=c(-2,2),lty=2)

plot(modelo33$fitted.values,rstudent(modelo33),pch=16,
     xlab = "Valores Preditos",
     ylab = "Res�duos Studentizados",
     main="Res�duos vs Preditos", bty="l")

hist(modelo33$residuals, density = 50, main="Histograma dos Erros",
     probability = T, xlab = "Res�duos", ylab="Probabilidade",
     col="grey11")

plot(rstudent(modelo33),pch=16, main="Res�duos Studentizados",
     xlab = "Ordem das Observa��es", ylab = "Res. Studentizados",
     bty="l")
par(mfrow=c(1,1))


#   +-----------------+
#   |   EXERC�CIO 2   |
#   +-----------------+

leucemia = read.csv2(file.choose(),header = T, sep = ";")

modeloE2 = glm(log(WBC) ~ TEMP + factor(AG), family=Gamma(link="identity"), data = leucemia)
summary(modeloE2)
AIC_modelE2 = data.frame("Modelo 1",round(modeloE2$aic,2))
names(AIC_modelE2)=c("Modelo","AIC")

modeloE2.1 = glm(log(WBC) ~ TEMP + factor(AG), family=Gamma(link="log"), data = leucemia)
summary(modeloE2.1)
AIC_modelE2.1 = data.frame("Modelo 2",round(modeloE2.1$aic,2))
names(AIC_modelE2.1)=c("Modelo","AIC")

modeloE2.2 = glm(log(WBC) ~ TEMP + factor(AG), family=Gamma(link="inverse"), data = leucemia)
summary(modeloE2.2)
AIC_modelE2.2 = data.frame("Modelo 3",round(modeloE2.2$aic,2))
names(AIC_modelE2.2)=c("Modelo","AIC")

Modelos_AIC2 = rbind(AIC_modelE2,AIC_modelE2.1,AIC_modelE2.2)
Modelos_AIC2

#Pelo crit�rio do AIC o modelo escolhido sera o modelo 1
# dado por distribui��o gama com liga��o log
# CONTINUA............


#   +-----------------+
#   |   EXERC�CIO 3   |
#   +-----------------+

carros = read.csv2(file.choose(),header = T, sep = ";", dec = ",")
head(carros,10)

# a)
par(mfrow=c(3,2))
plot(carros$x2, log(carros$y), pch=16, main="Dispers�o dos dados",
     xlab = "Qtde de cilindros", ylab = "Log(Milhas por gal�o)",
     bty="l")

plot(carros$x3, log(carros$y), pch=16, main="Dispers�o dos dados",
     xlab = "Cilindradas", ylab = "Log(Milhas por gal�o)",
     bty="l")

plot(carros$x5, log(carros$y), pch=16, main="Dispers�o dos dados",
     xlab = "Peso do ve�culo", ylab = "Log(Milhas por gal�o)",
     bty="l")

plot(carros$x6, log(carros$y), pch=16, main="Dispers�o dos dados",
     xlab = "Acelera��o", ylab = "Log(Milhas por gal�o)",
     bty="l")

plot(carros$x7, log(carros$y), pch=16, main="Dispers�o dos dados",
     xlab = "Ano do modelo", ylab = "Log(Milhas por gal�o)",
     bty="l")

plot(carros$x8, log(carros$y), pch=16, main="Dispers�o dos dados",
     xlab = "Origem", ylab = "Log(Milhas por gal�o)",
     bty="l")
par(mfrow=c(1,1))

# b)
source("C:\\Users\\Cl�zio Lopes\\Desktop\\Cl�zio Lopes\\Curso Estat�stica\\8� Semestre\\MLG\\stepwise.R")
stepwise(full.model = glm(carros$y ~ carros$x2 + carros$x3 + carros$x5 + carros$x6 + carros$x7 + factor(carros$x8), family=Gamma(link="log"))
         ,initial.model = glm(carros$y~1, family=Gamma(link="log"))
         ,alpha.to.enter = 0.05
         ,alpha.to.leave = 0.10
)

carros.glm2 = glm(carros$y ~ carros$x3 + carros$x5 + carros$x6 + carros$x7 + factor(carros$x8), family = Gamma(link="log"))
summary(carros.glm2)

#-----------
carros.glm2 = glm(carros$y ~ carros$x5,family = Gamma(link="log"))
summary(carros.glm2)

carros.glm21 = glm(carros$y ~ carros$x5 + carros$x7,family = Gamma(link="log"))
summary(carros.glm21)

carros.glm22 = glm(carros$y ~ carros$x5 + carros$x7 + factor(carros$x8),family = Gamma(link="log"))
summary(carros.glm22)

carros.glm23 = glm(carros$y ~ carros$x5 + carros$x7 + factor(carros$x8) + carros$x3,family = Gamma(link="log"))
summary(carros.glm23)

carros.glm24 = glm(carros$y ~ carros$x5 + carros$x7 + factor(carros$x8) + carros$x3 + carros$x6, family = Gamma(link="log"))
summary(carros.glm24)

carros.glmCompleto = glm(y ~. ,family = Gamma(link="log"), data=carros)
summary(carros.glmCompleto)

#-- d)
#-------------- DIAGNOSTICO
par(mfrow=c(2,2))
plot(carros$x3, rstudent(carros.glm24), pch=16, main="Dispers�o dos dados",
     xlab = "Cilindradas", ylab = "Res. Studentizados",
     bty="l")

plot(carros$x5, rstudent(carros.glm24), pch=16, main="Dispers�o dos dados",
     xlab = "Peso do ve�culo", ylab = "Res. Studentizados",
     bty="l")

plot(carros$x7, rstudent(carros.glm24), pch=16, main="Dispers�o dos dados",
     xlab = "Ano do modelo", ylab = "Res. Studentizados",
     bty="l")

plot(carros$x8, rstudent(carros.glm24), pch=16, main="Dispers�o dos dados",
     xlab = "Origem", ylab = "Res. Studentizados",
     bty="l")

plot(carros$x6, rstudent(carros.glm24), pch=16, main="Dispers�o dos dados",
     xlab = "Acelera��o", ylab = "Res. Studentizados",
     bty="l")

plot(carros.glm24$fitted.values,rstudent(carros.glm24),pch=16,
     xlab = "Valores Preditos",
     ylab = "Res�duos Studentizados",
     main="Res�duos vs Preditos", bty="l")

hist(carros.glm24$residuals, density = 50, main="Histograma dos Erros",
     probability = T, xlab = "Res�duos", ylab="Probabilidade",
     col="grey11")

plot(rstudent(carros.glm24),pch=16, main="Res�duos Studentizados",
     xlab = "Ordem das Observa��es", ylab = "Res. Studentizados",
     bty="l")
par(mfrow=c(1,1))

fit.model = carros.glmCompleto
source("C:\\Users\\Cl�zio Lopes\\Desktop\\Cl�zio Lopes\\Curso Estat�stica\\8� Semestre\\MLG\\envelope_gama_ident.R")


#--- e)

# An�lise de diagn�stico
X <- model.matrix(carros.glm24)
w <- carros.glm24$weights
W <- diag(w)

par(mfrow=c(2,2))
# Pontos alavanca
auxh <- solve(t(X)%*%W%*%X) 
H <- sqrt(W)%*%X%*%auxh%*%t(X)%*%sqrt(W)
h <- diag(H)
plot(fitted(carros.glm24),h,xlab="Valores ajustados",
     ylab="Alavanca", pch=16, bty="l", 
     main="Pontos Alavancas")
identify(fitted(carros.glm24),h,n=1)

# Res�duo deviance
aux_tdi <- resid(carros.glm24, type = 'deviance')
aux_fi <- summary(carros.glm24)
fi <- 1/aux_fi$dispersion
tdi <- aux_tdi*sqrt(fi/(1-h))
plot(fitted(carros.glm24),tdi,xlab="Valores ajustados",
     ylab="Res�duo deviance", pch=16, bty="l",
     main="Res�duo Deviance")
identify(fitted(carros.glm24),tdi,n=6)

# Res�duo de Pearson
aux_tsi <- resid(carros.glm24, type = 'pearson')
tsi <- aux_tsi*sqrt(fi/(1-h))
plot(fitted(carros.glm24),tsi,xlab="Valores ajustados",
     ylab="Res�duo de Pearson", pch=16, bty="l",
     main="Res�duo de Pearson")
identify(fitted(carros.glm24),tsi,n=2)


# Dist�ncia de Cook
ldi <- h*(tsi^2)/(1 - h)
ldi
plot(ldi,xlab="�ndice",ylab="Dist�ncia de Cook", pch=16, bty="l",
     main="Dist�ncia de Cook's")
identify(ldi,n=2)
par(mfrow=c(1,1))

carros.glm24.1 = glm(carros$y ~ carros$x5 + carros$x7 + factor(carros$x8) + carros$x3 + carros$x6, family = Gamma(link="log"), subset=-c(14))
summary(carros.glm24.1)


#   +-----------------+
#   |   EXERC�CIO 4   |
#   +-----------------+

# ---  a)
install.packages("lmtest", dependencies = T)
library(lmtest)

modelo1 = glm(carros$y ~ carros$x5 + carros$x7 + factor(carros$x8) + carros$x3 + carros$x6, family = Gamma(link="log"))
modelo2 = glm(carros$y ~ carros$x5 + carros$x7 + carros$x3 + carros$x6, family = Gamma(link="log"))

lrtest(modelo1,modelo2)

# ---- b)
install.packages("var", dependencies = T)
library(car)

#- beta5
linearHypothesis(carros.glm24,hypothesis.matrix=c(0,0,0,1,0,0,0))

#- beta6
linearHypothesis(carros.glm24,hypothesis.matrix=c(0,0,0,0,1,0,0))

#- beta5 - beta6
linearHypothesis(carros.glm24,hypothesis.matrix=c(0,0,0,1,-1,0,0))

#- c)

#- d)
#x2 = 4, x3 = 150,
#x4 = 100, x5 = 2300, x6 = 17, x7 = 80, x8 = 1.
y_estimado = sum(carros.glm24$coefficients * c(1,2300,80,0,0,150,17))

mean(log(carros$y))
exp(y_estimado)

a = sqrt(t(c(1,2300,80,0,0,150,17)) %*% summary(carros.glm24)$cov.scale %*% c(1,2300,80,0,0,150,17))

#xt beta
exp(c(1,2300,80,0,0,150,17) %*% carros.glm24$coefficients + a )


#- e)

#Criando categorias
# Ano de fabrica��o (x <= 74 = 1, 75 < x < 79 = 2, 3 c.c. )

carros2 = carros
carros2$x7[carros2$x7 <= 74] = 1
carros2$x7[carros2$x7 >= 75 & carros2$x7 <= 79] = 2
carros2$x7[carros2$x7 >= 80] = 3

#Novo modelo � dado por

carros2.glm = glm(carros2$y ~ carros2$x5 + factor(carros2$x7) + factor(carros2$x8) + carros2$x3 + carros2$x6, family = Gamma(link="log"))
summary(carros2.glm)

stepwise(full.model = glm(carros2$y ~ carros2$x5 + factor(carros2$x7) + 
                                  factor(carros2$x8) + carros2$x3 + 
                                  carros2$x6, factor(carros2$x2)
                          ,family = Gamma(link="log"))
         ,initial.model = glm(carros2$y~1, family=Gamma(link="log"))
         ,alpha.to.enter = 0.05
         ,alpha.to.leave = 0.10
)

ajuste_novo = glm(carros2$y ~ carros2$x5 + factor(carros2$x7) + 
                          factor(carros2$x8) ,family = Gamma(link="log"))

summary(ajuste_novo)

#- f) Intera��es
ajuste_novo_int = glm(carros2$y ~ carros2$x5 + factor(carros2$x7) + 
                          factor(carros2$x8) + 
                          factor(carros2$x7)*carros2$x5 +
                          factor(carros2$x7)*factor(carros2$x8)
                          ,family = Gamma(link="log"))

summary(ajuste_novo_int)
