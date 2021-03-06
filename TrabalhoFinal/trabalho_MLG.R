library(dplyr)
library(stringr)
library(gamlss) 
library(MASS)

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#-  getwd()
#-  setwd("C:/Users/PET_01/Documents/Membros do PET/Cl�zio Lopes/MLG/Trabalho Final")
#-  setwd("C:\\Users\\Cl�zio Lopes\\Desktop\\Cl�zio Lopes\\Curso Estat�stica\\8� Semestre\\MLG\\Trabalho Final")
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------

#--   Baixando conjunto de dados
dados = read.csv("dados.txt", dec=".", sep="", header=F)
names(dados)= c("province","total_incidents", "total_casualties", "hectares", 
                "populations", "area", "mountainous","literacy","water", 
                "minimum_calories", "season_roads", "under_mortality", 
                "pashtun_Majority", "foreign_troops")

#--   Descritivas Respostas
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
par(bty="l", col="DarkGreen", pch=16)
dados$total_casualties %>% hist(density=55, col="DarkGreen", 
                               main="N�mero de baixas por Prov�ncia",
                               ylab="Frequ�ncia absoluta", xlab="N�mero de mortes")
dados$total_casualties %>% summary()

dados$total_casualties %>% var() #--    vari�ncia
dados$total_casualties %>% sd() #--     desvio padr�o

#--   Teste de correla��o entre incidentes e mortes
cor.test(dados$total_casualties,dados$total_incidents)

#--   Descritivas Preditoras
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------

par(mfrow=c(2,2), bty="l", col="DarkGreen", pch=16)
dados$total_incidents %>% hist(density=50, col="DarkGreen", 
                               main="N�mero de baixas no atentado",
                               ylab="Frequ�ncia absoluta", xlab="N�mero de Incidentes")

dados$hectares %>% hist(density=80, col="DarkGreen", main="Hect�res por prov�ncia",
                        ylab="Frequ�ncia absoluta", xlab="Prov�ncia")

dados$populations %>% boxplot(col="DarkGreen", main="Popula��o das prov�cias",
                              ylab="Popula��o (1000s)")

dados$area %>% hist(density=70, col="DarkGreen", main="�rea das prov�ncias",
                    ylab="Frequ�ncia absoluta", xlab="�rea Territorial (1000s km�)")

#--   Ajustando modelos
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
plot(log(dados$total_casualties)~dados$water, pch=16)


modelo1 = glm(total_casualties ~ total_incidents + hectares + populations + 
                area + mountainous + literacy + water + minimum_calories + 
                season_roads + under_mortality + factor(pashtun_Majority) + foreign_troops, 
              family=poisson(link="log"), data=dados[-c(1)])
summary(modelo1)


modelo2 = glm(total_casualties ~ total_incidents + hectares + populations + 
                area + mountainous + literacy + water + minimum_calories + 
                season_roads + under_mortality + factor(pashtun_Majority) + foreign_troops,
              family=poisson(link="sqrt"), data=dados[-c(1)])
summary(modelo2)


modelo3 = glm.nb(total_casualties ~ total_incidents + hectares + populations + 
                 area + mountainous + literacy + water + minimum_calories + 
                 season_roads + under_mortality + factor(pashtun_Majority) + foreign_troops,
                 link="log", data=dados[-c(1)])
summary(modelo3)


modelo4 = glm.nb(total_casualties ~ total_incidents + hectares + populations + 
                    area + mountainous + literacy + water + minimum_calories + 
                    season_roads + under_mortality + factor(pashtun_Majority) + foreign_troops,
                 link="sqrt", data=dados[-c(1)])
summary(modelo4)

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------

AICs = rbind(modelo1$aic, modelo2$aic, modelo3$aic, modelo4$aic)
nomes = rbind("Poisson-log","Poisson-sqrt","B. Negativa-log","B. Negativa-sqrt")
AIC = data.frame(nomes, AICs) 
AIC[order(AIC$AICs),]

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------

#--   Como os dados parecem bemajustados pelo modelo Gamma-log
#--   Vamos a selecao de variaveis

completo = modelo3
reduzido = glm.nb(total_casualties ~ 1, link="log", data=dados[-c(1)])
summary(reduzido)

selecao_step = step(completo)
selecao_step

passo_step = data.frame(Saida=selecao_step[["anova"]][["Step"]],
                        AIC=round(selecao_step[["anova"]][["AIC"]],2))
passo_step

#--   Modelo inicial para se trabalhar
modelo_step = glm.nb(total_casualties ~ populations + area + mountainous + 
                     water + minimum_calories + season_roads + under_mortality + 
                     factor(pashtun_Majority), link="log", data=dados[-c(1)])
summary(modelo_step)

#-----------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
#--   Com o modelo final escolhido vamos ao diagnostico do mesmo
#--   Analise de diagn�stico
X = model.matrix(modelo_step)
w = modelo_novo$weights
W = diag(w)

#--   Pontos alavanca
auxh = solve(t(X)%*%W%*%X) 
H = sqrt(W)%*%X%*%auxh%*%t(X)%*%sqrt(W)
h = diag(H)

#--   Res�duo deviance
aux_tdi = resid(modelo_novo, type = 'deviance')
aux_fi = summary(modelo_novo)
fi = 1/aux_fi$dispersion
tdi = aux_tdi*sqrt(fi/(1-h))

#--   Res�duo de Pearson
aux_tsi = resid(modelo_novo, type = 'pearson')
tsi = aux_tsi*sqrt(fi/(1-h))

#--   Dist�ncia de Cook
ldi = h*(tsi^2)/(1 - h)


#--   Gr�ficos Diagnosticos
par(mfrow=c(1,2), bty="l", col="DarkGreen", pch=16)


plot(dados$populations,modelo_step$residuals,
     xlab = "Preditora (Tamanho da Popula��o)",
     ylab = "Res�duos", main="Res�duos vs Preditoras")
identify(dados$populations,modelo_step$residuals, n=2)

plot(dados$area,modelo_step$residuals,
     xlab = "Preditora (�rea Territorial)",
     ylab = "Res�duos", main="Res�duos vs Preditoras")
identify(dados$area,modelo_step$residuals, n=2)

plot(dados$mountainous,modelo_step$residuals,
     xlab = "Preditora (% de Montanhas na regi�o)",
     ylab = "Res�duos", main="Res�duos vs Preditoras")
identify(dados$mountainous,modelo_step$residuals, n=2)

plot(dados$water,modelo_step$residuals,
     xlab = "Preditora (Acesso �gua pot�vel)",
     ylab = "Res�duos", main="Res�duos vs Preditoras")
identify(dados$water,modelo_step$residuals, n=2)

plot(dados$minimum_calories,modelo_step$residuals,
     xlab = "Preditora (Min�mo de calorias)",
     ylab = "Res�duos", main="Res�duos vs Preditoras")
identify(dados$minimum_calories,modelo_step$residuals, n=2)

plot(dados$season_roads,modelo_step$residuals,
     xlab = "Preditora (Estradas sem restri��es)",
     ylab = "Res�duos", main="Res�duos vs Preditoras")
identify(dados$season_roads,modelo_step$residuals, n=2)

plot(dados$under_mortality,modelo_step$residuals,
     xlab = "Preditora (Mortalidade Infantil)",
     ylab = "Res�duos", main="Res�duos vs Preditoras")
identify(dados$under_mortality,modelo_step$residuals, n=2)

plot(dados$pashtun_Majority,modelo_step$residuals,
     xlab = "Preditora (Maioria � Pashun)",
     ylab = "Res�duos", main="Res�duos vs Preditoras")
identify(dados$pashtun_Majority,modelo_step$residuals, n=2)

plot(fitted(modelo_step), tdi, xlab="Valores ajustados", ylab="Res�duo deviance",
     main="Res�duo Deviance", pch=16)
identify(fitted(modelo_step), tdi, n=2)

plot(fitted(modelo_step), tsi, xlab="Valores ajustados", ylab="Res�duo de Pearson",
     main="Res�duo de Pearson")
identify(fitted(modelo_step), tsi, n=2)

plot(fitted(modelo_step), xlab="Ordem das observa��es", 
     ylab="Alavanca", main="Ponto Alavanca")
identify(fitted(modelo_step), n=1)

plot(ldi, xlab="Ordem das observa��es", ylab="Dist�ncia de Cook",
     main="Dist�ncia de Cook's")
identify(ldi, n=2)

plot(modelo_step$fitted.values,modelo_step$residuals, main="Res�duos vs Valores ajustados",
     xlab = "Valores ajustados", ylab = "Res�duos")
#identify(modelo_step$fitted.values,modelo_novo$residuals, n=2)

fit.model = modelo4
source("C:\\Users\\PET_01\\Documents\\Membros do PET\\Cl�zio Lopes\\MLG\\envelope_negbin.R")
#source("C:\\Users\\Cl�zio Lopes\\Desktop\\Cl�zio Lopes\\Curso Estat�stica\\8� Semestre\\MLG\\envelope_negbin.R")
title("Normal Q-Q Plot")

#-----------------------------------------------------------------------------------------
#--             TESTANDO SE AS OBSERVA��ES 4 E 24 S�O INTERFERENTES NO MODELO
#-----------------------------------------------------------------------------------------

#--   Modelo sem a observa��o 4
modelo_11 = glm.nb(total_casualties ~ populations + area + mountainous + 
                             water + minimum_calories + season_roads + under_mortality + 
                             factor(pashtun_Majority), link="log", data=dados[-c(1)],
                   subset=-c(4))
summary(modelo_11)

#--   Modelo sem a observa��o 24
modelo_12 = glm.nb(total_casualties ~ populations + area + mountainous + 
                           water + minimum_calories + season_roads + under_mortality + 
                           factor(pashtun_Majority), link="log", data=dados[-c(1)],
                   subset=-c(24))
summary(modelo_12)

#--   Modelo sem as observa��es 4, 24
modelo_13 = glm.nb(total_casualties ~ populations + area + mountainous + 
                           water + minimum_calories + season_roads + under_mortality + 
                           factor(pashtun_Majority), link="log", data=dados[-c(1)],
                   subset=-c(4,24))
summary(modelo_13)

#--   Comparativo de estimativas
cbind(round(modelo_step$coefficients,4),round(modelo_11$coefficients,4),
      round(modelo_12$coefficients,4),round(modelo_13$coefficients,4))

#--   Como n�o houve um aumento consider�vel nas estimativas optamos por n�o retirar esta observa��o
#--   Sendo assim, o modelo final � dado por:
#--

#--   Com estimativas dadas por:
summary(modelo_step)

round(modelo_step$coefficients,4)
round(exp(modelo_step$coefficients),4)

# (Intercept)   populations 
# 0.0133        1.0007 

# area          mountainous 
# 1.0223        1.0295 

# water         minimum_calories 
# 1.0549        1.0279 

# season_roads  under_mortality 
# 1.0425        1.0092 

# factor(pashtun_Majority)1 
# 3.6876

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
