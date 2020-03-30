#-----------------------------------------------------------------------------------------
library(dplyr)
library(stringr)
library(gamlss) 
library(MASS)
library(xtable)

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#-  getwd()
#-  setwd("C:/Users/PET_01/Documents/Membros do PET/Clézio Lopes/MLG/Trabalho Final")
#-  setwd("C:\\Users\\Clézio Lopes\\Desktop\\Clézio Lopes\\Curso Estatística\\8º Semestre\\MLG\\Trabalho Final")

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------

#--   Baixando conjunto de dados
dados = read.csv("dados.txt", sep = "", header = F)
names(dados)= c("province","total_incidents", "total_casualties", "hectares", 
                "populations", "area", "mountainous","literacy","water", 
                "minimum_calories", "season_roads", "under_mortality", 
                "pashtun_Majority", "foreign_troops")

#--   Descritivas Respostas
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
par(mfrow = c(1,2))
par(bty="l", col="DarkGreen", pch=16)
dados$total_casualties %>% hist(density=55, col="DarkGreen", 
                                main="Número de baixas por Província",
                                ylab="Frequência absoluta", xlab="Número de baixas")

plot(dados$total_incidents~dados$total_casualties, main = "Total de baixas vs Total de incidências"
     ,xlab = "Total de baixas", ylab = "Total de ocorrências", col ="Darkgreen", pch = 16)
abline(lm(dados$total_incidents~dados$total_casualties), col =  1, lty=2)

dados$total_casualties %>% summary()

dados$total_casualties %>% var() #--    variância
dados$total_casualties %>% sd() #--     desvio padrão


plot(log(dados$foreign_troops)~dados$water)

#--   Matriz de correlação
names(dados)= c("province","Incid.", "Mortes", "Hectares", 
                "Pop.", "Area", "Montanha","Alfab.","Agua", 
                "calorias", "Estrad.", "M. Inf.", 
                "Pashtun", "Tropas Es.")

M = dados[,2:14] %>% cor() %>% round(4)
# ggcorr(M, low = "Black", color = "Black", hjust = 0.5,
#        mid = "white", high = "DarkGreen", size = 5,
#        label = TRUE, label_size = 4, label_round = 2, 
#        label_alpha = F, label_color = "white", geom="tile")

#--   Descritivas Preditoras
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
par(mfrow=c(2,2), bty="l", col="DarkGreen",
    pch=16, mar=c(0.8,4.1,1.5,0.5))
dados$populations %>% boxplot(col="DarkGreen", main="População das provícias",
                              ylab="População (1000s)")
dados$total_incidents %>% boxplot(col="DarkGreen", main="Número de atentados",
                                  ylab="Número de ocorrências")
dados$hectares %>% boxplot(col="DarkGreen", main="Cultivo de ópio",
                           ylab="Hectares")
dados$area %>% boxplot(col="DarkGreen", main="Área das províncias",
                       ylab="Área (1000s km^2)")

dados$mountainous %>% boxplot(col="DarkGreen", main="% de áreas montanhosas",
                              ylab="% de montanhas")
dados$literacy %>% boxplot(col="DarkGreen", main="% da população alfabetizada",
                           ylab="% de alfabetização")
dados$water %>% boxplot(col="DarkGreen", main="% de acesso a água potável",
                        ylab="% de água potável")
dados$minimum_calories %>% boxplot(col="DarkGreen", main="% da população na fome extrema",
                                   ylab="% de calorias minímas")

par(mfrow=c(2,2), bty="l", col="DarkGreen",
    pch=16, mar=c(2,4.1,1.5,0.5))
dados$season_roads %>% boxplot(col="DarkGreen", main="% de estradas sem limitação",
                               ylab="% de estradas")
dados$under_mortality %>% boxplot(col="DarkGreen", main="Taxa de mortalidade infantil",
                                  ylab="Mortalidade (1000)")
# dados$foreign_troops %>% boxplot(col="DarkGreen", main="Tropas estrangerias",
#                                  ylab="Quantidade")

barplot(table(dados2$foreign_troops), col="DarkGreen", main="Tropas estrangeiras",
                                          ylab="Quantidade")

plot(dados$total_casualties~dados$pashtun_Majority, col = "Darkgreen", pch = 16, 
     main = "Etnia Pashtun nas províncias",
     xlab = "Maioria da população é Pashtun", 
     ylab = "Número total de baixas")

#--   Ajustando modelos
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------

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

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------

AICs = rbind(modelo1$aic, modelo2$aic, modelo3$aic)
nomes = rbind("Poisson-log","Poisson-sqrt","B. Negativa")
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
#--   Analise de diagnóstico
X = model.matrix(modelo_step)
w = modelo_step$weights
W = diag(w)

#--   Pontos alavanca
auxh = solve(t(X)%*%W%*%X) 
H = sqrt(W)%*%X%*%auxh%*%t(X)%*%sqrt(W)
h = diag(H)

#--   Resíduo deviance
aux_tdi = resid(modelo_step, type = 'deviance')
aux_fi = summary(modelo_step)
fi = 1/aux_fi$dispersion
tdi = aux_tdi*sqrt(fi/(1-h))

#--   Resíduo de Pearson
aux_tsi = resid(modelo_step, type = 'pearson')
tsi = aux_tsi*sqrt(fi/(1-h))

#--   Distância de Cook
ldi = h*(tsi^2)/(1 - h)


#--   Gráficos Diagnosticos
par(mfrow=c(1,2), bty="l", col="DarkGreen", pch=16)


plot(dados$populations,modelo_step$residuals,
     xlab = "Preditora (Tamanho da População)",
     ylab = "Resíduos", main="Resíduos vs Preditoras")
identify(dados$populations,modelo_step$residuals, n=2)

plot(dados$area,modelo_step$residuals,
     xlab = "Preditora (Área Territorial)",
     ylab = "Resíduos", main="Resíduos vs Preditoras")
identify(dados$area,modelo_step$residuals, n=2)

plot(dados$mountainous,modelo_step$residuals,
     xlab = "Preditora (% de Montanhas na região)",
     ylab = "Resíduos", main="Resíduos vs Preditoras")
identify(dados$mountainous,modelo_step$residuals, n=2)

plot(dados$water,modelo_step$residuals,
     xlab = "Preditora (Acesso água potável)",
     ylab = "Resíduos", main="Resíduos vs Preditoras")
identify(dados$water,modelo_step$residuals, n=2)

plot(dados$minimum_calories,modelo_step$residuals,
     xlab = "Preditora (Minímo de calorias)",
     ylab = "Resíduos", main="Resíduos vs Preditoras")
identify(dados$minimum_calories,modelo_step$residuals, n=2)

plot(dados$season_roads,modelo_step$residuals,
     xlab = "Preditora (Estradas sem restrições)",
     ylab = "Resíduos", main="Resíduos vs Preditoras")
identify(dados$season_roads,modelo_step$residuals, n=2)

plot(dados$under_mortality,modelo_step$residuals,
     xlab = "Preditora (Mortalidade Infantil)",
     ylab = "Resíduos", main="Resíduos vs Preditoras")
identify(dados$under_mortality,modelo_step$residuals, n=2)

plot(dados$pashtun_Majority,modelo_step$residuals,
     xlab = "Preditora (Maioria é Pashun)",
     ylab = "Resíduos", main="Resíduos vs Preditoras")
identify(dados$pashtun_Majority,modelo_step$residuals, n=2)

plot(fitted(modelo_step), tdi, xlab="Valores ajustados", ylab="Resíduo deviance",
     main="Resíduo Deviance", pch=16)
identify(fitted(modelo_step), tdi, n=3)

# plot(fitted(modelo_step), tsi, xlab="Valores ajustados", ylab="Resíduo de Pearson",
#      main="Resíduo de Pearson")
# identify(fitted(modelo_step), tsi, n=3)

plot(resid(modelo_step), ylab="Resíduo do modelo",
     main="Resíduos", pch=16, xlab="Ordem das observações")


plot(fitted(modelo_step), xlab="Ordem das observações", 
     ylab="Alavanca", main="Ponto Alavanca")
identify(fitted(modelo_step), n=1)

plot(ldi, xlab="Ordem das observações", ylab="Distância de Cook",
     main="Distância de Cook's")
identify(ldi, n=2)

plot(modelo_step$fitted.values,modelo_step$residuals, main="Resíduos vs Valores ajustados",
     xlab = "Valores ajustados", ylab = "Resíduos")
identify(modelo_step$fitted.values,modelo_step$residuals, n=3)

fit.model = modelo_step
source("C:\\Users\\PET_01\\Documents\\Membros do PET\\Clézio Lopes\\MLG\\envelope_negbin.R")
source("C:\\Users\\Clézio Lopes\\Desktop\\Clézio Lopes\\Curso Estatística\\8º Semestre\\MLG\\envelope_negbin.R")

#-----------------------------------------------------------------------------------------
#--             TESTANDO SE AS OBSERVAÇÕES 18 E 23 SÃO INTERFERENTES NO MODELO
#-----------------------------------------------------------------------------------------

#--   Modelo sem a observação 18
modelo_11 = glm.nb(total_casualties ~ populations + area + mountainous + 
                     water + minimum_calories + season_roads + under_mortality + 
                     factor(pashtun_Majority), link="log", data=dados[-c(1)],
                   subset=-c(18))
summary(modelo_11)

#--   Modelo sem a observação 23
modelo_12 = glm.nb(total_casualties ~ populations + area + mountainous + 
                     water + minimum_calories + season_roads + under_mortality + 
                     factor(pashtun_Majority), link="log", data=dados[-c(1)],
                   subset=-c(23))
summary(modelo_12)

#--   Modelo sem as observações 18, 23
modelo_13 = glm.nb(total_casualties ~ populations + area + mountainous + 
                     water + minimum_calories + season_roads + under_mortality + 
                     factor(pashtun_Majority), link="log", data=dados[-c(1)],
                   subset=-c(18,23))
summary(modelo_13)

#--   Comparativo de estimativas
cbind(round(modelo_step$coefficients,4),round(modelo_11$coefficients,4),
      round(modelo_12$coefficients,4),round(modelo_13$coefficients,4))

#--   Como não houve um aumento considerável nas estimativas optamos por não retirar esta observação
#--   Sendo assim, o modelo final é dado por:
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
