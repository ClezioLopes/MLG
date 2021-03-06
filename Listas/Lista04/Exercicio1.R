#------   EXERCICIO 1
dados_teste = read.csv(file.choose(), sep="", header=F)
names(dados_teste) = rep(c("idade","ano","tempo","cancer","p_ano"),3)
dados_cancer = rbind(dados_teste[,1:5],dados_teste[,6:10],dados_teste[,11:15])
rm(dados_teste)

#- Salvando os dados
write.csv(dados_cancer,
  file="C:\\Users\\PET_01\\Documents\\Membros do PET\\Cl�zio Lopes\\MLG\\Lista 04\\d_cancer.txt",
  row.names = F)

#- Ajustando um modelo poisson
modelo1 = glm(cancer ~ offset(log(p_ano)) + factor(idade) + factor(ano) + factor(tempo),
              family=poisson(link="log"), data=dados_cancer)
summary(modelo1)

#- Ao incluir todas intera��es temos um numero maior de parametros, sendo assim
#      precisamos incluir por partes

#- Incluindo interacao idade*ano
modelo2 = glm(cancer ~ offset(log(p_ano)) + factor(idade) + factor(ano) + factor(tempo)
              + factor(idade)*factor(ano),
              family=poisson(link="log"), data=dados_cancer)
summary(modelo2)

#- Incluindo interacao idade*tempo
modelo3 = glm(cancer ~ offset(log(p_ano)) + factor(idade) + factor(ano) + factor(tempo)
              + factor(idade)*factor(tempo),
              family=poisson(link="log"), data=dados_cancer)
summary(modelo3)

#- Incluindo interacao tempo*ano
modelo4 = glm(cancer ~ offse(log(p_ano)) + factor(idade) + factor(ano) + factor(tempo)
              + factor(tempo)*factor(ano),
              family=poisson(link="log"), data=dados_cancer)
summary(modelo4)


#- Observando os AICs do modelos
AIC = rbind(modelo1$aic,modelo2$aic,modelo3$aic,modelo4$aic)
AIC

#- Como o modelo de menos AIC possui somente os efeitos principais valos ao diagnostico
#diagn�stico para o modelo1
X = model.matrix(modelo1)
w = modelo1$weights
W = diag(w)

# Pontos alavanca
auxh = solve(t(X)%*%W%*%X) 
H = sqrt(W)%*%X%*%auxh%*%t(X)%*%sqrt(W)
h = diag(H)

# Res�duo deviance
aux_tdi = resid(modelo1, type = 'deviance')
aux_fi = summary(modelo1)
fi = 1/aux_fi$dispersion
tdi = aux_tdi*sqrt(fi/(1-h))

# Res�duo de Pearson
aux_tsi = resid(modelo1, type = 'pearson')
tsi = aux_tsi*sqrt(fi/(1-h))

# Dist�ncia de Cook
ldi = h*(tsi^2)/(1 - h)

par(mfrow=c(2,2))
plot(dados_cancer$idade,modelo1$residuals,pch=16,
     xlab = "Preditora (Idade)",
     ylab = "Res�duos", main="Res�duos vs Preditoras", bty="l")
abline(h=c(-2,2),lty=2)
identify(dados_cancer$idade,modelo1$residuals, n=1)

plot(dados_cancer$ano,modelo1$residuals,pch=16,
     xlab = "Preditora (Ano)",
     ylab = "Res�duos", main="Res�duos vs Preditoras", bty="l")
abline(h=c(-2,2),lty=2)
identify(dados_cancer$ano,modelo1$residuals, n=1)

plot(dados_cancer$tempo,modelo1$residuals,pch=16,
     xlab = "Preditora (Tempo)",
     ylab = "Res�duos", main="Res�duos vs Preditoras", bty="l")
abline(h=c(-2,2),lty=2)
identify(dados_cancer$tempo,modelo1$residuals, n=1)

plot(fitted(modelo1), h, xlab="Valores ajustados", 
     ylab="Alavanca", pch=16, bty="l", main="Ponto Alavanca")
identify(fitted(modelo1), h, n=1)
par(mfrow=c(1,1))

#- Algumas outras informacoes
par(mfrow=c(2,2))
plot(fitted(modelo1), tdi, xlab="Valores ajustados", ylab="Res�duo deviance",
     pch=16, bty="l", main="Res�duo Deviance")
identify(fitted(modelo1), tdi, n=1)

plot(fitted(modelo1), tsi, xlab="Valores ajustados", ylab="Res�duo de Pearson",
     pch=16, bty="l", main="Res�duo de Pearson")
identify(fitted(modelo1), tsi, n=1)

# hist(modelo1$residuals, density = 50, main="Histograma dos Erros",
#      probability = T, xlab = "Res�duos", ylab="Probabilidade",
#      col="grey11")

# qqnorm(modelo1$residuals, pch=16, main="Normal QQ-Plot",
#        xlab="Percentis te�ricos",
#        ylab="Percentis amostrais", lwd=2, bty="l")
# qqline(modelo1$residuals,col=2)

# plot(rstudent(modelo1),pch=16, main="Res�duos Studentizados",
#      xlab = "Ordem das Observa��es", ylab = "Res. Studentizados",
#      bty="l")

plot(ldi, xlab="Ordem das observa��es", ylab="Dist�ncia de Cook", bty="l",
     main="Dist�ncia de Cook's", pch=16)
identify(ldi, n=2)

plot(modelo1$fitted.values,modelo1$residuals, main="Res�duos vs Valores ajustados",
     xlab = "Valores ajustados", ylab = "Res�duos", bty="l", pch=16)
identify(modelo1$fitted.values,modelo1$residuals, n=1)
par(mfrow=c(1,1))

#- Envelope simulado
fit.model = modelo1
source("C:\\Users\\PET_01\\Documents\\Membros do PET\\Cl�zio Lopes\\MLG\\envelope_poisson.R")
title("Envelope Simulado Poisson(log)")

#-sem a observa��o 18
modelo1.1 = glm(cancer ~ factor(idade) + factor(ano) + factor(tempo),
                family=poisson(link="log"), data=dados_cancer, offset=log(p_ano), 
                subset=-c(18))

#-sem a observa��o 69
modelo1.2 = glm(cancer ~ factor(idade) + factor(ano) + factor(tempo),
                family=poisson(link="log"), data=dados_cancer, offset=log(p_ano), 
                subset=-c(69))

#-sem a observa��o 18 e 69
modelo1.3 = glm(cancer ~ factor(idade) + factor(ano) + factor(tempo),
                family=poisson(link="log"), data=dados_cancer, offset=log(p_ano), 
                subset=-c(18,69))

coef = cbind(modelo1$coefficients,
             modelo1.1$coefficients,
             modelo1.2$coefficients,
             modelo1.3$coefficients)

#- Como as diferencas s�o pouco significativa para a maioria dos parametros, n�o retiraremos obs

