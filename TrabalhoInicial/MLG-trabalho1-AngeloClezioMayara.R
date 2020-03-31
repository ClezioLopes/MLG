####################################################################
Estimar_f = function(converge) {

  #Importando o conjunto de dados
  ###############################
  data = read.csv2(file.choose(),sep="\t", dec=".", header = T)
  data1 = as.data.frame(lapply(data[,c(1:length(data))], as.numeric))
  dados = na.omit(data1)
  
  
  #Criando as matrizes
  ####################
  y = dados[,1]
  x = dados[,-1]
  
  
  #PASSO 1 - "Manualmente"
  ########################
  m_x = cbind(rep(1,nrow(dados)),x)
  eta_0 = log(y) #eta = log(mi_0) para a dist. Gama
  
  #Construindo z_0 e G_0
  G_0 = as.matrix(diag(as.vector(1/y), nrow(dados), nrow(dados)))
  mi_0 = exp(eta_0)
  z0 = eta_0 + (G_0 %*% (y - mi_0))
  
  
  #Construindo a matriz W_0
  theta = -1/mi_0
  bi_theta = -1/theta               #beta linha
  bii_theta = 1/((theta)^2)         #beta 2 linha (Função de variância)
  w0 = as.matrix(diag(as.vector((1/bii_theta)*(exp(eta_0)^2))))
  
  #Estimando Beta1
  B0 = round(solve( t(as.matrix(m_x)) %*% as.matrix(w0) %*% as.matrix(m_x)) %*% (t(as.matrix(m_x)) %*% as.matrix(w0) %*% as.matrix(z0)),6)
  # 
  #Armazenando os valores dos Betas
  i = 1
  passo = as.data.frame(matrix(rep(NA,length(B0)+1),1,length(B0)+1))
  names(passo) = c("Passo",paste("B", seq(0,(length(B0)-1)), sep = ""))
  passo[i,1:(length(B0)+1)] = c(paste("Passo",i, sep = " "),t(B0)) 
  
  
  #PASSO 2 - "Manualmente"
  ########################
  eta_1 =  as.matrix(m_x) %*% B0 
  mi_1 = exp(eta_1)
  
  #Construindo z_1 e G_1
  G_1 = as.matrix(diag(as.vector(1/mi_1), nrow(dados), nrow(dados)))
  z1 = eta_1 + (G_1 %*% (y - mi_1))
  
  #Reconstruindo a matriz W
  theta = -1/mi_1
  bi_theta = -1/theta          #beta linha
  bii_theta = 1/((theta)^2)    #beta 2 linha (Função de variância)
  w1 = as.matrix(diag(as.vector((1/bii_theta)*(exp(eta_1)^2))))

  
  #Estimando Beta2
  B1 = round(solve( t(as.matrix(m_x)) %*% as.matrix(w1) %*% as.matrix(m_x)) %*% (t(as.matrix(m_x)) %*% as.matrix(w1) %*% as.matrix(z1)),6)
  
  #Armazenando os valores dos Betas
  i = 2
  passo[i,1:(length(B1)+1)] = c(paste("Passo",i, sep = " "),t(B1)) 
  
  
  #Critério de parada
  ###################
  parada = sum((B1 - B0)/B0)
  
  
  #Começando o processo interativo
  ################################
  B = B1
  while (abs(parada) > converge) {
    eta =  as.matrix(m_x) %*% B 
    mi = exp(eta)
    
    #Construindo z e G
    G = as.matrix(diag(as.vector(1/mi), nrow(dados), nrow(dados)))
    z = eta + (G %*% (y - mi))
    
    #Reconstruindo a matriz W
    theta = -1/mi
    bi_theta = -1/theta        #beta linha
    bii_theta = 1/((theta)^2)  #beta 2 linha (Função de variância)
    w = as.matrix(diag(as.vector((1/bii_theta)*(exp(eta)^2))))
    
    #Estimando Beta
    Beta = round(solve( t(as.matrix(m_x)) %*% as.matrix(w) %*% as.matrix(m_x)) %*% (t(as.matrix(m_x)) %*% as.matrix(w) %*% as.matrix(z)),6)
    
    #Armazenando os valores dos Betas
    aux = i + 1
    passo[aux,1:(length(Beta)+1)] = c(paste("Passo",aux, sep = " "),t(Beta)) 
    
    #Critério de parada
    parada = sum((Beta - B)/B)
    
    #Para conseguir dar o próximo passo
    B = Beta
    i = aux
  }
  
  
  #  Estimando fhi
  ################
  phi = (nrow(dados) - (length(dados)-1) - 1)/ sum(((y - mi)^2)/bii_theta)
  
  
  #Saída da função
  ################
  lista = list("Valores estimados para bets's" = t(B),
               "Número de iteração(ões) no ajuste" = nrow(passo),
               "Valor de 'phi' por MM" = phi,
               "Beta's no passo a passo" = passo)
  
  return(lista)
}


Estimar_f(0.0001)




