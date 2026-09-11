#------------------------------------------------------------------------------#
# 10) Considere a decomposição espectral de uma matriz A_{pxp} positiva        #
#definida, isto é, A_{pxp} = P D P^T. Seja                                     #
#                                                                              #
#                  A = 3 2 3 2                                                 # 
#                      2 5 1 1                                                 #
#                      3 1 8 2                                                 #
#                      2 1 2 3                                                 #
#------------------------------------------------------------------------------#----

A <- matrix(c(3,2,3,2,2,5,1,1,3,1,8,2,2,1,2,3),byrow = TRUE, nrow = 4, ncol = 4)

#A) Obtenha P e D

#Lembra que P é o autovetor ortogonal associado as autovalores da diagonal de D. 

autoA <- eigen(A)

P <- autoA$vectors
D <- diag(autoA$values)
PT <- t(P)


#B) Obtenha (A^1/2)^2 = A 

#Sabendo que pela decomposição espectral temos que:
# A = PDP^T
# Então, podemos mostrar a partir da decomposição espectral que:
# [(PD^T)^1/2]^2 = [P (D)^1/2 P^T]^2 = P D P^T = A

#Assim, podemos mostrar que

D_um_meio <- D^0.5
D_um_meio_elevado_dois <- D_um_meio %*% D_um_meio

A
P %*% D_um_meio_elevado_dois %*% PT

#C) Obtenha (A^1/2)^-1 (descreva seus elementos) e mostre que (A^1/2)^-1 A^1/2 = I

#Utilizando a decomposição espectral novamente

#1) Obter (A^1/2)^-1

D_um_meio <- D^0.5
inversa_D_um_meio <- solve(D_um_meio)

inversa_D_um_meio

#2) Então se Fizermos P (D^1/2)^-1 PT %*% P (D^1/2) PT 

A_inversa_um_meio <- P %*% inversa_D_um_meio %*% PT 

A_um_meio <- P %*% D_um_meio %*% PT


# A função zapsmall substitui valores muito próximos de zero por zero.
zapsmall(A_inversa_um_meio %*% A_um_meio)


#------------------------------------------------------------------------------#
# 13) Considere os dados da Tabela 1.1 de Artes e Barroso (2023) sobre Taxa de #
#delitos por 100.000 habitantes no estado de São Paulo em 2002                 #
#                                                                              #
# A)Obtenha a decomposição espectral e verifique se existe indicação de uma    #                      
#possível redução da dimensão do estudo em questão. Justique.                  #
#------------------------------------------------------------------------------#----
library(tidyverse)

Taxa_Delitos <- data.frame(
  Regiao = c('SJRP', 'RP', 'Bauru', 'Campinas', 'Sorocaba', 'SP', 'SJC', 'Santos', 'GSP'),
  Homicidio_doloso = c(10.85, 14.13, 8.62, 23.04, 16.04, 43.74, 25.39, 42.86, 42.55),
  Furto = c(1500.8, 1496.07, 1448.79, 1277.33, 1204.02, 1190.94, 1292.91, 1590.66, 797.16),
  Roubo = c(149.35, 187.99, 130.97, 424.87, 214.36, 1139.52, 358.39, 721.90, 520.73),
  Roubo_Furto_Veiculos = c(108.38, 116.66, 69.98, 435.75, 207.06, 909.21, 268.24, 275.89, 602.63)
  )


#Para decomposição espectral precisamos da matriz de Covariancia 

Taxa_Delitos_Numericos <- Taxa_Delitos %>% 
  select(-1)

S <- cov(Taxa_Delitos_Numericos)

#Realizando decomposição espectral

autoS <- eigen(S)

P <- autoS$vectors
D <- diag(autoS$values)


prop_autovalore <-  (autoS$values/sum(autoS$values))*100
# Como os dois primeiros componentes conseguem reter 99,03452% da informação original, 
# podemos reduzir a dimensionalidade do estudo de 4 dimensões (variáveis originais) 
# para 2 dimensões (PC1 e PC2), com uma perda mínima de informação (0.9654835%).

