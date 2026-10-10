library(tidyverse)
library(AMR)
library(readxl)

#------------------------------------------------------------------------------#
# O pacote kohonen do R possui dados sobre análise química 177 vinhos de três 
# tipos de cultivares de uvas (Nebbiolo, Barberas e Grignolino) da região de
# Piemonte na Itália. O vinho da uva Nebbiolo é chamado de Barolo. A identifica-
# ção dos vinhos está no objeto vintage. O artigo de origem destes dados é
# 
# Forina, M.; Armanino, C.; M. e Ubigli, M. (1986) Multivariate data analysis
# as a discriinating method of the origin of wines. Vitis, 189-201. O arquivo
# pode ser obtido com os seguintes comandos no R:
#------------------------------------------------------------------------------#----

install.packages("kohonen")
library(kohonen)
data("wines")
wines

wines <- as.data.frame(wines)
vintages <- as.data.frame(vintages)

# Organizando os dados

#Retirando o OD Ratio, informado no exercício
wines <- wines %>% 
  select(-`OD ratio`)
#Juntando Vintages com Wines

wines_vintages <- cbind(vintages,wines)


#Tentando visualizar os dados:

pairs(wines_vintages[,-1], pch = 19, cex=1,
      col=wines_vintages$vintages, lower.panel = NULL)

# Componentes Principais

#ATENÇÃO: Como as variáveis tem dimensões muito diferentes, principalmente proline, então é 
#importante que a gente padronize as variáveis antes de fazer as componentes
pcwines <- prcomp(wines[,-1], scale. = TRUE)

# Standart deviation, raiz dos autovalores
pcwines$sdev

# Componentes

pcwines$rotation

pcwines$center

pcwines$scale

#Primeira componente explica 99,81% da variancia total
summary(pcwines)

plot(pcwines)

ggplot_pca(pcwines, groups = as.character(wines_vintages$vintages),
           labels_textsize = 0.01) +
  geom_point(aes(color = as.character(wines_vintages$vintages)), size = 2.5)+
  labs(title = "Wines Data", colors = "vintages")

plot(pcwines$sdev^2/sum(pcwines$sdev^2), type = "b", ylab = "% da Variancia")
#Isso significa que 1 componente é suficiente pra explicar a variação, o que significa escolher 
#uma só componenete ?





#------------------------------------------------------------------------------#----
# Considere novamente o Exercício 34.
# O estudo apresenta dados nutricionais e analisa 961 alimentos. Os componenetes
# nutricionais observados foram: gordura (gramas), energia (calorias), carbo-
# idratos (gramas), proteínas (gramas), colesterol (miligramas), peso (gramas) e
# gordura satura (gramas). Os alimentos são listados e porções variadas e por 
# isso as porções variadas e por isso as porções são divididas pelo peso cor-
# respondente de cada item.
# A matriz de variância dos dados é (colunas referente a gordura, calorias, 
# carboidratos, proteínas, colesterol e gordura saturada),
#------------------------------------------------------------------------------#----
S <- matrix(c(0.037, 0.306, -0.007, 0.003, 0.023, 0.010,
              0.306, 3.747, 0.157, 0.045, 0.178, 0.082,
              -0.007, 0.157, 0.062, -0.002, -0.027, -0.002,
              0.003, 0.045, -0.002, 0.008, 0.020, 0.001,
              0.023, 0.178, -0.027, 0.020, 0.456, 0.014,
              0.010, 0.082, -0.002, 0.001, 0.014, 0.004), byrow = T, nrow = 6, ncol = 6)

# e a respectiva matriz de correlação,


R <- matrix(c(
  1.000, 0.816, -0.140, 0.157, 0.175, 0.747, 
  0.816, 1.000, 0.324, 0.261, 0.136, 0.643, 
  -0.140, 0.324, 1.000, -0.087, -0.163, -0.141,
  0.157, 0.261, -0.087, 1.000, 0.328, 0.142,
  0.175, 0.136, -0.163, 0.328, 1.000, 0.311,
  0.747, 0.643, -0.141, 0.142, 0.311, 1.000), byrow = T, nrow = 6, ncol = 6)

#Utilizando os dados apresentadosm responda as seguintes questões:

#a) Qual matriz, variância-covariância ou correlações você usaria para reduzir a
# dimensionalidade dos dados? Justifique.

# Correlação, por causa das unidades de medidas diferentes (calorias, gramas, miligramas)

#b) Com base em sua escolha para o item anterior, obtenha as duas primeiras compo-
# nentes principais para os dados e faça o gráfico scree plot. 

pca_alimentos <- prcomp(R, scale. = TRUE) 

#Raiz dos Autovalores
pca_alimentos$sdev

#Autovetores (loading)
pca_alimentos$rotation

#Médias de cada variável 
pca_alimentos$center

# ?
pca_alimentos$scale


summary(pca_alimentos)

#As duas primeiras componentes principais são:
pca_alimentos$rotation

#       PC1         PC2          
# [1,] -0.580500814  0.03515377  
# [2,] -0.485347651  0.32296790  
# [3,]  0.319043748  0.55669063 
# [4,]  0.089175132 -0.47875969  
# [5,] -0.003587049 -0.59183963 
# [6,] -0.563651025 -0.07117940

#Gráfico de ScreePlot
plot(pca_alimentos$sdev^2/sum(pca_alimentos$sdev^2), 
     type = "b",
     ylab = "% da Variancia")

#C) Quantas componentes principais você reteria no estudo? Justifique. Interprete
# (se possível) as duas primeiras componentes principais. 

summary(pca_alimentos)

#Gráfico de ScreePlot
plot(pca_alimentos$sdev^2/sum(pca_alimentos$sdev^2), 
     type = "b",
     ylab = "% da Variancia")

#Pela proproção da variação acumulada (summary) e pelo Scree Plot, manteria as 3
# primeiras PC, pois, elas já explicam 96% da variação total. 

#D) Sabendo que 

# rho(Yi, Xk) = eik*raiz(lambdai)/raiz(sigmakk)

# calcule a correlação entre a primeira componente principal e cada uma das variáveis
# originais. Indique se o resultado é compatível com a variância amostral generalizada.
# Justifique.

#Como já temos a matriz de correlação, podemos só pegar a diagonal principal, 
# Como estamos utilizando a matriz de correlação a diagonal será apenas de 1. 

sigma_kk <- diag(R)
raiz_lambdai <- pca_alimentos$sdev

e <- pca_alimentos$rotation

rho_Y1_X1 <- (e[,1]*raiz_lambdai[1])/sigma_kk

# Organizar
resultado <- data.frame(
  #variavel = rownames(e),
  loading = e[, 1],
  correlacao = rho_Y1_X1,
  correlacao_quadrado = rho_Y1_X1^2
)

resultado <- resultado[order(-abs(resultado$correlacao)), ]
resultado


#------------------------------------------------------------------------------#----
# Utilizando os dados ILE2020.xlsx

ILE2020 <- read_excel("banco/ILE2020.xlsx")


# A) Aplique a análise de componentes principais sobre a matriz de covariâncias.

pcwines <- prcomp(wines[,-1], scale. = TRUE)



# B) Aplique a análise de componentes principais sobre a matriz de correlações.
# C) Compare as análises obtidas nos itens (a) e (b)


