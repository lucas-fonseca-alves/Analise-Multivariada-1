#A Tabela 2.12 traz a descrição dos dados da aba dados do arquivo Car-ros.xlsx.21 
#Esses dados são características de automóveis.

library(tidyverse) 
library(readxl)
library(aplpack) #Pacote do gráfico Chernoff Faces
library(lattice) #Pacote do gráfico Parallel plot
library(fmsb)    #Pacote do gráfico Radar
library(corrplot)
library(car)
library(heplots)


Carros <- read_excel("banco/Bancos - Lista_3/Carros.xlsx")

# a) Construa os gráficos para representação de casos: faces de Chernoff, perfis e radares. 
# Você formaria grupos de carros com base nesses gráficos? Comente.
Carros_Numericas <- Carros %>% 
  select(-1,-2)

#Faces de Chernoff

faces(Carros_Numericas, 
      labels = as.character(Carros$Nome),
      main = "Faces de Chernoff - Dataset Carros",
      print.info = TRUE)

#Parecem fazer parte do mesmo grupo

# Grupo 1: Ferrari, Lamborghini, Jaguar, Rolls, Porsche
# Grupo 2: Onix, Sandero, Peugeot, Up, HB20, Argo, Chery, JAC, March, KA, Corolla, Civic, Azera, Fusiona

#Gráfico de Perfis 

parallelplot(~Carros[3:6] | Nome, Carros, col = 'blue')


parallelplot(~Carros[3:6], Carros,
             #groups = Nome,
             horizontal.axis = FALSE,
             scales = list(x = list(rot = 90)))

parallelplot(~Carros[3:6], Carros,
             horizontal.axis = FALSE,
             scales = list(x = list(rot = 90)),
             col = "steelblue",     # <-- cor única
             lwd = 1.5)             # opcional: espessura

# Existem dois grupos claramente:
#   1) Os que te maior cilindrada tem um médio desepenho, mínimo consumo e mínima autonomia
#   2) os que tem menor cilindrada, maior consumo e média pra máxima autonomia. 
# Mas há um distinção clara pelas variáveis Cilindrada e desempenho.


#Gráfico de Radares 

# 1) Moldura: max e min de cada variável
moldura <- rbind(
  apply(Carros_Numericas, 2, max),
  apply(Carros_Numericas, 2, min)
)
colnames(moldura) <- colnames(Carros_Numericas)
rownames(moldura) <- c("max", "min")

# 2) Junta moldura + dados
dados_radar <- rbind(moldura, Carros_Numericas)

# 3) Plota
fmsb::radarchart(dados_radar,
           plty = 1,
           plwd = 1,
           pcol = rainbow(nrow(Carros_Numericas)))

# c) Construa um gráfico matriz para descrever a associação entre as variáveis e analise-as.


splom(~Carros[3:8], data = Carros,
      xlab = " ",
      main = "Matriz de Dispersão - Carros")

#Todas as variáveis apresentam uma relação linear entre si. A maioria representando uma relação
#linear
#Ao verificar as relações linear com a variável Cilindrada, observa-se que com
#desempenho e potência elas são positivas, e as demais negativas. 
#Ao varificar com desempenho, apenas a relação com potencia é positiva, e as demais são negativas. 
#Ao verificar com Consumo, apenas a relação com potência é negativa, as demais são positivas
#Autonomia e potência são negativas e autonomia e acelaração são positiva
# Por fim, Aceleração e potencia é ngativa. 
# 


Miris <- cor(Carros[,3:8])
Miris_round <- round(Miris, 2)

corrplot(Miris_round,
         method = 'color',
         type = 'upper',
         addCoef.col = "white")

# Então a maioria das relações entra as variaveis são negativas, e apenas as relações de autonomia
# com desempenho, cilindrada, potência, acelaração apresentam correlação menor que 0.8 em modulo. 
# Todas as outras variáveis apresentam correlação maior que 0.8. 
# Ou seja, as variáveis são altamente correlacionadas. 

# e) Construa o QQ-plot e avalie a normalidade desses dados.

#Univariada
par(mfrow = c(1, 2))
qqPlot(Carros$Cilindrada, col = "red", distribution = 'norm')
qqPlot(Carros$Desempenho, col = "red", distribution = 'norm')
par(mfrow = c(1, 1))

par(mfrow = c(1, 2))
qqPlot(Carros$Consumo,    col='red',   distribution = 'norm')
qqPlot(Carros$Autonomia,  col='red',   distribution = 'norm')
par(mfrow = c(1, 1))

par(mfrow = c(1, 2))
qqPlot(Carros$Potência,   col='red',   distribution = 'norm')
qqPlot(Carros$Aceleração ,col='red',   distribution = 'norm')
par(mfrow = c(1, 1))


ndc <- function(dados) {
  n <- nrow(dados); p <- ncol(dados)
  
  # Centraliza TODAS as colunas de uma vez
  dadosc <- scale(dados, center = TRUE, scale = FALSE)
  
  # Matriz de covariância MLE
  S <- (n - 1) * var(dados) / n
  
  # Distâncias de Mahalanobis ao quadrado
  D2 <- apply(dadosc, 1, function(x)
    as.numeric(t(x) %*% solve(S) %*% x))
  
  # Quantidade escalonada
  ui <- sort(n * D2 / (n - 1)^2)
  
  # Posições de plotagem
  alpha <- 0.5 * (p - 2) / p
  beta  <- 0.5 * (n - p - 3) / (n - p - 1)
  q.vi  <- (1:n - alpha) / (n - alpha - beta + 1)
  
  # Quantis teóricos
  a  <- p / 2                       # <-- corrigido
  b  <- (n - p - 1) / 2             # <-- corrigido
  vi <- qbeta(q.vi, a, b)
  
  # Plot
  plot(ui, vi,
       xlab = "Quantis observados", ylab = "Quantis teóricos",
       main = "QQ-plot - Normalidade Multivariada",
       pch = 19, col = "steelblue")
  abline(lm(vi ~ ui), col = "red", lwd = 2)
}

ndc(Carros_Numericas)

#Multivariada aproximação Qui-quadrado 

Carros <- Carros_Numericas

cqplot(Carros)

Carros <- read_excel("banco/Bancos - Lista_3/Carros.xlsx")
Carros_Numericas <- Carros %>% 
  select(-1,-2)


