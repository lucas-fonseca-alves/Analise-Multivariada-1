library(tidyverse)
library(psych)


#------------------------------------------------------------------------------#
# Brown, Williams, Barlow (1984)
# Uma menina de 12 anos de idade avaliou 7 conhecidos com base em cinco adjetivos
# utilizando uma escala de 9 pontos. A tabela seguinte apresenta os resultados.
#------------------------------------------------------------------------------#----

adjetivos <- data.frame(
  conhecidos = c('AFE1', 'IRMÃ', 'AFE2', 'PAI', 'PROFESSOR', 'AME', 'AFE3'),
  gentil = c(1,8,9,9,1,9,9),
  inteligente = c(5,9,8,9,9,7,7),
  feliz = c(5,7,9,9,1,7,9),
  simpatico = c(1,9,9,9,1,9,9),
  justo = c(1,8,8,9,9,9,7)
)

# AFE = Amiga da escola, AME = amigo da escola

# a) Obtenha a matriz de correlações R e avalie se existe indicação da existência
# de dois ou mais grupos.

R <- cor(adjetivos[,-1])

# Verificamos que as seguintes variáveis possuem correlação forte:

# gentil x feliz: 0,88
# gentil x simpático: 0.99 (quase perfeita, podemos ter problema de multicolinearidade)
# justo x inteligente: 0,83
# feliz x simpático: 0,86

#Assim notamos que variáveis mais relacionadas a sociabilidade tem forte correlação. 

#Quando verificamos as demais correlações:
# inteligente x gentil: 0.29
# inteligente x feliz: -.0.02
# inteligente x simpático: 0.32
# justo x gentil : 0.54
# justo x feliz: 0.13
# justo x simpático: 0.54

# verificamos que as correlações tendem a ser fracas, o que pode indicar a existencia
#de dois grupo, um mais voltado para as características sociais e outro mais para o intelecto. 

# B) Obtenha os autovalores de R e interprete o resultado

autovalores <- eigen(R)

vat <- autovalores$values/sum(autovalores$values)
vat
cumsum(vat)

plot(vat, type = "b", ylab = "% da Variancia")
# LEMBRAR que aqui o acumulado é apenas um indicativo da quantidade de fatores a serem
# retidos.

# Os dois primeiros autovalores explicam 96% da variabilidade total, o que sugere a 
# retenção de dois fatores. A inclusão de um terceiro fator acrescentaria apenas 3,36%, 
# um ganho marginal pequeno em relação ao aumento da dimensionalidade. Além disso, a 
# análise da matriz de correlação da questão (a) e a interpretabilidade dos resultados 
# indicam a presença de dois grupos de variáveis: um relacionado à cordialidade 
# (gentil, simpático, feliz) e outro à competência (inteligente, justo). 
# Portanto, a solução com dois fatores é adequada.

# C) Faça uma análise fatorial por componentes principais utilizando dois fatores. 
# Obtenha a contribuição dos fatores na variância amostral

afadjetivos <- principal(adjetivos[,-1], nfactors = 2, rotate='none')

# A análise de componentes principais identificou dois fatores que explicam 96% da 
# variabilidade total dos cinco adjetivos. O primeiro fator representa uma avaliação 
# geral positiva (todas as cargas positivas), enquanto o segundo contrasta competência 
# (inteligente, justo) com cordialidade (gentil, simpático, feliz). As comunalidades são 
# muito altas (0.92–0.99), indicando que os dois fatores explicam quase toda a variância 
# das variáveis. A complexidade média é 1.6, sugerindo que a maioria das variáveis carrega 
# predominantemente em um fator.


#D) Verifique a qualidade do ajuste do item anterior.

# Responder a pergunta: O modelo fatorial com 2 fatores representa bem a estrutura de 
# correlação dos dados?

residuo <- afadjetivos$residual


# A qualidade do ajuste do modelo fatorial com dois fatores foi avaliada por várias métricas. 
# As comunalidades variaram de 0.92 a 0.99, indicando que os fatores explicam quase toda a 
# variância de cada variável. As unicidades foram muito baixas (0.007 a 0.079), confirmando 
# que pouca variância não é explicada. A complexidade média foi 1.6, sugerindo que a maioria 
# das variáveis carrega predominantemente em um fator. 

# A matriz de resíduos mostra que todos os resíduos fora da diagonal são pequenos (< 0.07 em 
# valor absoluto). O maior resíduo é entre inteligente e justo (-0.067), indicando que o modelo 
# subestima ligeiramente a correlação entre essas duas variáveis. Os demais resíduos são 
# inferiores a 0.05, indicando excelente ajuste. Portanto, o modelo com dois fatores reproduz 
# muito bem a estrutura de correlação dos dados.


  
#E) rotacione os fatores e compare os gráficos dos fatores com e sem rotação

#Sem rotação:
afadjetivos

plot(afadjetivos$loadings , type="n",
     xlim=c(-1, 1), 
     ylim = c(-1, 1),
     main = "Sem Rotação")
text(load , labels=c("gentil","inteligente","feliz","Simpático ","justo") ,cex=1)

# Logo conseguimos ver uma separação clara, ambas na PC1 estão acima do 0, com valores positivos
# e no PC2, os atributos simpático, feliz, gentil, se encontram abaixo do 0.
# Enquanto, inteligente e justo estão acima do 0. O que indica características com sentidos
# opostos no fator 2 

#Com rotação 

## Ortogonal Varimax
afadjetivos_rotOrt_varimax <- principal(adjetivos[,-1], nfactors = 2, rotate='varimax')
afadjetivos_rotOrt_varimax
plot(afadjetivos_rotOrt_varimax$loadings , type="n",
     xlim=c(-1, 1), 
     ylim = c(-1, 1),
     main = "Rotação Varimax")
text(afadjetivos_rotOrt_varimax$loadings , labels=c("gentil","inteligente","feliz","Simpático ","justo") ,cex=1)

# Não mudou

## Ortogonal Quartimax
afadjetivos_rotOrt_quartimax <- principal(adjetivos[,-1], nfactors = 2, rotate='quartimax')
afadjetivos_rotOrt_quartimax
plot(afadjetivos_rotOrt_quartimax$loadings , type="n",
     xlim=c(-1, 1), 
     ylim = c(-1, 1),
     main = "Rotação Quartimax")
text(afadjetivos_rotOrt_quartimax$loadings , labels=c("gentil","inteligente","feliz","Simpático ","justo") ,cex=1)

#não mudou

# Não ortogonal
afadjetivos_rotNOrt_promax <- principal(adjetivos[,-1], nfactors = 2, rotate='promax')
afadjetivos_rotNOrt_promax
plot(afadjetivos_rotNOrt_promax$loadings , type="n",
     xlim=c(-1, 1), 
     ylim = c(-1, 1),
     main = "Rotação Oblíqua")
text(afadjetivos_rotNOrt_promax$loadings , labels=c("gentil","inteligente","feliz","Simpático ","justo") ,cex=1)

# A rotação foi fundamental para tornar a estrutura fatorial interpretável. Sem rotação, 
# os fatores eram confusos, com todas as variáveis carregando no primeiro fator e uma mudança 
# de sinal no segundo que dificultava a nomeação. Com a rotação Varimax, obteve-se uma estrutura 
# simples em que:
#   
#   RC1 = Cordialidade (gentil, simpático, feliz);
#   RC2 = Competência (inteligente, justo).
# 
# A complexidade média caiu de 1.6 para 1.1, indicando que as variáveis se tornaram 
# mais "puras". As comunalidades e a variância total explicada (96%) permaneceram 
# inalteradas, confirmando que a rotação melhorou a interpretabilidade sem prejudicar 
# a qualidade do ajuste. A rotação Promax revelou ainda que os dois fatores são correlacionados 
# (r = 0.31), o que pode ser relevante dependendo do referencial teórico adotado.


# F) Repita a análise utilizando estimação por máxima verossimilhança 
adjetivos2 <- adjetivos[,-2]

afadjetivos_mv <- fa(adjetivos2[,-1], nfactors = 1, fm = "ml", rotate = "none")
afadjetivos_mv

afadjetivos2_mv <- fa(adjetivos2[,-1], nfactors = 2, fm = "ml", rotate = "none")
afadjetivos2_mv

af_minres <- fa(adjetivos2[,-1], nfactors = 2, fm = "minres", rotate = "none")
af_minres

# A análise por máxima verossimilhança foi tentada, mas não pôde ser realizada com as 
# 5 variáveis originais devido à multicolinearidade entre gentil e simpatico (r = 0.9954), 
# que torna a matriz singular. Removendo gentil, foi possível estimar 1 fator com as 4 
# variáveis restantes (df = 2). No entanto, o modelo não apresentou bom ajuste: o teste de 
# hipótese rejeitou a suficiência de 1 fator (p = 0.027), o RMSEA foi alto (0.593) e o RMSR 
# foi 0.33. Além disso, a variável simpatico apresentou um Heywood case (h² = 1.00), e as 
# variáveis inteligente e justo tiveram comunalidades muito baixas (0.11 e 0.29). Esses 
# resultados indicam que 1 fator não é suficiente para explicar a estrutura dos dados, 
# que requer 2 fatores (cordialidade e competência). Como o ML não permite estimar 2 fatores 
# com apenas 4 variáveis, conclui-se que a estimação por máxima verossimilhança não é adequada 
# para este conjunto de dados. A análise de componentes principais com rotação Varimax, 
# utilizando as 5 variáveis, mostrou-se mais apropriada, produzindo uma solução clara e 
# interpretável.