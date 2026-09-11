#--------------------------------------------------------------------------------------#
# Izenman(2008)apresenta um estudo sobre dados nutricionais que analisa 961 alimentos.
# Os componentes nutricionais observados foram: gordura(gramas),energia(calorias),
# carbohidratos (gramas), proteínas (gramas), colesterol (miligramas), peso (gramas) e
# gordurasaturada(gramas).Os alimentos são listados em porções variadas e por isso as
# porções são divididas pelo peso correspondente de cada item.
#
# A matriz de variância dos dados é (colunas referentes a gordura,calorias,carbohidratos,
# proteínas,colesterol e gordura saturada)
#--------------------------------------------------------------------------------------#----

S <- matrix(c(0.037, 0.306, -0.007, 0.003, 0.023, 0.010,
              0.306, 3.747, 0.157, 0.045, 0.178, 0.082,
              -0.007, 0.157, 0.062, -0.002, -0.027, -0.002,
              0.003, 0.045, -0.002, 0.008, 0.020, 0.001,
              0.023, 0.178, -0.027, 0.020, 0.456, 0.014,
              0.010, 0.082, -0.002, 0.001, 0.014, 0.004), byrow = T, nrow = 6, ncol = 6)

#Utilizando os resultados apresentados, responda as seguintes questões

#A) Obtenha os autovalores de S e respectivos autovetores

autoS <- eigen(S)

D_autoValor <- diag(autoS$values) 
P <- autoS$vectors


#B) Obtenha a matriz de Correlações R, seus autovalores e respectivos autovetores. 
#Podeos obter a matriz R pela seguinte relação R = D^-1/2 S D^-1/2, com D = diag(diag(S)) e
# S = matriz de correlação

D <- diag(diag(S))
D_um_meio <- D^0.5
D_inversa_um_meio <- solve(D_um_meio)

R <- D_inversa_um_meio %*% S %*% D_inversa_um_meio

#c) Obtenha a variância amostral generalizada (VAG) e a Variância amostral total (VAT). Interprete
#os resultados. Qual o motivo da grande diferença entre VAG e VAT? 
#Este fato é refletido em alguma medida obtida de S?

TSV <- sum(diag(S))

#Lembrar que a soma de autovalores é igual a soma da diagonal de S 
sum(diag(D))

GSV <- det(S)
#Lembrar que a multiplicação dos autovalores é igual ao det de S 
det(D_autoValor)

# GSV é praticamente zero, logo indica que há dependência linear 

# Quando a VAG é muito menor que a VAT, é sinal de multicolinearidade severa. 
# As variáveis estão tão correlacionadas que, na prática, a dimensão efetiva dos 
# dados é menor que p (aqui p = 6). Os dados se concentram em um subespaço de dimensão reduzida.

# Ao calcular os autovalores de S verificamos que da 2 pra última variável os valores 
# são próximos de 0, o que indica que as variáveis não conseguem reter a informação original. 
# Então, pode-se reduzir a dimensão. 

#Uma VAG muito menor que a VAT indica redundância entre as variáveis.


#D) Sobre a matriz S 

# 1) Indique se esta matriz é positiva definida. Justifique

# Como todos os autovalores são mmaiores que zero, então, S é positiva definida. 

# 2) Indique como você faria para obter a decomposição espectral da matriz S 

# A decomposição espctral é obtida:
#   A = P D PT
# P: A matriz de autovetores ortogonais associados aos autovalores de S 
# D: Matriz de autovalores de S 

# 3) Indique um procedimento para obter S^-1 e S^1/2 (se existir)

#O procedimento para o cálculo é usando a decomposição espectral, assim, basta realizar as 
# operações na matriz diagonal de autovalores. 

#------------------------------------------------------------------------------------------#----
# 2.4 O ambiente institucional de um país interfere na estratégia e desempenho das empresas. 
# Para avaliar esse ambiente, várias entidades internacionais criaram índices comparativos 
# entre os países. Um deles é o Índice de Liberdade Econômica (ILE), publicado anualmente 
# pela Fundação Heritage e o itálico: The Wall Street Journal. O índice é calculado por meio 
# da avaliação de doze itens que procuram mensurar o grau de liberdade concedido aos agentes 
# econômicos. Cada item é classificado em uma escala de 0 a 100, e quanto maior for a avaliação, 
# mais
# 
# Tabela 2.7: Variáveis disponíveis em municípios de Minas Gerais e de São Paulo Variável
# Descrição
# Município Nome do município Casos
# Óbitos
# IDHR IDHL IDHE
# Casos de Covid-19 notificados até 01/10/2020 Óbitos por Covid-19 notificados até 01/10/2020
# População População estimada em 2020 IDHM
# Índice de Desenvolvimento Humano Municipal em 2010 Índice de Desenvolvimento Humano Renda em 2010
# Índice de Desenvolvimento Humano Longevidade em 2010 Índice de Desenvolvimento Humano Educação em 2010
# Prevalência Casos de Covid-19 por 100.000 habitantes até 01/10/2020 
# Mortalidade Óbitos por Covid-19 por 100.000 habitantes até 01/10/2020 
# Letalidade Óbitos por Covid-19 por 100 casos até 01/10/2020
# 
# liberdade econômica é reconhecida institucionalmente.17 Os dados apresentados no 
# arquivo ILE2020.xlsx correspondem ao ano de 2020. A Tabela 2.8 resume os itens dessa escala.
#-----------------------------------------------------------------------------------------#----
# a) Estude, por meio de análises gráficas e numéricas, as relações existentes entre as variáveis.
# b) Investigue a existência de valores aberrantes


#------------------------------------------------------------------------------------#----
# 2.5 A Tabela 2.10 traz os dados da aba Medias de Pizza.xlsx,18 com infor-mações 
# sobre os valores médios de características físico-químicas avaliadas em amostras 
# de dez marcas de pizzas. A lista das características está na Tabela 2.9.

# Tabela 2.8: Variáveis sobre liberdade econômica
# Variável Descrição 
# ID - Identificação
# País - Nome do país (em inglês) 
# Região- Região
# Classif -   Classificação do país (quanto menor, maior é a liberdade econômica)
# ClassifReg -  Classificação do país por região 
# ILE - Índice de Liberdade Econômica
# X1 - Direitos de propriedade (quanto maior, mais direitos) 
# X2 - Efetividade judicial (quanto maior, mais efetiva é a justiça) 
# X3 - Integridade governamental (quanto maior, mais íntegro é o governo) 
# X4 - Carga fiscal (quanto maior, mais justa é considerada a carga fiscal) 
# X5 - Gastos governamentais (quanto maior, melhor é a estrutura de gastos) 
# X6 - Saúde fiscal (quanto maior, melhor é a saúde fiscal)
# X7 - Liberdade de negócios (quanto maior, maior é a liberdade de negócios)
# X8 - Liberdade de trabalho (quanto maior, maior é a liberdade de trabalho) 
# X9 - Liberdade monetária (quanto maior, maior é a liberdade monetária) 
# X10 - Liberdade comercial (quanto maior, maior é a liberdade comercial) 
# X11 - Liberdade deiInvestimentos (quanto maior, maior é a liberdade) 
# X12 - Liberdade financeira (quanto maior, maior é a liberdade financeira)

# a) Baseado nesses dados, compare as marcas de pizzas utilizando as faces de Chernoff 
# e o gráfico radar. Identifique marcas com produtos semelhantes.
#
# b) Construa e analise um biplot para esses dados.(utilizar um dos gráficos visto na aula)
#
# c) Baseado no item b), o proprietário da marca I gostaria de ter produtos parecidos 
# com os da marca C. Que mudanças ele deveria promover em seus produtos?
#
# d) Na aba Completo, estão as observações originais. Faça e interprete um gráfico 
# matriz construído a partir dos dados dessa aba
#-----------------------------------------------------------------------------------#----





