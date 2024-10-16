#Correlação####

#Correlação de Pearson####
# Aqui, simulamos um conjunto de dados com variáveis 
# relacionadas ao nível de educação e à renda anual, 
# temas centrais nas análises socioeconômicas.

#Criar uma base de dados

set.seed(123)

educ <- sample(8:20, 100, replace = TRUE)
renda <- educ *5000 + rnorm(100, mean= 30000, sd=10000)

dados_educ_renda <- data.frame(educ, renda)

# Visualizar as primeiras linhas da base de dados
head(dados_educ_renda)

cor.test(dados_educ_renda$educ, dados_educ_renda$renda, 
         method = "pearson")

options(scipen = 999)

# Calcular a correlação de Pearson entre educação e renda
cor(dados_educ_renda$educ, dados_educ_renda$renda, method = "pearson")

#Apresentação gráfica####

# Gráfico de Dispersão (Scatter Plot) com Linha de Tendência
# O gráfico de dispersão é uma das maneiras mais comuns de 
# visualizar a correlação entre duas variáveis. 
# Podemos adicionar uma linha de regressão para destacar a 
# relação entre educação e renda.

# Carregar pacotes necessários
library(ggplot2)

# Criar o gráfico de dispersão com linha de tendência
ggplot(dados_educ_renda, aes(x = educ, y = renda)) +
  geom_point(color = "blue") +  # Adiciona pontos
  geom_smooth(method = "lm", color = "red", se = FALSE) +  
  # Adiciona linha de regressão linear
  labs(title = "Relação entre Educação e Renda",
       x = "Anos de Escolaridade",
       y = "Renda Anual (R$)") +
  theme_minimal()

# Esse exemplo simula a relação entre o nível educacional 
# (anos de escolaridade) e a renda anual. Em pesquisas sociais, 
# esperamos encontrar uma correlação positiva, pois mais anos de educação 
# geralmente estão associados a maiores rendimentos.

#Boxplot: Mostra a variação da renda em cada nível educacional, 
#incluindo valores atípicos.
# Gráfico de Boxplot
ggplot(dados_educ_renda, aes(x = factor(educ), y = renda)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribuição da Renda por Anos de Escolaridade",
       x = "Anos de Escolaridade",
       y = "Renda Anual (R$)") +
  theme_minimal()

# Correlação de Spearman####
# Primeiro, vamos carregar o conjunto de dados 
# e calcular a correlação de Pearson entre duas 
# variáveis: mpg (milhas por galão) e 
# wt (peso do carro).

# Carregar conjunto de dados
data(mtcars)

# Visualizar as primeiras linhas dos dados
head(mtcars)

# Calcular correlação de Pearson
cor(mtcars$mpg, mtcars$wt)

cor.test(mtcars$mpg, mtcars$wt)

# Neste caso, estamos calculando a correlação 
# entre o consumo de combustível e o peso do carro. 
# Espera-se uma correlação negativa, ou seja, 
# quanto maior o peso, menor a 
# eficiência do carro em termos de consumo.

# Se suspeitarmos que a relação não é linear, 
# podemos utilizar a correlação de Spearman,
# que não exige linearidade entre as variáveis.

# Calcular correlação de Spearman
cor.test(mtcars$mpg, mtcars$wt, method = "spearman")



# Matriz de correlação
# Podemos calcular a correlação entre várias 
# variáveis de uma vez, gerando uma matriz de 
# correlação.

# Matriz de correlação entre algumas variáveis 
#do conjunto de dados
cor(mtcars[, c("mpg", "wt", "hp", "qsec")])

# Para facilitar a interpretação,  é possível visualizar a correlação 
# utilizando um gráfico de calor.

# Instalar e carregar a biblioteca necessária para visualização
install.packages("corrplot")
library(corrplot)

# Criar a matriz de correlação
matriz_cor <- cor(mtcars)

# Plotar o gráfico de correlação
corrplot(matriz_cor, method = "color",
         type = "upper", tl.col = "black")
# Esse gráfico mostra a força e a direção 
# da correlação entre as variáveis. 
# As cores indicam a intensidade da correlação,
# com azul representando correlação positiva e 
# vermelho correlação negativa.

#Correlação de Kendall####
# 
# (Confiança Social e Participação Religiosa)
# Vamos agora criar um exemplo onde avaliamos a correlação 
# entre a confiança nas instituições sociais e a frequência à igreja,
# medidos em escalas ordinais.

# Criar uma base de dados fictícia
set.seed(456)

dados_conf_religiao <- data.frame(
  confiança_instituições = sample(1:5, 100, replace = TRUE),  
  # 1 = Nenhuma confiança, 5 = Muita confiança
  freq_igreja = sample(1:5, 100, replace = TRUE)  
  # 1 = Nunca, 5 = Todo fim de semana
)

# Visualizar as primeiras linhas da base de dados
head(dados_conf_religiao)

# Calcular a correlação de Kendall entre confiança nas
#instituições e frequência à igreja
cor.test(dados_conf_religiao$confiança_instituições,
         dados_conf_religiao$freq_igreja, method = "kendall")

cor(dados_conf_religiao$confiança_instituições, 
    dados_conf_religiao$freq_igreja, method = "kendall")
#[1] -0.05724389

# Neste exemplo fictício, estamos analisando a correlação
# entre a confiança nas instituições sociais e a frequência
# à igreja. Esses dados são frequentemente utilizados para 
# investigar a relação 
# entre crenças religiosas e confiança na sociedade.

help("cor.test")