#TÓPICO 5 - ####
#BASE DE DADOS UTILIZADA: BRA2014MENOR
#ESSA BASE É FRUTO DE UM SUBSET DA BASE 
#DO LAPOP 2014

#Falar sobre salvamento de imagem ! 

#Apresentação gráfica####
#Boxplot####

#Nós vimos que a versão mais simples do boxplot já 
#Vem com a mediana, mas podemos inserir também  
#o ponto da média

#No R base nós fazemo isso
#através da criação de um objeto contendo a média
#Que depois será inserido junto ao gráfico
Média <- mean(Bra2014Menor$Idade)

boxplot(Bra2014Menor$Idade)
points(Média, pch=16,col="red")



#PCH significa Plot character, ou plotagem de caracter
#é a função que define qual será o caracter plotado 
#existem 25 modelos, você pode escrever pch no help 
#Para encontrar todos os tipos
#Col é a cor que o seu ponto terá

#Agora vamos etender como isso funciona no ggplot 
#Documentação do pacote 33

#Pacote para criação de gráficos 
library(ggplot2)

#Antes de enteder como criar um boxplot 
#É preciso entender que qualquer projeção gráfica utilizando 
#o ggplot é criada através de uma série de camadas específicas 
#Cada camada irá inserir um detalhe ao gráfico 

#Além disso, existe um mapeamento geral que compõe cada gráfico
#Vamos ver como isso funciona

#1º versão:
ggplot(Bra2014Menor, aes(y= Idade)) + 
  geom_boxplot()



#2ª versão:
ggplot(Bra2014Menor, aes(y= Idade)) + 
  geom_boxplot() + 
  theme_classic() + #Temas 
  labs(title = "Gráfico de idades", y="Idade", x="Contagem")


#todos os gráficos possuem em seu mapeamento o eixo y e o eixo x
#No caso do boxplot o ideal é que em um dos eixos
#exista uma variável qualitativa, quando temos a 
#intenção de comparar categorias entre si, inserimos uma variável
#específica, quando não temos a inteção de trabalhar com variáveis 
#cruzadas temos a intenção de fazer uma análise cruzada
#podemos não inserir nada, porém isso fará diferença quando quisermos
#realizar certas análises, como a média
#Por isso, inserimos algum outra variável no eixo faltante
#Pode ser uma contagem com a função count ou então uma única
#informação

#3ª versão: Inserção da média

Bra2014Menor$V <- c("pessoa")

#*Falar sobre as cores
ggplot(Bra2014Menor, aes(y= Idade, x = V)) + 
  geom_boxplot(colour = "#59566B", fill= "#C6BEEE")+ #Cores do gráfico
  theme_classic() + #Temas 
  labs(title = "Gráfico de idades", y="Idade", x="Contagem")

#Versão 4
ggplot(Bra2014Menor, aes(y= Idade, x = V)) + 
  geom_boxplot(colour = "#59566B", fill= "#C6BEEE")+ 
  theme_classic() + #Temas 
  labs(title = "Gráfico de idades", y="Idade", x="Contagem") +
  stat_summary(fun = mean, geom="point",  #Linha para colocar média
               shape=16, size=4, color="#87C4EB") 

#Versão 5
#Fonte####
Fonte <-  theme(text = element_text(family = "serif", size = 13),
                title = element_text(color = "#8b0000"),
                axis.line = element_line(color = "#8b0000"), 
                axis.text = element_text(colour = "#8b0000", size = rel(0.5)),
                plot.background = element_rect(fill = "grey90", colour = "black", 
                                               linewidth = 1)) 
#rel() is used to specify sizes relative




#Análise cruzada
#Versão 1
ggplot(Bra2014Menor, aes(y= Idade, x=Sexo)) + 
  geom_boxplot() 



#Versão 2
ggplot(Bra2014Menor, aes(y= Idade, x=Sexo,
                         fill= Sexo)) + 
  geom_boxplot()
#

#Versão 3
ggplot(Bra2014Menor, aes(y= Idade, x=Sexo,
                         color= Sexo)) + 
  geom_boxplot()



#Versão 4
ggplot(Bra2014Menor, aes(y= Idade, x=Sexo)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=3, 
               fill= c("blue", "red"), alpha = 0.2)

#Versão 5
#Salvar em um objeto
Gráfico <- ggplot(Bra2014Menor, aes(y= Idade, x=Sexo)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=3, 
               fill= c("blue", "red"), alpha = 0.2)
Gráfico

#Versão 6
Gráfico1 <-   Gráfico + 
  theme_classic() + #Temas 
  labs(title = "Gráfico de idades", y="Idade", x="Contagem") +
  stat_summary(fun = mean, geom="point",  #Linha para colocar média
               shape=16, size=2, color="red")



#Versão 7
Gráfico1 + Fonte


#Salvamento de imagem####
jpeg("Grafico2.jpeg")
Média <- mean(Bra2014Menor$Idade)
boxplot(Bra2014Menor$Idade)
points(Média, pch=16,col="red")
dev.off() 




#Histograma####
#Versão1
Bra2014Menor$Id <- c(1:1500)

Gráfico1 <- ggplot(Bra2014Menor, aes(x= Idade))+
  geom_histogram()

Gráfico1

#Versão2
Gráfico2 <- ggplot(Bra2014Menor, aes(x= Idade, fill= Sexo))+
  geom_histogram()

Gráfico2

#Versão3
Gráfico3 <- ggplot(Bra2014Menor, aes(x= Idade, fill= Sexo))+
  geom_histogram(position = "dodge") #Para colocar lado a lado as barras

Gráfico3

#Versão4
Gráfico4 <- ggplot(Bra2014Menor, aes(x= Idade, fill= Sexo))+
  geom_histogram(position = "dodge", binwidth = 5) #Para colocar lado a lado as barras
#tamanho das barras
Gráfico4

#Versão5 - cor
# https://colorbrewer2.org/#type=qualitative&scheme=Pastel2&n=3

Gráfico5 <- ggplot(Bra2014Menor, aes(x= Idade, fill= Sexo))+
  geom_histogram(position = "dodge", binwidth = 5) +
  scale_fill_brewer(palette="Pastel2", type = "div")

Gráfico5

#Versão6 

Gráfico6 <- ggplot(Bra2014Menor, aes(x= Idade, fill= Sexo))+
  geom_histogram(position = "dodge", binwidth = 5) +
  scale_fill_brewer(palette="Pastel2", type = "div")+
  scale_y_continuous(limits = c(0, 103))+
  scale_x_continuous(limits = c(0, 103)) +
  labs(title = "Idade por sexo", y="Contagem", x= "Idade") +
  theme_classic()


Gráfico6

#versão7
Gráfico6 + Fonte

#Gráficco de barras geom_bar####
library(scales) #Pacote para criação de percentual


#Versão1
Gráfico1 <- ggplot(Bra2014Menor, aes(x= PercRend)) +
  geom_bar() 

Gráfico1

#Para retirar os NA
library(tidyr)
Bra2014Menor <- drop_na(Bra2014Menor, PercRend)

#Se estiver em notação científica, mostrar como faz para mudar
Gráfico1

#Versão2
#Percentual####
#Ao invés de eixo número, podemos utilizar um eixo percentual
#Para entender mais:http://rstudio-pubs-static.s3.amazonaws.com/15833_7ab6dc7be6b64b53a576093a364dacf9.html

Gráfico2 <- ggplot(Bra2014Menor, aes(x= PercRend, 
                                     y= (..count..)/sum(..count..))) +
  geom_bar()

Gráfico2

#Versão3
#Podemos mostrar o eixo em percentual, para que não seja 
#Preciso mentalmente ficar multiplicando por 100
#Vamos também colocar uma cor, de acordo com o Percentual de renda

Gráfico3 <- ggplot(Bra2014Menor, aes(x= PercRend, 
                                     y= (..count..)/sum(..count..), fill= PercRend)) +
  geom_bar() +
  scale_y_continuous(labels = percent)  +
  theme_classic()

Gráfico3

#Versão4
#Inserção dos labs
Gráfico4 <-  Gráfico3 +
  labs(title = "Percepção sobre a renda",
       y= "Percentual",
       x = "Percepção")

#Versão5
#Além do título e rótulos dos eixos
#podemos inserir os rótulos dos dados

Gráfico5 <- Gráfico4 +
  geom_text(aes(y = ((..count..)/sum(..count..)),
                label = percent((..count..)/sum(..count..))),
            stat = "count", vjust = -0.25, color="black",size=4)

Gráfico5

#Versão6
#Na fase final de organização podemos cuidar 
#das cores e demais detalhes

Gráfico6 <- Gráfico5 +  
  scale_fill_brewer(palette="BuPu", type = "seq")

Gráfico6

#Versão7
#Juntar a fonte
Gráfico7 <- 
  Gráfico6 + Fonte +
  theme(legend.position="none")

#Local: "bottom", "top", "left", ou "right" 

#Função facet_wrap
#Sequência de paineis * Dar a tese como exemplo

Gráfico7 + facet_wrap( ~ Bra2014Menor$Sexo)


#Gráfico de barras sobrepostas####
#Versão1
Gráfico1 <- ggplot(Bra2014Menor, aes(x= SatVida, 
                                     y= (..count..)/sum(..count..))) +
  geom_bar()

Gráfico1

#Para retirar os NA
library(tidyr)
Bra2014Menor <- drop_na(Bra2014Menor, SatVida)

#Versão2 Criação do gráfico de barras sobrepostas
Gráfico2 <- ggplot(Bra2014Menor, aes(x= SatVida, 
                                     y= (..count..)/sum(..count..),
                                     fill=Sexo)) +
  geom_bar()
Gráfico2
#Versão3 Percentual e organização do eixo y
library(scales) #Pacote para criação de percentual

Gráfico3 <- ggplot(Bra2014Menor, aes(x= SatVida, 
                                     y= (..count..)/sum(..count..),
                                     fill=Sexo)) +
  geom_bar() +
  scale_y_continuous(labels = percent, limits = c(0, 0.6),
                     breaks = pretty(c(0,0.6), n = 5)) 


Gráfico3

#Versão4 Cor e título

Gráfico4 <-  Gráfico3 + scale_fill_brewer(palette = "Spectral") +
  labs(title = "Satisfação em relação a vida",
       y="Percentual", x="Satisfação") +
  theme_light()

Gráfico4

#Versão 5 Inserção da fonte

Gráfico4 + Fonte

#Gráfico de pizza&seção####


# PACOTE
library(ggplot2)
library(descr)
#Criar uma pequena base com 
freq(Bra2014Menor$Sexo, plot = F)

#            Frequência Percentual
# Homem         749      49.93
# Mulher        751      50.07
# Total        1500     100.00

#Criação de uma base de dados pequena
data <- data.frame(
  category = c("Homem", "Mulher"),
  count= c(746, 749), 
  percentual= c(49.9, 50.0)
)


# Computar os valores
data$fraction <- data$count / sum(data$count)

# Computador os valores do eixo y
data$ymax <- cumsum(data$fraction)

# Cálculo do eixo y 
data$ymin <- c(0, head(data$ymax, n=-1))

# Cálculo da posição do label
data$labelPosition <- (data$ymax + data$ymin) / 2

# Cálculo do label
#1
data$label <- paste0(data$category, "\n 50% ", data$count)
#2
data$label <- paste0(data$category, " 50% ", data$count)

#3QUANDO CRIAR A BASE DE DADOS DIRETO COM O PERCENTUAL
data$label <- paste0(data$percentual, " % ", data$category)

# gráfico
#Versão1
obj1 <- ggplot(data, aes(ymax=ymax, ymin=ymin, 
                         xmax=4, xmin=3, fill=category))+
  geom_rect() #Cria um gráfico reto

obj1
#Versão 2

obj2 <-  ggplot(data, aes(ymax=ymax, ymin=ymin, 
                          xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  coord_polar(theta="y") #Cria um gráfico redondo

obj2

#Versão 3
obj3 <-  ggplot(data, aes(ymax=ymax, ymin=ymin, 
                          xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  coord_polar(theta="y")+ #Cria um gráfico redondo
  theme_void() #Retira todas as marcas de eixos

obj3

#Versão 4
obj4 <-  ggplot(data, aes(ymax=ymax, ymin=ymin, 
                          xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  coord_polar(theta="y")+ #Cria um gráfico redondo
  theme_void() + #Retira todas as marcas de eixos
  theme(legend.position = "bottom") + #Altera o lugar da legenda
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=3, 
              fill= "white", color= "black")

obj4

#Versão 5 Gráfico de seção
obj5 <-  ggplot(data, aes(ymax=ymax, ymin=ymin, 
                          xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  coord_polar(theta="y")+ #Cria um gráfico redondo
  theme_void() + #Retira todas as marcas de eixos
  theme(legend.position = "bottom") + #Altera o lugar da legenda
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=3, 
              fill= "white", color= "black")+ 
  xlim(c(2, 4)) 

obj5

#Versão 6 organização do gráfico

obj6 <- obj5 + scale_fill_brewer(palette="Pastel2")+
  labs (fill = "Sexo", title = "       
                     Sexo dos entrevistados") + 
  theme(legend.position = "none")


#Para apagar somente o título do gráfico
# theme(legend.title = element_blank())

obj6

#Gráfico de dispersão#####
library(tidyverse)
Bra2014Menor$Filhos <- as.numeric(Bra2014Menor$Filhos)

#Versão 1
ggplot(Bra2014Menor, aes(x=Idade, y= Filhos)) +
  geom_point()

#Versão 2 
ggplot(Bra2014Menor, aes(x= Idade, y= Filhos))+
  geom_point(size= 3, alpha= 0.7) #Tamanho dos pontos e transparência

#Versão 3
ggplot(Bra2014Menor, aes(x= Idade, y= Filhos,
                         color= Idade))+ #Cor dos pontos
  geom_point(size= 3, alpha= 0.7) 

#Versão 4 
ggplot(Bra2014Menor, aes(x= Idade, y= Filhos,
                         color= Idade))+ 
  geom_point(size= 3, alpha= 0.7) +
  geom_smooth(method = "lm") #Inserção de uma reta linear model

#Versão 5 
ggplot(Bra2014Menor, aes(x= Idade, y= Filhos,
                         color= Idade))+
  geom_point(size= 3, alpha= 0.7) +
  geom_smooth(method = "lm", color = "black", se = T) #Organização da reta 

#Salvando a versão 5 para organizar a formatação
gráfico5 <-  ggplot(Bra2014Menor, aes(x= Idade, y= Filhos,
                                      color= Idade))+
  geom_point(size= 3, alpha= 0.7) +
  geom_smooth(method = "lm", color = "black", se = T)

#Versão 6
gráfico5 + 
  Fonte +
  theme(legend.position = "none") +
  labs(title = "               Idade por quantidade de filhos") 

#Gráfico de linhas####
library(tidyverse)

#Com a nossa base atual é difícil construir 
#Um gráfico de linhas, então vamos montar uma base pequen
library(descr)


#Criação da base
Base_Filhos <- data.frame(
  Anos = c(2010, 2012, 2014, 2015,2017),
  Med_Filhos_Bra = c(8, 6, 4, 7, 3),
  Med_Filhos_PR = c(5, 4, 8, 6, 2),
  Med_filhos_MG = c(6, 3, 2, 5, 7))

#Salvar a base 
save(Base_Filhos, file = "Base_Filhos.RData")

#Construir o gráfico
#Versão 1 
ggplot(Base_Filhos, aes (x = Anos, y=Med_Filhos_Bra))+
  geom_line()

#Versão 2 
ggplot(Base_Filhos, aes (x = Anos, y=Med_Filhos_Bra))+
  geom_line(arrow = arrow()) #Inserir uma flecha

#Versão 3
ggplot(Base_Filhos, aes (x = Anos, y=Med_Filhos_Bra))+
  geom_line(arrow = arrow(
    angle = 15, ends = "both", type = "closed"
  ))#inserir flecha de tam 15 dos dois lados e fechada (preta)

#Versão 4
ggplot(Base_Filhos, aes (x = Anos, y=Med_Filhos_Bra))+
  geom_line(color= "blue", linetype = 2) #Cores e tipo de linha

#Versão 5
ggplot(Base_Filhos, aes (x = Anos, y=Med_Filhos_Bra))+
  geom_line(color= "blue", linetype = 2) +
  geom_point(color="black", size=2)#inserção de pontos marcando

#Versão 6
ggplot(Base_Filhos, aes (x = Anos, y=Med_Filhos_Bra))+
  geom_step()

#Tipos de linhas 
#http://www.sthda.com/english/wiki/ggplot2-line-types-how-to-change-line-types-of-a-graph-in-r-software

################
#Versão 7
library(tidyverse)
#Vamos usar o tidyverse para criar uma base de dados novas
#gather ou seja juntar, as informações da média (numérica),
#Med_Est (categorica com o nome dos estudos), tudo isso pelos 
#anos 
BaseNewF <- Base_Filhos %>% gather(Med_Est, Media, -Anos)


ggplot(BaseNewF, aes (x = Anos, y=Media,
                      fill= Med_Est))+ #Dividindo por grupos
  geom_line()

#Versão 8 
ggplot(BaseNewF, aes (x = Anos, y=Media,
                      linetype =Med_Est, #Organizando o tipo da linha por grupo
                      color= Med_Est))+
  geom_line(size = 1)

#Versão 9 
Vers9 <- ggplot(BaseNewF, aes (x = Anos, y=Media,
                               colour = Med_Est))+
  geom_line(size = 1) +
  geom_point( alpha=0.5, color = "black", size = 2)
scale_color_brewer(palette="Set1") #Inserção de cor

Vers9

#Versão 10 dividindo em gráfico pequenininhos

Vers9 +
  facet_wrap(~ Med_Est, nrow = 3, ncol = 1) + #Função facet_wrap
  theme(legend.position = "none") + Fonte


#Ribbon####
#Organizar pra ensinar no futuro
ggplot(BaseNewF, aes (x = Anos, y=Media,
                      fill= Med_Est))+
  geom_ribbon(aes(ymax = Media,
                  ymin = 0),
              alpha = 0.3)