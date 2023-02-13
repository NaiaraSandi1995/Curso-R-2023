#TÓPICO 4 - Análise descritiva####

#Pacote para abertura da base de dados
library(haven)

#Abertura da base de dados
Brasil2014dta <-  
  read_dta("636339374Brazil LAPOP AmericasBarometer 2014 v3.0_W.dta")

#Para olhar a base
View(Brasil2014dta)

#Seleção das variáveis####
colnames(Brasil2014dta)
Brasil2014dta$q11n

#Variáveis utilizadas####
# ls3 = Satisfação com a vida  (SatVida) - inverter a escala 
# q2 = Idade  
# q1 = Sexo
# q10new = Renda familiar (RendFam)
# q10d = Percepção sobre a renda (PercRend) - Inverter a escala
# q11n = Estado civil (EstCiv)
# q12c = Quantidade de moradores no mesmo domicílio (QuantMor)
# ocup4a = Ocupação (Ocupacao)
# q12= Quantos filhos tem (Filhos)


#Após definir todas as variáveis que iremos querer basta 
#utilizar a função subset que aprendemos na seção anterior

Bra2014Menor <-  subset(Brasil2014dta, select = 
                          c("ls3", "q2", "q1", 
                            "q10new", "q10d",
                            "q11n", "q12c", "ocup4a",
                            "q12"))

#Salve a base de dados
save(Bra2014Menor, file = "Bra2014Menor.RData")
#Agora vejam como está a base de dados

#Toda a base é codificada numéricamente, 
#Para algumas variáveis analisar dessa forma não faz sentido, 
#Por isso iremos ter que recodificar e reorganizar

#Para isso, vamos entender como estão organizado os dados 
#através da função table ou summary que são funções
#que nos auxiliam a olhar a organização das variáveis

str(Bra2014Menor)

#Para alterar as variáveis iremos utilizar a função
#Recode que percente ao pacote:
library(memisc)
#Além disso, precisaremos utilizar o as.factor para 
#alterar algumas variáveis, pois essa função 
#somente funciona para variáveis que são fatores.

table(Bra2014Menor$ls3)
# 1   2   3   4 
#825 480 131  62 

#Essa variável ls3 deve ser recodificada e sua 
#escala alterada. 

#transformar uma variável que está como caracter 
#em fator
Bra2014Menor$ls3 <-  as.factor(Bra2014Menor$ls3)

#Recodificar
Brasil2014Menor$SatVida <- recode(Brasil2014Menor$ls3, 
                                  "Muito Satisfeito" <- 1,
                                  "Pouco Satisfeito" <- 2,
                                  "Pouco Insatisfeito" <- 3,
                                  "Muito insatisfeito" <- 4)

#Vamos olhar como estão os levels, e se for preciso 
#alterar
levels(Bra2014Menor$SatVida)
#[1] "Muito satisfeito"   "Pouco satisfeito"   "Pouco insatisfeito" "Muito insatisfeito"

#Queremos que seja o contrário disso
Bra2014Menor$SatVida <-  
  factor(Bra2014Menor$SatVida, 
         levels = c("Muito insatisfeito", "Pouco insatisfeito",
                    "Pouco satisfeito", "Muito satisfeito"))
#Olhamos de novo, só pra garantir e ok! 
levels(Bra2014Menor$SatVida)

summary(Bra2014Menor$q2)#qual é a nossa preocupação 
#quando trabalhamos com idade?
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#16.00   26.00   38.00   39.63   51.00   89.00 

#A idade é numérica, então está ok, só vamos alterar seu nome
Bra2014Menor$Idade <- Bra2014Menor$q2
#Vejam que na verdade não mudamos o nome da variável, 
#Só criamos uma cópia dela com outro nome
#Se for preciso, depois conseguimos trabalhar com a original

table(Bra2014Menor$q1)
# 1   2 
#749 751 

#Vamos alterar o nome para sexo e categorizar, podemos manter a ordem.

Bra2014Menor$q1 <-  as.factor(Bra2014Menor$q1)

Bra2014Menor$Sexo <-  recode(Bra2014Menor$q1, 
                             "Homem" <- 1, 
                             "Mulher" <- 2)

#A renda está em faixas, ou a gente faz toda a recategorização
#Ou mantem dessa forma lembrando que os números não representam a 
#quantidade de salários mínimos ou algo assim, mas as faixas de renda 
table(Bra2014Menor$q10new)
# 0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16 
# 1  71  63 129  68  83  63  86  76 146  80 142 185 113  56  33  49 

#Vamos manter dessa forma e alterar apenas o nome
Bra2014Menor$RendFam <- Bra2014Menor$q10new

#Aqui, sobre a percepção sobre a renda familiar, vamos
#recategorizar juntando categorias
table(Bra2014Menor$q10d)
# 1   2   3   4 
# 120 645 545 187 

Bra2014Menor$PercRend <- as.factor(Bra2014Menor$q10d)

Bra2014Menor$PercRend <- recode(Bra2014Menor$PercRend, 
                                "Satisfeito" <- c(1,2),
                                "Insatisfeito" <- c(3,4))
table(Bra2014Menor$PercRend)
# Satisfeito Insatisfeito 
# 765          732 

levels(Bra2014Menor$PercRend)
#[1] "Satisfeito"   "Insatisfeito"


Bra2014Menor$PercRend <-  
  factor(Bra2014Menor$PercRend, 
         levels = c( "Insatisfeito", "Satisfeito"))

#O estado civil tem algumas categorias que podem ser agregadas
#porque para as nossas análises não é necessário tanta especificidade
table(Bra2014Menor$q11n)
#   1   2   3   4   5   6   7 
# 471 611 246  45  40  76  11 

Bra2014Menor$q11n <-  as.factor(Bra2014Menor$q11n)

Bra2014Menor$EstCiv <- recode(Bra2014Menor$q11n, 
                              "Solteiro" <- 1, 
                              "Casado" <-  c(2,3,7),
                              "Separado" <- c(4,5),
                              "Viúvo" <- 6) 
table(Bra2014Menor$EstCiv)
# Solteiro   Casado Separado    Viúvo 
#   471      868       85       76 

#Quantidade de pessoas na casa é numérica, então vamos manter assim
#Vamos apenas mudar o nome
summary(Bra2014Menor$q12c)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   3.000   4.000   3.827   5.000  15.000 

Bra2014Menor$QuantMor <- Bra2014Menor$q12c

#A ocupação também podemos juntar algumas categorias
table(Bra2014Menor$ocup4a)
# 1   2   3   4   5   6   7 
# 812  63  81 125 200 180  39 

Bra2014Menor$ocup4a <-  as.factor(Bra2014Menor$ocup4a)

Bra2014Menor$Ocupacao <- recode(Bra2014Menor$ocup4a,
                                "Emprego_Rem" <-  1,
                                "Emprego_N_Rem" <-  5,
                                "Estudante" <-  4,
                                "Aposentado" <- 6,
                                "Nao_Empregado" <- c(2,3,7))
table(Bra2014Menor$Ocupacao)

#Quantidade de filhos é numérica, 
#Sendo assim somente mudaremos o nome
summary(Bra2014Menor$q12)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.000   0.000   2.000   1.888   3.000  16.000       2 

Bra2014Menor$Filhos <-  Bra2014Menor$q12

#Após observar a organização de cada uma das variáveis
#vamos retornar organizando cada uma delas 

#Medidas de tendência central####

#Média e mediana####

#Funções já utilizadas
summary(Bra2014Menor$Filhos)
table(Bra2014Menor$Filhos)
table(Bra2014Menor$Ocupacao)

#Funçoes específicas para média e mediana
mean(Bra2014Menor$Filhos, na.rm = T)
median(Bra2014Menor$Filhos, na.rm = T)

#Para o cálculo da moda:http://www.dma.uem.br/kit/outros-arquivos/moda.pdf
#Explicações do help
moda <- function(x) {
  modal <- unique(x) 
  modal[which.max(tabulate(match(x, modal)))]
}


#unique returns a vector or data frame like x 
#but with duplicate elements/rows removed.

#which.max: Determines the location, i.e., 
#index of the (first) minimum or maximum of 
#a numeric (or logical) vector.

#tabulete:counts the number of times each integer occurs in it.

#match returns a vector 
#of the positions  of its first argument in its second.

# Agora podemos rodar a função moda:
moda(Bra2014Menor$Filhos)
#[1] 0

#Frequência
#Pacote
library(descr)
freq(Bra2014Menor$SatVida)

freq(Bra2014Menor$SatVida)

#Tratar como valor perdido uma determinada categoria
freq(Bra2014Menor$SatVida, user.missing = "Muito satisfeito")

#Table - Análise cruzada####

table(Bra2014Menor$SatVida, Bra2014Menor$Sexo)
#                    Homem Mulher
# Muito insatisfeito   433    392
# Pouco insatisfeito   236    244
# Pouco satisfeito      51     80
# Muito satisfeito      28     34


#Vamos salvar o table que criamos dentro de um 
#objeto e depois vamos chamar ele usando 
#Uma outra função.
Obj <- table(Bra2014Menor$SatVida, Bra2014Menor$Sexo)

#Vamos utilizar a função: prop.table()
#Para apresentar os valores percentuais, contudo
#Teremos que multiplicar por 100, por isso salvamos 
#tudo dentro de um novo objeto
Obj2 <- prop.table(Obj, margin = 1)
#Apresenta o percentual na linha 
Obj2*100

Obj3 <- prop.table(Obj, margin = 2 )
#Apresenta o percentual na coluna
Obj3 * 100


#Escolher quantas casas após a vírgula queremos utilizar
options(digits = 1)

###
plot(Bra2014Menor$Idade)
plot(Bra2014Menor$Ocupacao)
#
hist(Bra2014Menor$Idade)
boxplot(Bra2014Menor$Idade)
#
boxplot(Idade ~ Sexo, data= Bra2014Menor)
#
boxplot(Idade ~ SatVida, data= Bra2014Menor)