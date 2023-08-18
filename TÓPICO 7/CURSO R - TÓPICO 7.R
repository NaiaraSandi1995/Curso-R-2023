#Tópico 7#####
#Vamos utilizar a base de candidatos eleitos
#e não eleitos para o senado 2022
#Testes inferenciais bivariados####
library(readxl)
BASE_SEN_2022 <- read_excel("Senado 2022/BASE SEN 2022.xlsx", 
                            col_types = c("numeric", "text", "text", 
                                          "text", "text", "numeric", "numeric", 
                                          "text", "numeric", "text", "text", 
                                          "numeric", "numeric", "text", "numeric", 
                                          "text", "numeric", "text", "numeric", 
                                          "text", "numeric", "text", "numeric", 
                                          "numeric", "text", "text"))


##Teste de qui-quadrado####
#Diferença de média entre eleitos e não eleitos por cor e dps sexo

Teste1 <- table(BASE_SEN_2022$DS_SIT_TOT_TURNO, 
                BASE_SEN_2022$DS_COR_RACA)

chisq.test(Teste1)

# 
# Pearson's Chi-squared test
# 
# data:  Teste1
# X-squared = 15.701, df = 5, p-value = 0.00775

Teste2 <- table(BASE_SEN_2022$DS_SIT_TOT_TURNO, 
                BASE_SEN_2022$DS_GENERO)

chisq.test(Teste2)

# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  Teste2
# X-squared = 1.0059, df = 1, p-value = 0.3159




##Lambda####
#Para rodar o teste de lambda 

#Antes de rodar o teste, vamos atribuir os levels
BASE_SEN_2022$DS_COR_RACA <- 
  factor(BASE_SEN_2022$DS_COR_RACA, 
         levels = c("PRETA",  "PARDA", "INDÍGENA", 
                    "NÃO INFORMADO",  "AMARELA", "BRANCA"))

levels(BASE_SEN_2022$DS_COR_RACA)


BASE_SEN_2022$DS_GENERO <- 
  factor(BASE_SEN_2022$DS_GENERO, 
         levels = c("FEMININO",  "MASCULINO"))

levels(BASE_SEN_2022$DS_GENERO)

install.packages("rapportools")
library(rapportools)

lambda.test(Teste1)

# $row
# [1] 0
# 
# $col
# [1] 0.07407407

lambda.test(Teste2)

# $row
# [1] 0
# 
# $col
# [1] 0


##Gamma####

#Teste de Gamma (Y)
install.packages("vcdExtra")
library(vcdExtra)

#Vamos criar uma variável chamada
#Satisfação com a vida, somente para rodar o teste
#Vamos criá-la, a partir do estdo civil

# table(BASE_SEN_2022$DS_ESTADO_CIVIL)
# CASADO(A)             DIVORCIADO(A)   SEPARADO(A) JUDICIALMENTE 
# 133                        36                         4 
# SOLTEIRO(A)                  VIÚVO(A) 
# 27                         5 

#1= será o totalmente insastisfeito e 5=totalmente satisfeito
library(memisc)
BASE_SEN_2022$SatVida <- recode(BASE_SEN_2022$DS_ESTADO_CIVIL, 
                                "Totalmente insatisfeito" <- "VIÚVO(A)",
                                "Insatisfeito" <- "SEPARADO(A) JUDICIALMENTE",
                                "Meio termo" <- "DIVORCIADO(A)",
                                "Satisfeito" <-  "SOLTEIRO(A)",
                                "Totalmente satisfeito" <- "CASADO(A)")
table(BASE_SEN_2022$SatVida)
BASE_SEN_2022$SatVida <-  as.factor(BASE_SEN_2022$SatVida)
BASE_SEN_2022$DS_GRAU_INSTRUCAO <-  as.factor(BASE_SEN_2022$DS_GRAU_INSTRUCAO)

tab2 <- table(BASE_SEN_2022$SatVida, 
              BASE_SEN_2022$DS_GRAU_INSTRUCAO)
GKgamma(tab2)
# gamma        : -0.371 
# std. error   : 0.041 
# CI           : -0.451 -0.291 

chisq.test(tab2, simulate.p.value = T) #Apenas para simular o valor de p
# Pearson's Chi-squared test with simulated p-value (based on 2000 replicates)
# 
# data:  tab2
# X-squared = 26.001, df = NA, p-value = 0.3208


##Kendal####
install.packages("Kendall")
library(Kendall)

Kendall(BASE_SEN_2022$SatVida, 
        BASE_SEN_2022$DS_GRAU_INSTRUCAO)

#tau = -0.00451, 2-sided pvalue =0.94604

#Teste T####
#não pareado –APROPRIADO PARA AMOSTRAS INDEPENDENTES ####
options(scipen = 999, digits = 1)

HOMEM <- subset(BASE_SEN_2022, CD_GENERO == 2)
summary(HOMEM$TOTAL_VOTOS)

MULHER <- subset(BASE_SEN_2022, CD_GENERO == 4)
summary(MULHER$TOTAL_VOTOS)


t.test(MULHER$TOTAL_VOTOS,HOMEM$TOTAL_VOTOS)
# 
# Welch Two Sample t-test
# 
# data:  MULHER$TOTAL_VOTOS and HOMEM$TOTAL_VOTOS
# t = -2, df = 203, p-value = 0.02
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -506729  -35829
# sample estimates:
#   mean of x mean of y 
# 280697    551975 

#não pareado –APROPRIADO PARA AMOSTRAS DEPENDENTES ####

#Criação da base de dados para a realização do teste t
BaseHipSen <- data.frame(
  "Regiões" = c("Norte", "Nordeste", "Centro-Oeste", "Sudeste",
                "Sul", "Argentina"),
  "Can_2020" = c(5, 10, 7, 6, 11, 8), 
  "Can_2050" = c(25, 15, 4, 12, 30, 17)
)
View(BaseHipSen)

#Realização do teste 
t.test(BaseHipSen$Can_2020, BaseHipSen$Can_2050, paired = T)

#saída do teste

# Paired t-test
# 
# data:  BaseHipSen$Can_2020 and BaseHipSen$Can_2050
# t = -2.5908, df = 5, p-value = 0.04879
# alternative hypothesis: true mean 
#difference is not equal to 0
# 95 percent confidence interval:
#   -18.59377288  -0.07289378
# sample estimates:
#   mean difference 
# -9.333333 


#teste-t de Student para uma amostra####

#Primeiro vamos conferir qual é a média na Argentina
#depois excluir esse dado da amostra. 
#Em 2050 é 17 e em 2020 é 8. 

#Agora criamos uma base sem esses dados
BaseMenor <-  BaseHipSen[-6,]

#Rodamos o teste:
t.test(BaseMenor$Can_2050, mu=17)

# One Sample t-test
# 
# data:  BaseMenor$Can_2050
# t = 0.043093, df = 4, p-value = 0.9677
# alternative hypothesis: true mean is not equal to 17
# 95 percent confidence interval:
#   4.314184 30.085816
# sample estimates:
#   mean of x 
# 17.2 