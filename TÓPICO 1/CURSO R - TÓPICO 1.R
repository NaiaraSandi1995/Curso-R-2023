#Curso 2023####

#TÓPICO 1 - INTRODUÇÃO AO R#### 

#1ºtítulo####
##2ºtítulo####
###3ºtítulo####


#Atalha para abrir o diretório de trabalho
#ctrl + shift + h

#Atalho para salvar ctrl+s


##Criar data frame####

#Criação dos vetores####
Id <-  c(1:4)
Nomes <- c("Ana", "Gilberto", "Rodrigo", "Marcela")
Peso <- c(75.6, 99, 62.8, 102)
Idades <- c(25, 18, 44, 23)
Escolaridade <- c("Graduação", "Mestrado", "Primário", "Graduação")
Exerc_Recomend <- c("Natação", "Pilates", "Musculação", "Corrida")
Comidas_preferidas <- c("Chocolate", "Sorvete", "Milho", "Pão")

#Criação do data frame
Ficha_Pacientes <- data.frame(Id, Nomes, Peso, 
                              Idades, Escolaridade, 
                              Exerc_Recomend, Comidas_preferidas)
#Função para ver o data frame
View(Ficha_Pacientes)

#Identificar o diretório de trabalho####
#Função para identificar o caminho em que o arquivo de script será salvo
getwd()
#[1] "C:/Users/nayar/OneDrive/8. AMBIENTE DE PROGRAMAÇÃO R/1. CURSO 2023"

#Função manual para selecionar o diretório de trabalho
setwd("C:/Users/nayar/OneDrive/8. AMBIENTE DE PROGRAMAÇÃO R/1. CURSO 2023")

#salvar#####
#Função para salvar o data frame
save(Ficha_Pacientes, file = "Ficha_Pacientes.RData")
