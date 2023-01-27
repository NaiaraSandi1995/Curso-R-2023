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
