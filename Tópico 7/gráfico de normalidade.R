
library(ggplot2)


media <- 0      # Média
desvio <- 1     # Desvio padrão


x <- seq(-3, 3, length=1000)


y <- dnorm(x, mean=media, sd=desvio)


grafico <- ggplot() +
  geom_line(aes(x, y), color="blue", size=1) +
  geom_vline(aes(xintercept=media), color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=media+desvio), color="green", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=media-desvio), color="green", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=media), color="red", size=1) +
  geom_vline(aes(xintercept=media+desvio), color="green", size=1) +
  geom_vline(aes(xintercept=media-desvio), color="green", size=1) +
  annotate("text", x = media+0.5, y = 0.2, label = "Média", color = "red") +
  annotate("text", x = media+desvio+0.5, y = 0.1, label = "Desvio Padrão", color = "green") +
  annotate("text", x = media-desvio-1.5, y = 0.1, label = "Desvio Padrão", color = "green") +
  xlab("Valores") +
  ylab("Densidade") +
  ggtitle("Distribuição Normal") +
  theme_minimal()


print(grafico)
