library('rpart')
library('rpart.plot')
library('caret')
source('metrica.R')

regressao <- function() {
  data <- read.table('forestfires.csv', header = TRUE, sep = ",")
  sample <- createDataPartition(data$area, p = 0.75, list = FALSE)
  
  treinamento <- data[sample,]
  teste <- data[-sample,]
  
  #Árvore de regressão
  arvore <- rpart(formula = area ~ X + Y + month + day + FFMC + DMC + DC + ISI + temp + RH + wind + rain, 
                 data = treinamento, method = "anova", model = TRUE)
  
  # Predição
  predicao <- predict(arvore, teste, type = "vector")
  
  # Erro Médio Quadrático
  error <- mse(teste$area, predicao)
  

  retorno <- list()
  retorno$arvore <- arvore 
  retorno$mse <- error
  
  return(retorno)
}

resultado <- regressao()
cat("Regressão\n")
cat("Erro Médio Quadrático: ",resultado$mse,"\n")
rpart.plot(resultado$arvore)