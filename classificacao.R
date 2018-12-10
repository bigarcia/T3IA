library('rpart')
library('rpart.plot')
library('caret')
source('metrica.R')

main <- function(){
  data <- read.table('iris.csv', header = TRUE, sep = ",")
  sample <- createDataPartition(data$class, p = 0.75, list = FALSE)
  
  treinamento <- data[sample,]
  teste<- data[-sample,]
  
  arvore = rpart(formula = class ~ sepal_length_in_cm + sepal_width_in_cm + petal_length_in_cm + petal_width_in_cm,
                 data = treinamento, method = "class", control = rpart.control(minsplit = 1), 
                 parms = list(split = "Information"), model = TRUE)
  
  
  # Predição
  predicao <- predict(arvore, teste, type = "class")
  
  # Métricas de precisão e acurácia
  confusionMatrix <- table(predicted=predicao, truth=teste$class)
  acuracia <- accuracy(confusionMatrix) 
  precisao <- precision(confusionMatrix)
  
  
  retorno<- list()
  retorno$arvore <- arvore
  retorno$acuracia <- acuracia
  retorno$precisao <- precisao
  
  return(retorno)
}

resultado <- main()
cat("Classificação\n")
cat("Acurácia: ",resultado$acuracia,"\n" )
cat("Precisão: ",resultado$precisao,"\n\n")
rpart.plot(resultado$arvore)