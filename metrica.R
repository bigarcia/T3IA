# acurácia = soma(true positive + true negative)/soma(todos)
accuracy <- function(confusionMatrix) {
  return(sum(diag(confusionMatrix))/sum(confusionMatrix))
}

# precisão = soma(true positive)/soma(true positive + false positive)
precision <- function(confusionMatrix) {
  return(sum(confusionMatrix[2,2])/sum(confusionMatrix[2,]))
}

# erro médio quadrático = media(dados_reais - dados_preditos)**2
mse <- function(real, predicted) {
  return(mean(real-predicted)^2)
} 