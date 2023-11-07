# --------------------FORWARD SELECTION----------------------------
#paso 1 construir el analisis univariado
datosC = read.csv("~/TONI/R/ML/bcdr_d01_features.csv", stringsAsFactors=TRUE)
datosC$classification = ifelse(datosC$classification == " Malign ", 1, 0)
#analisis Univariado
resultados = data.frame(Feature = '', AUC = 0)
#Probar una variable
for (i in c(16:ncol(mini_dataset)-1)) {
  
  mini_dataset = datosC[, c(i, ncol(datosC))]
  
  #construir un modelo
  
  modeloX = glm("classification ~ .",
                data = mini_dataset,
                family = "binomial"
                )
  
  #Generar la tabla para obtener el desempeno en la metrica AUC
  prediccionesX = predict(modeloX, newdata = mini_dataset, type = "response")
  
  tablaX = data.frame(Deberia = mini_dataset$classification, Predicciones = prediccionesX)
  
  library(pROC)
  curva = roc(
    tablaX$Deberia,
    tablaX$Predicciones,
    levels = c(0,1),
    plot = F,
    ci = T,
    smooth = F,
    direction = 'auto',
    col = 'blue',
    main = "Curva ROC (BCDR)"
  )
  
  resultados = rbind(resultados, data.frame(Feature = colnames(mini_dataset)[1],
                                            AUC=curva$auc))
  print(paste(i, " - ", curva$auc))
}

#quitamos el 1er renglon
resultados = resultados[-1,]

# Paso 2 ordenamos
#order(resultados$AUC)
#resultados = resultados[order(resultados$AUC)] asc
resultados = resultados[order(-resultados$AUC),]


#Paso 3 generar una matriz de correlacion
datosC_correlacion = datosC
matriz_correlacion = cor(datosC_correlacion[, c(16: (ncol(datosC_correlacion)-1))])
# cuales tiene una alta correlacion
nombres = which(matriz_correlacion > 0.95 & matriz_correlacion < 1.0, arr.ind = T)
rownames(nombres)

#
indice = match("t_svarh", colnames(datosC_correlacion))
#por cada variable si la correlacion es mayor a 0.95 la quitamos
datosC_correlacion = datosC_correlacion[,-indice]


#una vez que esta limpio el dataset, dejamos en el dataset de resultaos
#solo las variables que estan presentes en el dataset NO Correlacionado
#which(resultados$Feature, colnames(datosC_correlacion))
indices = is.na(match(resultados$Feature, colnames(datosC_correlacion)))
resultados_limpios = resultados[indices==F,]
#datos_correlacionados = matriz_correlacion[matriz_correlacion > 0.95]
#datos_correlacionados = datos_correlacionados[datos_correlacionados < 1]

#buscamos los valores altamente correlacionados
#match(0.9896933, matriz_correlacion)
