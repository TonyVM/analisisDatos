# Cargar los datos desde un archivo CSV.
datos <- read.csv("./bcdr_d01_feature.csv")

# Inicializar un dataframe para almacenar los resultados del análisis univariado,
# que incluye el nombre de la característica, el AUC y el valor p.
resultados = data.frame(Caracteristica='', AUC=0, pValor=1)

# Cargar librerías necesarias para análisis estadístico y curva ROC.
library(pROC)
library(stats)

# Bucle para analizar cada variable de manera univariada, comenzando desde la columna 16 hasta la penúltima.
for(i in c(16:ncol(datos)-1)){
  # Crear un mini_dataset con la variable actual y la variable de respuesta.
  mini_dataset = datos[,c(i,ncol(datos))]
  
  # Construir un modelo de regresión logística utilizando la variable actual.
  modelox = glm(classification ~ ., data=mini_dataset, family = "binomial")
  
  # Generar predicciones del modelo para calcular la curva ROC y el AUC.
  prediccionesx = predict(modelox, newdata=mini_dataset, type="response")
  curva <- roc(mini_dataset$classification, prediccionesx)
  
  # Calcular el valor p del modelo mediante un test de Chi-cuadrado.
  p_valor_actual = anova(modelox, test="Chisq")[1, "Pr(>Chi)"]
  
  # Almacenar el nombre de la característica, el AUC y el valor p en el dataframe de resultados.
  resultados = rbind(resultados, data.frame(Caracteristica=colnames(mini_dataset)[1], AUC=curva$auc, pValor=p_valor_actual))
  print(paste(i, " - AUC:", curva$auc, "p-Valor:", p_valor_actual))
}

# Eliminar la primera fila del dataframe de resultados que fue inicializada en blanco.
resultados <- resultados[-1,] 

# Ordenar los resultados primero por AUC de forma descendente y luego por valor p de forma ascendente.
resultados <- resultados[order(-resultados$AUC, resultados$pValor),]


# Iniciar el proceso de forward selection.
variables_seleccionadas <- c() # Lista para almacenar las variables seleccionadas.
metrica_anterior <- 0 # Variable para comparar el AUC anterior con el AUC actual.
aucs <- c() # Vector para almacenar el AUC de cada modelo.

# Bucle para iterar sobre las variables ordenadas por su desempeño univariado.
for(var in resultados$Caracteristica){
  # Construir la fórmula del modelo incluyendo las variables seleccionadas más la nueva variable.
  formula <- as.formula(paste("classification ~", paste(variables_seleccionadas, collapse="+"), "+", var))
  modelo <- glm(formula, data=datos, family="binomial")
  
  # Calcular el AUC del modelo actualizado.
  predicciones <- predict(modelo, newdata=datos, type="response")
  auc_nuevo <- roc(datos$classification, predicciones)$auc
  
  # Almacenar el AUC del modelo actual en el vector de AUCs.
  aucs <- c(aucs, auc_nuevo)
  
  # Si el AUC mejora con la nueva variable, se añade a la lista de variables seleccionadas.
  if(auc_nuevo > metrica_anterior){
    variables_seleccionadas <- c(variables_seleccionadas, var)
    metrica_anterior <- auc_nuevo
  }
  
  # Si alcanzamos un AUC de 1, detenemos el proceso ya que no se puede mejorar más.
  if(metrica_anterior == 1){
    break
  }
}

# Imprimir las variables finales seleccionadas para el modelo.
print(variables_seleccionadas)

# Graficar el AUC de cada modelo.
plot(aucs, type = "b", pch = 19, xlab = "Número de Variables", ylab = "AUC", main = "AUC de Modelos en Selección Hacia Adelante")