install.packages("arules")
library(arules)
install.packages("readxl")

library(readxl)
file_path <- "C:\\Users\\Mario CIfuentes\\Downloads\\personas.xlsx"
data <- read_excel(file_path)
View(data)

data2 <- data [, c("P03A02", "P03A06", "P04A05A", "P06C01")]
View(data2)


data2[] <- lapply(data2, factor)

str(data2)

transactions <- as(data2, "transactions")
summary(transactions)


rules <- apriori(transactions, parameter = list(supp = 0.1, conf = 0.8))
inspect(head(rules))
-------------------------------------------------------------------------------------------------
  # Librerías necesarias
library(arules)
library(arulesViz)
library(readxl)

# Cargar datos
file_path <- "C:\\Users\\Mario CIfuentes\\Downloads\\personas.xlsx"
data <- read_excel(file_path)

# Limpieza inicial
data[is.na(data)] <- 0

# Filtrar columnas con más de un valor único
data_clean <- data[, sapply(data, function(x) length(unique(x)) > 1)]

# Discretización de la columna P03A03
breaks <- c(0, 20, 40, 60, 80, Inf)
labels <- c("0-20", "21-40", "41-60", "61-80", "80+")
if ("P03A03" %in% names(data_clean)) {
  data_clean$P03A03 <- cut(data_clean$P03A03, breaks = breaks, labels = labels, include.lowest = TRUE)
}

# Convertir a transacciones
data_trans <- as(data_clean, "transactions")

# Aplicar el algoritmo Apriori
reglas <- apriori(data_trans, parameter = list(supp = 0.05, conf = 0.3, maxlen = 3))

# Visualizar las primeras reglas
inspect(head(reglas, 10))

# Visualización gráfica
plot(reglas, method = "graph", control = list(type = "items"))
----------------------------------------------------------------------------------
file_path <- "C:/Users/Mario CIfuentes/Downloads/personas.xlsx"
data <- read_excel(file_path)

# Verificar los datos cargados
View(data)
summary(data) 


# Reemplazar NA con 0
data[is.na(data)] <- 0

# Convertir variables categóricas a factores
data[] <- lapply(data, function(x) {
  if (is.character(x)) x <- as.factor(x)
  return(x)
})

# Convertir a transacciones
data_trans <- as(data, "transactions")

# Verificar las transacciones
summary(data_trans)
inspect(data_trans[1:5])  # Ver las primeras 5 transacciones



# Aplicar FP-Growth para encontrar patrones frecuentes
patrones_frecuentes <- eclat(data_trans, parameter = list(supp = 0.05, maxlen = 3))

# Inspeccionar patrones frecuentes
inspect(head(sort(patrones_frecuentes, by = "support"), 10))


# Generar reglas de asociación a partir de los patrones frecuentes
reglas <- ruleInduction(patrones_frecuentes, data_trans, confidence = 0.3)

# Inspeccionar las reglas
inspect(head(sort(reglas, by = "lift"), 10))


# Visualización básica de reglas
plot(reglas)

# Visualización agrupada
plot(reglas, method = "grouped")

# Gráfica de red (network graph)
plot(reglas, method = "graph", control = list(type = "items"))


--------------------------------------------------------------------------------------------------------------

install.packages("ggplot2")      
install.packages("factoextra")  
install.packages("cluster")      

library(ggplot2)
library(factoextra)
library(cluster)

library(readxl)
file_path <- "C:/Users/Mario CIfuentes/Downloads/personas.xlsx"
data <- read_excel(file_path)


data <- data[, !(names(data) %in% c("dominio", "id", "hogar_num"))]


data[] <- lapply(data, function(x) {
  if (is.factor(x) || is.character(x)) {
    x <- as.numeric(as.factor(x))
  }
  return(x)
})


data[is.na(data)] <- 0 

data_scaled <- scale(data)


fviz_nbclust(data_scaled, kmeans, method = "silhouette") +
  labs(title = "Método de la silueta para determinar K", x = "Número de clústeres (K)", y = "Ancho promedio de silueta")


k <- 3
set.seed(123) 
kmeans_result <- kmeans(data_scaled, centers = k, nstart = 25)

print(kmeans_result$centers)  
print(kmeans_result$cluster)  


fviz_cluster(kmeans_result, data = data_scaled, geom = "point", ellipse.type = "euclid") +
  labs(title = "Visualización de Clústeres (K-Means)", x = "Componente Principal 1", y = "Componente Principal 2")


pairs(data_scaled, col = kmeans_result$cluster, main = "Gráfico de Pares por Clúster")


data$cluster <- kmeans_result$cluster

aggregate(. ~ cluster, data = data, mean)


ggplot(data, aes(x = as.factor(cluster), y = P03A02, fill = as.factor(cluster))) +
  geom_boxplot() +
  labs(title = "Distribución de Género por Clúster", x = "Clúster", y = "P03A02 (Género)") +
  theme_minimal()




