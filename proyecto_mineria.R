data<- read.csv('C:\\Users\\Mario CIfuentes\\Downloads\\personas')
View(data)


file_path <- "C:\\Users\\Mario CIfuentes\\Downloads\\personas.xlsx"
data <- read.csv(file_path)


install.packages("readxl")
library(readxl)

file_path <- "C:\\Users\\Mario CIfuentes\\Downloads\\personas.xlsx"
data <- read_excel(file_path)
View(data)


install.packages("arules")
install.packages("arulesViz")  # Para la visualización de reglas
library(arules)
library(arulesViz)
install.packages("genero")
library(genero)


data2<- read.csv('C:\\Users\\Mario CIfuentes\\Downloads\\personass.csv')
View(data2)

file_path <- "C:\\Users\\Mario CIfuentes\\Downloads\\personas.xlsx"
data <- read_excel(file_path)
View(data)


data_clean <- data
data_clean[is.na(data_clean)] <- 0

data_clean <- data
data_clean[is.na(data_clean)] <- 0


data_clean[] <- lapply(data_clean, function(x) {
  if(is.factor(x)) {
    x <- as.character(x)  
  }
  x[is.na(x)] <- 0 
  return(x)
})


summary(data_clean)


sum(is.na(data_clean))  

View (data)
     

# Asegúrate de tener una copia de tu dataframe
data_clean <- data

# Reemplazar NA por 0 en todas las columnas, incluyendo factores y caracteres
data_clean[] <- lapply(data_clean, function(x) {
  if(is.factor(x)) {
    # Convertir factores a caracteres antes de reemplazar NA
    x <- as.character(x)
  }
  x[is.na(x)] <- 0  # Reemplazar NA por 0
  return(x)
})

# Verificar si todavía quedan NA
sum(is.na(data_clean))  # Debería devolver 0 si no hay NA

# Ver las primeras filas para confirmar que se reemplazaron los NA
head(data_clean)

data

View(data)

data <- ifelse(is.na(data), 0, data)


datareglas <- apriori(data_clean, parameter = list(supp = 0.01, conf = 0.3))

data_clean <- data[ , !(names(data) %in% c("P03A13"))]

# Filtrar las columnas con más de un valor único
valid_columns <- sapply(data, function(x) length(unique(x)) > 1)

# Aplicar discretización solo a las columnas válidas
data_valid <- data[, valid_columns]


data_clean$pet[is.na(data_clean$pet)] <- "0"
data_clean$pea[is.na(data_clean$pea)] <- "0"
data_clean$Ocupados[is.na(data_clean$Ocupados)] <- "0"
data_clean$Desocupados[is.na(data_clean$Desocupados)] <- "0"
data_clean$Subvisibles[is.na(data_clean$Subvisibles)] <- "0"
data_clean$Inactivos[is.na(data_clean$Inactivos)] <- "0"


inspect(datareglas[0:100])

# Ahora puedes aplicar la discretización a data_valid
library(arules)
data_discretized <- discretizeDF(data_valid)


data_priori <- data[, c("dominio","areag","factor","upm","hogar_num","id","P03A02", "P03A03", "P03A05", "P03A06", "P03A08", "P03A09A", "P03A09B", "P03A09C", "P03A09D", "P03A09E", "P03A09F","P03A10","P03A11")]

reglas <- apriori(data_priori, parameter = list(supp = 0.05, conf = 0.3, maxlen = 3))
exists("datarules")
warnings()
# Filtrar columnas con más de un valor único (eliminando las columnas con valores constantes)
data_priori_clean <- data_priori[, sapply(data_priori, function(x) length(unique(x)) > 1)]

# Tomar una muestra aleatoria de 5000 transacciones (ajusta el número según sea necesario)
set.seed(123)  # Para reproducibilidad
data_priori_sample <- data_priori[sample(1:nrow(data_priori), 5000), ]

data_priori_clean[is.na(data_priori_clean)] <- 0

inspect(datarules [1:10])
reglas <- apriori(data_priori_sample, parameter = list(supp = 0.05, conf = 0.3, maxlen = 2))




data$P03A03 <- discretize(data_priori_sample$P03A03, method = "interval", categories = 5, labels = c("0-20", "21-40", "41-60", "61-80", "80+"))

# Ver los primeros registros
head(data$P03A03)

breaks <- c(0, 20, 40, 60, 80, Inf)  # Define breaks: 0-20, 21-40, 41-60, 61-80, 80+
labels <- c("0-20", "21-40", "41-60", "61-80", "80+")  # Assign labels to intervals

# Apply the discretization to 'P03A03'
data_priori_sample$P03A03 <- cut(data_priori_sample$P03A03, breaks = breaks, labels = labels, include.lowest = TRUE)

# View the results
head(data_priori_sample$P03A03)


# Convert other necessary columns to factors (if they are not already)
data_priori_sample$P03A03 <- as.factor(data_priori_sample$P03A03)

# Run Apriori algorithm on the discretized data
reglas <- apriori(data_priori_sample, parameter = list(supp = 0.05, conf = 0.3, maxlen = 3))

# Inspect the first 10 rules
inspect(head(reglas, 10))



inspect(head(reglas, 10))
---------------------------------------------------------------------------------
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




