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

# Ejecutar el algoritmo Apriori para encontrar reglas de asociación
reglas <- apriori(data, parameter = list(supp = 0.2, conf = 0.5, target = "rules"))

# Supongamos que tienes un data.frame con valores binarios (0/1)
# Convertirlo en un objeto de tipo 'transactions'
data_trans <- as(data, "transactions")
