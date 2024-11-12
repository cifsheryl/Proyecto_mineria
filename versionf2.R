install.packages("arules")
library(arules)
install.packages("readxl")



library(readxl)
file_path <- "C:\\Users\\Mario CIfuentes\\Downloads\\personas.xlsx"
data <- read_excel(file_path)
View(data)


data$P03A02[data$P03A02== "NA"] <- 0

data$P03A06[data$P03A06== "NA"] <- 0
data$P04A05A[data$P04A05A== "NA"] <- 0
data$P06C01data$P06C01== "NA"] <- 0

data$P05B11I[is.na(data$P05B11I)] <- 0

data$P05B11I[data$P05B11I == 0] <- NA
data$P04C01A[data$P04C01A == 0] <- NA
data$P04C01B[data$P04C01B == 0] <- NA
data$P04B01[data$P04B01 == 0] <- NA


View(data)

data2 <- data [, c("P03A02", "P03A06", "P04A05A", "P06C01")]

View(data_apriori)

data$P05B11I <- factor(data$P05B11I, levels = c(1, 2), labels = c(1, 0))
data$P04C01A <- factor(data$P04C01A, levels = c(1, 2), labels = c(1, 0))
data$P04C01B <- factor(data$P04C01B, levels = c(1, 2), labels = c(1, 0))
data$P04B01 <- factor(data$P04B01, levels = c(1, 2), labels = c(1, 0))


data_binaria <- data.frame(
  
  # Convertir 'Sexo' en variables binarias (Hombre = 1, Mujer = 2)
  Sexo_Hombre = as.factor(ifelse(data_apriori$P03A02 == 1, 1, 0)),
  Sexo_Mujer = as.factor(ifelse(data_apriori$P03A02 == 2, 1, 0)),
  
  # Convertir 'Pueblo de pertenencia' en variables binarias
  Pueblo_Xinka = as.factor(ifelse(data_apriori$P03A06 == 1, 1, 0)),
  Pueblo_Garífuna = as.factor(ifelse(data_apriori$P03A06 == 2, 1, 0)),
  Pueblo_Ladino = as.factor(ifelse(data_apriori$P03A06 == 3, 1, 0)),
  Pueblo_Afrodescendiente = as.factor(ifelse(data_apriori$P03A06 == 4, 1, 0)),
  Pueblo_Extranjero = as.factor(ifelse(data_apriori$P03A06 == 5, 1, 0)),
  Pueblo_Maya = as.factor(ifelse(data_apriori$P03A06 == 6, 1, 0)),
  
  # Convertir 'Nivel de educación' en variables binarias
  Nivel_Ninguno = as.factor(ifelse(data_apriori$P04A05A == 0, 1, 0)),
  Nivel_Preprimaria = as.factor(ifelse(data_apriori$P04A05A == 1, 1, 0)),
  Nivel_Primaria = as.factor(ifelse(data_apriori$P04A05A == 2, 1, 0)),
  Nivel_Básico = as.factor(ifelse(data_apriori$P04A05A == 3, 1, 0)),
  Nivel_Diversificado = as.factor(ifelse(data_apriori$P04A05A == 4, 1, 0)),
  Nivel_Superior = as.factor(ifelse(data_apriori$P04A05A == 5, 1, 0)),
  Nivel_Maestría = as.factor(ifelse(data_apriori$P04A05A == 6, 1, 0)),
  Nivel_Doctorado = as.factor(ifelse(data_apriori$P04A05A == 7, 1, 0)),
  
  # Convertir 'Recibió remesas' en variables binarias
  Recibio_Remesas_Si = as.factor(ifelse(data_apriori$P06C01 == 1, 1, 0)),
  Recibio_Remesas_No = as.factor(ifelse(data_apriori$P06C01 == 2, 1, 0))
)


head(data_binaria)

data_transacciones <- as(data_binaria, "transactions")


summary(data_transacciones)

reglas <- apriori(data_transacciones, 
                  parameter = list(support = 0.05, confidence = 0.5, maxlen = 20),
                  appearance = list(default = "lhs", rhs = c("Sexo_Hombre=0", "Nivel_Primaria=0", "Recibio_Remesas_No=1")))

rules_filtered <- subset(reglas, subset = support > 0.1 & confidence > 0.7)
inspect(head(rules_filtered))




data2$P03A02 <- factor(data2$P03A02, levels = c(1, 2), labels = c("Hombre", "Mujer"))
data2$P03A06 <- factor(data2$P03A06, levels = 1:6, labels = c("Xinka", "Garífuna", "Ladino", "Afrodescendiente", "Extranjero", "Maya"))
data2$P04A05A <- factor(data2$P04A05A, levels = 0:7, labels = c("Ninguno","Preprimaria", "Primaria", "Básico", "Diversificado", "Superior", "Maestría", "Doctorado"))
data2$P06C01 <- factor(data2$P06C01, levels = c(1, 2), labels = c("No", "Sí"))

# Verificar los primeros datos
head(data2)

data_apriori$P04A05A[is.na(data_apriori$P04A05A)] <- "Otro"

View(data_apriori)
str(data_apriori)

data_transacciones <- as(data_apriori, "transactions")

# Ver el resumen de las transacciones
summary(data_transacciones)



data$Sexo <- factor(data$P03A02, levels = c(1, 2), labels = c("Hombre", "Mujer"))
data$Pueblo <- factor(data$P03A06, levels = 1:6, labels = c("Xinka", "Garífuna", "Ladino", "Afrodescendiente", "Extranjero", "Maya"))
data$Nivel_educativo <- factor(data$P04A05A, levels = 0:7, labels = c("Ninguno", "Preprimaria" ,"Primaria", "Básico", "Diversificado", "Superior", "Maestría", "Doctorado", "Otro"))
data$Recibio_remesas <- factor(data$P06C01, levels = c(1, 2), labels = c("Sí", "No"))


