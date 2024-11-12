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
