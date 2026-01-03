library(readxl)

arquivo <- file.choose()

dados <- read.csv(arquivo)

head(dados)