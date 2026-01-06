#Análise de dados sobre fatores que contribuem para o desempenho do exame do aluno

#Dataset https://www.kaggle.com/datasets/kundanbedmutha/exam-score-prediction-dataset


# Define o diretório
setwd('D:/Documentos/Dados R')
getwd()


# Instala os pacotes
install.packages("dplyr")
install.packages("ggplot2")

# Carrega os pacotes
library(dplyr)
library(ggplot2)

# Carrega o dataset
df = read.table('Exam_Score_Prediction.csv', header = TRUE, sep = ',')


#View(dataset)
str(df)

head(df)


#### Pré Processamento ####

colnames(df)
ncol(df)

df <- df %>%
  rename(
  id_do_estudante="student_id",
  idade="age",
  genero="gender",
  curso="course",
  horas_de_estudo="study_hours",
  frequencia_nas_aulas="class_attendance",
  acesso_a_internet="internet_access",
  horas_de_sono="sleep_hours",
  qualidade_de_sono="sleep_quality",
  metodo_de_estudo="study_method",
  avaliação_da_infraestrutura="facility_rating",
  dificuldade_da_prova="exam_difficulty",
  nota_da_prova="exam_score"
)

head(df)


# Entendendo a distribuição de idades dos alunos na pesquisa
summary(df$idade)

ggplot(df, aes(x =df$idade.Length , y =df$idade)) +
  geom_boxplot()



# Entendendo a distribuição de horas de estudo dos alunos na pesquisa
summary(df$horas_de_estudo)

ggplot(df, aes(x =df$horas_de_estudo.Length , y =df$horas_de_estudo)) +
  geom_boxplot()


# Entendendo a distribuição de horas de sono dos alunos na pesquisa

ggplot(df, aes(x =df$horas_de_sono.Length , y =df$horas_de_sono)) +
  geom_boxplot()