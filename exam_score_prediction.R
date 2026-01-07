#Análise de dados sobre fatores que contribuem para o desempenho do exame do aluno

#Dataset https://www.kaggle.com/datasets/kundanbedmutha/exam-score-prediction-dataset


# Definição do diretório  ----------------------------------------------------------------------------

setwd('D:/Documentos/Dados R')
getwd()


# Instalação dos pacotes -----------------------------------------------------------------------------
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("effectsize")
install.packages("psych")
install.packages("Hmisc")
install.packages("pastecs")
install.packages("DataExplorer")


# Carregar os pacotes --------------------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(ggplot2)
library(effectsize)
library(psych)
library(Hmisc)
library(pastecs)


# Carrega o dados ------------------------------------------------------------------------------------

df = read.table('Exam_Score_Prediction.csv', header = TRUE, sep = ',')





############ ENTENDIMENTO DOS DADOS ############

#View(dataset)  ---------------------------------------------------------------------------------------

str(df)
head(df)
glimpse(df)
names(df)


# Estatística Descritiva com a função summary() -------------------------------------------------------

summary(df$idade)
summary(df$horas_de_estudo)
summary(df$frequencia_nas_aulas)
summary(df$horas_de_sono)
summary(df$nota_da_prova)

# Estatística descritiva com a função describe() -------------------------------------------------------

describe(df)
describe(df$idade)
describe(df$horas_de_estudo)
describe(df$frequencia_nas_aulas)
describe(df$horas_de_sono)
describe(df$nota_da_prova)



############ TRATAMENTO DE DADOS ############


# Renomeando Colunas ------------------------------------------------------------------------------------

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










#### Tipos de Dados ####


df %>%
  summarise(
    media_nota = mean(nota_da_prova, na.rm = TRUE),
    max_nota   = max(nota_da_prova, na.rm = TRUE),
    min_nota   = min(nota_da_prova, na.rm = TRUE)
  )

df %>%
  group_by(dificuldade_da_prova) %>%
  summarise(
    media_nota = mean(nota_da_prova, na.rm = TRUE),
    max_nota   = max(nota_da_prova, na.rm = TRUE),
    min_nota   = min(nota_da_prova, na.rm = TRUE)
  )




############## LEVANTAMENTO DO PERFIL DO ESTUDANTE ##############

# Idade

hist(df$idade)

ggplot(df, aes(x =df$idade.Length , y =df$idade)) +
  geom_boxplot()



# Criação de coluna "faixa_etaria" em categorias de idade
#Transformação de variáveis contínuas em variável analítica interpretável

df <- df %>%
  mutate(
    faixa_etaria = case_when(
      idade < 18 ~ "Menor de 18",
      idade >= 18 & idade <= 22 ~ "18–22",
      idade >= 23 & idade <= 27 ~ "23–27",
      idade >= 28 ~ "28+"
    )
  )

# Estatística descritiva por faixa etária

resumo_faixa_etaria <- df %>%
  group_by(faixa_etaria) %>%
  summarise(
    media_nota = mean(nota_da_prova, na.rm = TRUE),
    mediana_nota = median(nota_da_prova, na.rm = TRUE),
    desvio_padrao = sd(nota_da_prova, na.rm = TRUE),
    n = n()
  )

resumo_faixa_etaria


# Plotagem do gráfico da relação entre categorias de faixa etária e nota da prova

ggplot(df, aes(x = faixa_etaria, y = nota_da_prova)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Distribuição da Nota da Prova por Faixa Etária",
    x = "Faixa Etária",
    y = "Nota da Prova"
  ) +
  theme_minimal()


#Cálculo da média, mediana e desvio padrão em cada categoria de gênero
resumo_genero <- df %>%
  group_by(genero) %>%
  summarise(
    media_nota = mean(nota_da_prova, na.rm = TRUE),
    mediana_nota = median(nota_da_prova, na.rm = TRUE),
    desvio_padrao = sd(nota_da_prova, na.rm = TRUE),
    n = n()
  )

resumo_genero



#Plotagem de gráfico com a distribuição de notas da prova por gênero
ggplot(df, aes(x = genero, y = nota_da_prova)) +
  geom_boxplot(fill = "darkorange", alpha = 0.7) +
  labs(
    title = "Distribuição da Nota da Prova por Gênero",
    x = "Gênero",
    y = "Nota da Prova"
  ) +
  theme_minimal()


############## CONTEXTO ACADÊMICO ##############

resumo_curso <- df %>%
  group_by(curso) %>%
  summarise(
    media_nota = mean(nota_da_prova, na.rm = TRUE),
    mediana_nota = median(nota_da_prova, na.rm = TRUE),
    desvio_padrao = sd(nota_da_prova, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(desc(media_nota))

resumo_curso


ggplot(resumo_curso, aes(x = reorder(curso, media_nota), y = media_nota)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Média da Nota da Prova por Curso",
    x = "Curso",
    y = "Nota Média"
  ) +
  theme_minimal()


