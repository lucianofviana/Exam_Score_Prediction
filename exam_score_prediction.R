#Análise de dados sobre fatores que contribuem para o desempenho do exame do aluno

#Dataset https://www.kaggle.com/datasets/kundanbedmutha/exam-score-prediction-dataset


setwd('D:/Documentos/Dados R')
getwd()


# Carrega o dataset
dataset = read.table('Exam_Score_Prediction.csv', header = TRUE, sep = ',')

#View(dataset)
str(dataset)

head(dataset)

