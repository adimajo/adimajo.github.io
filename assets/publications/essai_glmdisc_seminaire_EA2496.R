# Installation du package
library(devtools)
install_github("adimajo/glmdisc", build_vignettes = TRUE)

# Chargement des données
library(MASS)
Pima.tr

library(mlbench)
data(BreastCancer)
BreastCancer$Id <- NULL

heart.data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",header=FALSE,sep=",",na.strings = '?')
names(heart.data) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg",
                        "thalach","exang", "oldpeak","slope", "ca", "thal", "num")

heart.data$target = heart.data$num>0
heart.data$num = NULL

birthwt
birthwt$bwt <- NULL

# Séparation test train pour BreastCancer

int_train = sample.int(nrow(BreastCancer), 0.7*nrow(BreastCancer))
breast_train = BreastCancer[int_train,]
breast_test = BreastCancer[!(1:nrow(BreastCancer) %in% int_train),]

int_train = sample.int(nrow(heart.data), 0.7*nrow(heart.data))
heart_data_train = heart.data[int_train,]
heart_data_test = heart.data[!(1:nrow(heart.data) %in% int_train),]

int_train = sample.int(nrow(birthwt), 0.7*nrow(birthwt))
birthwt_data_train = birthwt[int_train,]
birthwt_data_test = birthwt[!(1:nrow(birthwt) %in% int_train),]

# Régression logistique linéaire sans interaction

naive_glm_pima = glm(type ~ .,family=binomial(link="logit"), data=Pima.tr)
naive_glm_breast = glm(Class ~ ., family=binomial(link="logit"), data=breast_train)
naive_glm_heart = glm(target ~ ., family=binomial(link="logit"), data=heart_data_train)
naive_glm_birthwt = glm(low ~ ., family=binomial(link="logit"), data=birthwt_data_train)

# Régression logistique linéaire avec toutes les interactions

naive_glm_pima_inter = glm(type ~ .:.,family=binomial(link="logit"), data=Pima.tr)
naive_glm_breast_inter = glm(Class ~ .:., family=binomial(link="logit"), data=breast_train) # très long à calculer !
naive_glm_heart_inter = glm(target ~ .:., family=binomial(link="logit"), data=heart_data_train)
naive_glm_birthwt_inter = glm(low ~ .:., family=binomial(link="logit"), data=birthwt_data_train)

# Régression logistique glmdisc sans interaction

library(glmdisc)

Pima.tr <- sapply(Pima.tr, as.numeric)

glmdisc_pima = glmdisc(predictors = Pima.tr[,-ncol(Pima.tr)], labels = Pima.tr[,ncol(Pima.tr)]-1, validation=FALSE, 
                       test=FALSE, criterion="aic", iter = 100, m_start = 4, inter=FALSE)

breast_train <- as.data.frame(sapply(breast_train, as.factor))

glmdisc_breast = glmdisc(predictors = breast_train[,-ncol(breast_train)], labels = as.numeric(breast_train[,ncol(breast_train)])-1,
                         validation = FALSE, test = FALSE, criterion = "aic", iter = 100, m_start = 4, inter=FALSE)

glmdisc_heart_data = glmdisc(predictors = heart_data_train[,-ncol(heart_data_train)], labels = heart_data_train[,ncol(heart_data_train)]*1, validation=FALSE, 
                       test=FALSE, criterion="aic", iter = 100, m_start = 4, inter=FALSE)

birthwt_data_train <- sapply(birthwt_data_train, as.numeric)

glmdisc_birthwt = glmdisc(predictors = birthwt_data_train[,-1], labels = birthwt_data_train[,1], validation=FALSE, 
                             test=FALSE, criterion="aic", iter = 100, m_start = 4, inter=FALSE)


# Régression logistique glmdisc avec sélection des interactions

colnames(Pima.tr) = paste0("X",1:ncol(Pima.tr))

glmdisc_pima_inter = glmdisc(predictors = Pima.tr[,-ncol(Pima.tr)], labels = Pima.tr[,ncol(Pima.tr)]-1, validation=FALSE, 
                             test=FALSE, criterion="aic", iter = 100, m_start = 4, inter=TRUE)

colnames(breast_train) = paste0("X",1:ncol(breast_train))

glmdisc_breast_inter = glmdisc(predictors = breast_train[,-ncol(breast_train)], labels = as.numeric(breast_train[,ncol(breast_train)])-1,
                               validation = FALSE, test = FALSE, criterion = "aic", iter = 100, m_start = 4, inter=TRUE)

colnames(heart_data_train) = paste0("X",1:ncol(heart_data_train))

glmdisc_heart_data_inter = glmdisc(predictors = heart_data_train[,-ncol(heart_data_train)], labels = heart_data_train[,ncol(heart_data_train)]*1, validation=FALSE, 
                             test=FALSE, criterion="aic", iter = 100, m_start = 4, inter=TRUE)

colnames(birthwt_data_train) = paste0("X",0:(ncol(birthwt_data_train)-1))

glmdisc_birthwt_inter = glmdisc(predictors = birthwt_data_train[,-1], labels = birthwt_data_train[,1], validation=FALSE, 
                          test=FALSE, criterion="aic", iter = 100, m_start = 4, inter=TRUE)


# Comparaison des performances de chaque modèle

## Calcul des probabilités de chaque modèle

naive_glm_pima_pred = predict(naive_glm_pima, Pima.te, type="response")
naive_glm_breast_pred = predict(naive_glm_breast, breast_test, type="response")
naive_glm_heart_data_pred = predict(naive_glm_heart, heart_data_test, type="response")
naive_glm_birthwt_data_pred = predict(naive_glm_birthwt, birthwt_data_test, type="response")

naive_glm_pima_inter_pred = predict(naive_glm_pima_inter, Pima.te, type="response")
naive_glm_breast_inter_pred = predict(naive_glm_breast_inter, breast_test, type="response")
naive_glm_heart_inter_pred = predict(naive_glm_heart_inter, heart_data_test, type="response")
naive_glm_birthwt_inter_pred = predict(naive_glm_birthwt_inter, birthwt_data_test, type="response")

Pima.te <- sapply(Pima.te, as.numeric)
breast_test <- as.data.frame(sapply(breast_test, as.factor))
birthwt_data_test <- sapply(birthwt_data_test, as.numeric)

glmdisc_pima_pred = predict(glmdisc_pima, Pima.te[,-ncol(Pima.te)])
glmdisc_breast_pred = predict(glmdisc_breast, breast_test[,-ncol(breast_test)])
glmdisc_heart_pred = predict(glmdisc_heart_data, heart_data_test[,-ncol(heart_data_test)])
glmdisc_birthwt_pred = predict(glmdisc_birthwt, birthwt_data_test[,-1])

colnames(Pima.te) = paste0("X",1:ncol(Pima.te))
colnames(breast_test) = paste0("X",1:ncol(breast_test))
colnames(heart_data_test) = paste0("X",1:ncol(heart_data_test))
colnames(birthwt_data_test) = paste0("X",0:(ncol(birthwt_data_test)-1))

glmdisc_pima_inter_pred = predict(glmdisc_pima_inter, Pima.te[,-ncol(Pima.te)])
glmdisc_breast_inter_pred = predict(glmdisc_breast_inter, breast_test[,-ncol(breast_test)])
glmdisc_heart_inter_pred = predict(glmdisc_heart_data_inter, heart_data_test[,-ncol(heart_data_test)])
glmdisc_birthwt_inter_pred = predict(glmdisc_birthwt_inter, birthwt_data_test[,-1])

## Calcul du Gini de chaque modèle

naive_glm_pima_gini = normalizedGini(Pima.te[,ncol(Pima.te)]-1,naive_glm_pima_pred)
naive_glm_breast_gini = normalizedGini(as.numeric(breast_test[,ncol(breast_test)])-1,naive_glm_breast_pred)
naive_glm_heart_gini = normalizedGini(heart_data_test[,ncol(heart_data_test)]*1,naive_glm_heart_data_pred)
naive_glm_birthwt_gini = normalizedGini(birthwt_data_test[,1],naive_glm_birthwt_data_pred)

naive_glm_pima_inter_gini = normalizedGini(Pima.te[,ncol(Pima.te)]-1,naive_glm_pima_inter_pred)
naive_glm_breast_inter_gini = normalizedGini(as.numeric(breast_test[,ncol(breast_test)])-1,naive_glm_breast_inter_pred)
naive_glm_heart_inter_gini = normalizedGini(heart_data_test[,ncol(heart_data_test)]*1,naive_glm_heart_inter_pred)
naive_glm_birthwt_inter_gini = normalizedGini(birthwt_data_test[,1],naive_glm_birthwt_inter_pred)

glmdisc_pima_gini = normalizedGini(Pima.te[,ncol(Pima.te)]-1,glmdisc_pima_pred)
glmdisc_breast_gini = normalizedGini(as.numeric(breast_test[,ncol(breast_test)])-1,glmdisc_breast_pred)
glmdisc_heart_gini = normalizedGini(heart_data_test[,ncol(heart_data_test)]*1,glmdisc_heart_pred)
glmdisc_birthwt_gini = normalizedGini(birthwt_data_test[,1],glmdisc_birthwt_pred)

glmdisc_pima_inter_gini = normalizedGini(Pima.te[,ncol(Pima.te)]-1,glmdisc_pima_inter_pred)
glmdisc_breast_inter_gini = normalizedGini(as.numeric(breast_test[,ncol(breast_test)])-1,glmdisc_breast_inter_pred)
glmdisc_heart_inter_gini = normalizedGini(heart_data_test[,ncol(heart_data_test)]*1,glmdisc_heart_inter_pred)
glmdisc_birthwt_inter_gini = normalizedGini(birthwt_data_test[,1],glmdisc_birthwt_inter_pred)

## Tableau des indices de Gini

xtable(
rbind(
c(naive_glm_pima_gini,
naive_glm_breast_gini,
naive_glm_heart_gini,
naive_glm_birthwt_gini)
,
c(naive_glm_pima_inter_gini,
naive_glm_breast_inter_gini,
naive_glm_heart_inter_gini,
naive_glm_birthwt_inter_gini)
,
c(glmdisc_pima_gini,
glmdisc_breast_gini,
glmdisc_heart_gini,
glmdisc_birthwt_gini)
,
c(glmdisc_pima_inter_gini,
glmdisc_breast_inter_gini,
glmdisc_heart_inter_gini,
glmdisc_birthwt_inter_gini)
)
)