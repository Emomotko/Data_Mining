########################################################
### PROJEKT KONCOWY - DATA MINING
#######################################################
### AUTORZY: EMILIA MOMTOKO, MARTYNA ŚPIEWAK
########################################################
### TEMAT
########################################################

########################################################
### DANE
########################################################

# zbior treningowy
load("Data//Train.rda")
# zbior testowy
load("Data//Test.rda")
# caly zbior: CNN
load("Data//cnn.rda")

########################################################
### PAKIETY
########################################################
library(e1071)
library(rpart)
library(ROCR)
library(class)
library(ipred)
library(randomForest)
library(klaR)

########################################################
### FUNKCJE POMOCNICZE
########################################################

procent <- function(df) sum(diag(df))/sum(df)

dokladnosc <- function(df) sum(diag(df))/sum(df)

czulosc <- function(df) df[2,2]/sum(df[2,])

specyficznosc <- function(df) df[1,1]/sum(df[1,])

precyzja <- function(df) df[2,2]/sum(df[,2])

auc <- function(Pred){
  perf_AUC <- performance(Pred, "auc")
  unlist(slot(perf_AUC, "y.values"))
}

kroswalidacja <- function(dane, model_typ){
  
  n <- nrow(dane)
  
  random <- dane[sample(1:n, n, replace=FALSE),]
  
  granice <- seq(from=1, to=n, length = 11)
  
  pstwo_wartosc <- numeric(10)
  czulosc_wartosc <- numeric(10)
  specyficznosc_wartosc <- numeric(10)
  precyzja_wartosc <- numeric(10)
  
  
  
  for(i in 1:(length(granice)-1)){
    
    train <- random[-c(granice[i]:granice[i+1]),]
    test <- random[granice[i]:granice[i+1],]
    
    if(model_typ=="lda"){
      
      model <- lda(Class~., data=random[-c(granice[i]:granice[i+1]),])
      Predykcja <- predict(model, newdata=random[granice[i]:granice[i+1],])
      Pred=Predykcja$class
      
    } else if(model_typ=="qda"){
      
      model <- qda(Class~., data=random[-c(granice[i]:granice[i+1]),])
      Predykcja <- predict(model, newdata=random[granice[i]:granice[i+1],])
      Pred=Predykcja$class
      
    } else if(model_typ=="logistyczna"){
      
      model <- glm(Class~., data=random[-c(granice[i]:granice[i+1]),],
                   family="binomial")
      
      Predykcja <- predict(model, newdata=random[granice[i]:granice[i+1],],
                           type="response")
      
      Pred <- ifelse(Predykcja>=0.5, 1,0)
      
    } else if(model_typ=="regularyzowana"){
      
      x <- as.matrix(train[,-1])
      y <- train[,1]
      print(i)
      cv1 <- cv.glmnet(x,y,family="binomial",alpha=1)
      model <- glmnet(x, y,  family = "binomial", alpha = 1, lambda=cv1$lambda.min)
      
      Predykcja <- predict(model, newx=as.matrix(test[,-1]), type="response")
      Pred <- ifelse(Predykcja>=0.5, 1,0)
      
    } else if(model_typ=="drzewo"){
      
      minsplit <- as.numeric(audit_rpart$"best.parameters")
      
      drzewo_pelne <- rpart(Class~., data=train, cp=0, minsplit=minsplit)   
      Z=prune.rpart(drzewo_pelne,cp=0.00037)   
      Pred <- predict(Z, newdata= test, type="class")
      
    } else if(model_typ=="perceptron"){
      
      x <- train[, -1]
      y <- 2*as.double(as.character(train[,1]))-1
      
      b <- perceptron(x, y, 0.5, 100, 0.01)
      
      nowyx <- test[,-1]    
      Pred <- (sign(b[length(b)] + as.matrix(nowyx)%*%b[-length(b)])+1)/2
      
    }
    
    t <- table(test$Class, Pred)
    pstwo_wartosc[i] <- procent(t)
    czulosc_wartosc[i] <- czulosc(t)
    precyzja_wartosc <- precyzja(t)
    specyficznosc_wartosc <- specyficznosc(t)
    
  }
  return(list(procent=mean(pstwo_wartosc), czulosc = mean(czulosc_wartosc),
              precyzja=mean(precyzja_wartosc),
              specyficznosc = mean(specyficznosc_wartosc)))
  
}

perceptron<-function(x, y, alpha, maxiter, thrs){
  
  p<-ncol(x)
  x<-as.matrix(x)
  beta<-runif(p, -3, 3)
  beta_0<-runif(1, -3, 3)
  beta_old<-rep(100,p)
  j<-0
  while (max(abs(beta_old-beta))>thrs){
    j<-j+1
    beta_old <- beta
    for (i in 1:nrow(x)){
      temp<-(x %*% beta) + beta_0
      if (temp[i]*y[i] < 0){
        beta<-beta+alpha*y[i]*x[i,]
        beta_0<-beta_0+alpha*y[i]
      }
    }
    if (j==maxiter)
      break
  }
  c(beta,beta_0)
}

#########################################################
#########################################################
#########################################################
### I. 
#########################################################
#########################################################
#########################################################


#########################################################
#########################################################
#########################################################
### II. WYBOR MODELU 
#########################################################
#########################################################
#########################################################

#########################################################
### NAIWNY KLASYFIKATOR BAYESA
#########################################################

# dopasowanie modelu
nb <- NaiveBayes(Class ~ ., data = Train, prior = TRUE)

Probs <- predict(nb, newdata = Test)
pred_test <- Probs$class
Probs <- Probs$posterior[,2]
Labels <-  Test$Class

# Predykcja na zbiorze testowym
(tab <- table(pred_test, Test$Class, dnn = c("Observed Class", "Predicted Class")))

# Charakterystyki
dokladnosc_NaiveBayes <- dokladnosc(tab)
czulosc_NaiveBayes <- czulosc(tab)
precyzja_NaiveBayes <- precyzja(tab)

# Krzywe
  
NaiveBayes <- prediction(Probs, Labels)

# ROC
ROC_NaiveBayes <- performance(NaiveBayes,"tpr", "fpr")
plot(ROC_NaiveBayes, main="Krzywa ROC - NAIWNY BAYES",col=2, lwd=2, type = "S")
abline(a=0, b=1, lwd=2, lty=2, col="gray")

# CAP 
CAP_NaiveBayes <- performance(NaiveBayes, "tpr", "rpp")
plot(CAP_NaiveBayes, main="Krzywa CAP - NAIWNY BAYES",col=2, lwd=2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")

#LIFT
LIFT_NaiveBayes <- performance(NaiveBayes, "lift", "rpp")
plot(LIFT_NaiveBayes, main="Krzywa LIFT - NAIWNY BAYES",col=2,lwd=2)

AUC_NaiveBayes <- auc(NaiveBayes)

# Blad predykcji - kroswalidacja 10-krotna

mypredict <- function(object, newdata)
  predict(object, newdata = newdata)$class

mymodel <- function(formula, data) {
  NaiveBayes(formula, data = data)
}

err <- errorest(Class ~ ., data=cnn, model = mymodel, estimator = "cv", 
                predict= mypredict)
error_NaiveBayes <- err$error

######################################################
### K-NN
######################################################

# Wybor parametru k:
K <- tune.knn(Train[,-1], as.factor(Train[,1]), 
              k = c(1:10, 15, 20, 25, 30, 35, 40, 45, 50, 60, 70, 80, 90, 100))
plot(K$performances$k, K$performances$error, type = "b", 
     ylab = "Błąd klasyfikacji", xlab = "Liczba sąsiadów")

# wybieramy k = 15

# Dopasowanie modelu:
KNN <- knn(Train, Test, cl = as.factor(Train), k = 15, prob = TRUE)

# Charakterystyki:

Probs <- attr(KNN, which = "prob")
Labels <-  Test$Class

# Predykcja na zbiorze testowym
(tab <- table(KNN, Test$Class, dnn = c("Observed Class", "Predicted Class")))

dokladnosc_KNN <- dokladnosc(tab)
czulosc_KNN <- czulosc(tab)
precyzja_KNN <- precyzja(tab) 

Knn <- prediction(Probs, Labels)

# ROC
ROC_KNN <- performance(Knn,"tpr", "fpr")
plot(ROC_KNN, main="Krzywa ROC - k-nn",col=2,lwd=2)
abline(a=0, b=1, lwd=2,  lty=2,col="gray")
# AUC
AUC_KNN <- auc(Knn)

# CAP 
CAP_KNN <- performance(NaiveBayes, "tpr", "rpp")
plot(CAP_KNN, main="Krzywa CAP - KNN",col=2, lwd=2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")

#LIFT
LIFT_KNN <- performance(Knn, "lift", "rpp")
plot(LIFT_KNN, main="Krzywa LIFT - k-nn",col=2,lwd=2)

# Błąd klasyfikacji na podsatwie kroswalidacji 10 - krotnej

mypredict <- function(object, newdata) object

mymodel <- function(data, formula) {
  knn(data, setdiff(cnn, data), cl = as.factor(data$Class), k = 15, prob = TRUE)
}

err <- errorest(Class~., data=cnn, 
                model=mymodel, 
                estimator = "cv", predict= mypredict)

error_KNN <- err$error

###########################################################
### BAGGING
###########################################################

# nasza funkcja bagging, nie wykorzystalismy jej ze wzgledu 
# na zlozonosc czasowa

bagging <- function(B, train, test){
  
  # liczba obserwacji w zbiorze treningowym
  nr_train <- nrow(train)
  
  pred <- lapply(1:B, function(i){
    jakie <- sample(1:nr_train, replace = TRUE)
    train_new <- train[jakie,]
    tree <- rpart(Class ~ ., data = train_new, )
    round(predict(tree, newdata = test)[,2], 0)
  })
  bycol <- mapply("+", pred)
  suma <- apply(bycol, 1, sum)
  
  # przynaleznosc do klasy:
  pred <- ifelse(suma >= B/2, 1, 0)
  
  tab <- table(test$Class, pred, dnn = c("Observed Class", "Predicted Class"))
  
  return(confusion = tab)
}

# Dopasowanie modelu:
bag <- bagging(Class ~ ., data=Train,  nbagg=25)

# charakterystyki:
Probs <- Probs
Labels <-  Test$Class

# Predykcja na zbiorze testowym
pred_test <- predict(bag, newdata=Test)

Bagging  <- prediction(Probs[,2], Labels)

(tab <- table(pred_test, Test$Class, dnn = c("Observed Class", "Predicted Class")))

dokladnosc_Bagging <- dokladnosc(tab)
czulosc_Bagging  <- czulosc(tab)
precyzja_Bagging  <- precyzja(tab)

# ROC
ROC_Bagging  <- performance(Bagging ,"tpr", "fpr")
plot(ROC_Bagging, main="Krzywa ROC - BAGGING",col=2,lwd=2)
abline(a=0, b=1, lwd=2,  lty=2,col="gray")

# AUC
AUC_Bagging  <- auc(Bagging)

# CAP 
CAP_Bagging <- performance(Bagging, "tpr", "rpp")
plot(CAP_Bagging, main="Krzywa CAP - BAGGING",col=2, lwd=2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")

#LIFT
LIFT_Bagging  <- performance(Bagging , "lift", "rpp")
plot(LIFT_Bagging, main="Krzywa LIFT - BAGGING",col=2,lwd=2)

# Blad kroswalidacji 10 krotnej 

err <- errorest(Class ~ ., data = cnn, model=bagging, estimator = "cv", predict= predict)
error_Bagging <- err$error

###########################################################
### RANDOM FOREST
###########################################################

rf <- randomForest(Class ~ ., data=Train, test= Test)

Probs <- predict(rf, newdata=Test, type = "prob")[,2]
Labels <-  Test$Class

# Predykcja na zbiorze testowym
pred_test <- predict(rf, newdata=Test)
(tab <- table(pred_test, Test$Class, dnn = c("Observed Class", "Predicted Class")))

dokladnosc_RandomForest <- dokladnosc(tab)
czulosc_RandomForest <- czulosc(tab)
precyzja_RandomForest <- precyzja(tab)

RandomForest <- prediction(Probs, Labels)

# ROC
ROC_RandomForest <- performance(RandomForest,"tpr", "fpr")
plot(ROC_RandomForest, main="Krzywa ROC - LASY LOSOWE",col=2,lwd=2, type = "s")
abline(a=0, b=1, lwd=2,  lty=2,col="gray")

# AUC
AUC_RandomForest <- auc(RandomForest)

# CAP 
CAP_RandomForest <- performance(RandomForest, "tpr", "rpp")
plot(CAP_RandomForest, main="Krzywa CAP - RANDOM FOREST",col=2, lwd=2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")

#LIFT
LIFT_RandomForest <- performance(RandomForest, "lift", "rpp")
plot(LIFT_RandomForest, main="Krzywa LIFT - LASY LOSOWE",col=2,lwd=2)

error_RandomForest <- errorest(Class ~ ., data=bbc_cnn, model = randomForest, estimator = "cv", predict= predict)

###########################################################
### LDA
###########################################################

model_lda_train <- lda(Class~., data=Train)

predykcja_lda_test <- predict(model_lda_train, newdata = Test)
(tabela_lda_test <- table(Test[,1], predykcja_lda_test$class,dnn=c("Observed Class", "Predicted Class")))
(procent_lda_test <- procent(tabela_lda_test))
(czulosc_lda_test <- czulosc(tabela_lda_test))
(specyficznosc_lda_test <- specyficznosc(tabela_lda_test))
(precyzja_lda_test <- precyzja(tabela_lda_test))

k_lda <- kroswalidacja(cnn, "lda")


Labels = ifelse(Test$Class==1,1,0)
P1 <- prediction(predykcja_lda_test$posterior[,2], Labels)
#CAP
CAP_lda <- performance(P1, "tpr", "rpp")

plot(CAP_lda, main="Krzywa CAP - LDA",col=2, lwd=2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")

#ROC
ROC_lda <- performance(P1, "tpr", "fpr")

plot(ROC_lda, main="Krzywa ROC - LDA",col=2, lwd=2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")

#AUC
AUC_lda <- performance(P1, "auc")@y.values[[1]]

#LIFT
LIFT_lda <- performance(P1, measure="lift", x.measure="rpp")

plot(LIFT_lda, main="Krzywa LIFT - LDA",col=2, lwd=2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")

###########################################################
### QDA
###########################################################

model_qda_train <- qda(Class~., data=Train)

predykcja_qda_test <- predict(model_qda_train, newdata = Test)
(tabela_qda_test <- table(Test[,1], predykcja_qda_test$class,dnn=c("Observed Class", "Predicted Class")))
(procent_qda_test <- procent(tabela_qda_test))
(czulosc_qda_test <- czulosc(tabela_qda_test))
(specyficznosc_qda_test <- specyficznosc(tabela_qda_test))
(precyzja_qda_test <- precyzja(tabela_qda_test))

k_qda <- kroswalidacja(cnn, "qda")


Labels = ifelse(Test$Class==1,1,0)
P1 <- prediction(predykcja_qda_test$posterior[,2], Labels)
#CAP
CAP_qda <- performance(P1, "tpr", "rpp")

plot(CAP_qda, main="Krzywa CAP - QDA",col=2, lwd=2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")

#ROC
ROC_qda <- performance(P1, "tpr", "fpr")

plot(ROC_qda, main="Krzywa ROC - QDA",col=2, lwd=2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")

#AUC
AUC_qda <- performance(P1, "auc")@y.values[[1]]

#LIFT
LIFT_qda <- performance(P1, measure="lift", x.measure="rpp")

plot(LIFT_qda, main="Krzywa LIFT - QDA",col=2, lwd=2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")


###########################################################
### LOGISTYCZNA
###########################################################

model_log_train <- glm(Class~., data=Train, family="binomial")

predykcja_log_test_pstwa <- predict(model_log_train, newdata = Test, type="response")
predykcja_log_test <- ifelse(predykcja_log_test_pstwa>=0.5, 1,0)
(tabela_log_test <- table(Test[,1], predykcja_log_test,dnn=c("Observed Class", "Predicted Class")))
(procent_log_test <- procent(tabela_log_test))
(czulosc_log_test <- czulosc(tabela_log_test))
(specyficznosc_log_test <- specyficznosc(tabela_log_test))
(precyzja_log_test <- precyzja(tabela_log_test))

k_log <- kroswalidacja(cnn, "logistyczna")

Labels = ifelse(Test$Class==1,1,0)
P1 <- prediction(predykcja_log_test_pstwa, Labels)
#CAP
CAP_log <- performance(P1, "tpr", "rpp")

plot(CAP_log, main="Krzywa CAP - LOGISTYCZNY",col=2, lwd=2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")

#ROC
ROC_log <- performance(P1, "tpr", "fpr")

plot(ROC_log, main="Krzywa ROC - LOGISTYCZNY",col=2, lwd=2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")

#AUC
AUC_log <- performance(P1, "auc")@y.values[[1]]

#LIFT
LIFT_log <- performance(P1, measure="lift", x.measure="rpp")

plot(LIFT_log, main="Krzywa LIFT - LOGISTYCZNY",col=2, lwd=2)

###########################################################
### LOGISTYCZNA REGULARYZOWANA
###########################################################

x <- as.matrix(Train[,-1])
y <- Train[,1]

#ogolnie
model_reg_train <- glmnet(x, y,  family = "binomial", alpha = 1)

#wykres 1
plot(model_reg_train, xvar="lambda")

#wykres 2
nsteps <- 20
b1 <- coef(model_reg_train)[-1, 1:nsteps]
w <- nonzeroCoef(b1)
b1 <- as.matrix(b1[w, ])

matplot(1:nsteps, t(b1), type = "o", pch = 19, col = "blue", xlab = "Step", ylab = "Coefficients", lty = 1)
title("Lasso")
abline(h = 0, lty = 2)

#kroswalidacja - wybor lambda
cv1 = cv.glmnet(x,y,family="binomial",alpha=1)

#modele min i 1se
model_reg_train_min <- glmnet(x, y,  family = "binomial", alpha = 1, lambda=cv1$lambda.min)
model_reg_train_1se <- glmnet(x, y,  family = "binomial", alpha = 1, lambda=cv1$lambda.1se)

#predykcja
x2 <- as.matrix(Test[,-1])
y <- Test[,1]

predykcja_reg1_test <- predict(model_reg_train_min, newx=x2, type="response")
predykcja_reg1_test_klasy <- ifelse(predykcja_reg1_test>=0.5, 1, 0)
(tabela_reg1_test <- table(y,predykcja_reg1_test_klasy,dnn=c("Observed Class", "Predicted Class")))
(procent_reg1_test <- procent(tabela_reg1_test))
(czulosc_reg1_test <- czulosc(tabela_reg1_test))
(specyficznosc_reg1_test <- specyficznosc(tabela_reg1_test))
(precyzja_reg1_test <- precyzja(tabela_reg1_test))

#kroswalidacja

k_reg <- kroswalidacja(cnn, "regularyzowana")

Labels = ifelse(Test$Class==1,1,0)
P1 <- prediction(predykcja_reg1_test, Labels)
#CAP
CAP_reg <- performance(P1, "tpr", "rpp")

plot(CAP_reg, main="Krzywa CAP - LOGISTYCZNY REGULARYZOWANY",col=2, lwd=2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")

#ROC
ROC_reg <- performance(P1, "tpr", "fpr")

plot(ROC_reg, main="Krzywa ROC - LOGISTYCZNY REGULARYZOWANY",col=2, lwd=2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")

#AUC
AUC_reg <- performance(P1, "auc")@y.values[[1]]

#LIFT
LIFT_reg <- performance(P1, measure="lift", x.measure="rpp")

plot(LIFT_reg, main="Krzywa LIFT - LOGISTYCZNY REGULARYZOWANY",col=2, lwd=2)

###########################################################
### DRZEWA DECYZYJNE
###########################################################

audit_rpart <- tune.rpart(Class~., data=Train, minsplit=seq(50,1000,50))

#wykres 1
plot(audit_rpart)

minsplit <- as.numeric(audit_rpart$"best.parameters")

model_drzewo_pelne_train <- rpart(Class~., data=Train, cp=0, minsplit=minsplit)

#wykres 2
plotcp(model_drzewo_pelne_train)

model_tree_train <- prune.rpart(model_drzewo_pelne_train,cp=0.0065)

#drzewo
rpart.plot(model_tree_train)

#predykcja
predykcja_tree_test <- predict(model_tree_train, newdata = Test, type="class")
(tabela_tree_test <- table(Test[,1], predykcja_tree_test,dnn=c("Observed Class", "Predicted Class")))
(procent_tree_test <- procent(tabela_tree_test))
(czulosc_tree_test <- czulosc(tabela_tree_test))
(specyficznosc_tree_test <- specyficznosc(tabela_tree_test))
(precyzja_tree_test <- precyzja(tabela_tree_test))

predykcja_tree_test_pstwa <- predict(model_tree_train, newdata = Test, type="prob")

#kroswalidacja
k_tree <- kroswalidacja(cnn, "drzewo")

Labels = ifelse(Test$Class==1,1,0)
P1 <- prediction(predykcja_tree_test_pstwa[,2], Labels)

#CAP
CAP_tree <- performance(P1, "tpr", "rpp")

plot(CAP_tree, main="Krzywa CAP - DRZEWO",col=2, lwd=2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")

#ROC
ROC_tree <- performance(P1, "tpr", "fpr")

plot(ROC_tree, main="Krzywa ROC - DRZEWO",col=2, lwd=2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")

#AUC
AUC_tree <- performance(P1, "auc")@y.values[[1]]

#LIFT
LIFT_tree <- performance(P1, measure="lift", x.measure="rpp")

plot(LIFT_tree, main="Krzywa LIFT - DRZEWO",col=2, lwd=2)

###########################################################
### PERCEPTRON
###########################################################

x <- Train[, -1]
y <- 2*as.double(as.character(Train[,1]))-1

model_perceptron <- perceptron(x, y, 0.5, 100, 0.01)

nowyx <- Test[,-1]

#predykcja
predykcja_p_test <- (sign(model_perceptron[length(model_perceptron)] + as.matrix(nowyx)%*%model_perceptron[-length(model_perceptron)])+1)/2
(tabela_p_test<- table(Test[,1], predykcja_p_test,dnn=c("Observed Class", "Predicted Class")))
(procent_p_test <- procent(tabela_p_test))
(czulosc_p_test <- czulosc(tabela_p_test))
(specyficznosc_p_test <- specyficznosc(tabela_p_test))
(precyzja_p_test <- precyzja(tabela_p_test))

Labels = ifelse(Test$Class==1,1,0)
P1 <- prediction(predykcja_p_test, Labels)

#CAP
CAP_p <- performance(P1, "tpr", "rpp")

plot(CAP_p, main="Krzywa CAP - PERCEPTRON",col=2, lwd=2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")

#ROC
ROC_p <- performance(P1, "tpr", "fpr")

plot(ROC_p, main="Krzywa ROC - PERCEPTRON",col=2, lwd=2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")

#AUC
AUC_p <- performance(P1, "auc")@y.values[[1]]

#LIFT
LIFT_p <- performance(P1, measure="lift", x.measure="rpp")
plot(LIFT_p, main="Krzywa LIFT - PERCEPTRON",col=2, lwd=2)

