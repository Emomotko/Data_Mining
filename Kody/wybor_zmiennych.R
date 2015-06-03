###########################################################
### WYBOR ZMIENNYCH
###########################################################

library(MASS)
library(rpart)
library(stringi)

load("CNN.rda")

dane <- CNN

dane[,1]<- ifelse(dane[,1]==1,1,0)
dane[,1] <- as.factor(dane[,1])

zmienne <- colnames(dane)[-1]
formuly <- stri_paste("Class ~ ", zmienne)

wskaznik <- numeric(length(formuly))
names(wskaznik) <- zmienne

model2 <- glm(Class~1, data=dane, family="binomial")

for(i in seq_along(formuly)){
  
  model <- glm(as.formula(formuly[i]), data=dane, family="binomial")
  wskaznik[i] <- anova(model2, model)$Deviance[2]
  
}

a <- sort(wskaznik, decreasing=TRUE)

frakcje <- numeric(length(a)+1)

for(i in 2:(length(frakcje))){
  
  frakcje[i] <- frakcje[i-1]+a[i-1]/sum(a)
  
}

do_ilu <- min(which(frakcje>=0.8))

k <- which(colnames(dane) %in% names(a)[1:do_ilu])

cnn <- dane[,c(1,k)]