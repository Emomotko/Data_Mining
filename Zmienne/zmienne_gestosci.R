load("Data//cnn.rda")

##### V7
plot(density(cnn[cnn$Class == 0, 3]), col="grey", main = "V3", , xlab = "")
legend("topright", col = c("red", "grey"), 
       legend = c("Reklama", "TV News"), lty = 1)
lines(density(cnn[cnn$Class == 1,3]), col="red", 
      main = "V3", , xlab = "")

#zmienna V3 nie jest normalna!!!!

n1 <- length(cnn[cnn$Class == 1,3])
n2 <- length(cnn[cnn$Class == 0,3])

shapiro.test(cnn[cnn$Class == 1,3][sample(1:n1, 5000)])
shapiro.test(cnn[cnn$Class == 0,3][sample(1:n2, 5000)])

#wykresy qqnorm
qqnorm(cnn[cnn$Class == 1,3])
qqline(cnn[cnn$Class == 1,3], col = 2)
qqnorm(cnn[cnn$Class == 0,3])
qqline(cnn[cnn$Class == 0,3], col = 2)

hist(cnn[cnn$Class == 1,3])
hist(cnn[cnn$Class == 0,3])

##### V12

plot(density(cnn[cnn$Class == 0,7]), col="grey", main = "V12", , xlab = "")
legend("topleft", col = c("red", "grey"), 
       legend = c("Reklama", "TV News"), lty = 1)
lines(density(cnn[cnn$Class == 1,7]), col="red", 
      main = "V12", , xlab = "")

#zmienna V12 nie jest normalna!!!!
shapiro.test(cnn[cnn$Class == 1,7][sample(1:n1, 5000)])
shapiro.test(cnn[cnn$Class == 0,7][sample(1:n2, 5000)])

#wykresy qqnorm
qqnorm(cnn[cnn$Class == 1,7])
qqline(cnn[cnn$Class == 1,7], col = 2)
qqnorm(cnn[cnn$Class == 0,7])
qqline(cnn[cnn$Class == 0,7], col = 2)

hist(cnn[cnn$Class == 1,7])
hist(cnn[cnn$Class == 0,7])

