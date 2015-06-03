load("Data//cnn.rda")

lines(density(cnn[cnn$Class == 1,'V10']), col="red", 
      main = "V10", , xlab = "")
plot(density(cnn[cnn$Class == 0, 10]), col="grey", main = "V10", , xlab = "")
legend("topright", col = c("red", "grey"), 
       legend = c("Reklama", "TV News"), lty = 1)


##### V3
plot(density(cnn[cnn$Class == 0, 3]), col="grey", main = "V3", , xlab = "")
legend("topright", col = c("red", "grey"), 
       legend = c("Reklama", "TV News"), lty = 1)
lines(density(cnn[cnn$Class == 1,3]), col="red", 
      main = "V3", , xlab = "")

#zmienna V3 nie jest normalna!!!!
shapiro.test(cnn[cnn$Class == 1,3][1:5000])
shapiro.test(cnn[cnn$Class == 0,3][1:5000])

#wykresy qqnorm
qqnorm(cnn[cnn$Class == 1,3][1:5000])
qqline(cnn[cnn$Class == 1,3], col = 2)
qqnorm(cnn[cnn$Class == 0,3][1:5000])
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
shapiro.test(cnn[cnn$Class == 1,7][1:5000])
shapiro.test(cnn[cnn$Class == 0,7][1:5000])

#wykresy qqnorm
qqnorm(cnn[cnn$Class == 1,7][1:5000])
qqline(cnn[cnn$Class == 1,7], col = 2)
qqnorm(cnn[cnn$Class == 0,7][1:5000])
qqline(cnn[cnn$Class == 0,7], col = 2)

hist(cnn[cnn$Class == 1,7])
hist(cnn[cnn$Class == 0,7])

