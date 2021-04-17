model <- glm(formula= vs ~ wt + disp, data=mtcars, family=binomial)
summary(model)
newdata = data.frame(wt = 2.1, disp = 180)
newdata
predict(model, newdata, type="response")
0.2361081

model <- glm(formula= vs ~ wt + disp, data=mtcars, family=binomial)
summary(model)

install.packages("ResourceSelection")

library(ResourceSelection)
hoslem.test(mtcars$vs, fitted(model))

model_wt <- glm(formula= vs ~ wt , data=mtcars, family=binomial)
summary(model_wt)



range(mtcars$wt)
xweight <- seq(0, 6, 0.01)
yweight <- predict(model_wt, list(wt = xweight),type="response")


plot(mtcars$wt, mtcars$vs, pch = 16, xlab = "WEIGHT (g)", ylab = "VS")
lines(xweight, yweight)

model_disp<-glm(vs~disp,data=mtcars,family = "binomial")
summary(model_disp)

range(mtcars$disp)

xdisp<-seq(70,475,0.01)
ydisp<-predict(model_disp,list(disp=xdisp),type="response")


plot(mtcars$disp,mtcars$vs,pch=16,xlab="disp",ylab="VS",col="red")
lines(xdisp,ydisp,col="blue")



A <- structure(list(numeracy = c(6.6, 7.1, 7.3, 7.5, 7.9, 7.9, 8, 
8.2, 8.3, 8.3, 8.4, 8.4, 8.6, 8.7, 8.8, 8.8, 9.1, 9.1, 9.1, 9.3, 
9.5, 9.8, 10.1, 10.5, 10.6, 10.6, 10.6, 10.7, 10.8, 11, 11.1, 
11.2, 11.3, 12, 12.3, 12.4, 12.8, 12.8, 12.9, 13.4, 13.5, 13.6, 
13.8, 14.2, 14.3, 14.5, 14.6, 15, 15.1, 15.7), 
anxiety = c(13.8,14.6, 17.4, 14.9, 13.4, 13.5, 13.8, 16.6, 13.5, 15.7, 13.6, 14, 
16.1, 10.5, 16.9, 17.4, 13.9, 15.8, 16.4, 14.7, 15, 13.3, 10.9, 
12.4, 12.9, 16.6, 16.9, 15.4, 13.1, 17.3, 13.1, 14, 17.7, 10.6, 
14.7, 10.1, 11.6, 14.2, 12.1, 13.9, 11.4, 15.1, 13, 11.3, 11.4, 
10.4, 14.4, 11, 14, 13.4), 
success = c(0L, 0L, 0L, 1L, 0L, 1L,0L, 0L, 1L, 0L, 1L, 1L, 0L, 1L, 
0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 1L, 1L, 1L, 0L, 0L, 0L, 1L, 0L, 1L,
0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L)), 
Names = c("numeracy", "anxiety", "success"), row.names = c(NA, -50L), 
class = "data.frame")
attach(A)
A


names(A)
head(A)

mean(numeracy)
mean(anxiety)


model1 <- glm(success ~ numeracy*anxiety, binomial)
summary(model1)

model_numeracy <- glm(success ~ numeracy, binomial)
summary(model_numeracy)


model_anxiety <- glm(success ~ anxiety, binomial)
summary(model_anxiety)

range(numeracy)
range(anxiety)

xnumeracy <-seq (0, 15, 0.01)

ynumeracy <- predict(model_numeracy, list(numeracy=xnumeracy),type="response")


plot(numeracy, success, pch = 16, xlab = "NUMERACY SCORE", ylab = "ADMISSION")

lines(xnumeracy, ynumeracy, col = "red", lwd = 2)


xanxiety <- seq(10, 20, 0.1)

yanxiety <- predict(model_anxiety, list(anxiety=xanxiety),type="response")

plot(anxiety, success, pch = 16, xlab = "ANXIETY SCORE", ylab = "SUCCESS")

lines(xanxiety, yanxiety, col= "blue", lwd = 2)




cases <-  
  structure(list(Days = c(1L, 2L, 3L, 3L, 4L, 4L, 4L, 6L, 7L, 8L, 
  8L, 8L, 8L, 12L, 14L, 15L, 17L, 17L, 17L, 18L, 19L, 19L, 20L, 
  23L, 23L, 23L, 24L, 24L, 25L, 26L, 27L, 28L, 29L, 34L, 36L, 36L, 
  42L, 42L, 43L, 43L, 44L, 44L, 44L, 44L, 45L, 46L, 48L, 48L, 49L, 
  49L, 53L, 53L, 53L, 54L, 55L, 56L, 56L, 58L, 60L, 63L, 65L, 67L, 
  67L, 68L, 71L, 71L, 72L, 72L, 72L, 73L, 74L, 74L, 74L, 75L, 75L, 
  80L, 81L, 81L, 81L, 81L, 88L, 88L, 90L, 93L, 93L, 94L, 95L, 95L, 
  95L, 96L, 96L, 97L, 98L, 100L, 101L, 102L, 103L, 104L, 105L, 
  106L, 107L, 108L, 109L, 110L, 111L, 112L, 113L, 114L, 115L), 
  Students = c(6L, 8L, 12L, 9L, 3L, 3L, 11L, 5L, 7L, 3L, 8L, 
  4L, 6L, 8L, 3L, 6L, 3L, 2L, 2L, 6L, 3L, 7L, 7L, 2L, 2L, 8L, 
  3L, 6L, 5L, 7L, 6L, 4L, 4L, 3L, 3L, 5L, 3L, 3L, 3L, 5L, 3L, 
  5L, 6L, 3L, 3L, 3L, 3L, 2L, 3L, 1L, 3L, 3L, 5L, 4L, 4L, 3L, 
  5L, 4L, 3L, 5L, 3L, 4L, 2L, 3L, 3L, 1L, 3L, 2L, 5L, 4L, 3L, 
  0L, 3L, 3L, 4L, 0L, 3L, 3L, 4L, 0L, 2L, 2L, 1L, 1L, 2L, 0L, 
  2L, 1L, 1L, 0L, 0L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 0L, 0L, 
  0L, 1L, 1L, 0L, 0L, 0L, 0L, 0L)),
  Names = c("Days", "Students" ), class = "data.frame", row.names = c(NA, -109L))
cases

attach(cases)

head(cases) 


plot(Days, Students, xlab = "DAYS", ylab = "STUDENTS", pch = 16)

model1 <- glm(Students ~ Days, poisson)

summary(model1)


model2 <- glm(Students ~ Days, quasipoisson) 
summary(model2)

model2$coefficients


timeaxis <-seq (0,150,0.1)

Y<-predict(model2, list(Days = timeaxis))

plot(Days, Students, xlab = "DAYS", ylab = "STUDENTS", pch = 16)

lines(timeaxis, exp(Y), lwd = 2, col = "blue")


Z <- predict(model2, list(Days = timeaxis), type = "response")

plot(Days, Students, xlab = "DAYS", ylab = "NUMBER", pch = 16)

lines(timeaxis, Z, lwd = 2, col = "red")

coeffs <- exp(coef(model2))

coeffs

CI <- exp(confint.default(model2))

CI

1 - 0.9826884
