data("InsectSprays")
InsectSprays
head(InsectSprays)
tail(InsectSprays)
summary(InsectSprays)
is.factor(InsectSprays$spray)

fit1<-glm(count~spray,data = InsectSprays,family = poisson)
summary(fit1)

fit2<-glm(count~spray-1,data = InsectSprays,family = poisson)
summary(fit1)
coef(fit2)
exp(coef(fit2))
AIC(fit1)
AIC(fit2)

x1<-c(7,6,8,6,1,3,7,8,2,1,3,2,3,3)
x2<-c(8,8,9,7,2,4,8,9,3,2,2,5,5,5)
x3<-c(4,5,7,7,5,5,8,6,5,4,6,6,4,5)
x4<-c(5,4,8,7,3,3,6,5,6,4,5,8,6,6)
x5<-c(2,2,9,8,4,5,6,5,5,2,7,9,3,3)
data=data.frame(x1,x2,x3,x4,x5)
data
cluster1=c(6,7,7,7,8)
dist(data)

A<-matrix(c(1,1,1,2),2,2,byrow = T)
A
solve(A)

V<-matrix(c(2,1.41,1.41,4),2,2)
V
solve(V)

