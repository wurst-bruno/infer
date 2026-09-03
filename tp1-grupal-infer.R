set.seed(12)

#Uniforme (0,14)
a1aesperanza <- (0+14)/2
a1avarianza <- ((14-0)^2)/12

#1b
X_dist <- function(R){
  runif(R,0,14)
}
R<- 10000
X_dist(R) #la devuelve por consola

#1C
muchas_X <-X_dist(10000)
n_primeros<-numeric(10000)

for(i in 1:10000){
  n_primeros[i] <-sum(muchas_X[1:i])/i
}

plot(n_primeros, type = 'l')
abline(h=a1aesperanza)

#D
# Las otras 29 curvas
for (j in 2:30) {
  muchas_X <- X_dist(10000)
  n_primeros <- numeric(R)
  for (i in 1:R) {
    n_primeros[i] <- sum(muchas_X[1:i]) / i
  }
  lines(n_primeros, col = j)
}

#1E
concien<- runif(100,0,14)
hist(concien, freq = FALSE)
curve(dunif(x, 0, 14), add = TRUE, col = "red")

conmil<- runif(10000,0,14)
hist(conmil, freq = FALSE)
curve(dunif(x, 0, 14), add = TRUE, col = "red")




#2
#Sea Y ∼ E(1/7) una variable con distribuci´on exponencial de par´ametro λ = 1/7

#A
esperanza_2a<-7
varianza_2a<-49 

#B
x_dist2a <- function(R){
  rexp(R,1/7)
}
R<-10000
x_dist2a(R)

#C
muchas_X_2<- x_dist2a(10000)
promi2a<-numeric(10000)

for (i in 1:10000) {
  promi2a[i]<- sum(muchas_X_2[1:i])/i
}

plot(promi2a, type = 'l')
abline(h=esperanza_2a, col='red')

#D 
#29 veces todo nuevo

for (j in 2:30) {
  muchas_X_n<-x_dist2a(10000)
  promi2a29<-numeric(R)
  
  for (i in 1:R) {
    promi2a29[i]<-sum(muchas_X_n[1:i])/i
  }  
  lines(promi2a29 ,col=j)
}

#E
ej2_cien<-rexp(100,1/7)
hist(ej2_cien, freq = FALSE)
curve(dexp(x,1/7),add = TRUE, col = 'red')

ej2_10mil<-rexp(10000,1/7)
hist(ej2_10mil, freq = FALSE)
curve(dexp(x,1/7),add = TRUE, col = 'red')




#3a

#3b
espx15<-a1aesperanza
varx15<-a1avarianza/15

#3c
R<-1000
Xn_dist<-function(n,R){
  promedio3a<-numeric(R)
  for (i in 1:R) {
    promedio3a[i]<-(sum(runif(n,0,14))/n)
  }
  return(promedio3a)
}

Xn_dist(2,10) #ejemeplo mio que lo devuelve por consola

#3d
tres_d<-Xn_dist(15,10000)
hist(tres_d)


#4
con40<-Xn_dist(40,1000000)
hist(con40, freq = FALSE)
curve(dnorm(x,7,sqrt(a1avarianza/ 40)), add = TRUE, col = "red")

con_n_libre<-runif(1000000,0,14)
hist(con_n_libre, freq=FALSE)
curve(dunif(x, 0, 14), add = TRUE, col = "red")


#4b
xn1<-Xn_dist(1,100)
xn1.2<-Xn_dist(1,1000000)


xn2<-Xn_dist(2,100)
xn2.2<-Xn_dist(2,1000000)


xn5<-Xn_dist(5,100)
xn5.2<-Xn_dist(5,1000000)


xn15<-Xn_dist(15,100)
xn15.2<-Xn_dist(15,1000000)


xn40<-Xn_dist(40,100)
xn40.2<-Xn_dist(40,1000000)


par(mfrow = c(5, 2)) ##le agrego con ia un titulito para entenderlo mejor
hist(xn1,   main = "n=1, 100 intentos")
hist(xn1.2, main = "n=1, 1M intentos")

hist(xn2,   main = "n=2, 100 intentos")
hist(xn2.2, main = "n=2, 1M intentos")

hist(xn5,   main = "n=5, 100 intentos")
hist(xn5.2, main = "n=5, 1M intentos")

hist(xn15,   main = "n=15, 100 intentos")
hist(xn15.2, main = "n=15, 1M intentos")

hist(xn40,   main = "n=40, 100 intentos")
hist(xn40.2, main = "n=40, 1M intentos")
par(mfrow = c(1, 1))

#ahora el 4 para Y
ylibre<-rexp(1000000,1/7)

Ydist<-function(n,R){
  promedioY<-numeric(R)
  for (i in 1:R) {
    promedioY[i]<-sum(rexp(n,1/7))/n
  }
  return(promedioY)
}

y40<-Ydist(40,1000000)

hist(ylibre, freq=FALSE)
curve(dexp(x,1/7),add=TRUE, col ='red')

hist(y40, freq = FALSE)
curve(dnorm(x, 7, sqrt(49 / 40)), add = TRUE, col = "red")



#4d
yn1<-Ydist(1,100)
yn1.2<-Ydist(1,1000000)

yn2<-Ydist(2,100)
yn2.2<-Ydist(2,1000000)

yn5<-Ydist(5,100)
yn5.2<-Ydist(5,1000000)

yn15<-Ydist(15,100)
yn15.2<-Ydist(15,1000000)

yn40<-Ydist(40,100)
yn40.2<-Ydist(40,1000000)

par(mfrow = c(5, 2)) ##le agrego con ia un titulito para entenderlo mejor
hist(yn1,   main = "n=1, 100 intentos")
hist(yn1.2, main = "n=1, 1M intentos")

hist(yn2,   main = "n=2, 100 intentos")
hist(yn2.2, main = "n=2, 1M intentos")

hist(yn5,   main = "n=5, 100 intentos")
hist(yn5.2, main = "n=5, 1M intentos")

hist(yn15,   main = "n=15, 100 intentos")
hist(yn15.2, main = "n=15, 1M intentos")

hist(yn40,   main = "n=40, 100 intentos")
hist(yn40.2, main = "n=40, 1M intentos")
par(mfrow = c(1, 1))
