#random walk

rm(list=ls())


#exemplo sem drift com distribuição uniforme


t = c(0:30)
y_t = c(12)
i = 0
while(i <= length(t)){
	y_t = append(y_t, y_t[i-1] + sample(-1:1,1)) #numero inteiro numa distribuição uniforme
	i = i + 1							  #se quer real, usa runif(n, start, final)
	}
length(y_t)
plot(t, y_t, type="l")

#exemplo com numero real sem drift com distribuição uniforme

erros = runif(51, -1, 1)
mean(erros)
sd(erros)

t = c(0:50)
y_t = c(12)
i = 0
while(i <= length(t)){
	y_t = append(y_t, y_t[i-1] + erros[i]) 
	i = i + 1}
length(y_t)
plot(t, y_t, type="l")


#random walk sem drift e a normal


erros = rnorm(51, 0, 9)
mean(erros)
sd(erros)

t = c(0:50)
y_t = c(12)
i = 0
while(i <= length(t)){
	y_t = append(y_t, y_t[i-1] + erros[i]) 
	i = i + 1}
length(y_t)
plot(t, y_t, type="l")

#random walk com drift e a normal

a_0 = 2
erros = rnorm(51, 0, 9)
mean(erros)
sd(erros)

t = c(0:50)
y_t = c(12)
i = 0
while(i <= length(t)){
	y_t = append(y_t, a_0 + y_t[i-1] + erros[i]) 
	i = i + 1}
length(y_t)

#plot horrivel

plot(y_t,type="l",ylim=c(min(y_t),max(y_t)),col="red",lty=1,ylab="Value",lwd=2,xlab="t",xaxt="n")
lines(y_t,type="l",col="black",lty=2,lwd=2)
lines(a_0*t,type="l",col="blue",lty=3,lwd=2)
grid()
legend("topleft",legend="y_t",lty=c(1,2,3),col=c("red","black","blue"),bg="white",lwd=2)
axis(1,at=c(1:length(y_t),labels=y_t))


