#simulacao monte carlo - cap 1.9 do Heiss

#finite sample properties of estimators

#caso y ~ normal(mi, delta^2)

#os estimador da média segue \bar{y} ~normal(mi, (delta^2)/n), com n sendo o 
#tamanho da amostra

#ex: caso mi=10, delta=2 e n=100

########uma vez############

set.seed(123456)
?rnorm
sample = rnorm(100,10,2)
mean(sample)
sample = rnorm(100,10,2)
mean(sample)    #valores diferentes com var=\frac{\delta^2}{n}=0.04

#tirar 10000 vezes e analisar

#vetor de 10000 numericos com 0
ybar = numeric(10000)

for (j in 1:10000){
	sample = rnorm(100,10,2)
	ybar[j] = mean(sample)}

ybar[1:20]

mean(ybar)

var(ybar)

plot(density(ybar)) #como é

curve(dnorm(x, 10, sqrt(.04)), add=TRUE, lty=2) #como deveria ser

#Lei dos grandes numeros: quando n tende ao infinito, o valor esperado da media amostral 
#para a media populacional  e a variancia  da media amostral tende a zero

#Central limit theorem: quando n tende ao infinito, a media amostral de Y seguira 
#uma distribuicao normal, nao importa a distribuicao original de Y (convergencia na distribuicao)

ybar = numeric(10000)

for (j in 1:10000){
	sample = rchisq(100,1)
	ybar[j] = mean(sample)}

ybar[1:20]

mean(ybar)

var(ybar)

plot(density(ybar)) #como é

#dá para usar testes e calcular intervalos de confianca 

#y~normal(10,4)
#10000 sample de tamanho n=100

#de cada sample, tira-se
#>os limites do intervalo de confiança e coloca eles em CIlower e CIupper
#>pvalue1: valor p do teste bilateral quando h0: mi=10
#>pvalue2: valor p no teste bilateral quando h0: mi=9.5
#calcular os vetores logicos reject1 e 2 sendo true se rejeitam a hipotese
#nula a um nivel de significancia de 0.05 (se sao menores)

set.seed(123456)
CIlower = numeric(10000)
CIupper = numeric(10000)
pvalue1 = numeric(10000)
pvalue2 = numeric(10000)

#repete

for (j in 1:10000){
	sample = rnorm(100,10,2)

	#testar a (correta) hipotese nula mu=10:
	testres1 = t.test(sample,mu=10)
	CIlower[j] = testres1$conf.int[1]
	CIupper[j] = testres1$conf.int[2]
	pvalue1[j] = testres1$p.value

	#testar a (incorreta) hipotese nula mu=9.5

	pvalue2[j] = t.test(sample, mu=9.5)$p.value
}

reject1=pvalue1<=0.05
reject2=pvalue2<=0.05

table(reject1)

table(reject2)

#plotando os intervalos de confianca

color = rep(gray(.5), 100)
color[reject1[1:100]] = "black"

plot(0, xlim=c(9,11), ylim=c(1,100),
	ylab="Sample nº", xlab="", main="Cinza: correct H0 (IC)")

#linha vertical no 10
abline(v=10, lty=2)

for (j in 1:100){
	lines(c(CIlower[j], CIupper[j]), c(j,j), col=color[j],lwd=2)}



#exemplo do gujarati para uma vez (pag.88)

#b1= 20 e b2=0.6, n=25

x = c(1:25)
y = numeric(25)
u = runif(25) #os 25 erros uniformemente distribuidos

for (j in 1:25){
	y_i = 20+0.6*x[j] + u[j]
	y[j] = y_i}
y
fit = lm(y~x)

summary(fit)

coefficients(fit)[2]

#para 1000 vezes

x = c(1:25)
b1 = numeric(10000)
b2 = numeric(10000)


for (i in 1:10000){
	y = numeric(25)
	u = runif(25)
	
	for (j in 1:25){
		y_i = 20+0.6*x[j] + u[j]
		y[j] = y_i}
	fit = lm(y~x)
	b1[i] = coefficients(fit)[1]
	b2[i] = coefficients(fit)[2]
}
mean(b1)	#como a media é perto do que achavamos, o montecarlo diz que o
mean(b2)	#estimador é nao viesado

#fazer teste de media


	

