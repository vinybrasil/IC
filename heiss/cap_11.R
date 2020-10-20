#cap 11 do heiss

#exemplo 11.4: efficient markets hypothesis

library(foreign);library(dynlm);library(stargazer)

nyse = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\NYSE.DTA")

head(nyse) #cprice é o price lagged

tsdata = ts(nyse) #o indice vira t

#tres regressoes, com cada um com um lag a mais

reg1 = dynlm(return~L(return), data=tsdata)
reg2 = dynlm(return~L(return)+ L(return,2), data=tsdata)
reg3 = dynlm(return~L(return)+ L(return,2)+ L(return,3), data=tsdata)

#comparar as três regressoes (nenhuma é significitiva)

stargazer(reg1, reg2, reg3, type="text", keep.stat=c("n", "rsq", "adj.rsq", "f"))



####fazendo a msm coisa pros dados da GE

library(zoo);library(pdfetch);library(dynlm);library(stargazer)

data_GE = zoo(pdfetch_YAHOO("GE", fields="adjclose",
						from="2000-01-01", to="2013-12-31"))

ret = diff(log(data_GE$GE))  #calcula a diferença de dia pra dia, nao importa
					  #se é fim de semana
plot(ret)

reg1 = dynlm(ret~L(ret))
reg2 = dynlm(ret~L(ret)+L(ret,2))
reg3 = dynlm(ret~L(ret)+L(ret,2)+L(ret,3))

stargazer(reg1, reg2, reg3, type="text", keep.stat=c("n", "rsq", "adj.rsq", "f"))

#script 11.3: simulating random walks sem drift

set.seed(348546)
plot(c(0,50), c(0,0), type="l", lwd=2, ylim=c(-18,18)) #linha

#loop gerando as random walks 

for (r in 1){            #o originial é 1:30
	e = rnorm(50)
	y = ts(cumsum(e))  #como gera numeros aleatorios, a curva "cai"
	lines(y, col=gray(.6))}


#script 11.4: simulating random walks com drift

set.seed(348546)
plot(c(0,50), c(0,100), type="l", lwd=2) #linha

for (r in 1){
	e = rnorm(50)
	y = ts(cumsum(2+e)) #drift
	lines(y, col=gray(.6))
}

#script 11.5 Diff da random walk com drift

set.seed(348546)
plot(c(0,50),c(2,2),type="l",lwd=2,ylim=c(-1,5)) #a0 = 2

for(r in 1:30){
	e = rnorm(50)
	y = ts(cumsum(2+e))
	dy = diff(y)
	lines(dy, col=gray(.6))
}


#exemplo 11.6:regressao na diff

library(foreign);library(dynlm);library(stargazer)


fertil = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\fertil3.dta")

head(fertil)

tsdata = ts(fertil, start=1913)

res1 = dynlm( d(gfr) ~ d(pe), data=tsdata)

res2 = dynlm( d(gfr) ~ d(pe) + L(d(pe)) + L(d(pe),2),data=tsdata)

stargazer(res1, res2, type="text") #agora é significativo


