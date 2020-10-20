#exercicio 5: nível de preços com os salários

library(foreign);library(dynlm);library(stargazer)

rm(list=ls())

wages = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\WAGEPRC.dta")

head(wages)

model = lm(gprice ~ gwage + gwage_1 + gwage_2 + gwage_3 + gwage_4 + gwage_5 + gwage_6 + gwage_7 + gwage_8 + gwage_9 + gwage_10 + gwage_11 + gwage_12, data=wages)

summary(model) 


#exercicio c1

data_house = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\HSEINV.dta")
head(data_house)

#a) calculando a first order autocorrelation do log do investimento


#=========NAO FUNCIONA====================


d = acf(data_house$linvpc) #first order: 0.594, no livro dá .639
d

plot(1:length(data_house$linvpc), data_house$linvpc)

#achando os residuos para detrend a serie

modelo_detrend = lm(linvpc ~ t, data=data_house)
summary(modelo_detrend)
dot_linvpc = residuals(modelo_detrend)
data_house$dot_linvpc = dot_linvpc

#achando a autocorrelacao apos o detrend

d = acf(data_house$dot_linvpc, lag.max=1)
d

plot(1:length(dot_linvpc), dot_linvpc)

#calculando a first order autocorrelation para o preço

d = acf(data_house$lprice, lag.max=1) #deu errado tambem
d

modelo_detrend_price = lm(lprice ~ t, data = data_house)
dot_price = residuals(modelo_detrend_price)
d = acf(dot_price, lag.max=1)
d

plot(1:length(dot_price), dot_price) #tem o autocorrel maior., logo tem
							#a maior chance de ser unit root


#===============================

#solução:


#log(inv)
n = length(data_house$linvpc)
x_t0 <- data_house$linvpc[-1] 
x_t1 <- data_house$linvpc[-n]
cor(x_t0, x_t1) #0.6391246


acf(data_house$linvpc, lag.max = 1, plot = FALSE) #diferente mas é pra ser assim

#cor(x_t1, x_t0) * (n-1)/n #difference dos metodos 
				     #(https://www.datacamp.com/community/tutorials/autocorrelation-r)

#modelo detrended inv

modelo_detrend = lm(linvpc ~ t, data=data_house)
summary(modelo_detrend)
dot_linvpc = residuals(modelo_detrend)
data_house$dot_linvpc = dot_linvpc

n = length(data_house$dot_linvpc)
x_t0 <- data_house$dot_linvpc[-1] 
x_t1 <- data_house$dot_linvpc[-n]
cor(x_t0, x_t1) #0.4847401


#log(price)
n = length(data_house$lprice)
x_t0 <- data_house$lprice[-1] 
x_t1 <- data_house$lprice[-n]
cor(x_t0, x_t1) #0.9491586


#detrended log(price)

modelo_detrend_price = lm(lprice ~ t, data = data_house)
dot_price = residuals(modelo_detrend_price)
n = length(dot_price)
x_t0 <- dot_price[-1] 
x_t1 <- dot_price[-n]
cor(x_t0, x_t1) #0.8215256


#b) estimar linvpc ~ b0 + b1*diff(log(price)) + b2t

data_house$gprice_manual = diff(data_house$lprice)

model = lm(linvpc ~ gprice + t, data=data_house)
summary(model)

#forma alternativa se nao tivesse o gprice

gprice_manual <- diff(data_house$lprice)
gprice_manual = append(gprice_manual, NA, 0)
data_house$gprice_manual = gprice_manual
model = lm(linvpc ~ gprice_manual + t, data=data_house)
summary(model)


#c) detrend linvpc e usar como variavel dependente em b)

model = lm(linvpc ~ t, data=data_house)
data_house$linvpc_detrended = residuals(model)
model = lm(linvpc_detrended ~ gprice + t, data=data_house)
summary(model)

#d) diff(linvpc) ~ gprice_manual + t

model = lm(ginvpc ~ gprice + t, data=data_house)
summary(model)



#c2: salário e produtividade por hora

#a) estimar ghrwage ~ goutphr + goutph_1 (lembra que g é a diff do log)

data_earns = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\EARNS.dta")
head(data_earns)

summary(lm(ghrwage ~ goutphr + goutph_1, data=data_earns))

#b) isola b1 em theta = b1 + b2 e agora regressa
	# y ~ b0 + theta*x + b2(x2 - x1)

#falhamos em rejeitar h0: theta = 1 (aumento na produtividade é repassado integralmente ao salário)
summary(lm(ghrwage ~ goutphr + append(diff(goutphr), NA, 0), data=data_earns))

#c) tentando colocar goutph_2

summary(lm(ghrwage ~ goutphr + goutph_1 + goutph_2, data=data_earns)) 


#c3:

data_nyse = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\NYSE.dta")
head(data_nyse)

#a) regredir r = b0 + b1(r-1) + r2(r - 1)^2

#usar a função I()
summary(lm(return ~ return_1 + I(return_1^2), data=data_nyse))

#alternativa:

summary(lm(return ~ poly(return_1, 2, raw = TRUE), data=data_nyse))

#b) testar a hipotese nula 

#teste f, onde h0: b1 = b2 = 0, que dá F=2.16 e p=.116 (já tá ali em cima)

#c) testar r = (r-1) * (r-1)*(r-2)


#NAO TA DANDO CERTO, OLHAR DE NOVO

library(foreign);library(dynlm);library(stargazer)

library(Hmisc)

tsdata = ts(data_nyse)

a_start<-8 
b_start<-2*log(2)/a_start
c <- 2
data_nyse$return_2 = Lag(data_nyse$return_1, +1)
summary(nls(return ~ a*return_1 + b*return_1*return_2 + c, data=data_nyse, start=list(a=a_start,b=b_start, c = c)))


#d) não dá pra prever returnos futuros com base nos retornos passados


#c4:

data_phillips = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\PHILLIPS.dta")
head(data_phillips)

summary(lm(cinf ~ cunem, data=data_phillips))

#c5:

fertility = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\FERTIL3.DTA")
head(fertility)

#a) colocar t na equacao

summary(lm(cgfr ~ cpe + cpe_1 + cpe_2 +t, data=fertility)) #nao precisa de t


#b) dropar t e colocar ww2 e pill. depois, testar a 5% a joint significance

model = lm(cgfr ~ cpe + cpe_1 + cpe_2 + ww2 + pill, data=fertility)
summary(model)
linearHypothesis(model, c("ww2=0", "pill=0"))


#c)

model = lm(cgfr ~ cpe + cpe_1 + cpe_2 + ww2 + pill + t, data=fertility)
summary(model)

fertility$diff_cpe_1 = fertility$cpe_1 - fertility$cpe
fertility$diff_cpe_2 = fertility$cpe_2 - fertility$cpe

model = lm(cgfr ~ cpe + diff_cpe_1 + diff_cpe_2 + ww2 + pill , data=fertility)
summary(model)

#deveria dar -.075 o theta
 

#c6:

data_inven = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\INVEN.dta")
head(data_inven)
#inventory = estoques?

#a)

summary(lm(cinven ~ cgdp, data=data_inven)) #gdp é significativo

#b)

summary(lm(cinven ~ cgdp + r3 , data=data_inven)) #r3 nao é

#c)

summary(lm(cinven ~ cgdp + cr3 , data=data_inven)) #nao melhora com a primeira diferenca

 

#c7:

data_consump = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\CONSUMP.dta")
head(data_consump)

#a)

summary(lm(gc ~ gc_1, data=data_consump)) #dá pra prever com o anterior

#b) 

model = lm(gc ~ gc_1 + gy_1 + r3_1 +lag(inf), data=data_consump) #só a inf é significativa
summary(model)
linearHypothesis(model, c("gc_1=0", "gy_1=0", "r3_1=0", "lag(inf)=0"))
#significativo a 0.001

#c) caiu, o que faz com que b nao dê suporte à hipotese mais


#d) #F-statistic: 5.475 on 4 and 30 DF,  p-value: 0.00197, sim



#c8:

#a)

data_phillips = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\PHILLIPS.dta")
head(data_phillips)
tsdata = ts(data_phillips, start=1948)
model = dynlm(unem ~ L(unem), data=tsdata)
summary(model) #altamente significativo

#b)
?dynlm

#this is f*cking stupid, man

predict(model, newdata=data.frame(unem=5.672923)) #1997
predict(model, newdata=data.frame(unem=5.726328 )) #1998
predict(model, newdata=data.frame(unem=5.765439)) #1999
predict(model, newdata=data.frame(unem=5.794083 )) #2000
predict(model, newdata=data.frame(unem=5.81506 )) #2001
predict(model, newdata=data.frame(unem=5.830423 )) #2002
predict(model, newdata=data.frame(unem=5.841674 )) #2003
predict(model, newdata=data.frame(unem=5.849914 )) #2004

#desemprgo em 2004: 5.855948


#c) Adicionando a inflação laggada
tail(data_phillips)

model = dynlm(unem ~ L(unem) + L(inf), data=tsdata) #melhorou
predict(model)
i = 1996
while (i < 2005){
print("salve")
i = i + 1}

