#data: https://www.cengage.com/aise/economics/wooldridge_3e_datasets/, em statafiles.zip

#capitulo 10

#exemplo de modelo de serie temporal estatico (10.2)

#efeito da inflacao e deficit na taxa de juros

library(foreign)
library(tidyverse)

intdef <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/intdef.dta")
View(intdef)
?ts

typeof(intdef)
df = data.frame(intdef)
names(df)
plot(intdef$year,intdef$inf)
print(columns(intdef))
head(df)

#fazendo a regressao

summary(lm(i3~inf+def, data=intdef))


plot(intdef$def,intdef$i3)



#exemplo de serie equiespacada: barium

barium = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\BARIUM.dta")
head(barium)

#colocando em ts (comeca no 2º mes de 1978)
impts = ts(barium$chnimp, start=c(1978,2), frequency=12)
plot(impts)

#lidar com series temporais irregulares (nao equiespacadas) é com o zoo


#pdfetch: base de dados com varios orgaos gov dos USA [NAO FUNCIONA]
library(pdfetch)
tickernames = c("^gspc", "^ixic", "AAPL")
yahoo = pdfetch_YAHOO(tickernames, fields="adjclose", from="2000-01-01")


#exemplo 10.4: FDL(finite distributed lag models) testando os fatores que
#mudam a fertilidade(ww2, pilula anticoncepcional e personal tax

library(foreign);library(dynlm);library(lmtest);library(car)

fertilidade = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\FERTIL3.dta")

head(fertilidade)

tsdata = ts(fertilidade, start=1913) #le por ano
#L(x) é x lagado uma vez; L(x, p) é x lagado p tempos

res = dynlm(gfr ~ pe + L(pe) + L(pe, 2) + ww2 + pill, data=tsdata)
coeftest(res)
pilula = ts(fertilidade$pill, start=1913) #dummy zzz
plot(pilula)

fertil = ts(fertilidade$gfr, start=1913) #dummy zzz
plot(fertil)


#F teste para H0: todos os coeficientes sao zero

linearHypothesis(res, matchCoefs(res,"pe")) # o 0.01165 aponta para um problema 
								 # de multicolinearidade

#calculando a propensao de LP, onde somasse os deltas

b=coef(res)
b
b['pe']+b['L(pe)']+b['L(pe, 2)']

#F teste para H0: LRP = 0

linearHypothesis(res, "pe + L(pe) + L(pe, 2) = 0") #é significativo



#exemplo 10.7: trends nos precos das casas (sem o trend sao significativos)

library(foreign);library(dynlm);library(stargazer)

hseinv = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\hseinv.dta")

tsdata=ts(hseinv, start=1947)

#LM com lags mas sem trend
res1 = dynlm(log(invpc) ~ log(price), data=tsdata)   #preco signicativo

#LM com trend e lags
res2 = dynlm(log(invpc) ~ log(price) + trend(tsdata), data=tsdata) #preco nao é

stargazer(res1, res2, type='text')



#exemplo 10.11: sazonalidade no dataset do bario

library(foreign);library(dynlm);library(lmtest)

barium = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\BARIUM.dta")
tsdata = ts(barium, start=c(1978,2), frequency=12)

res = dynlm(log(chnimp) ~ log(chempi)+log(gas)+log(rtwex)+befile6+ affile6+afdec6+season(tsdata), data=tsdata)
coeftest(res)	#nao sao significativas				




