#exemplo 12.1: calculando a curva de philips e a curva de philips aumentada

library(foreign);library(dynlm);library(lmtest)

data_phillips = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\PHILLIPS.dta")

head(data_phillips)


tsdata = ts(data_phillips, start=1948)

#curva de phillips normal
reg.s = dynlm(inf ~ unem, data=tsdata,end=1996)
residual.s = resid(reg.s)

coeftest(dynlm(residual.s ~ L(residual.s))) #erros e t statistic dos 
								 #residuos com eles laggados,
								 # que é forte evidencia de correlação

#curva de phillips aumentada

reg.ea = dynlm(d(inf) ~ unem, data=tsdata, end=1996)

residual.ea = resid(reg.ea) #pegar os residuos

coeftest(dynlm(residual.ea ~ L(residual.ea)))


##############
bgtest(reg.ea, type="F", order=1) # é pra automatizar, mas não sei como
##############


#exemplo 12.3: autocorrelacao nos erros com AR(3)

library(foreign);library(dynlm);library(car);library(lmtest)

barium = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\BARIUM.dta")

tsdata = ts(barium, start=c(1978, 2), frequency=12)
reg = dynlm(log(chnimp) ~ log(chempi) + log(gas) + log(rtwex) +
				    befile6 + affile6 + afdec6, data=tsdata)

#teste manual

residual = resid(reg)
resreg = dynlm(residual ~ L(residual) + L(residual, 2) +
					L(residual, 3) + log(chempi) + 
					log(gas)+log(rtwex)+befile6+
					affile6+afdec6, data=tsdata)

linearHypothesis(resreg, 
			c("L(residual)", "L(residual, 2)", "L(residual, 3)"))




#teste automatico usando o Lagrange multiplier (dá o resultado um pouco diferente do livro):

bgtest(reg, order=3, type="F")


#exmplo do teste Durbin-watson, usado na msm ideia que o LM

library(foreign);library(dynlm);library(lmtest)

data_phillips = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\PHILLIPS.dta")

tsdata = ts(data_phillips, start=1948)

reg.s = dynlm(inf ~ unem, data=tsdata, end=1996)

reg.ea = dynlm(d(inf) ~ unem, data=tsdata, end=1996)

dwtest(reg.s) #rejeitada

dwtest(reg.ea) #aumentada rejeitada



#exemplo 12.4: Cochrane-Orcutt (FGLS estimation)

library(foreign);library(dynlm);library(car);library(orcutt)

data_barium = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\barium.dta")

tsdata = ts(data_barium, start=c(1978,2), frequency=12)

olsres = dynlm(log(chnimp) ~ log(chempi)+log(gas)+log(rtwex)+
			befile6+affile6+afdec6, data=tsdata) 
cochrane.orcutt(olsres)



#exemplo 12.7: serial correlation-robust inference with ols

#a heterocedasticidade e correlacao serial sao unbiased e consistent,
# mas o erro padrao sao prejudicados; para arrumar, temos

library(foreign);library(dynlm);library(lmtest);library(sandwich)

data_puertorico = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\prminwge.dta")
head(data_puertorico)

tsdata = ts(data_puertorico, start=1950)

reg = dynlm(log(prepop) ~ log(mincov) + log(prgnp) + log(usgnp) + trend(tsdata), data=tsdata)

#resultados normais
coeftest(reg)


#resultados com a correção
coeftest(reg, vcovHAC)


#exemplo 12.9 arch em stocks returns

library(foreign);library(dynlm);library(lmtest)

nyse = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\nyse.dta")

tsdata = ts(nyse)

reg = dynlm(return ~ L(return), data=tsdata)

residual.sq = resid(reg)^2

ARCHreg =  dynlm(residual.sq ~L(residual.sq))

coeftest(ARCHreg) #extremamente significativo


#exemplo 12.7
















