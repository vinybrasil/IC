#computer exercises do wooldrigde


#c1:teste para ver se ter AR(1) nos erros

library(foreign);library(dynlm);library(lmtest);library(car);library(orcutt);library(prais);library(sandwich)

data_fertil = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\FERTIL3.dta")

head(data_fertil)

tsdata = ts(data_fertil)

residuos = resid(dynlm(cgfr ~cpe + L(cpe) + L(cpe,2), data=tsdata  ))

coeftest(dynlm(residuos ~ L(residuos))) #extremamente significativo

#c2

#i) 

data_wage = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\WAGEPRC.dta")
head(data_wage)
tsdata = ts(data_wage)
residuos = resid(dynlm(gprice ~ gwage + L(gwage) + L(gwage,2) + L(gwage,3) + L(gwage,4) + L(gwage,5) + L(gwage,6) + L(gwage,7) +
							   L(gwage,8) + L(gwage,9) + L(gwage,10) + L(gwage,11) + L(gwage,12), data=tsdata ))


coeftest(dynlm(residuos ~ L(residuos)))  #extremamente significativo


#ii) 

#modelo = dynlm(gprice ~ gwage + L(gwage) + L(gwage,2) + L(gwage,3) + L(gwage,4) + L(gwage,5) + L(gwage,6) + L(gwage,7) +
#							   L(gwage,8) + L(gwage,9) + L(gwage,10) + L(gwage,11) + L(gwage,12), data=data_wage)

modelo = lm(gprice ~ gwage + gwage_1 + gwage_2 + gwage_3 + gwage_4 + gwage_5 + gwage_6 + gwage_7 + gwage_8 + gwage_9 + gwage_10 + gwage_11 + gwage_12, data=data_wage)

cochrane.orcutt(modelo)

#1.109778 é LRP

#iii)

data_wage$gwage_1_1 = data_wage$gwage - data_wage$gwage_1
data_wage$gwage_2_1 = data_wage$gwage - data_wage$gwage_2
data_wage$gwage_3_1 = data_wage$gwage - data_wage$gwage_3
data_wage$gwage_4_1 = data_wage$gwage - data_wage$gwage_4
data_wage$gwage_5_1 = data_wage$gwage - data_wage$gwage_5
data_wage$gwage_6_1 = data_wage$gwage - data_wage$gwage_6
data_wage$gwage_7_1 = data_wage$gwage - data_wage$gwage_7
data_wage$gwage_8_1 = data_wage$gwage - data_wage$gwage_8
data_wage$gwage_9_1 = data_wage$gwage - data_wage$gwage_9
data_wage$gwage_10_1 = data_wage$gwage - data_wage$gwage_10
data_wage$gwage_11_1 = data_wage$gwage - data_wage$gwage_11
data_wage$gwage_12_1 = data_wage$gwage - data_wage$gwage_12



modelo = lm(gprice ~  gwage + gwage_1_1 + gwage_2_1 + gwage_3_1 + gwage_4_1 + gwage_5_1 + gwage_6_1 + gwage_7_1 + gwage_8_1 + gwage_9_1 + gwage_10_1 + gwage_11_1 + gwage_12_1, data=data_wage)
summary(modelo)


#se da LRP = 0.1107317

#c3: 

data_inven = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\INVEN.dta")
tsdata =ts(data_inven)
summary(lm(cinven ~ cgdp, data=tsdata)) #gdp é significativo

residuos = resid(lm(cinven ~ cgdp, data=data_inven))

coeftest(dynlm(residuos ~ L(residuos)))

cochrane.orcutt(lm(cinven ~ cgdp, data=data_inven)) #intercepto: 2.556473    beta: 0.152384 


#c4: não tá dando certo regredir os residuos, nao sei o pq

data_nyse = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\NYSE.dta")
head(data_nyse)

#residuos = resid(lm(return ~ return_1, data=data_nyse))
#length(residuos)
#length(data_nyse$return)
#data_nyse$residuos_quad = 0

#summary(lm( residuos_quad ~ return_1))

#c5: 

consumption = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\CONSUMP.dta")
names(consumption)
head(consumption)

#i)
summary(lm(gc ~ gy, data=consumption))
residuos = resid(lm(gc ~ gy, data=consumption))
ar = dynlm(residuos ~ L(residuos))
summary(ar)


#ii)
residuos = resid(dynlm(gc ~L(gc), data=consumption))
consumption$gc_1_2 = consumption$gc_1 ^ 2
ar = lm(residuos[2:length(residuos)] ~ gc_1 + gc_1_2, data=consumption)
summary(ar)

#c7:

data_bario = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\BARIUM.dta")
head(data_bario)

#i)

cochrane.orcutt(lm(log(chnimp) ~ log(chempi) + log(gas) + log(rtwex) + befile6 + affile6 + afdec6,data=data_bario))

#ii)

prais_winsten(lm(log(chnimp) ~ log(chempi) + log(gas) + log(rtwex) + befile6 + affile6 + afdec6,data=data_bario),data=data_bario)

length(data_bario)

#c8: 

data_trafego = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\TRAFFIC2.dta")
head(data_trafego)

#i)

residuos = resid(lm(prcfat ~ t + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec + wkends + unem + spdlaw + beltlaw,data=data_trafego))

summary(dynlm(residuos ~ L(residuos)))

#ii)




model = lm(prcfat ~ t + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec + wkends + unem + spdlaw + beltlaw,data=data_trafego)
summary(model)
NeweyWest(model, lag = 4)

coeftest(model, vcovHAC)

prais_winsten(lm(prcfat ~ t + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec + wkends + unem + spdlaw + beltlaw,data=data_trafego),data=data_trafego)


#c9:

data_peixe = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\FISH.dta")
head(data_peixe)

#i)

summary(lm(lavgprc ~ mon + tues + wed + thurs, data=data_peixe)) #nao

#ii) 

summary(lm(lavgprc ~ mon + tues + wed + thurs + wave2 + wave3 , data=data_peixe)) #wave 2 e wave3 sao significantes a 1%


#iii)

summary(lm(lavgprc ~ mon + tues + wed + thurs + wave2 + wave3 + t, data=data_peixe)) #aumenta a significancia de wave2

#iv) pq o preço depende da demanda e da oferta, não do dia da semana

#v)

residuos = resid(lm(lavgprc ~ mon + tues + wed + thurs + wave2 + wave3 + t, data=data_peixe))
summary(dynlm(residuos ~ L(residuos)))

#vi)
NeweyWest(lm(lavgprc ~ mon + tues + wed + thurs + wave2 + wave3 + t, data=data_peixe), lag = 4)

#vii)
model = lm(lavgprc ~ mon + tues + wed + thurs + wave2 + wave3 , data=data_peixe)
prais_winsten(lm(lavgprc ~ mon + tues + wed + thurs + wave2 + wave3 , data=data_peixe), data=data_peixe)

linearHypothesis(model, c("wave2=0", "wave3=0"))


#C10:

data_phillips = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\PHILLIPS.dta")
head(data_phillips)

#i)

model = lm(inf ~ unem,data=data_phillips)
summary(model)

#ii)

residuos = resid(model)
summary(dynlm(residuos ~L(residuos)))

#iii)

prais_winsten(model,data=data_phillips)

#iv)
cochrane.orcutt(model)

#C11:

nyse = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\nyse.dta")
tsdata = ts(nyse)

#i)

reg = dynlm(return ~ L(return), data=tsdata)
residual.sq = resid(reg)^2
max(residual.sq)
min(residual.sq)
mean(residual.sq)

#ii)



#C12)

inven =  read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\INVEN.dta")
head(inven)
tsdata = ts(inven)

#i)
model = lm(cinven ~cgdp, data=tsdata)
residuo = resid(model)
summary(dynlm(residuo ~L(residuo)))

#ii)

prais_winsten(model,data=tsdata)


#C13) não tem o arquivo zzz

#C14) não tem o arquivo zzz

#C15)

#I)

data_bario =  read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\BARIUM.dta")
















