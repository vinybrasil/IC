#solucao dos computer exercises 

#joint hypotesis: https://www.econometrics-with-r.org/7-3-joint-hypothesis-testing-using-the-f-statistic.html

#exercicio c1

library(foreign);library(tidyverse)

juros = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\INTDEF.DTA")

View(juros)

names(juros)

juros$dummy = ifelse(juros$year > 1979, 1, 0)

juros_tibble = as_tibble(juros)
juros_tibble

juros_tibble %>% select(year, dummy)

summary(lm(juros$i3 ~ juros$inf+juros$def+juros$dummy)) #a dummy é siginificativa

#exercicio c2

barium = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\BARIUM.dta")

as_tibble(barium$t)

names(barium)

summary(lm(log(barium$chnimp) ~ barium$t +  log(barium$chempi) + log(barium$gas) + log(barium$rtwex) + barium$befile6 + barium$affile6 + barium$afdec6))

impts = ts(barium$chnimp, start=c(1978,2), frequency=12)
plot(impts)

model = lm(chnimp ~ t + chempi +gas + rtwex + befile6 + affile6 + afdec6, data=barium)
summary(model)

#testado a joint sigficance, fora a time trend (não funcionou, diz que a matrix é singular)
library(car)
linearHypothesis(model, c("gas=0", "chempi=0", "rtwex=0", "befile6=0", "affile6=0", "afdec6=0"))

model = lm(chnimp ~ t + chempi +gas + rtwex + befile6 + affile6 + afdec6 + feb + mar+ apr+ may + jun + jul + aug+ sep + oct+ nov + dec, data=barium)


#exercicio c3

puerto_rico = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\PRMINWGE.dta")
names(puerto_rico)

summary(lm(log(prepop) ~   log(mincov) + log(usgnp) + t, data=puerto_rico)) #tudo é significativo com a trend
plot(ts(puerto_rico$prepop))

summary(lm(log(prepop) ~   log(mincov) + log(usgnp) + t + log(prgnp), data=puerto_rico))


#exercicio c4

library(dynlm)

fertilidade = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\FERTIL3.dta")
tsdata = ts(fertilidade, start=1913)

res = dynlm(gfr ~ pe + L(pe)  + L(pe, 2) + ww2 + pill, data=tsdata)
summary(res)


#exercicio c5

ez = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\EZANDERS.DTA")
as_tibble(ez)
names(ez)
modelo = (lm(log(uclms) ~ feb + mar + apr + may+ jun + jul + aug + sep + oct + nov + dec, data=ez))

#exercicio c6: detrending

fertility = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\FERTIL3.DTA")

names(fertility)

model = lm(gfr ~ t + tsq, data=fertility)
summary(model)
dot_gfr = residuals(model)
fertility$dot_gfr = dot_gfr
summary(lm(gfr ~ t + tsq + ww2 + pe + pill , data=fertility))
model = lm(dot_gfr ~ t + tsq + ww2 + pe + pill , data=fertility)
summary(model) #as variaveis ainda sao significativas mesmo com a variavei detrended 
fertility$t_3 = fertility$t^3

summary(lm(gfr ~ t + tsq + t_3 + ww2 + pe + pill , data=fertility)) #é significativo mas nao faz sentido logico


#exercicio c7

consumption = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\CONSUMP.dta")
names(consumption)
head(consumption)
summary(lm(gc ~ gy, data=consumption)) #nao sei qual variavel usar



#exercicio c8 

fertility = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\FERTIL3.DTA")

names(fertility)

model = (lm(gfr ~ pe_3 + pe_2 + pe_1 + pe_4 + ww2 + pe + pill , data=fertility))
summary(model)
library(car)
linearHypothesis(model, c("pe_3=0", "pe_4=0")) #há pouca força contra h0

fertility$pet_1_menos_pet = fertility$pe_1 - fertility$pe
fertility$pet_2_menos_pet = fertility$pe_2 - fertility$pe
fertility$pet_3_menos_pet = fertility$pe_3 - fertility$pe
fertility$pet_4_menos_pet = fertility$pe_4 - fertility$pe

model = (lm(gfr ~ pet_1_menos_pet + pet_2_menos_pet + pet_3_menos_pet + pet_4_menos_pet + ww2 + pe + pill , data=fertility))
summary(model)
sum(coefficients(model))



#exercicio c9


volat = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\VOLAT.dta")
names(volat)
library(Hmisc)
describe(volat)

model = lm(rsp500 ~ pcip + i3, data=volat)
summary(model) #nada é significativo fora o juros pq é do msm período


#exercicio c10

int = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\INTDEF.dta")
head(int)

cor(int$inf, int$def) #muito baixa

library(dynlm)

#modelo lagado
res = dynlm(i3 ~ inf + inf_1 + def_1 + def, data=int)
summary(res)


#exercicio c11

trafego = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\TRAFFIC2.dta")
head(trafego)

subset(trafego, spdlaw == 1) 
model = lm(log(totacc) ~ t + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec + wkends + unem + spdlaw + beltlaw, data=trafego)
summary(model)

mean(trafego$prcfat) #porcentagem de acidentes com pelo menos uma morte.
max(trafego$prcfat) #no ano maximo, em 1.2% dos acidentes houve morte

model = lm(log(prcfat) ~ t + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec + wkends + unem + spdlaw + beltlaw, data=trafego)
summary(model)


#exercicio c12

philips = read.dta("C:\\Users\\vinic\\Desktop\\R\\heiss\\data_wooldridge\\PHILLIPS.dta")
names(philips)
describe(philips)

model = lm(inf ~ unem, data=philips)
summary(model) #???


