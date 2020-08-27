#aula 2 do curso de intro do R do vinicius vale
#25/08/2020

v = matrix(1, ncol = 10, nrow = 10)
v
v = matrix(data = seq(1,100), ncol = 10, nrow = 10)

v = matrix(data = seq(1,100), ncol = 10, nrow = 10, byrow=TRUE)
v
is.matrix(v)

c1 = c(-1,4)
c2 = c(3,2)

x = cbind(c1, c2) #cada vetor é uma coluna
x

x = rbind(c1, c2) #cada vetor é uma linha
x

c = cbind(c(-1,4), c(3,2))
c

N = matrix(sample(c("salve", "quebrada"), 25, replace=TRUE)

d = c[,1]
d

c / 10
c[c>= 1]
h = c[, c(1:2)]
h

c[1,] = c[1,] * 10 #multiplica a primeira linha por 10
c

sum(c)
sd(c)
range(c)

rowSums(c)
rowMeans(c)
summary(c)
colSums(c)

t(c) #transposta

diag(c)

det(c)

q = cbind(c(0,4), c(3,2))
q
c + q

c %*% q

c * q             #elemento por elemento

diag(3)           #cria a matriz identidade 3x3

install.packages("wooldridge")


library(wooldridge)
data("wage1")
str(wage1)
class(wage1)
?wage1
View(wage1)
wage1
install.packages('tibble')
library(tibble)
wage1tib = as_tibble(wage1)
class(wage1tib)
View(wage1tib)
wage1tib


wage1[2, 3] #item em 2,3
wage1$educ  #pega essa coluna
wage1[, c("wage", "educ")]


subset(wage1, wage > 10 & educ > 10)

head(wage1)
tail(wage1)

wage1$wage2x = wage1$wage * 2
View(wage1)

library(readxl)
library(readr)


#se o separador é ;, usa o read_csv2


dados = xlsx::read.xlsx() #usa a função read.xlsx do pacote xlsx

rm(list = ls()[!ls() %ini% c("dexp")]) #só nao deleta o dexp


library(dplyr)
library(tidyr)

names(exp)
dexp_mod = select(dexp, c("CO_ANO", "CO_MES"))
dexp_mod = rename(dexp_mod, ano = CO_ANO, mes = CO_MES)
dexp_mod = mutate(dexp_mod, log_exp = log(exp))

dexp_mod = group_by(dexp_mod,ano,uf) #o que preserva

dexp_mod = summarise(dexp_mod, exp = sum(exp))

dexp_pr = filter(dexp_mod, uf == "PR")


#tudo isso é mudado por:

dexp_mod2 = dexp %>% select("CO_ANO", "CO_MES", "SG_UF_NCM", "CL_FOB") %>%
	rename(
	  ano = CO_ANO,
	  mes = CO_MES,
	  uf = SG_UF_NCM,
       exp = VL_FOB) %>% mutate(log_exo = log(exp)) %>% 
	  mutate(dexp_mod, log_exp = log(exp)) %>% 
		mutate(dexp_mod, log_exp = log(exp))  %>% 
		group_by(dexp_mod,ano,uf)  %>% 
	summarise(dexp_mod, exp = sum(exp))





