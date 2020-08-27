#aula 1 do curso de introdução do prof Vale

x = 1
y = TRUE

class(x)
class(y)
rm(x)   #limpa o x da memoria
x

#vetores

x = c(1,5,6)
View(x) #forma de tabela
class(x)

nomes = c("lindo", "salve")
class(nomes)
g = c("salve", TRUE, 9) #vira caractere
class(g)

q = seq(2,4,0.25) # de onde, para onde, qual o tamanho do passo
q

q = seq(2,4,length=5) 
q

pi^2 # = g kkk

rep(1:2, times=3)

rep(1:2, each=3)

rm(list=ls()) #limpa toda a memoria e ctrl + L limpa o console

g = c(1, 5, "salve", 2, 6)
g
b = g[-3] #copia o vetor menos o 3 elemento
b
n = g[c(1,5)] #pega  o elemento 1 e o 5
n

v = c(1, 2, 2, 4, 5)
v
f = v[v != 2 & v > 3] #subconjunto com operador lógico
f

v[v %in% c(1,2)] #elemtos de v dento do outro conjunto

v >= 3 #retorna n bools

which(b > 2) #posicao dos maiores que 2

order(v)

length(v)

cumsum(v) #soma comulativa

is.na(v) #bool (posicao) dos NA

paste(v, f) #no formato de caracther

table(v)

unique(v) #só os unicos