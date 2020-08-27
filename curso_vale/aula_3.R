#aula 3 do curso de introdução ao R do Prof Vinicius Vale


#há um formato RDS que é específico do R

#ordenar colunas 

dexp_mod = dexp_mod %>% select("log_exp", "exp")

#ordem crescente de acordo com a coluna exp

dexp_mod = dexp_mod %>% arrange(exp) %>% ...

#o %>% é o pipeline

#decrescente

dexp_mod = dexp_mod %>% arrange(desc(exp))

#exportar dados 

library(openxlsx) #sem java

library(xlsx) #com java

getwd()
setwd()

write_csv(dexp_mod, "dados_prontos.csv", na="NA")
write_csv2(dexp_mod, "dados_prontos.csv")
write_delim(dexp_mod, "dados_prontos.csv", delim=";")
saveRDS(dexp_mod, file="dados.rds")
dados = readRDS(file = "dados.rds") #unico objeto
save(dexp, dexp_mod, dexp_pr, file="Exp.RData") #varios objetos
 
write_xlsx(dexp_mod, file = "dados_prontos.xlsx", sheetName="shit")

library(ggplot2)

load("Exp.RData")

#barra
ggplot(data = dexp_mod, aes(x = uf, y = exp)) +
	geom_col(fill = "blue")

#barras crescentes
ggplot(data = dexp_mod, aes(reorder(uf, exp), exp)) + 
	geom_col(fill = "blue") +
	xlab("Unidades da federação") +
	ylab("Valor em US$ bi") +  
	ggtitle("Exportações) + 
	labs(subtitle = "2019") + 
	labs(caption = "Fonte: seu cu") +
	theme(plot.title = element_text(hjust = 0.5)) +
	theme(plot.sibtitle = element_text(hjust = 0.5))+
	theme(plot.caption = element_text(hjust=0)) +
	scale_y_continous(limits = c(0,100), 
				 breaks = seq(from 0, to = 50, by = 5)) +
	geom_text(aes(label = round(exp, digits = 2)), vjust = -0.5, size = 3)

#0 é esqueda e 1 é direita

#barra decrescentes
ggplot(data = dexp_mod, aes(reorder(uf, -exp), exp)) + 
	geom_col(fill="blue)

#mapas: geobr

install.packages("sf")
install.packages("geobr")

library(sf)
library(geobr)
library(dplyr)

load("Exp.RData")

shapeUF = read.state()

class(shapeUF)

View(shapeUF)

#juntar dois df

expuf = left_join(shapeUF, dexp_mod, by = c("abbrev_state" = "uf"))

ggplot() + 
	geom_sf(data = expuf, 
		  aes(fill = exp),
		  color = "white", 
    		  size = .15) +
	labs(title = "Exportações",
		subtitle = "2019", 
		caption = "Fonte: Oku, Thireido") + 
	scale = fill_distiller(palette = "Reds", name = "Valor em bi") + 
	theme_minimal()

ggplot() +
	geom_sf(data = expuf,
		   aes(fill = exp),
		   color = "white",
		   size = .15) +
	labs(title = "Exportações") +
theme_minimal() +
theme(
	axis.text.x = element_blank(),
panel.grid = element_blank())

mun = read_municipality(code_muni=33, year=2010)
mun = read_municipality(code_muni="PR", year=2018)
mun = read_municipality(code_muni="all", year=2018)	


