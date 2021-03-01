# Instalando pacotes
if (!require(knitr)) install.packages('knitr')
if (!require(rmarkdown)) install.packages('rmarkdown')
if (!require(grDevices)) install.packages("grDevices")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(readxl)) install.packages("readxl")
library("readxl")
rm(list = ls())

# Setando diretorio
if(.Platform$OS.type == "windows") {
  setwd("C:\Users\lueij\Documents\AED")
} else {
  setwd("~/Library/Mobile Documents/com~apple~CloudDocs/GitLab")
}

# Lendo as planilhas, arrumando tipos dos dados e retirando NA
dados_casos <- read_excel("./dados/covid_19_bauru_casos_geral.xlsx", col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
dados_mortes <- read_excel("./dados/covid_19_bauru_mortes.xlsx", col_types = c("date", "text", "numeric", "date", "text", "text", "date"))

head(dados_mortes)
head(dados_casos)

dados_limpos <- na.omit(dados_casos)
mortes_limpas <- na.omit(dados_mortes)

head(dados_limpos)
head(mortes_limpas)

# Numero de pesquisados com informacoes completas
num_pesq <- nrow(dados_limpos) 
num_pesq

num_mort <- nrow(mortes_limpas)
num_mort  

##### Grafico de genero dos mortos
genero = table(dados_mortes$sexo)
pct.s <- round(genero/sum(genero)*100, digits=1)
pct.s

lbls <- c("Feminino", "Masculino")
lbls <- paste(lbls, pct.s, "%", sep=" ") # adicionar porcentagens aos rótulos 
jpeg(filename = "./graficos/obitos-genero.jpg")
pie(genero,
    labels = lbls,
    edges = 100,
    angle = 45,
    col = c("red","blue"),
    main = "Figura 4 - Obitos por genero",
    )
dev.off()

##### Grafico de faixa etaria
pct.i <- round(table(dados_mortes$idade)/sum(table(dados_mortes$idade))*100, digits=1)
pct.i

jpeg(filename = "./graficos/faixa_etaria.jpg")
barplot(table(dados_mortes$idade),
        main = "Figura 6 - Faixa etaria de obitos",
        col = c("gray", "red","gray", "green3", "red"),
        xlab = "Idade",
        ylim = c(0,max(table(dados_mortes$idade)+3)),
        ylab = "N. de Obitos")
dev.off()

##### Grafico de comorbidade

if (!require(stringr, quietly = TRUE)) install.packages("stringr")

string <- dados_mortes$comorbidade
dado.1 <- strsplit(string, " e ")
dado.2 = unlist(dado.1, use.names=FALSE)
morb.vi = table(trimws(dado.2))


maior.freq.dv = max(morb.vi)
maior.freq.dv.name = names(which.max(morb.vi))
maior.freq.dv.i = unname(which.max(morb.vi))

indice.vi.1<-which.max(morb.vi > 5)
indice.vi.2<-which.max(morb.vi < 84)

pct.m<-sort(round((morb.vi[which(morb.vi >= 5)] / sum(morb.vi))*100,1))

pct.m.p = paste0(pct.m, "%")
jpeg(filename = "./graficos/comorbidades.jpg")
graph.diseases = pie(sort(morb.vi[which(morb.vi >= 5)]), 
                         
                         main = "Figura 5 - Principais Comorbidades",
                         col = c("gray", "blue", "purple", "green", "yellow", "red", "brown", "pink"),
                         names.arg = names(sort(morb.vi[which(morb.vi >= 5)])),
                         )
                  
dev.off()

#### Grafico Infectados/Curados/Obitos
ic <- ggplot() + 
  geom_line(data=dados_casos[!is.na(dados_casos$confirmacoes_total),], aes(x =data_boletim, y=confirmacoes_total,colour="yellow")) +
  geom_line(data=dados_casos[!is.na(dados_casos$total_mortes),], aes(x =data_boletim, y=total_mortes,colour="red")) +
  geom_line(data=dados_casos[!is.na(dados_casos$curados),], aes(x =data_boletim, y=curados,colour="green")) +
  scale_color_discrete(name = "Total de casos", labels = c("Curados", "Obitos", "Confirmados")) +
  ylab("Quantidade") +
  xlab("Meses 2020-2021") +
  jpeg(filename = "./graficos/infectados-curados.jpg")
  ggtitle("Figura 2 - Confirmados/Curados/Obitos") 
print(ic) 
dev.off()

#### Grafico Rede publica/privada
pp = table(dados_mortes$tipo_hosp)
pct.s <- round(pp/sum(pp)*100, digits=1)
pct.s

lbls <- c("Publica", "Privado")
lbls <- paste(lbls, pct.s, "%", sep=" ") # adicionar porcentagens aos rótulos 
jpeg(filename = "./graficos/publico-privado.jpg")
pie(pp,
    labels = lbls,
    edges = 100,
    angle = 45,
    col = c("red","green"),
    main = "Figura 3 - Tipos de hospitalizacao",
)
dev.off()

#### Grafico novos casos
nc <- ggplot() + 
  geom_line(data=dados_casos[!is.na(dados_casos$casos_novos),], aes(x =data_boletim, y=casos_novos)) +
  ylab("Quantidade") +
  xlab("Meses 2020-2021") +
  
  jpeg(filename = "./graficos/novos-casos.jpg")
ggtitle("Figura 1 - Novos casos em 2020 a 2021") 
print(nc) 
dev.off()

