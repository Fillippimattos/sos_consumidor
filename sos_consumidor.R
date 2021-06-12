# Import packages
{ 
  # Check if the packages that we need are installed
  want = c("plotrix","tidyverse", "readr", "data.table", "dplyr", "stringr", "magrittr", "pipeR","geobr","crul","ggspatial","esquisse")
  have = want %in% rownames(installed.packages())
  # Install the packages that we miss
  if ( any(!have) ) { install.packages( want[!have] ) }
  # Load the packages
  junk <- lapply(want, library, character.only = T)
  # Remove the objects we created
  rm(have, want, junk)
} 


setwd("C:/UseR/consumidor/db")


# ABRINDO E COMPILANDO ARQUIVOS DAS RECLAMAÇOES DOS CONSUMIDORES
{ 
arq <- c("2020-01.","2020-02.","2020-03.","2020-04.","2020-05.","2020-06.","2020-07.")
arq2 <- c("2020-08.","2020-09.","2020-10.","2020-11.","2020-12.","2021-01.","2021-02.","2021-03.","2021-04.","2021-05.")
x ="csv"

X1 <- read_delim(paste0(arq[1],x), ";", 
                 escape_double = FALSE, col_types = cols(`Data Finalização` = col_date(format = "%d/%m/%Y")), 
                 locale = locale(encoding = "LATIN1"), 
                 trim_ws = TRUE)

X3 <- read_delim(paste0(arq2[1],x), ";", 
                 escape_double = FALSE, trim_ws = TRUE)

for (i in 2:length(arq2) ) {
  X2 <- read_delim(paste0(arq2[i],x), ";", 
                   escape_double = FALSE, trim_ws = TRUE)
  
  X3<- bind_rows(X3,X2)
  
  
}


for (i in 2:length(arq) ) {
  X2 <- read_delim(paste0(arq[i],x), ";", 
                   escape_double = FALSE, col_types = cols(`Data Finalização` = col_date(format = "%d/%m/%Y")), 
                   locale = locale(encoding = "LATIN1"), 
                   trim_ws = TRUE)
  
  X1<- bind_rows(X1,X2)
  
  
}

X1<- subset(X1, select = -Total)

X1<- bind_rows(X1,X3)

X1=X1  %>% mutate(cidade_estado=paste0(Cidade,"_",UF))
}

############ IMPORTANDO INFORMAÇOES SOBRE A POPULAÇÃO DOS ESTADOS E MUNICIPIOS
{
pop <- read_delim("pop.csv", ";", escape_double = FALSE, 
                  locale = locale(encoding = "LATIN1"), 
                  trim_ws = TRUE)
pop_es <- read_delim("pop_es.csv", ";", escape_double = FALSE, 
                  locale = locale(encoding = "LATIN1"), 
                  trim_ws = TRUE)

pop_es = pop_es  %>% select(Sigla,'2010')
pop_es <- rename(pop_es, População =  '2010')

pop = pop  %>% select(Codigo,'2010')
pop <- rename(pop, População =  '2010')

}


#### CRIANDO TABELAS PARA ANALISES EXPLORATORIAS DE DADOS
{
Sexo = data.frame(table(X1$Sexo))
  Sexo=Sexo  %>%mutate(Proporção = round(100*Freq/sum(Freq),2))
  
UF = data.frame(table(X1$UF))
UF=UF  %>%mutate(Proporção = round(100*Freq/sum(Freq),2))

Cidade = data.frame(table(X1$cidade_estado))

idade = data.frame(table(X1$`Faixa Etária`))
idade=idade  %>%mutate(Proporção = round(100*Freq/sum(Freq),2))



cw= c("Quarta","Quinta","Sexta","Sábado","Domingo","Segunda","Terça")
cw= c(cw,cw,cw,cw,cw,cw,cw,cw,cw,cw)
cw= c(cw,cw,cw,cw,cw,cw,cw,cw,cw,cw)
data = data.frame(table(X1$`Data Finalização`))
data=data  %>%mutate(Dia_semana = cw[seq(1,516,1)])




tempo = data.frame(table(X1$`Tempo Resposta`))
tempo=tempo  %>%mutate(Proporção = round(100*Freq/sum(Freq),2))

segmento = data.frame(table(X1$`Segmento de Mercado`))
segmento=segmento  %>%mutate(Proporção = round(100*Freq/sum(Freq),2))

area = data.frame(table(X1$'Área'))
area=area  %>%mutate(Proporção = round(100*Freq/sum(Freq),2))

assunto = data.frame(table(X1$Assunto))
assunto=assunto  %>%mutate(Proporção = round(100*Freq/sum(Freq),2))

grupo_problema  = data.frame(table(X1$`Grupo Problema`))
grupo_problema=grupo_problema  %>%mutate(Proporção = round(100*Freq/sum(Freq),2))

problema  = data.frame(table(X1$Problema))
problema=problema  %>%mutate(Proporção = round(100*Freq/sum(Freq),2))


tipo=  data.frame(table(X1$`Como Comprou Contratou`))
tipo=tipo  %>%mutate(Proporção = round(100*Freq/sum(Freq),2))

nota = data.frame(table(X1$`Nota do Consumidor`))
nota=nota  %>%mutate(Proporção = round(100*Freq/sum(Freq),2))
}

###################

# CRIANDO MAPAS
{
  ## Dados Para a Criação dos Mapas
  {
estados <- read_state(code_state = "all")

municipios <- read_municipality(code_muni = "all")
estados <- rename(estados, geom_st = geom,UF= abbrev_state)

municipios <- rename(municipios, geom_mu =  geom, UF= abbrev_state)

municipios=municipios  %>% mutate(cidade_estado=paste0(name_muni,"_",UF))  
estados  = estados %>% select(UF, geom_st)
municipios = municipios  %>% select(code_muni,cidade_estado,UF,geom_mu)

mapa_es = left_join(estados,UF, by=c("UF"="Var1"))

mapa_es = left_join(mapa_es,pop_es, by=c("UF"="Sigla"))

mapa_es= mapa_es  %>% mutate(per_reclama=(Freq/População)*1000)



mapa_mu = left_join(municipios,Cidade, by=c("cidade_estado"="Var1"))

mapa_mu = left_join(mapa_mu,pop, by=c("code_muni"="Codigo"))

mapa_mu= mapa_mu  %>% mutate(per_reclama=(Freq/População)*1000)

consumidor_PR <- mapa_mu %>%filter(UF=="PR")
  }
  # Plotando Gráficos
  {
ggplot()+
  geom_sf(data= consumidor_PR , aes(fill=per_reclama), color ="Black", size= .05)+
  labs(title = "Reclamações Por Município", size = 8)+
  scale_fill_distiller(palette = "RdYlBu", limits = c(0.0,41),
                       name = "Reclamaçõs Por Mil Habitantes")


ggplot()+
  geom_sf(data= mapa_es , aes(fill=per_reclama), color = "Black", size= .15)+
  labs(title = "Reclamações Por Unidade Federativa", size = 8)+
  scale_fill_distiller(palette = "RdYlBu", limits = c(0.0,30.0),
                       name = "Reclamaçõs Por Mil Habitantes")

ggplot()+
  geom_sf(data= mapa_mu , aes(fill=per_reclama), color =NA, size= .05)+
  labs(title = "Reclamações por município",caption = "Fonte: Elaboração própria", size = 8)+
  scale_fill_distiller(palette = "RdYlBu", limits = c(0.0,3),
                       name = "Percentual de reclamaçoes")



ggplot()+
  geom_sf(data= mapa_es , aes(fill=Freq), color = "Black", size= .15)+
  labs(title = "Reclamações por unidade federativa",caption = "Fonte: Elaboração própria", size = 8)+
  scale_fill_distiller(palette = "RdYlBu", limits = c(2300,460000),
                       name = "Percentual de reclamaçoes")

}

}

# CRIANDO GRÁFICOS
{
  esquisser(area)
  # IDADE
  {cores= rainbow(length(idade$Freq))
  
  pie(x=idade$Freq, labels =  paste(paste0(idade$Proporção, "%"),idade$Var1), 
      col = cores,main= "Reclamações por Faixa Etária")}
# ÁREA
  {
   cores= rainbow(length(area$Var1))
 ggplot(area) +
   aes(x = Var1, colour = Var1, weight = Proporção) +
   geom_bar(fill = cores,size = 1) +
   labs(
     x = "Segmentos",
     y = "Numero de Reclamaçoes",
     title = "Reclamaçõs por Segmento")
 
 }
 
}

#write.table(X1, file = "consumidor_gov.cvs", sep = ",", row.names = FALSE )
