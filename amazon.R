### Importing all packages

if (!require("pacman")) install.packages("pacman")
p_load(ggplot2,deflateBR, readxl, tidyverse, lubridate, sf, zoo, data.table, factoextra, ggbiplot, magrittr, grid,
       rnaturalearth, rnaturalearthdata, rgeos,car,SciViews,scales,caret,vroom,ggpattern,maps,geobr,maptools,FNN,plm,
       fixest,modelsummary,vtable,plm,did,Matching,formattable,sf,rgeos,MatchIt,devtools,DAPSm,did2s,cowplot,ggmap)

# Cleaning the environment
   
rm(list=ls())
gc()

# Importing datasets and cleaning them
desmatamento_municipio <- read_excel("desmatamento_municipio.xltx",  # this xltx contains information about deforestation for every municipality in the amazon rainforest
                                     col_types = c("numeric", "numeric", "numeric", 
                                                   "numeric", "text", "numeric", "text", 
                                                   "numeric", "numeric", "numeric"))
                                                   
desmatamento_municipio <- desmatamento_municipio %>% filter(ano >= 2006) # deforestation only from 2006
desmatamento_municipio <- desmatamento_municipio %>% dplyr::rename(code_muni = id_municipio) #fixing the municipality code name

# Creating normalized by hand [could do: desmatamento_municipio %>% mutate_at(c("incremento"), ~(scale(.) %>% as.vector)) but that way I can make every step clear] and ln deforestation increase

as.numeric(desmatamento_municipio$incremento) -> desmatamento_municipio$incremento
mnincremento <- aggregate((desmatamento_municipio$incremento), by = list(desmatamento_municipio$code_muni)
                          , FUN=mean) 

mnincremento$sdincremento <- (aggregate((desmatamento_municipio$incremento), by = list(desmatamento_municipio$code_muni), FUN=sd))[2] #sd
mnincremento <- mnincremento %>% dplyr::rename(code_muni = Group.1 , 
                                        mean_desm = x,  sd_desm = sdincremento)

base <- left_join(desmatamento_municipio, mnincremento, by = c("code_muni"))

base$desm_norm <- ((base$incremento-base$mean_desm)/base$sd_desm)
base$desm_norm <- base$desm_norm$x

base <- base %>% dplyr::select(ano, code_muni,incremento,desm_norm)

base$ln_incremento <- ln(base$incremento)

rm(list=setdiff(ls(), "base")) # cleaning the environment

# Importing a list of municipalities inside the amazon rainforest which are "blacklisted" i.e. have a harsher monitoring by the federal government
lista_prioritaria <- read_excel("lista prioritaria.xlsx")
base <- left_join(base, lista_prioritaria, by = c("ano","code_muni")) # cbinding with deforestation and export
base <- base %>% mutate_at(vars(-c(mes)), ~replace(., is.na(.), 0))

# municipality area column
area_mun <- read_excel("area_mun.xlsx", col_types = c("numeric","numeric"))
base <- left_join(base,area_mun, by = "code_muni") 

rm(list=setdiff(ls(), "base")) 

# Areas of conservation units/indigenous territory per year, for each municipality

UC_area <- read_excel("UC_TI_area.xlsx")
UC_area <- UC_area %>% filter(amz_legal==1)
UC_area$ano <- as.numeric(UC_area$ano)

UC_area<-aggregate(area_kmq ~ code_muni + ano, data=UC_area, FUN=sum) # sum all territories for each municipality and every year
UC_area<- UC_area%>%
  group_by(code_muni,ano)%>%
  mutate(cumsum=cumsum(area_kmq))

options(scipen = 999)
UC_area <- UC_area %>%  dplyr::select(-area_kmq)
UC_area <- UC_area %>% dplyr::rename(area_prot = cumsum)

base <- left_join(base, UC_area, by = c("code_muni","ano"))
base <- base %>% mutate_at(vars(c(area_prot)), ~replace(., is.na(.), 0))

rm(list=setdiff(ls(), "base"))

# GDP per capita for each municipality
pib_per_capita <- read_excel("pib_per_capita.xlsx") 

pib_per_capita$data <- as.Date(paste(pib_per_capita$ano, 12, 31, sep = "-"))  # creating a date to deflate
pib_per_capita$pib_per_deflac <- deflate(pib_per_capita$pib_per_capita,
                                         pib_per_capita$data, real_date = "01/2020")

base  <- left_join(base, pib_per_capita, by = c("ano","code_muni")) #juntando na base dnv
rm(list=setdiff(ls(), "base"))

# Creating two variables (inside the Legal Amazon) to account for cattle and agriculture. inspired by the article "Deforestation slowdown in the Brazilian Amazon: prices or policies?" by Gandour et. al, I create a single index, weighted according to principal component analysis for agricultural production and another to cattle.

# These variables can be used to control for cattle and agriculture when applying any kind of regression with deforestation as dependent variable.

municipios_AL <- read_excel("municipios_AL.xls") # municipalities inside the Legal Amazon
precos_agricolas <- read_excel("precos agricolas.xlsx") # agricultural prices
area_mun <- read_excel("area_mun.xlsx", col_types = c("numeric","numeric")) # returning municipalities areas

precos_agricolas$date <- (with(precos_agricolas, # deflating prices
                               paste(precos_agricolas$mes,precos_agricolas$ano,  sep="/")))
precos_agricolas$date<- as.Date(as.yearmon(precos_agricolas$date,"%m/%Y"))
precos_agricolas$precos_defl<-deflate(precos_agricolas$preco,precos_agricolas$date, real_date = "01/2020") 

media_def<-aggregate((precos_agricolas$precos_defl), by = list(precos_agricolas$ano,precos_agricolas$produto)
                                 , FUN=mean)
media_def <- media_def %>% dplyr::rename(ano=Group.1, produto = Group.2, prec_medio_defl = x)

media<-aggregate((precos_agricolas$preco), by = list(precos_agricolas$ano,precos_agricolas$produto)
                     , FUN=mean)
media <- media %>% dplyr::rename(ano=Group.1, produto = Group.2, prec_medio = x)

municipio_lavouras<- vroom("municipio_lavouras_temporarias.csv") # agricultural land size

municipio_lavouras <- municipio_lavouras %>% filter(ano==2006|ano==2007) %>% # the treatment analyzed during my master's dissertation started in 2008 so I chose the last couple years before the treatment started.
  dplyr::select(ano, id_municipio,produto,area_colhida) %>% filter(produto=="Cana-de-açúcar"|produto=="Milho (em grão)" # picking only the most relevant agricultural products
                                                            |produto=="Arroz (em casca)"|produto=="Mandioca"|produto=="Soja (em grão)")%>% 
  dplyr::rename(code_muni = id_municipio)


municipio_lavouras$produto <-dplyr::recode(municipio_lavouras$produto,"Arroz (em casca)" = "arroz","Cana-de-açúcar"="cana",
                                    "Milho (em grão)"="milho", "Mandioca"="mandioca","Soja (em grão)"="soja") # recoding them

#municipio_lavouras$area_colhida[is.na(municipio_lavouras$area_colhida)] <- 0 # shouldn't do this.

municipio_lavouras$area_colhida <- municipio_lavouras$area_colhida*0.01 # from hectar to squared kilometer

municipio_lavouras<- left_join(municipio_lavouras,area_mun, by = "code_muni") # joining area with agricultural output
municipio_lavouras$share_crop <- municipio_lavouras$area_colhida/municipio_lavouras$area_territorial
sharemedia <- aggregate((municipio_lavouras$share_crop), by = list(municipio_lavouras$code_muni,municipio_lavouras$produto)
                                           , FUN=mean) %>% dplyr::rename(code_muni = Group.1, produto = Group.2, sharecrop = x) #average from 2006 and 2007

producao <- left_join(sharemedia,media,  by = c("produto")) # joining with prices

producao$ppa <- producao$sharecrop*producao$prec_medio # creating the final variable (PPAitc)

producao<-subset(producao, code_muni %in% municipios_AL$code_muni) # only for Legal Amazon municipalities

producao <- producao %>% mutate(weight = case_when( # creating the weights proposed by the authors
  produto == "arroz" ~ 0.4879, produto == "soja" ~ 0.594, produto == "arroz" ~ 0.4879, produto == "milho" ~ 0.6362,
  produto == "cana" ~ 0.0631, produto == "mandioca" ~ 0.0171
))

producao$wxp <- (producao$ppa * producao$weight) # weighting

#producao$wxp[is.na(producao$wxp)] <- 0 # shouldn't do this!

teste <-  # aggregating
  aggregate(producao$wxp, by = list(producao$ano,producao$code_muni), FUN=sum)

teste <- teste %>% dplyr::rename(ano = Group.1, code_muni = Group.2, ppa = x)

# now for cattle. same methodology just skip the weighting steps!
efetivo_rebanhos <- vroom("C:/Users/skyro/OneDrive - unb.br/tese/dados/efetivo_rebanhos.csv") 

efetivo_rebanhos <- efetivo_rebanhos %>% filter((ano==2006|ano==2007),tipo_rebanho=="Bovino")
efetivo_rebanhos$tipo_rebanho <-dplyr::recode(efetivo_rebanhos$tipo_rebanho,"Bovino" = "boi")
efetivo_rebanhos <- efetivo_rebanhos %>% dplyr::rename(code_muni = id_municipio, )

efetivo_rebanhos<-subset(efetivo_rebanhos, code_muni %in% municipios_AL$code_muni) # only Legal Amazon again

efetivo_rebanhos <- left_join(efetivo_rebanhos,area_mun,  by = c("code_muni"))
mediaboi <- media %>% filter(produto=="boi")
efetivo_rebanhos$share <- efetivo_rebanhos$quantidade_animais/efetivo_rebanhos$area_territorial
sharemedia <- aggregate((efetivo_rebanhos$share), by = list(efetivo_rebanhos$code_muni,efetivo_rebanhos$tipo_rebanho)
                        , FUN=mean) 
sharemedia <- sharemedia %>% dplyr::rename(code_muni = Group.1,produto = Group.2,shareboi = x)

producao <- left_join(sharemedia,mediaboi,  by = c("produto")) #juntando aos pre?os
producao$ppaboi <- producao$shareboi*producao$prec_medio
producao <- producao %>% dplyr::select(-produto,-shareboi,-prec_medio)

base <- left_join(base,producao,  by = c("code_muni","ano")) #jogando na base
base <- left_join(base,teste,  by = c("code_muni","ano")) #jogando na base


rm(list=setdiff(ls(), c("base"))) # cleaning the environment

### Now we will append information regarding export to Europe. The initial idea of the work was to understand the relationship between deforestation and exportation of primary products to developed countries, but I gave up on that. Either way, I will explore that below, plot a few graphs and create some maps.

### how to create a map? For me, the best way is to get coordinates at https://www.openstreetmap.org/export#map=14/-22.9645/-43.193 and apply them below. This method is my favorite since it's pretty straightforward, easy to visualizate the boundaries wished (just limit it as you wish at openstreetmap) and it has many maptypes. Either way, below I will also exlopre other ways.

nig_map <- get_stamenmap( #getting Nigeria's map coord 
  bbox = c(left = -2.8,bottom = 1.31,right=20.2,top = 16.5),
  maptype = "terrain",zoom=6
)
ggmap(nig_map)+geom_point(data= subs,aes(x = lon,y=lat,color=indicator))+scale_color_viridis_c(option = "magma")+
  theme_map()+labs(title="Figure 2") 

### Now what about blacklisting and accumulated deforestation? What is the historical pattern of deforestation?
