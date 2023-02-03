### Importing all packages

if (!require("pacman")) install.packages("pacman")
p_load(ggplot2,deflateBR, readxl, tidyverse, lubridate, sf, zoo, data.table, factoextra, ggbiplot, magrittr, grid,
       rnaturalearth, rnaturalearthdata, rgeos,car,SciViews,scales,caret,vroom,ggpattern,maps,geobr,maptools,FNN,plm,
       fixest,modelsummary,vtable,plm,did,Matching,formattable,sf,rgeos,MatchIt,devtools,DAPSm,did2s,cowplot,ggmap)

# Cleaning the environment
   
rm(list=ls())
gc()

setwd("C:/Users/skyro/OneDrive - unb.br/tese/dados") # set your wd

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
exportfiltered <- vroom("exportfiltered.csv")

# selecting only products used to create our cattle and agriculture indexes

exportfiltered <- exportfiltered %>% filter((SH4 == 201 |SH4 == 202|SH4 == 1201|SH4 == 1507|SH4 == 2304 # cattle + soy 
                                             |SH4 == 1108|SH4 == 1005|SH4 == 1102|SH4 == 1103 # corn
                                             |SH4 == 1701|SH4 == 1702|SH4 == 1703|SH4 == 2207 # sugar cane
                                             |SH4 == 1006 # rice
                                             |SH4 == 1108)) # cassava

pais_bloco <- vroom("PAIS_BLOCO.csv")
pais_bloco <- pais_bloco %>% dplyr::select(-NO_BLOCO)

pais_bloco <- left_join(exportfiltered,pais_bloco, by = "CO_PAIS") 
rm(exportfiltered)

pais_bloco <- pais_bloco %>% filter(CO_BLOCO == 112) # only europe

pais_bloco <- pais_bloco %>% group_by(CO_ANO,CO_MUN) %>% dplyr::summarise(exp_total = sum(VL_FOB)) # for each year and for each municipality, wanted to see how much they exported to Europe


cambio <- read_excel("cambio.xlsx") # exchange rate
pais_bloco <- left_join(pais_bloco, cambio, by = c("CO_ANO"="ano")) 

pais_bloco$exp_cambio <- pais_bloco$exp_total*pais_bloco$cambio

pais_bloco$data <- as.Date(paste(pais_bloco$CO_ANO, 12, 31, sep = "-"))  # now I'll try to deflate these values
pais_bloco$exp_defl <- deflate(pais_bloco$exp_cambio,pais_bloco$data, real_date = "01/2020") 
pais_bloco <- pais_bloco %>% dplyr::select(CO_ANO,CO_MUN,exp_cambio,exp_defl)

base <- left_join(base,pais_bloco,by = c("ano"="CO_ANO","code_muni"="CO_MUN")) # appending it to the databse

rm(list=setdiff(ls(), c("base"))) # cleaning the environment

### Is there some association between deforestation and export? Let's use the data only from municipalities of info about export
base_map <- base %>% drop_na(exp_defl) 

# the most logical thing is to assume some kind of lag. if the deforestation happens now, only on the following years this will impact export. 


### now that we have settled our base, let's go to the developed world and study the export (import, from the European country perspective) of primary products

exportfiltered <- exportfiltered %>% filter((SH4 == 201 |SH4 == 202|SH4 == 1201|SH4 == 1507|SH4 == 2304 # cattle + soy 
                                             |SH4 == 1108|SH4 == 1005|SH4 == 1102|SH4 == 1103 # corn
                                             |SH4 == 1701|SH4 == 1702|SH4 == 1703|SH4 == 2207 # sugar cane
                                             |SH4 == 1006 # rice
                                             |SH4 == 1108)) # cassava

pais_bloco <- vroom("PAIS_BLOCO.csv")
pais_bloco <- pais_bloco %>% dplyr::select(-NO_BLOCO)

pais_bloco <- left_join(exportfiltered,pais_bloco, by = "CO_PAIS") 
rm(exportfiltered)

pais_bloco <- pais_bloco %>% filter(CO_BLOCO == 112) # only europe


pais_bloco <- aggregate(pais_bloco$VL_FOB, by = list(pais_bloco$CO_ANO,pais_bloco$CO_MES, pais_bloco$CO_PAIS), FUN=sum) # sum of all export to each country for a given year

pais_bloco <- pais_bloco %>% dplyr::rename(CO_ANO = Group.1 , CO_MES = Group.2,  CO_PAIS = Group.3, VL_FOB = x) # renaming because its messy

pais_bloco <- aggregate(pais_bloco$VL_FOB, by = list(pais_bloco$CO_ANO, pais_bloco$CO_PAIS), FUN=sum) # now I'll sum for all months in a year, for each country

pais_bloco <- pais_bloco %>% dplyr::rename(CO_ANO = Group.1 , 
                                                  CO_PAIS = Group.2,  VL_FOB = x) # renaming again

cambio <- read_excel("cambio.xlsx")# exchange rate again

pais_bloco <- left_join(pais_bloco, cambio, by = c("CO_ANO"="ano")) 
rm(cambio)

pais_bloco$expreal <- pais_bloco$VL_FOB*pais_bloco$cambio # export in terms of real, for each country, for every year

pais_bloco$data <- as.Date(paste(pais_bloco$ano, 12, 31, sep = "-"))  # date to deflate (it rhymes)
pais_bloco$exp_defl <- deflate(pais_bloco$expreal,pais_bloco$data, real_date = "01/2020") # deflating

pais_bloco$teste <- dplyr::recode(pais_bloco$CO_PAIS,'17' = "Albania", # renaming it
                                                             '23' = "Germany",
                                                             "37" = "Andorra",
                                                             '72' = "Austria",
                                                             '85' = "Belarus", 
                                                             "87" = "Belgium",
                                                             '98' = "Bosnia and Herzegovina",
                                                             '111' = "Bulgaria", 
                                                             '151' = "Moldava",
                                                             '163' = "Chipre",
                                                             '195' = "Croatia",
                                                             '232' = "Denmark",
                                                             '245' = "Spain",
                                                             '246' = "Slovenia",
                                                             '247' = "Slovakia",
                                                             '251' = "Estonia",
                                                             '259' = "Faroe",
                                                             '271' = "Finland",
                                                             '275' = "France",
                                                             '293' = "Gibraltar",
                                                             '301' = "Greece",
                                                             "305" = "Greenland",
                                                             "321" = "Guernsey",
                                                             '355' = "Hungary",
                                                             '359' = "Isle of Man",
                                                             '375' = "Ireland",
                                                             '379' = "Iceland",
                                                             '386' = "Italy",
                                                             '393' = "Jersey",
                                                             '427' = "Latvia",
                                                             '440' = "Liechtenstein",
                                                             '442' = "Lithuania",
                                                             '445' = "Luxembourg",
                                                             '449' = "Macedonia",
                                                             '467' = "Malta",
                                                             '494' = "Moldova",
                                                             '495' = "Monaco",
                                                             '498' = "Montenegro",
                                                             '538' = "Norway",
                                                             '573' = "Netherlands",
                                                             '603' = "Poland",
                                                             '607' = "Portugal",
                                                             '628' = "United Kingdom",
                                                             '670' = "Romania",
                                                             '676' = "Russia",
                                                             '697' = "San Marino",
                                                             '737' = "Republic of Serbia",
                                                             '764' = "Sweden",
                                                             "767" = "Switzerland",
                                                             '791' = "Czech Republic",
                                                             '827' = "Turkey",
                                                             '831' = "Ukraine",
                                                             "848" = "Vatican")
pais_bloco <- pais_bloco %>% drop_na(teste)  %>% complete(nesting(teste),ano = full_seq(ano, period = 1))# dropping na and balacing the data

# Now let's create some maps!

world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$region_un == "Europe"),]
teste <- Europe %>% dplyr::select(geometry,geounit)
teste <- teste %>% dplyr::rename(teste =geounit)
teste <- left_join(teste,pais_bloco,by="teste")
teste <- subset(teste,ano!=2022)  # for all years except 2022 that I don't have all info
teste$exp_defl[is.na(teste$exp_defl)] <- 0 # since this database is very complete, we can suppose that NA equals to zero or around zero

                              
ggplot(teste) + 
 geom_sf(aes(geometry = geometry,fill =exp_defl)) +labs(fill='Import of Brazilian primary products \nwhich usually are associated with deforestation (R$)') +
scale_fill_viridis_c(option = "plasma",label=scales::comma) + theme_bw()+ facet_wrap(~ano)+
coord_sf(xlim = c(-25,50), ylim = c(35,70), expand = FALSE)

# As visible, a few countries "carry" this burden, such as Spain, Netherlands, France, and Russia. Quantifying how this global chain of primary products affects deforestation directly is, however, out of the boundaries of the present exploration. Still, we can suppose (and the literature corroborates that) that, at least to some degree, the food on the plate of a Spanish comes with CO2 and biodiversity loss.      

base_map <- aggregate(base$incremento, by = list(base$ano), FUN=sum) # summing to find overall deforestation increase
base_map <- base_map %>% dplyr::rename(year=Group.1,def_increase=x)

ggplot(data=base_map,aes(x=year,y=def_increase))+geom_line()+ xlab("Years") +
 ylab("Deforestation Increase")+theme_minimal() 

## hmm maybe some new maps because why not? let's see how this accumulated deforestation is geographically distributed
base_map <- aggregate(base$incremento, by = list(base$code_muni), FUN=sum) 
base_map <- base_map %>% dplyr::rename(muni_code=Group.1,def_increase=x)

lat_lon <- vroom("latitude.txt") # for all brazilian city, let's get latitude and longitude
base_map <- left_join(base_map,lat_lon,by=c("muni_code"="codigo_ibge"))
base_map <- base_map %>% dplyr::select(muni_code,latitude,longitude,def_increase) 

## another way to create maps (the easiest, in my opinion) is to get coordinates at https://www.openstreetmap.org/export#map=14/-22.9645/-43.193 and apply them below. This method  it's pretty straightforward, easy to see the boundaries wished (just limit it as you wish at openstreetmap) and it has many maptypes. However, for this kind of data, this doesn't work well, since we need "geom" to create municipalities. For that, I will also explore other ways.

am_map <- get_stamenmap( #getting Brazilian's Amazon Rainforest coords 
  bbox = c(left = -75.190,bottom = -22.187,right=-29.092,top = 8.146),
  maptype = "watercolor",zoom=6
)

ggmap(am_map)+geom_point(data= base_map,aes(x = longitude,y=latitude,color=def_increase))+scale_color_viridis_c(option = "magma")+
  theme_map()+labs(title="Figure 2") 


### Let's try another way!
rm(am_map) # cleaning the environment

# for Brazil, there is a package called "geobr" that deals with that easily
read_municipality() -> all_mun # getting all municipalities inside brazil and their geom
all_mun <- all_mun %>% dplyr::select(code_muni,geom) # what we trully want
base <- left_join(base, all_mun, by = c("code_muni"))
rm(all_mun,lat_lon)

base <-base %>%  dplyr::rename(geometry=geom)

# let's aggregate the deforestation

base_agg <- aggregate(incremento ~ code_muni, data=base, FUN=sum)

base_agg <- left_join(base_agg,base %>% dplyr::select(code_muni,geometry),by="code_muni")
base_agg<- base_agg[!duplicated(base_agg), ]

ggplot(base_agg) +
  geom_sf(aes(geometry = geometry,fill=as.numeric(incremento)))+
     scale_fill_viridis_c(option = "inferno") + theme_bw() + labs(fill='Deforestation increase \nsince 2006') 
   
# lets build an arch!
ggplot(base_agg) +
     geom_sf(aes(geometry = geometry,fill=as.numeric(incremento)))+
     scale_fill_viridis_c(option = "inferno") + theme_bw() + labs(fill='Deforestation increase \nsince 2006')  
   
ggplot(base) + 
  geom_sf(aes(geometry = geometry,fill =incremento)) +labs(fill='Import of Brazilian primary products \nwhich usually are associated with deforestation (R$)') +
  scale_fill_viridis_c(option = "plasma",label=scales::comma) + theme_bw()+ facet_wrap(~ano)+
  coord_sf(xlim = c(-25,50), ylim = c(35,70), expand = FALSE)
