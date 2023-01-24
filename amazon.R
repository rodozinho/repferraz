### Importing all packages

if (!require("pacman")) install.packages("pacman")
p_load(ggplot2,deflateBR, readxl, tidyverse, lubridate, sf, zoo, data.table, factoextra, ggbiplot, magrittr, grid,
       rnaturalearth, rnaturalearthdata, rgeos,car,SciViews,scales,caret,vroom,ggpattern,maps,geobr,maptools,FNN,plm,
       fixest,modelsummary,vtable,plm,did,Matching,formattable,sf,rgeos,MatchIt,devtools,DAPSm,did2s,cowplot)

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

# creating normalized and ln deforestation increase
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
