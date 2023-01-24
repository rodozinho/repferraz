# it is useful to get to know the paper being replicated here as I won't explain every step taken there. However, in general, the authors use an exogenous cap created by the brazilian Supreme Court to understand how campaing spendings affect political competition.

### Importing all (and more) required packages

if (!require("pacman")) install.packages("pacman")
p_load(ggplot2,tidyverse,fixest,vroom,SciViews,scales,caret, cowplot,data.table,deflateBR,
       modelsummary,openxlsx,vtable,readxl,rdd, jtools,gridExtra)

# Defining a function to trim outliers. you can choose the p according to your research design
w = function(x,p=0.01) psych::winsor(x = x, trim = p)

# Cleaning environment
rm(list=ls())
gc()

# importing our main data. this database contains informations regarding brazilian mayors election. 
dado <- vroom("municipality_panel.txt") # https://vroom.r-lib.org/articles/benchmarks.html

dado <- dado %>% filter(year >= 2006) # cleaning the data a little
dado$post <-ifelse(dado$year <= 2016, 0,1) # for the RDD, we will need a dummy marking the year

ln <- dado %>% select(starts_with("nr")) %>% ln() %>%
  rename_with(~paste0("ln","_",.x)) # ln nas variaveis comecando com nr
dado <- cbind(dado,ln) #juntando
