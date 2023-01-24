# it is useful to get to know the paper being replicated here as I won't explain every step taken. However, in general, the authors use an exogenous cap created by the brazilian Supreme Court to understand how campaing spendings affect political competition.

### Importing all (and more) required packages

if (!require("pacman")) install.packages("pacman")
p_load(ggplot2,tidyverse,fixest,vroom,SciViews,scales,caret, cowplot,data.table,deflateBR,
       modelsummary,openxlsx,vtable,readxl,rdd, jtools,gridExtra)

# Defining a function to trim outliers. you can choose the quantile according to your research design
qrep <- function(x){ 
  quantiles <- quantile(x, c(.01, .99 ),na.rm=T)
  x[x < quantiles[1]] <- quantiles[1]
  x[x > quantiles[2]] <- quantiles[2]
  x
}

# Cleaning environment
rm(list=ls())
gc()

# importing our main data. this database contains informations regarding brazilian mayors election. 
dado <- vroom("municipality_panel.txt") # https://vroom.r-lib.org/articles/benchmarks.html

dado <- dado %>% filter(year >= 2006) # cleaning the data a little
dado$post <-ifelse(dado$year <= 2016, 0,1) # for the RDD, we will need a dummy marking the year

ln <- dado %>% select(starts_with("nr")) %>% ln() %>%
  rename_with(~paste0("ln","_",.x)) # ln on variables starting with "nr"
dado <- cbind(dado,ln)

dado$muni_expenses <- 0

dado <- dado %>%
  rowwise() %>%
  mutate(muni_expenses = sum(c_across(ends_with("finbra")), na.rm = F)) # sum of finbra


ln <- dado %>% select(ends_with("finbra")) %>% ln() %>%
  rename_with(~paste0("ln","_",.x)) #ln on variables starting with "finbra"
dado <- cbind(dado,ln)

rm(ln) # cleaning the environment
