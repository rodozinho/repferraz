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

# importing our main data and setting it. this database contains informations regarding brazilian mayors election. 
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

dado$ln_gdp <- ln(dado$gdp)
dado$ln_muni_expenses <- ln(dado$muni_expenses)
dado$ln_empl <- ln(dado$nr_empl)

dado$higher_spending_cap <-ifelse(dado$max_despesa2012 >= 142858, 1,0) # this is the dummy used in the RDD. if the compaing spending in 2012 was higher than 142858 (a value defined by the brazilian supreme court)
dado$higher_spending_cap[is.na(dado$higher_spending_cap)] <- 0  # changing NA to zero

dado$diff <- log(dado$max_despesa2012) - log(142858) # taking the difference between max spending and the log of the exogenous cap. useful later if want to trim/limitate the RDD to values close to the discontinuity.
 
dado$burocratic <- (dado$legislative_finbra+dado$judiciary_finbra+dado$justice_finbra+dado$administration_finbra) # burocratic spendings
dado$burocratic <- (dado$burocratic/dado$muni_expenses) # burocratic spendings as fraction of total spending
dado$frac <-  (dado$health_finbra+dado$education_finbra)/dado$muni_expenses # spending on health and education as fraction of total spending

dado$diff_votos <- log(dado$votos_primeiro_colocado/dado$votos_segundo_colocado) # difference of votes between the first and the second candidate (our measure of political competition)

dado[] <- Map(function(x) replace(x, is.infinite(x), 0), dado) # replacing inf from the ln to zero

dat2 <- dado %>% select(starts_with("ln_"),frac,burocratic,diff_votos) %>% qrep(.) # applying the outliers function
            
dado[names(dat2)] <- dat2 # returning them to the initial dataframe (faster that way)
rm(dat2,qrep)
              
fwrite(dado,"dado.csv") # clean data. ready to be used on replicating the paper.

##### Replicating the paper --------
subdado <- dado %>% filter(year == 2017)
feols(nr_candidatos ~ higher_spending_cap| uf, data = subdado, cluster = subdado$ibge) # federative unit fixed effect
feols(nr_candidatos ~ higher_spending_cap + diff:i(higher_spending_cap)|uf, data = subdado, cluster = subdado$ibge) # varying slope (using diff)
feols(nr_candidatos ~ higher_spending_cap + diff2:i(higher_spending_cap)+diff:i(higher_spending_cap)| uf, 
      data = subdado %>% dplyr::mutate(diff2=diff*diff) , cluster = subdado$ibge)
              
ggplot(data=subdado %>% filter(year==2017,!is.na(higher_spending_cap)), # plotting
aes(y=nr_candidatos))+ ggtitle("Dummy higher_spending_cap") +
  geom_boxplot()+facet_grid(~higher_spending_cap) + theme(axis.title.x=element_blank(),
                                                                                     axis.text.x=element_blank(),
                                                                                     axis.ticks.x=element_blank())
              

