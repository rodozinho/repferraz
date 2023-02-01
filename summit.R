
# this file is divides in two. in the first part, I will try to see if there is some relationship between environmental conferences and major environmental disasters and the raise of radical views on environmental issues.
# my hypothesis is that by seeing how incapable governments are currently to deal with the demanding issue regarding climate change, people react to that by adopting more radical perspectives.
# for the second part, using textual analysis, web scrapping and machine learning, I will see how, in general, people behave towards left-wing radical writes and scientists.


if (!require("pacman")) install.packages("pacman")
p_load(ggplot2,tidyverse,gtrendsR, rtweet,modifiedmk,zoo,strucchange,xts)
rm(list=ls())
gc()

#---- Google trend ----
# First, lets use Google trend to see how, historically, some keywords have behaved. I will define a few words/expressions that will be used during this short experiment:

# 1 - "Eco-socialism" (and its variations);
# 2 - "Marxism" (and its variations);
# 3 - A small list of relevant eco-socialists (Foster, Lowy, malm, kohei saito). no politicians to not create spurious noise;
# 4 - Anthropocene;
# 5 - Degrowth;

# now, let's define major key summits and conferences that will be used during the analysis (since gtrend is mensal, can use only the month of the event):
# 1 - First major one was Earth Summit / Rio 92 happened during june of 1992 (out of our sample)
# 2 - Rio +10  aug/2002 (out of our sample)
# 3 - Rio +20 june/2012
# 4 - Cop 25 ( 12/2019)

## Let's start!

output <- gtrends(c("Degrowth","Anthropocene","Ecosocialism"), time="all") # if search for more than one keyword receive Error in FUN(X[[i]], ...) : #Status code was not 200. Returned status code:429. have to do it manually then

time_trend <-output$interest_over_time
time_trend <- time_trend %>% select(date,hits,keyword)
ggplot(data=time_trend,
            aes(x=date, y=as.numeric(hits), group=keyword, col=keyword)) +
  geom_line(size = .9, alpha = .75) + labs(
    title = "Google Search Volume",
    x = "Time",
    y = "General Interest",
    colour = "keyword"
  )+theme_minimal()

# since there isn't much relevant information for ecosocialism, let's stick with the other two and take the mean for the last month

time_trend_v2 <- time_trend %>% group_by(keyword) %>% dplyr::mutate(hit_30 = zoo::rollmean(as.numeric(hits), k = 30, fill = NA)) %>% 
  dplyr::ungroup() %>% filter(keyword!="Ecosocialism")

time_trend_v2$hits <- as.numeric(time_trend_v2$hits)

ggplot(data=time_trend_v2,
       aes(x=date, y= hit_30 , group=keyword, col=keyword)) +
  geom_line(size = .9, alpha = .75) + labs(
    title = "Google Search Volume", # now its smooth
    x = "Time",
    y = "General Interest") + theme_minimal() + facet_wrap("keyword")+ theme(legend.position="none")+
  geom_line(data=time_trend %>%  filter(keyword!="Ecosocialism"),
            aes(x=date, y=hits, group=keyword, col=keyword)) 


# there is some kind of trend? can use the mann kendall test! a non-parametric test which seeks trends in a time series.

mkttest(unlist(c(as.vector(time_trend_v2 %>% dplyr::filter(keyword=="Anthropocene")%>% select(hits)))))

# for that, we reject the null hypothesis and is very likely that there is some kind of trend on this data. how about for "Degrowth"?

mkttest(unlist(c(as.vector(time_trend_v2 %>% dplyr::filter(keyword=="Degrowth")%>% select(hits)))))

# the same! i.e., to both keywords, it is plausible to assume that there is some kind of trend. but could it be any structural/trend break? inspired by https://stats.stackexchange.com/questions/395078/how-to-detect-and-quantify-a-structural-break-in-time-series-r, lets analyze the trend regarding "Anthropocene" search.

a <- (time_trend_v2 %>% filter(keyword=="Anthropocene") %>% select(hits,date)) 
a$date <- as.Date(a$date) # turning it into a time series
a <- xts(a$hits, a$date)

a

autoplot.zoo(a) # to visualize a time series
             
test2 <- Fstats(a~1,from=0.05) # getting a sequence of fstats for all possible break points. in this case, we define to take observations from 0.05 to 0.95. as our sample has 458 observations, it goes from 23 to 435.

breakp <- breakpoints(test2$Fstats~1) # now trying to get the breakpoints
breakp # We get that: Breakpoints at observation number 43 74 105 142 173 

plot(test2) #plots the series
lines(breakp) #plots the break date implied by the sup F test

breakpdates <- breakdates(breakp) # getting the breakpoint dates
breakpdates <- breakpdates*365 # setting it to day number i.e. on the 84th day on our time series there is a break

sctest(test2) #Obtains a p-value for the implied breakpoint

ci <- confint(breakp) #95% CI for the location break date
plot(test2)
lines(ci) #This shows the interval around the estimated break date

rm(list=setdiff(ls(), "time_trend_v2")) # cleaning the environment

### this time series is kind of right-skewed. it is plausible to assume that people started to Google more something the more people had internet access. to bypass that, it would be good to log-linearize.
a <- (time_trend_v2 %>% filter(keyword=="Anthropocene") %>% select(hits,date)) %>% mutate(hits = log(hits))
a <-  a %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))# replacing inf from the ln to zero. it isn't wrong since they were 0 by default

a$date <- as.Date(a$date) # turning it into a time series
a <- xts(a$hits, a$date)

plot(a)

# now we can do the same as before looking for breakpoints! feel free to do it.
rm(a)

### keeping up, now I want to answer: when where we mostly worried with anthropocene? and why did we start to worry with the consequences of our acts?

ggplot(data=time_trend_v2 %>%  filter(keyword=="Anthropocene"),
       aes(x=date, y= hit_30 , group=keyword), col="deeppink") +
  geom_line(size = .9, alpha = .75) + labs(
    title = "Google Search Volume", # now its smooth
    x = "Time",
    y = "General Interest") + theme_minimal() + facet_wrap("keyword")+ theme(legend.position="none")+
  geom_line(data=time_trend_v2 %>%  filter(keyword=="Anthropocene"),
            aes(x=date, y=as.numeric(hits), group=keyword), col="deeppink4") + 
  geom_smooth(aes( color = keyword),method = "gam")



time_trend_v2[which.max(time_trend_v2$hits),] # that way, we know that the highest value for hits is 1000 and it took place during 02/2020. What happened during that month? Covid-19 was declared an outbreak on 30 January 2020.! Then, it is plausible to assume that February was a month of intense fear and worries about the future since many started to understand how our disregard for the environment impact not only biodiversity but humans as well. As we economists know, spillovers happen all the time and the same is valid for infections/diseases.


#---- Twitter ---- https://medium.com/swlh/how-to-train-word2vec-model-using-gensim-library-115b35440c90 https://github.com/bmschmidt/wordVectors
# Initially my idea was to see how the narrative changes after environmental conferences # the problem is, however, that the free twitter dev version only allows for a search through the last 7 days. due to that, I decided to explore a little of textual analysis and see how people perceive left ideas nowadays.

### first, you have to create a twitter token. this can be done here, using information obtained through the twitter developer platform.

# app
appname <- "your app name"

## api key
secret <- "your api key"

## api secret 
secret <- "your api secret "

## access_token
access_token <- "your access_token"

## access_secret

access_secret <- "your access_secret"

twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)

dfecos <- search_tweets("Eco-socialism OR ecosocialism", token = auth,n = Inf) 

dfmarx  <- search_tweets("Marxism OR marxist OR marxism", token = auth,n = Inf) 

dfantro  <- search_tweets("Anthropocene", token = auth,n = Inf) 

dfdeg  <- search_tweets("Degrowth", token = auth,n = Inf) 

dffoster <- search_tweets("John Bellamy Foster OR JB Foster",
                           token = auth,n = Inf)

dflowy <- search_tweets("Michael Löwy OR Löwy ",
                           token = auth,n = Inf)

dfsaito <- search_tweets("kohei saito",
                           token = auth,n = Inf)
