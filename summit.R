
# in this file, I will try to see if there is some relationship between environmental conferences/summit and the raise of a marxist view on environmental issues.
# my hypothesis is that by seeing how incapable governments are currently to deal with the demanding issue regarding climate change, people react to that by adopting more radical perspectives.
# for the second part, using textual analysis, web scrapping and machine learning, I will see how, in general, people behave towards these ideas and authors
# I will also be using it to understand how to sync R and GitHub.


if (!require("pacman")) install.packages("pacman")
p_load(ggplot2,tidyverse,gtrendsR, rtweet)


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

ggplot(data=time_trend,
            aes(x=date, y=as.numeric(hits), group=keyword, col=keyword)) +
  geom_line(size = .9, alpha = .75) + labs(
    title = "Google Search Volume",
    x = "Time",
    y = "General Interest",
    colour = "keyword"
  )+theme_minimal()



# there is some kind of trend break? chow test

#---- Twitter ---- https://medium.com/swlh/how-to-train-word2vec-model-using-gensim-library-115b35440c90 https://github.com/bmschmidt/wordVectors
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

dfecos <- search_tweets("Eco-socialism OR ecosocialism", token = auth,n = Inf) # the problem is that the free dev version only allows for a search through the last 7 days.

dfmarx  <- search_tweets("Marxism OR marxist OR marxism", token = auth,n = Inf) 

dfantro  <- search_tweets("Anthropocene", token = auth,n = Inf) 

dfdeg  <- search_tweets("Degrowth", token = auth,n = Inf) 

dffoster <- search_tweets("John Bellamy Foster OR JB Foster",
                           token = auth,n = Inf)

dflowy <- search_tweets("Michael Löwy OR Löwy ",
                           token = auth,n = Inf)

dfsaito <- search_tweets("kohei saito",
                           token = auth,n = Inf)
