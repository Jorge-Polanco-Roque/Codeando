####################################
##                                ##
###    Social Media Analytics    ###
###     Group     -     10       ###
##          Wells Fargo           ##
####################################




#Cleaning Environment to make sure no other objects in the environment:
#if needed /!\
rm(list = ls())

setwd("C:/Users/Brayan/OneDrive - IESEG/S2/Social Media analysis")

install.packages("readxl")
library("readxl")

#read the datafile
#if needed /!\
day30<-read_excel("C:/Users/Brayan/OneDrive - IESEG/S2/Social Media analysis/day30.xlsx")


# Import Packages
library(tidyr)
library(dplyr)
if(!require("rtweet")) install.packages("rtweet"); 
library("rtweet")
for (i in c('SnowballC','slam','tm','Matrix','tidytext','dplyr','hunspell','purrr')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}
#install.packages("writexl")
library(writexl)


#app details
appname="imsotoxiceuw"
consumerkey="5gfsShx128Y2ZEzAUCMBFlx9t"
consumersecret="TJts3dfCEae7uD2vuAe55R1fiOcBj767PCLVGhSAu7XjTXqftn"
accesstoken="1217365815802105856-KUB96nz3wtrHkjvytfjFjybgQhvml5"
accesssecret="tSe4ObHiOq8OUsCMyPOJgc9t1lJ8BWWXvmCVES9gBwpiU"
  
#Create and define the access token
twitter_token <- create_token(
  app = appname,
  consumer_key = consumerkey,
  consumer_secret = consumersecret,
  access_token = accesstoken,
  access_secret = accesssecret,
  set_renv=FALSE)

#♠full archive
#%code used to get the data
# day30_2<-rtweet::search_30day(q = q,
#                               n =20000, 
#                               fromDate ="202001040600",
#                               toDate ="202002010600",
#                               env_name = "Class",
#                               safedir="C:/Users/Brayan/OneDrive - IESEG/S2/Social Media analysis",
#                               token = twitter_token)
# 
# write.csv2(day30_2, file = "C:/Users/Brayan/OneDrive - IESEG/S2/Social Media analysis/day30.csv")
# write_xlsx(day30_2,"C:/Users/Brayan/OneDrive - IESEG/S2/Social Media analysis/day30.xlsx")
# 
# fullarchive<-rtweet::search_fullarchive(q = q,
#                    n =1000, 
#                    fromDate ="201601010600",
#                    toDate ="201901200600",
#                    env_name = "smaappfullarchive",
#                    token = twitter_token)

#**** Retrieve Data ***#

# Create twitter query to retrieve tweets which involves Wells Fargo 
# q is created with OR because the query expects only one string 

q<-c("WellsFargo OR Wells loan OR Wells finance OR Wells Fargo OR @WellsFargo OR @WellsFargoBank OR @WFAssetMgmt OR @WellsFargoJobs OR @Ask_WellsFargo OR @WFInvesting")

rtweets <- rtweet::search_tweets(q=q, n = 500,include_rts = FALSE)
rtweets<-day30
##########################################################"
#P1 PLOT THE MAP##############
############################################################"

#use latitude and longitude to plot on map

latrtweets <- lat_lng(rtweets)

par(mar = c(0, 0, 0, 0))
if(!require("maps")) install.packages("maps"); library("maps")

maps::map("world",bg="white",col="light grey" ,lwd = .25)

points(latrtweets$lng, latrtweets$lat, pch = 20, cex = 1,col="red")


##########################################################"

###########other data#################################

############################################################"

usersdata<-users_data(rtweets)


#Add all relevant user accounts of Wells Fargo
useraccounts<-c("WellsFargo", "WFAssetMgmt","WellsFargoJobs", "Ask_WellsFargo", "WFInvesting")

#getmore details of the accounts
WFaccounts<-lookup_users(useraccounts)


# Get the 3.000 most recently favorited tweets by WellsFargo.
WFFavourite <- get_favorites("WellsFargo", n = 500)

#Look for the followers of WF accounts - only brings the user IDs
#q does not work in this query hence had to query seperately and join

followers <- get_followers("WellsFargo",n=10)
WAM_followers<-get_followers("WFAssetMgmt",n=10)
WFG_followers<-get_followers("WellsFargoJobs",n=10)
AFG_followers<-get_followers("Ask_WellsFargo",n=10)
WFI_followers<-get_followers("WFInvesting",n=10)

joined_f<-data.frame(rbind(as.data.frame(followers),as.data.frame(WAM_followers)))
joined_f<-data.frame(rbind(as.data.frame(joined_f),as.data.frame(WFG_followers)))
joined_f<-data.frame(rbind(as.data.frame(joined_f),as.data.frame(AFG_followers)))
joined_f<-data.frame(rbind(as.data.frame(joined_f),as.data.frame(WFI_followers)))

followers<-joined_f %>%group_by(user_id)
followers<-joined_f%>%arrange(user_id)



#To get data of all the followers and their interests
wf_followers_info <- lookup_users(followers$user_id)


# Gather the Twitter list memberships of the accounts followed by WF
WFmemberships <- lists_memberships(
  user = useraccounts,
  cursor = "-1",
  filter_to_owned_lists = FALSE,
  parse = TRUE,
  previous_cursor = NULL
)

#n=50 number of memberships and fol[1:50,1] means number of users
fol<-as.data.frame(followers)
WF_followerinterest <- do_call_rbind(lapply(fol[1:10,], lists_memberships, n=50))


# Gather the Twitter users that are member of the list "finance" 
# get the timeline for all members

members<-lists_members(
  list_id = NULL,
  slug ="finance",
  owner_user = useraccounts,
  n = 50,
  cursor = "-1",
)
#textmin<-data.frame(members)
#timelines<-get_timelines(textmin[1:10,"user_id"],n=20)

#Get tweets of WellsFargo accounts for a period of time
WF<-get_timeline(useraccounts, n=100)

#Count observations for each account
table(WF$screen_name)

################################################################
#P2 PLOT TIMELINE#########################
##################################################################"


colnames(rtweets)

print(unique(rtweets$location))
glimpse(rtweets)
print(rtweets$created_at)

#☻creation of some variables

rtweets$Time <- format(as.POSIXct(rtweets$created_at,format="%Y:%m:%d %H:%M:%S"),"%H:%M:%S")
rtweets$Date <- format(as.POSIXct(rtweets$created_at,format="%Y:%m:%d %H:%M:%S"),"%d:%m:%Y")
rtweets$Year <- format(as.POSIXct(rtweets$created_at,format="%Y:%m:%d %H:%M:%S"),"%Y")
rtweets$Month <- format(as.POSIXct(rtweets$created_at,format="%Y:%m:%d %H:%M:%S"),"%m")
rtweets$Day <- format(as.POSIXct(rtweets$created_at,format="%Y:%m:%d %H:%M:%S"),"%d")
rtweets$HoursMin <- format(as.POSIXct(rtweets$created_at,format="%Y:%m:%d %H:%M:%S"),"%H:%M")
rtweets$Hour <- format(as.POSIXct(rtweets$created_at,format="%Y:%m:%d %H:%M:%S"),"%H")

rtweets <- rtweets %>% mutate(DayPeriod = ifelse(Hour < 12, "AM", "PM"))
print(rtweets$DayPeriod)


print(unique(rtweets$Year))
#plot through time

rtweets_year_month <- rtweets %>%count(Year,Month)


if (!require("ggplot2")) install.packages("ggplot2", quiet=TRUE) ; require("ggplot2")
library(ggplot2)

rtweets_year_month %>%
  ggplot(aes(x = Month, y = n)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_text(aes(label=n), vjust=1.6, color="white", size=3.5)+
  facet_wrap(~ Year, ncol = 2) +
  labs(title = "Number of tweets per period",
       subtitle = "Data ploted by year and month",
       y = "Number of tweets",
       x = "Months") + theme_bw(base_size = 10)

rtweets_Hour <- rtweets %>%count(Hour)

rtweets_Hour %>%
  ggplot(aes(x = Hour, y = n)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_text(aes(label=n), vjust=1.6, color="white", size=3.5)+
  labs(title = "Number of tweets per hour",
       subtitle = "Data ploted by hour",
       y = "Number of tweets",
       x = "Hours") + theme_bw(base_size = 10)



#####################################

######### TEXT PROCESSING ###########

#####################################


#**** Clean the Data ***#
#keep only the enlgish tweets
rtweets<-rtweets[rtweets$lang=="en",]


#Preprocessing with TidyText for tweet data by WellsFargo
# Remove punctuation and numbers with regular expressions
WFTweets <- mutate(rtweets, text = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))


# Tokenization and coverting to lowercase
WFTokenized <- WFTweets %>% unnest_tokens(output = "word", 
                                          input = text,
                                          token = "words", 
                                          drop=FALSE,to_lower=TRUE)
WFTokenized%>%
  count(word,Sort=TRUE)

#Remove some other elements such as # and @ signs if they might occur
WFTokenized <- filter(WFTokenized, substr(word, 1, 1) != '#', 
                      substr(word, 1, 1) != '@') # This compares for the first letter of a token# omit hashtags

#Standardizing to have correct spellings
correct_spelling <- function(input) {
  output <- case_when( 
    # any manual corrections
    input == 'license' ~ 'licence',
    # check and (if required) correct spelling
    !hunspell_check(input, dictionary('en_GB')) ~
      hunspell_suggest(input, dictionary('en_GB')) %>%
      # get first suggestion, or NA if suggestions list is empty
      map(1, .default = NA) %>%
      unlist(),
    TRUE ~ input # if word is correct
  )
  # if input incorrectly spelled but no suggestions, return input word
  ifelse(is.na(output), input, output)
}

#Apply function to correct spellings in WF dataframe
# create a new variable that contains the 'corrected' word
WFTokenized <- WFTokenized %>%  mutate(suggestion = correct_spelling(word))

#Remove stop words
WFTokenized <- WFTokenized %>% anti_join(get_stopwords())

#Remove Wells Fargo name

#WFTokenized <- WFTokenized[WFTokenized$word != c("wells", "fargo"),]
WFTokenized <- WFTokenized[!(WFTokenized$word == "wells" & !WFTokenized$word == "fargo"),]
WFTokenized<- WFTokenized %>% filter_all(all_vars(!grepl('wellsfargo',.)))
#Not stemming as this is going to be used for sentiment analysis
#WFTokenized <- WFTokenized %>% mutate(word = wordStem(word))

#***Document Matrix for machine learning***# 

WFTokenized <- WFTokenized %>% count(status_id,word)
head(WFTokenized)

#Using TFIDF to weight the words

WF_DTM <- WFTokenized %>% cast_dtm(status_id,word,n,weighting = tm::weightTfIdf)

# we can reduce sparseness by removing the most sparse terms:


WF_DTMDense <- removeSparseTerms(WF_DTM,0.9)
WF_DTMDensematrix<-data.matrix(WF_DTMDense)

#write_xlsx(WF_DTM_df,"C:/Users/dwijayaweera/Documents/2ndSocial Media Analytics/GroupAssignment/WF.xlsx")



########  Preprocessing with TidyText for other twitter data of WellsFargo

# Remove punctuation and numbers with regular expressions
tidytweets <- mutate(rtweets, text = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))


# Tokenization and coverting to lowercase
TweetsTokenized <- tidytweets %>% unnest_tokens(output = "word", 
                                                input = text,
                                                token = "words", 
                                                drop=FALSE,to_lower=TRUE)
TweetsTokenized%>%
  count(word,Sort=TRUE)

#Remove some other elements such as # and @ signs if they might occur
TweetsTokenized <- filter(TweetsTokenized, substr(word, 1, 1) != '#', 
                          substr(word, 1, 1) != '@') 


#Apply the previous function to correct spellings in dataframe
# create a new variable that contains the 'corrected' word
TweetsTokenized <- TweetsTokenized %>%  mutate(suggestion = correct_spelling(word))

#Remove Wells Fargo name as it would not add any value
TweetsTokenized <- TweetsTokenized[!(TweetsTokenized$word == "wells" & !TweetsTokenized$word == "fargo"),]

#Remove stop words
TweetsTokenized <- TweetsTokenized %>% anti_join(get_stopwords())


#Not stemming as this is going to be used for sentiment analysis
#TweetsTokenized <- TweetsTokenized %>% mutate(word = wordStem(word))

#***Document Matrix for machine learning***# 

TweetsTokenized <- TweetsTokenized %>% count(status_id,word)
head(TweetsTokenized)

#Using TFIDF to weight the words

UsersTweets_DTM <- TweetsTokenized %>% cast_dtm(status_id,word,n,weighting = tm::weightTfIdf)
UsersTweets_DTMm = data.matrix(UsersTweets_DTM)
UserTweets_DTMx<-data.frame(UsersTweets_DTMm)


# we can reduce sparseness by removing the most sparse terms:
UsersTweets_DTMDense <- removeSparseTerms(UsersTweets_DTM,0.9)
UsersTweets_DTMDense<-data.matrix(UsersTweets_DTMDense)

##############################################################

###########  Sentiment Analysis ##############################

##############################################################
# Dowloading libraries
library(ggplot2)
library(tidyr)
for (i in c('SnowballC','slam','tm','Matrix','tidytext','dplyr','hunspell','purrr')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)}

# Creating the main database

db = rtweets

db2 = db[,c("user_id","status_id","text","source","location")]

save(db2,file ='C:/Users/jpolancoroque/Desktop/IESEG/02. Second Semester/Social Media Analytics/Group assignment/RData files/db2.Rdata')

# Cleaning and filtering the data (we only need tweets in english)
wf_db = db[,c("status_id","text","lang")]
wf_db = filter(wf_db, lang == "en")

# Removing character that we don't need
wf_db = mutate(wf_db, message = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))

# Tokenization
wf_db_token <- wf_db %>% unnest_tokens(output = "word",
                                       input = message, 
                                       token = "words", 
                                       drop=FALSE,to_lower=TRUE) 

wf_db_token = wf_db_token[,c("status_id","message","word")]

# Removing elements such as # and @ signs
wf_db_token = filter(wf_db_token, substr(word, 1, 1) != '#', substr(word, 1, 1) != '@') 


# Removing stop words
wf_db_token = wf_db_token %>% anti_join(get_stopwords()) 

# Stemming
wf_db_token = wf_db_token %>% mutate(suggestion = wordStem(word)) 

# Creating DRM
wf_db_DTM = wf_db_token %>% count(status_id,suggestion)
wf_db_DTM = wf_db_DTM %>% rename(word = suggestion)

# DTM
# WF_DTM = wf_db_DTM %>% cast_dtm(status_id,word,n,weighting = tm::weightTfIdf)
# WF_DTM = as.data.frame(WF_DTM)

wf_freq = wf_db_DTM %>% group_by(word) %>% 
  summarize(freq = n()) %>%
  arrange(-freq) %>% filter(freq > 200)



wf_freq %>%
  ggplot(aes(x = reorder(word,freq), y = freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label=freq), color="black", size=2.5)+
  labs(title = "Top used Words",
       y = "Frequency of words",
       x = "Words") + theme_bw(base_size = 10) + coord_flip()

# Word cloud
tf = termFreq(wf_freq$word)
wordcloud(names(tf),tf,
          max.words=200,min.freq = 100,type="url",
          scale=c(1,1))

wf_freq2 = wf_db_DTM %>% group_by(word) %>% 
  summarize(freq = n()) %>%
  arrange(-freq) 



set.seed(1234) 
wordcloud(words = wf_freq2$word, freq = wf_freq2$freq, min.freq = 100,
          max.words=200, random.color=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"), scale = c(1,1))

#########################################

# Sentiment Analysis

#######################################

# looking at the diccionaries
get_sentiments(lexicon = c("afinn", "bing", "loughran", "nrc"))

# Counting Positive and Negative words in NRC diccionary
get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", 
                          "negative")) %>% 
  count(sentiment)

# Counting all the word classifications in NRC diccionary
get_sentiments("nrc") %>% 
  count(sentiment)

# Matching my list of words vs NRC diccionary's word
wf_sentiment_a = inner_join(wf_db_DTM,get_sentiments("afinn"))
wf_sentiment_l = inner_join(wf_db_DTM,get_sentiments("loughran"))
wf_sentiment_b = inner_join(wf_db_DTM,get_sentiments("bing"))


############################
###### NRC DICCIONARY ######
############################

# Matching my list of words vs NRC diccionary's word
wf_sentiment_n = inner_join(wf_db_DTM,get_sentiments("nrc"))

# The following step will give us back a propper table to plot
wf_summary_n = wf_sentiment_n %>% filter(n>1) %>% count(word,sentiment,sort=TRUE) %>%
  group_by(sentiment) %>%
  as.data.frame(stringsAsFactors=FALSE)

save(wf_summary_n, file = "C:/Users/jpolancoroque/Desktop/IESEG/02. Second Semester/Social Media Analytics/Group assignment/RData files/wf_summary_n.Rdata")

# Using ggplot, here we have the most mention words by sentiment classification
wf_summary_n %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# Getting a summary per post
wf_status_n = wf_sentiment_n %>%
  count(status_id, sentiment) %>%                
  spread(sentiment, n, fill = 0) %>%      
  mutate(net_sentiment = positive - negative)

############################
##### BING DICCIONARY ######
############################

# Matching my list of words vs Bing diccionary's word
wf_sentiment_b = inner_join(wf_db_DTM,get_sentiments("bing"))

# The following step will give us back a propper table to plot
wf_summary_b = wf_sentiment_b %>%  count(word,sentiment,sort=TRUE) %>%
  group_by(sentiment) %>% filter(n>5) %>%
  as.data.frame(stringsAsFactors=FALSE)

save(wf_summary_b, file = "C:/Users/jpolancoroque/Desktop/IESEG/02. Second Semester/Social Media Analytics/Group assignment/RData files/wf_summary_b.Rdata")


# Using ggplot, here we have the most mention words by sentiment classification
wf_summary_b %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# Getting a summary per post
wf_status_b = wf_sentiment_b %>%
  count(status_id, sentiment) %>%                
  spread(sentiment, n, fill = 0) %>%      
  mutate(net_sentiment = positive - negative)

############################
### LOUGHRAN DICCIONARY ####
############################

# Matching my list of words vs Bing diccionary's word
wf_sentiment_l = inner_join(wf_db_DTM,get_sentiments("loughran"))

# The following step will give us back a propper table to plot
wf_summary_l = wf_sentiment_l %>%  count(word,sentiment,sort=TRUE) %>%
  group_by(sentiment) %>%
  as.data.frame(stringsAsFactors=FALSE)

save(wf_summary_l, file = "C:/Users/jpolancoroque/Desktop/IESEG/02. Second Semester/Social Media Analytics/Group assignment/RData files/wf_summary_l.Rdata")


# Using ggplot, here we have the most mention words by sentiment classification
wf_summary_l %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# Getting a summary per post
wf_status_l = wf_sentiment_l %>%
  count(status_id, sentiment) %>%                
  spread(sentiment, n, fill = 0) %>%      
  mutate(net_sentiment = positive - negative)

############################
##### AFINN DICCIONARY #####
############################

# Matching my list of words vs Bing diccionary's word
wf_sentiment_a = inner_join(wf_db_DTM,get_sentiments("afinn")) %>%
  group_by(status_id) %>%                      
  summarize(Sentiment = sum(value))

save(wf_sentiment_a, file = "C:/Users/jpolancoroque/Desktop/IESEG/02. Second Semester/Social Media Analytics/Group assignment/RData files/wf_sentiment_a.Rdata")


# Total Average of Sentiment
mean(wf_sentiment_a$Sentiment)

ggplot(data = wf_sentiment_a, aes(x = "Tweets", y = Sentiment)) +
  geom_boxplot(alpha = 0) +
  geom_jitter(alpha = 0.3, color = "tomato")

##############################################################

###########  Topic modeling - Machine Learning ##########

##############################################################

if (!require("topicmodels")) install.packages("topicmodels", quiet=TRUE) ; require("topicmodels")
if (!require("Rmpfr")) install.packages("topicmodels", quiet=TRUE) ; require("topicmodels")
library("Rmpfr")

for (i in c('SnowballC','slam','tm','Matrix','dplyr','tidytext')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

WF_DTMfreq <- WFTokenized %>% cast_dtm(status_id,word,n,weighting = tm::weightTf)
class(WF_DTMfreq)

#find the optimum K using the harmonic mean function
#source 1:https://towardsdatascience.com/probability-concepts-explained-maximum-likelihood-estimation-c7b4342fdbb1
#source 2:https://stackoverflow.com/questions/21355156/topic-models-cross-validation-with-loglikelihood-or-perplexity/21394092#21394092

harmonicMean <- function(logLikelihoods, precision = 2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}

#set the interal of topic the function to research in
#in our case 37 is the optimal k, so we can put it manually to gain some times
seqk <- seq(2, 50, 1)
burnin <- 1000
iter <- 1000
keep <- 50
system.time(fitted_many <- lapply(seqk, function(k) topicmodels::LDA(WF_DTMfreq, k = k,
                                                                     method = "Gibbs",control = list(burnin = burnin,
                                                                                                     iter = iter, keep = keep) )))

logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])
hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))

#·number of optimal topic return by the function
kopti=seqk[which.max(hm_many)]
#or manual input
kopti=37

#use of the optimum K in the LDA model

lda_model <- LDA(x = WF_DTMfreq, k = kopti, control = list(seed = 283))

tweet_topics <- tidy(lda_model, matrix = "beta")

# you can use the following code to get the top terms per topic
top_tweet_terms <- tweet_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_tweet_terms

#######################################"
#P3 -  PLOT TOPICS
#######################################

top_tweet_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

top_tweet_terms2 <- top_tweet_terms %>% 
  group_by(topic) %>%
  dplyr::mutate(
    first = dplyr::first(term)
  )
top_tweet_terms2<-top_tweet_terms2 %>% distinct(topic, .keep_all = TRUE)

llis.topics <- topicmodels::topics(lda_model, 1)
# returning the top 20 terms.
llis.terms <- as.data.frame(topicmodels::terms(lda_model, 20), stringsAsFactors = FALSE)
llis.terms[1:5]

# Creates a dataframe to store the twitts Number and the most likely topic
doctopics.df <- as.data.frame(llis.topics)
doctopics.df <- dplyr::transmute(doctopics.df,  status_id = rownames(doctopics.df), Topic = llis.topics)
doctopics.df$statusid <- as.integer(doctopics.df$status_id)

## Adds topic number to original dataframe of tweets
rtweetstopics <- dplyr::inner_join(rtweets, doctopics.df, by = "status_id")

#######################################"
#P4 -  PLOT TOPICS DESCENDING AND PERCENTAGE
#######################################

topicsplot <- rtweetstopics%>%
                    count(Topic)%>%
                    mutate(Percent = (n/sum(n)*100))%>%
                    arrange(desc(n))  

topicsplot <- topicsplot %>%
                    mutate(csum=cumsum(Percent)/100)

t_plot<- ggplot(topicsplot,aes(x=reorder(factor(Topic),-Percent),y=Percent))+ 
                  geom_bar(stat="identity")+
                  labs(x="Topics",y="Percentage")+
                  geom_line(aes(y=csum,group=1,linetype="Cumulative Percentage"))+
                  ggtitle("Percentage per Topic")
t_plot

t_csum_plot <- ggplot(topicsplot,aes(x=reorder(factor(Topic),-Percent),y=Percent))+ 
                    labs(x="Topics",y="Cumulative Percentage")+
                    geom_line(aes(y=csum,group=1,linetype="Cumulative Percentage"))+
                    ggtitle("Cumulative Percentage per Topic")+
                    theme(axis.text.x = element_text(angle = 90, hjust = 1))
t_csum_plot


###other plot


unique(rtweets$location)

rtweets%>%
    count(location)

#function that round the numeric variables of a df
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}

#T1
wf_tweets <- rtweets[rtweets$is_retweet==FALSE,]
wf_retweets <- rtweets[rtweets$is_retweet==TRUE,]
wf_reply <- subset(wf_tweets,!is.na(wf_tweets$reply_to_status_id))
ctt<-as.numeric(count(wf_tweets))
crt<-as.numeric(count(wf_retweets))
crp<-as.numeric(count(wf_reply))
class(ctt)
ratio_tt <- data.frame(tweet_type=c("Original","Retweet","Reply"),count=c(ctt,crt,crp))

# columns
ratio_tt$fraction = ratio_tt$count / sum(ratio_tt$count)
ratio_tt$percentage = ratio_tt$count / sum(ratio_tt$count) * 100
ratio_tt$ymax = cumsum(ratio_tt$fraction)
ratio_tt$ymin = c(0, head(ratio_tt$ymax, n=-1))
ratio_tt <- round_df(ratio_tt,2)
Type_of_Tweet <- paste(ratio_tt$tweet_type, ratio_tt$percentage, "%")

ggplot(ratio_tt, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3,fill=Type_of_Tweet)) +
  geom_rect() +
  xlim(c(2, 4)) +
  coord_polar("y", start = 0)+
  scale_fill_brewer(palette="Blues")+
  theme_void() +
  theme(legend.position = "left")+
  ggtitle("Ratio of tweets category")


#T2

top10 <- rtweets[,c("user_id","name","hashtags","lang","retweet_source")]
glimpse(top10)

library(dplyr)

top10lang <- top10%>%
  count(lang)%>%
  arrange(desc(n))%>%
  top_n(5)

top10name <- top10%>%
  count(name)%>%
  arrange(desc(n))%>%
  top_n(5)


top10rts<- top10%>%
  count(retweet_source)%>%
  arrange(desc(n))%>%
  top_n(5)

top10rts$retweet_source[is.na(top10rts$retweet_source)]<- "No Identification"

# Load ggplot2
library(ggplot2)

# Barplots
ggplot(top10lang, aes(x=reorder(lang,n), y=n)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label=n), position=position_dodge(width = 1), hjust=-0.1,vjust=-0.5)+
  ggtitle("Number of tweets per language")+
  ylab("number")+
  xlab("language")+
  theme_minimal()+
  coord_flip()

ggplot(top10name, aes(x=reorder(name,n), y=n)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label=n), position=position_dodge(width = 1), hjust=-0.1,vjust=-0.5)+
  ggtitle("Top user mentionning Wells Fargo")+
  ylab("number")+
  xlab("user name")+
  coord_flip()

Type_of_Tweet <- ggplot(top10rts, aes(x=reorder(retweet_source,n), y=n)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label=n), position=position_dodge(width = 1), hjust=-0.1,vjust=-0.5)+
  ggtitle("Number per retweet source type")+
  ylab("number")+
  xlab("source type")+
  coord_flip()



##############################################################

#########################  SHINY ###########################"#

##############################################################


# Loading the libraries
# if (!require("shiny")) install.packages("shiny", quiet=TRUE) ; require("shiny")
# if (!require("shinydashboard")) install.packages("shinydashboard", quiet=TRUE) ; require("shinydashboard")
# if (!require("ggplot2")) install.packages("ggplot2", quiet=TRUE) ; require("ggplot2")
# if(!require("maps")) install.packages("maps"); library("maps")
# if (!require("tm")) install.packages("tm", quiet=TRUE) ; require("tm")
# if (!require("wordcloud")) install.packages("wordcloud", quiet=TRUE) ; require("wordcloud")

options(shiny.sanitize.errors = TRUE)

library(dplyr, warn.conflicts = FALSE)
library(shiny, warn.conflicts = FALSE)
library(shinydashboard, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(tm, warn.conflicts = FALSE)
library(maps, warn.conflicts = FALSE)
library(wordcloud, warn.conflicts = FALSE)
library(NLP, warn.conflicts = FALSE)
# library(RColorBrewer, warn.conflicts = FALSE)

### SECTION #1: Loading databases and tables ###

t0 = data.frame(wf_summary_n)

# load(file = 'wordsoup.Rdata')
# t1 = data.frame(wordsoup)

t2 = data.frame(latrtweets)

t3 = data.frame(rtweets_year_month)

t3_2 = data.frame(rtweets_Hour)

t4_1 = data.frame(ratio_tt)

t4_2 = data.frame(Type_of_Tweet)

t5 = data.frame(WFTokenized)

t6 = data.frame(wf_summary_b)

t7 = data.frame(wf_summary_l)

t8 = data.frame(wf_sentiment_a)

t9 = data.frame(wf_freq)

t10 = data.frame(wf_freq2)

t11 = data.frame(wf_summary_n)

t12 = data.frame(wf_summary_b)

t13 = data.frame(wf_summary_l)

t14 = data.frame(wf_sentiment_a)

t15 = data.frame(db2)

t16 = data.frame(top10lang)

t17 = data.frame(top_tweet_terms)

t18 = data.frame(top10name)

t19 = data.frame(top10rts)


### SECTION #2: Creating dashboard on Shiny ###

# Uploading to the server

rsconnect::setAccountInfo(name='bthomasemil',
                          token='00BC6E071139C79E5733F4116CC44538',
                          secret='cjC4qY0ooViKMW+JP4QBQ4/TJtplrE5W/aslNaPY')
# Creating a title for the dashboard 
header <- dashboardHeader(title = "Menu")

# Defining the sidebar/slides where we want to put our charts
# Here we are defining some subsections with their respective dashboards
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text = "Highlights",tabName = "d0",icon = icon("th")),
    
    menuItem('Dynamic Table',icon = icon("table"), tabName = "d8"),
    
    menuItem('1. Overview',icon = icon("bar-chart-o"),
             menuSubItem(text = "a) Tweet Map",tabName = "d3",icon = icon("document")),
             menuSubItem(text = "b) Tweets per Month",tabName = "d4",icon = icon("document")),
             menuSubItem(text = "c) Tweets per Hour",tabName = "d5",icon = icon("document")),
             menuSubItem(text = "d) Ratio of tweets category",tabName = "d6",icon = icon("document")),
             menuSubItem(text = "e) Top Words",tabName = "d13",icon = icon("document")),
             menuSubItem(text = "f) Top Languages",tabName = "d15",icon = icon("document")),
             menuSubItem(text = "g) Top Usernames",tabName = "d16",icon = icon("document")),
             menuSubItem(text = "h) Top RTs",tabName = "d17",icon = icon("document"))),
    
    menuItem('2. Dictionary Look-Up',icon = icon("bar-chart-o"),
             menuSubItem(text = "a) Wordcloud",tabName = "d2", icon = icon("document")),
             menuSubItem(text = "b) NRC Dictionary",tabName = "d14",icon = icon("document")),
             menuSubItem(text = "c) Bing Dictionary",tabName = "d10",icon = icon("document")),
             menuSubItem(text = "d) Loughran Dictionary",tabName = "d11",icon = icon("document")),
             menuSubItem(text = "e) AFINN Dictionary",tabName = "d12",icon = icon("document"))),
    
    menuItem('3. Model Reports',icon = icon("bar-chart-o"),
             menuSubItem(text = "a) Top terms per Topic",tabName = "d7", icon = icon("document")),
             menuSubItem(text = "b) Terms per Topics",tabName = "d9",icon = icon("document")))))

# Once created the tabs, here we are selecting the elements that will be withing each tab
# Each section will have a title, text box and a graphic
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "d0",
            # Slide of Highligths
            fluidRow(
              box(width = 12,title = "Key Takeaways","In the following charts, you will find a basic description of our datamart visualizing the main metrics."),
              column(width = 6,infoBox(width = 22,title = "Insight #1",subtitle = "98% of the digital conversation in the last 2 months was in english.",color = 'yellow')),
              column(width = 6,infoBox(width = 22,title = "Insight #2",subtitle = "Most part of the conversation is happening in the East Side of USA. Is these people tends more to give their opinion on social networks?",color = 'yellow')),
              column(width = 6,infoBox(width = 22,title = "Insight #3",subtitle = "Only comparing the data we have, it seems January is a low-commented month.",color = 'green')),
              column(width = 6,infoBox(width = 22,title = "Insight #4",subtitle = "The Twitter participation with original tweets is as big as retweets. Is it because people tend to use other's complaints as own?",color = 'yellow')),
              # valueBox(width = 12, value = "40,498",subtitle = "Unique Users", icon = icon("lightbulb-o")),
              # valueBox(width = 12, value = 80,subtitle = "Countries/Islands", icon = icon("lightbulb-o")),
              # valueBox(width = 12, value = "58%",subtitle = "of the clients come from Germany", icon = icon("lightbulb-o")),
            )),
    
    tabItem(tabName = "d1", h1("NRC Dictionary Look-Up"),
            fluidRow(
              box(width = 12,title = "Analysis","ZZZZZZZZZZZZZZ"),
              box(width = 12,heigth = 8,  plotOutput("plot1")))),
    
    tabItem(tabName = "d2", h1("Wordcloud"),
            fluidRow(
              box(width = 12,title = "Analysis","Most part of the conversation is around the Wells Fargo's centers."),
              box(width = 12,heigth = 8,  plotOutput("plot2")))),
    
    
    tabItem(tabName = "d3", h1("Tweet Map"),
            fluidRow(
              box(width = 12,title = "Analysis","Most of the conversation takes place on the east side of the united states."),
              box(width = 12,heigth = 8,  plotOutput("plot3")))),
    
    tabItem(tabName = "d4", h1("Tweets per Month"),
            fluidRow(
              box(width = 12,title = "Analysis","With just a few days in February, the conversation around WF has reached more than 25% of what we got the entire January. It is recommended to check the digital conversation closely in order to prevent any possible eventuality."),
              box(width = 12,heigth = 8,  plotOutput("plot4")))),
    
    tabItem(tabName = "d5", h1("Tweets per Hour"),
            fluidRow(
              box(width = 12,title = "Analysis","Most part of the digital conversation starts after 5 pm, showing a peak at 6 pm. In order to present a better service, it is importat having people ready to answer during the noon."),
              box(width = 12,heigth = 8,  plotOutput("plot5")))),
    
    tabItem(tabName = "d6", h1("Ratio of tweets category"),
            fluidRow(
              box(width = 12,title = "Analysis","The Twitter participation with original tweets is as big as retweets. Is it because people tend to use other's complaints as own?"),
              box(width = 12,heigth = 8,  plotOutput("plot6")))),
    
    tabItem(tabName = "d7", h1("Top terms per Topic"),
            fluidRow(
              box(width = 12,title = "Analysis","Describe the top terms per topic"),
              box(width = 12,heigth = 8,  plotOutput("plot7")))),
    
    tabItem(tabName = "d8", h1("Dynamic Table"),
            fluidRow(
              DT::dataTableOutput("table"),
              box(width = 12,heigth = 8,  dataTableOutput("plot8")))),
    
    tabItem(tabName = "d9", h1("Terms per Topic"),
            fluidRow(
              box(width = 12,title = "Analysis","Define the topics based on the optimum K"),
              box(width = 12,heigth = 8,  plotOutput("plot9")))),
    
    tabItem(tabName = "d10", h1("Bing Dictionary"),
            fluidRow(
              box(width = 12,title = "Analysis","As positive aspect, it seems people consider that ze have good prices in our services vs competence."),
              box(width = 12,heigth = 8,  plotOutput("plot10")))),
    
    tabItem(tabName = "d11", h1("Loughran Dictionary"),
            fluidRow(
              box(width = 12,title = "Analysis","Complementing the previous chart, other positive aspect is the rewarding system."),
              box(width = 12,heigth = 8,  plotOutput("plot11")))),
    
    tabItem(tabName = "d12", h1("AFINN Dictionary"),
            fluidRow(
              box(width = 12,title = "Analysis","In summary, there are opportunities to improve our service, which it could also improve our image as company. Right now, we are a bit bellow a neutral sentiment."),
              box(width = 12,heigth = 8,  plotOutput("plot12")))),
    
    tabItem(tabName = "d13", h1("Top Words"),
            fluidRow(
              box(width = 12,title = "Analysis","Most part of the conversation is around the Wells Fargo's centers."),
              box(width = 12,heigth = 8,  plotOutput("plot13")))),
    
    tabItem(tabName = "d14", h1("NRC Dictionary Sentiment"),
            fluidRow(
              box(width = 12,title = "Analysis","It seems that a solution to decrease negative sentiments towards us is through improving service time."),
              box(width = 12,heigth = 8,  plotOutput("plot14")))),
    
    tabItem(tabName = "d15", h1("Top Languages"),
            fluidRow(
              box(width = 12,title = "Analysis","98% of the digital conversation in the last 2 months was in english."),
              box(width = 12,heigth = 8,  plotOutput("plot15")))),
    
    tabItem(tabName = "d16", h1("Top Usernames"),
            fluidRow(
              box(width = 12,title = "Analysis","There are a few clients which are in constant conversation with our accounts, it is important to be sure we are solving our client's problems in this communication channel as well."),
              box(width = 12,heigth = 8,  plotOutput("plot16")))),
    
    tabItem(tabName = "d17", h1("Top RTs"),
            fluidRow(
              box(width = 12,title = "Analysis","A small % of the conversation happends through our App. There is a chance to increase our App penetration."),
              box(width = 12,heigth = 8,  plotOutput("plot17"))))
    
  ))

# The dashboard style will be define here
ui <- fluidPage(
  titlePanel("SMA Report"),
  dashboardPage(skin = "blue", header,sidebar,body))

# We are creating the graphs that will be in within the tabs 
# In order to format my graphs, we will use tidyr
server <- function(input, output) {
  output$plot1 <- renderPlot({
    t0 %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y") +
      labs(y = "Contribution to sentiment",
           x = NULL) +
      coord_flip()})
  
  output$plot2 <- renderPlot({
    set.seed(1234) 
    wordcloud(words = t10$word, freq = t10$freq, min.freq = 50,
              max.words=200, random.color=FALSE,random.order = FALSE , rot.per=0.35,
              colors=brewer.pal(8, "Dark2"), scale = c(1,1))})
  
  
  output$plot3 <- renderPlot({
    par(mar = c(0, 0, 0, 0))
    maps::map("world",bg="white",col="light grey" ,lwd = .25)
    points(t2$lng, t2$lat, pch = 20, cex = 1,col="steelblue")})
  
  
  
  output$plot4 <- renderPlot({
    t3 %>%
      ggplot(aes(x = Month, y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_text(aes(label=n), vjust=1.6, color="white", size=3.5)+
      facet_wrap(~ Year, ncol = 2) +
      labs(title = "Number of tweets per period",
           subtitle = "Data ploted by year and month",
           y = "Number of tweets",
           x = "Months") + theme_bw(base_size = 10)})
  
  
  
  output$plot5 <- renderPlot({
    t3_2 %>%
      ggplot(aes(x = Hour, y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_text(aes(label=n), vjust=1.6, color="white", size=3.5)+
      labs(title = "Number of tweets per hour",
           subtitle = "Data ploted by hour",
           y = "Number of tweets",
           x = "Hours") + theme_bw(base_size = 10)})
  
  
  output$plot6 <- renderPlot({
    ggplot(t4_1, aes(x="", y=percentage, fill=tweet_type))+
      geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) +
      scale_fill_brewer(palette="Blues")+
      theme_minimal()})
  
  
  output$plot7 <- renderPlot({
    top_tweet_terms2 %>%
      group_by(topic)%>%
      ggplot(aes(reorder(term,-beta), beta)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = "Top terms per Topic",y = "beta",x = "Term") + 
      facet_grid(.~topic,scales="free")+
      theme_bw(base_size = 10)})
  
  output$table <- DT::renderDataTable({t15
  })
  
  output$plot9 <- renderPlot({
    t17 %>%
      mutate(term = reorder_within(term, beta, topic)) %>%
      ggplot(aes(term, beta, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      coord_flip() +
      scale_x_reordered()})
  
  output$plot10 <- renderPlot({
    wf_summary_b %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y") +
      labs(y = "Contribution to sentiment",
           x = NULL) +
      coord_flip()})
  
  output$plot11 <- renderPlot({
    wf_summary_l %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y") +
      labs(y = "Contribution to sentiment",
           x = NULL) +
      coord_flip()})
  
  output$plot12 <- renderPlot({
    ggplot(data = wf_sentiment_a, aes(x = "Tweets", y = Sentiment)) +
      geom_boxplot(alpha = 0) +
      geom_jitter(alpha = 0.3, color = "Dark Blue")})
  
  output$plot13 <- renderPlot({
    t9 %>%
      ggplot(aes(x = reorder(word,freq), y = freq)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_text(aes(label=freq), color="black", size=2.5)+
      labs(title = "Top used Words",
           y = "Frequency of words",
           x = "Words") + theme_bw(base_size = 10) + coord_flip()})
  
  output$plot14 <- renderPlot({
    t11 %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y") +
      labs(y = "Contribution to sentiment",
           x = NULL) +
      coord_flip()})
  
  output$plot15 <- renderPlot({
    ggplot(t16, aes(x=reorder(lang,n), y=n)) + 
      geom_bar(stat = "identity") +
      geom_text(aes(label=n), position=position_dodge(width = 1), hjust=-0.1,vjust=-0.5)+
      ggtitle("Number of tweets per language")+
      ylab("number")+
      xlab("language")+
      theme_minimal()+
      coord_flip()})
  
  output$plot16 <- renderPlot({
    ggplot(t18, aes(x=reorder(name,n), y=n)) + 
      geom_bar(stat = "identity") +
      geom_text(aes(label=n), position=position_dodge(width = 1), hjust=-0.1,vjust=-0.5)+
      ggtitle("Top user mentionning Wells Fargo")+
      ylab("number")+
      xlab("user name")+
      coord_flip()})
  
  output$plot17 <- renderPlot({
    ggplot(t19, aes(x=reorder(retweet_source,n), y=n)) + 
      geom_bar(stat = "identity") +
      geom_text(aes(label=n), position=position_dodge(width = 1), hjust=-0.1,vjust=-0.5)+
      ggtitle("Number per retweet source type")+
      ylab("number")+
      xlab("source type")+
      coord_flip()
  })
  
}

# Here we are calling to our app
shinyApp(ui, server)