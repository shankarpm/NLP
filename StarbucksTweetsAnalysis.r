#https://sites.google.com/site/miningtwitter/questions/sentiment/sentiment

# if (!require('pacman')) install.packages('pacman')
# pacman::p_load(devtools, installr)
# install.Rtools()
# install_url('http://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz')
# install_url('http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz')


library(twitteR)
library(ROAuth)
library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)



setwd("C:/Users/Shankar/Documents/R Docs/NLP")
key="3sxT57jBQeOpWx7youauzNENF"
secret="aoDHi4eUZIwyOVSONzIXkioyX1jp0ST4yQtOQ7PryLEOBV87zk"
access_token = "36693237-7nAgQv8Xyqv0SecSWsjQNedp5GGVsm9zsGJmeKFSl"
access_token_secret = "LHD1uRtPvxWCk6NjQLHK1vezk7jZA0hxkazTUy8ITxuIs"
#download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem",method="auto")

authenticate <- OAuthFactory$new(consumerKey=key, consumerSecret=secret,
                                 requestURL="https://api.twitter.com/oauth/request_token",
                                 accessURL="https://api.twitter.com/oauth/access_token",
                                 authURL="https://api.twitter.com/oauth/authorize")

#authenticate$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
#registerTwitterOAuth('2803985')
setup_twitter_oauth(key, secret,access_token,access_token_secret)
save(authenticate, file="twitter authentication.Rdata")


some_tweets = searchTwitter("starbucks", n=1500, lang="en")
head(some_txt)
# get the text
some_txt = sapply(some_tweets, function(x) x$getText())


#Step 3: Prepare the text for sentiment analysis
# remove retweet entities
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
# remove at people
some_txt = gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)

# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
# lower case using try.error with sapply 
some_txt = sapply(some_txt, try.error)

# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL
#some_txt_1 <- some_txt
Encoding(some_txt) <- "latin1"   
#remove some junks
some_txt <- iconv(some_txt, "latin1", "ASCII", sub="")
                                                    
# classify emotion
class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(some_txt, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]

# data frame with results
sent_df = data.frame(text=some_txt, emotion=emotion,polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
head(sent_df[sent_df$polarity == 'negative',],100)


#Let's do some plots of the obtained results
# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
geom_bar(aes(y=..count.., fill=emotion)) +
scale_fill_brewer(palette="Dark2") +
labs(x="emotion categories", y="number of tweets") 
#+   ggtitle(label = "Sentiment Analysis of Tweets about Starbucks\n(classification by emotion)", title = theme_text(size=12))


ggplot(sent_df, aes(x=polarity)) +   geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +  labs(x="polarity categories", y="number of tweets") 
#+   opts(title = "Sentiment Analysis of Tweets about Starbucks\n(classification by polarity)",        plot.title = theme_text(size=12))


# separating text by emotion
emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo) 
for (i in 1:nemo)
{
  tmp = some_txt[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),  scale = c(3,.5), random.order = FALSE, title.size = 1.5)


