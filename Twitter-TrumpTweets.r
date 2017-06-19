#https://www.r-bloggers.com/sentiment-analysis-on-donald-trump-using-r-and-tableau/

library(twitteR)
library(ROAuth)
require(RCurl)
library(stringr)
library(tm)
library(ggmap)
library(dplyr)
library(plyr)
library(wordcloud)

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

N=2000  # tweets to request from each query
S=200  # radius in miles
lats=c(38.9,40.7,37.8,39,37.4)#,28,30,42.4,48,36)#,32.3,33.5,34.7,33.8,37.2,41.2,46.8,46.6,37.2,43,42.7,40.8,36.2,38.6,35.8,40.3,43.6,40.8,44.9,44.9)

lons=c(-77,-74,-122,-105.5,-122)#,-82.5,-98,-71,-122,-115)#,-86.3,-112,-92.3,-84.4,-93.3,-104.8,-100.8,-112, -93.3,-89,-84.5,-111.8,-86.8,-92.2,-78.6,-76.8,-116.2,-98.7,-123,-93)

length(lats)
#cities=DC,New York,San Fransisco,Colorado,Mountainview,Tampa,Austin,Boston,
#       Seatle,Vegas,Montgomery,Phoenix,Little Rock,Atlanta,Springfield,
#       Cheyenne,Bisruk,Helena,Springfield,Madison,Lansing,Salt Lake City,Nashville
#       Jefferson City,Raleigh,Harrisburg,Boise,Lincoln,Salem,St. Paul
#dt <- searchTwitter('donald+trump',              lang="en",n=N,resultType="recent"              )#
#dt1 <- searchTwitter('donald+trump',   geocode=paste(38.9,-77,paste0(S,"mi"),sep=",") ,lang="en",n=N,resultType="recent"              )
#class(donald)
#class(dt)

donald=do.call(rbind,lapply(1:length(lats), function(i) searchTwitter('donald+trump',
                                                                      lang="en",n=N,resultType="recent",
                                                                      geocode=paste(lats[i],lons[i],paste0(S,"mi"),sep=","))
                            ))

donaldlat=sapply(donald, function(x) as.numeric(x$getLatitude()))
donaldlat=sapply(donaldlat, function(z) ifelse(length(z)==0,NA,z))  

donaldlon=sapply(donald, function(x) as.numeric(x$getLongitude()))
donaldlon=sapply(donaldlon, function(z) ifelse(length(z)==0,NA,z))  

donalddate=lapply(donald, function(x) x$getCreated())
donalddate=sapply(donalddate,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))

donaldtext=sapply(donald, function(x) x$getText())
donaldtext=unlist(donaldtext)

isretweet=sapply(donald, function(x) x$getIsRetweet())
retweeted=sapply(donald, function(x) x$getRetweeted())
retweetcount=sapply(donald, function(x) x$getRetweetCount())

favoritecount=sapply(donald, function(x) x$getFavoriteCount())
favorited=sapply(donald, function(x) x$getFavorited())

data=as.data.frame(cbind(tweet=donaldtext,date=donalddate,lat=donaldlat,lon=donaldlon,
                         isretweet=isretweet,retweeted=retweeted, retweetcount=retweetcount,favoritecount=favoritecount,favorited=favorited))
data$lat
head(data)
colnames(data)
data$lat <- NULL
data$lon <- NULL
data$tweet <- iconv(data$tweet, to = "utf-8")
 # Create corpus
corpus=Corpus(VectorSource(data$tweet))
#remove all junk chars
corpus <- tm_map(corpus, function(x) iconv(enc2utf8(x), sub = "byte"))

# Convert to lower-case
corpus=tm_map(corpus,tolower)

# Remove stopwords
corpus=tm_map(corpus,function(x) removeWords(x,stopwords()))

# convert corpus to a Plain Text Document
corpus=tm_map(corpus,PlainTextDocument)

col=brewer.pal(6,"Dark2")

corpus <- Corpus(VectorSource(corpus)) 
tdm <- TermDocumentMatrix(corpus)  
matrix <- as.matrix(tdm)  # changed to term.matrix
v <- sort(rowSums(matrix),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
wordcloud(d$word)

wordcloud(d$word, min.freq=25, scale=c(5,2),rot.per = 0.25, random.color=T, max.word=45, random.order=F,colors=col)
