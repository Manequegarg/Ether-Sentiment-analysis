install.packages("tm")
library(tm)
library(twitteR)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(devtools)
library(wordcloud)
download.file("http://cran.cnr.berkeley.edu/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz", "Rstem_0.4-1.tar.gz") 
install.packages("Rstem_0.4-1.tar.gz", repos=NULL, type="source")
download.file("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz", "sentiment.tar.gz")
install.packages("sentiment.tar.gz", repos=NULL, type="source")
install.packages("RCurl")
consumer_key <- 'hXciUFIwuGeM1jjygmSP3mUxg'
consumer_secret <- 'EWORqRjKX3x2b3uTz6DkIt9Uya7zP4Jg5FIBAchWkeMTF5BKAA'
access_token <- '140815963-rNdoo8y0wwvzUAXFBgzE7GDYDpUrZ6AP4s42TV4j'
access_secret <- 'IQZHbyIQKWF72UJ2eWBru1eBjnSZRcfP3sR2pzxz2zBEi'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret )


# harvest some tweets
some_tweets <- searchTwitter("Ether", n=20000, lang="en")

#Saving Raw Tweets
tweetsdf <- twListToDF(some_tweets)

# get the text
some_txt <- sapply(some_tweets, function(x) x$getText())
some_date<-sapply(some_tweets, function(x) x$getCreated())
Date_df = as.POSIXct(some_date,origin = '1960-01-01',tz = "UTC")

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
sent_df = data.frame(text=some_txt, emotion=emotion, polarity=polarity,
  stringsAsFactors=FALSE)

analyse_df = data.frame(text=some_txt, emotion=emotion, polarity=polarity,date=Date_df,
                              stringsAsFactors=FALSE)
# sort data frame
#sent_df = within(sent_df,
                # emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

# Saving the file
write.csv(analyse_df,file = "/Users/manikgarg/Desktop/Tweets2.csv" )

# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of tweets") +
  labs(title = "Sentiment Analysis of Tweets about Starbucks\n(classification by emotion)",
       plot.title = element_text(size=12))

# plot distribution of polarity
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="number of tweets") +
  labs(title = "Sentiment Analysis of Tweets about Ether\n(classification by polarity)",
       plot.title = element_text(size=12))

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
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(4,.5), random.order = FALSE, title.size = 1.5)

