
install.packages("twitteR")
install.packages("ROAuth")
install.packages("NLP")
install.packages("syuzhet")
install.packages("tm")
install.packages("SnowballC")
install.packages("stringi")
install.packages("topicmodels")
install.packages("dplyr")
install.packages("pkgconfig")
install.packages("wordcloud")


library("dplyr")
library("NLP")
library("syuzhet")
library("tm")
library("SnowballC")
library("stringi")
library("topicmodels")
library("syuzhet")
library("twitteR")
library("ROAuth")
library("tm")


#DATA Extraction
consumer_key <- 'kBBwxjaDIFpQ5cCLfVqay7uxa'

consumer_secret <- '0R1Ld1Ob6trWJ93FjDEqj39KCVJgHXSP65FCAcBdxnqDys2kDi'

access_token <- '1596024325-Qzn4p4THOu8lNUMZRgfSZDMkj4CBNmejKXubady'

access_secret <- '3r3SKuvvE7FXhU0Wp0mIFma4LzX3Vc6gDAXnlA0jGCKAB'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#fetching Tweets
tweets_a <- searchTwitter("#amazonindia", n=1000,lang = "en")
tweets_a
tweets_s <- searchTwitter("#snapdeal", n=1000,lang = "en")
tweets_s
tweets_f <- searchTwitter("#flipkart", n=1000,lang = "en")
tweets_f

amazon_tweets <- twListToDF(tweets_a)
snapdeal_tweets <- twListToDF(tweets_s)
flipkart_tweets <- twListToDF(tweets_f)

#DATA Preprocessing
amazon_text<- amazon_tweets$text
snapdeal_text<- snapdeal_tweets$text
flipkart_text<- flipkart_tweets$text

#Remove Special Characters
amazon_text <- iconv(amazon_text, 'UTF-8', 'ASCII')
snapdeal_text <- iconv(snapdeal_text, 'UTF-8', 'ASCII')
flipkart_text <- iconv(flipkart_text, 'UTF-8', 'ASCII')

#convert all text to lower case
amazon_text  <- tolower(amazon_text)
snapdeal_text<- tolower(snapdeal_text)
flipkart_text<- tolower(flipkart_text)

# Replace blank space ("rt")
amazon_text <- gsub("rt", "", amazon_text)
snapdeal_text <- gsub("rt", "", snapdeal_text)
flipkart_text <- gsub("rt", "", flipkart_text)

# Replace @UserName
amazon_text <- gsub("@\\w+", "", amazon_text)
snapdeal_text <- gsub("@\\w+", "", snapdeal_text)
flipkart_text <- gsub("@\\w+", "", flipkart_text)

# Remove punctuation
amazon_text <- gsub("[[:punct:]]", "", amazon_text)
snapdeal_text <- gsub("[[:punct:]]", "", snapdeal_text)
flipkart_text <- gsub("[[:punct:]]", "", flipkart_text)

# Remove links
amazon_text <- gsub("http\\w+", "", amazon_text)
snapdeal_text <- gsub("http\\w+", "", snapdeal_text)
flipkart_text <- gsub("http\\w+", "", flipkart_text)

# Remove tabs
amazon_text <- gsub("[ |\t]{2,}", "", amazon_text)
flipkart_text <- gsub("[ |\t]{2,}", "", flipkart_text)
snapdeal_text <- gsub("[ |\t]{2,}", "", snapdeal_text)

# Remove blank spaces at the beginning
amazon_text <- gsub("^ ", "", amazon_text)
flipkart_text <- gsub("^ ", "", flipkart_text)
snapdeal_text <- gsub("^ ", "", snapdeal_text)

# Remove blank spaces at the end
amazon_text <- gsub(" $", "", amazon_text)
flipkart_text <- gsub(" $", "", flipkart_text)
snapdeal_text <- gsub(" $", "", snapdeal_text)

#create corpus
amazon_tweets.text.corpus <- Corpus(VectorSource(amazon_text))
snapdeal_tweets.text.corpus <- Corpus(VectorSource(snapdeal_text))
flipkart_tweets.text.corpus <- Corpus(VectorSource(flipkart_text))


#getting emotions using in-built function
mysentiment_amazon<-get_nrc_sentiment((amazon_text))
mysentiment_flipkart<-get_nrc_sentiment((flipkart_text))
mysentiment_snapdeal<-get_nrc_sentiment((snapdeal_text))

#calculationg total score for each sentiment
Sentimentscores_amazon<-data.frame(colSums(mysentiment_amazon[,]))

Sentimentscores_flipkart<-data.frame(colSums(mysentiment_flipkart[,]))

Sentimentscores_snapdeal<-data.frame(colSums(mysentiment_snapdeal[,]))

#
names(Sentimentscores_amazon)<-"Score"
Sentimentscores_amazon<-cbind("sentiment"=rownames(Sentimentscores_amazon),Sentimentscores_amazon)
rownames(Sentimentscores_amazon)<-NULL

names(Sentimentscores_flipkart)<-"Score"
Sentimentscores_flipkart<-cbind("sentiment"=rownames(Sentimentscores_flipkart),Sentimentscores_flipkart)
rownames(Sentimentscores_flipkart)<-NULL

names(Sentimentscores_snapdeal)<-"Score"
Sentimentscores_snapdeal<-cbind("sentiment"=rownames(Sentimentscores_snapdeal),Sentimentscores_snapdeal)
rownames(Sentimentscores_snapdeal)<-NULL


#plotting the sentiments with scores
library(ggplot2)
ggplot(data=Sentimentscores_amazon,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+ theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on AMAZON")


ggplot(data=Sentimentscores_flipkart,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+ theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on FLIPKART")


ggplot(data=Sentimentscores_snapdeal,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+ theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on Snapdeal")


#WORD Cloud                                                                        
library(tm)

amazon_tweets.text.corpus <- tm_map(amazon_tweets.text.corpus, function(x)removeWords(x,stopwords()))

snapdeal_tweets.text.corpus <- tm_map(snapdeal_tweets.text.corpus, function(x)removeWords(x,stopwords()))

flipkart_tweets.text.corpus <- tm_map(flipkart_tweets.text.corpus, function(x)removeWords(x,stopwords()))

library("wordcloud")

#generate wordcloud

wordcloud(amazon_tweets.text.corpus,min.freq = 40,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 50)

wordcloud(flipkart_tweets.text.corpus,min.freq = 200,colors=brewer.pal(10, "Dark2"),random.color = TRUE,max.words = 60)

wordcloud(snapdeal_tweets.text.corpus,min.freq = 400,colors=brewer.pal(10, "Dark2"),random.color = TRUE,max.words = 500)

