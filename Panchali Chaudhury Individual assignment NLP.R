### Exporting data from CSV file 

US_tweets <- tweets1
View(US_tweets)

### Loading libraries 

install.packages("RWeka")
install.packages("dplyr")
install.packages("igraph")
install.packages("gridExtra")
install.packages("memery") # install memery package
install.packages("ggimage") # install ggimage package
install.packages("magick")  # install magick package
install.packages("ggraph")
install.packages("reshape2")
install.packages("textdata")
library(ggplot2)
library(textdata)
library(tidytext)
library(dplyr)
library(magrittr)
library(reshape2)
library(tidyverse) # data manipulation
library(tm) # text mining
library(wordcloud) # word cloud generator
library(wordcloud2) # word cloud generator
library(tidytext) # text mining for word processing and sentiment analysis
library(reshape2) # reshapes a data frame
library(RWeka) # data mining tasks
library(knitr) # dynamic report generation
library(gridExtra) # miscellaneous Functions for "Grid" Graphics
library(grid) # add Grid to a Plot
library(magick) # advanced Image-Processing
library(ggimage) # supports image files and graphic objects to be visualized in 'ggplot2'
library(igraph) # creating and manipulating graphs and analyzing networks
library(ggraph) # graphs and Networks
library(topicmodels)
library(DT)
library(png)
library(broom)
library(topicmodels)
library(scales)
library(lubridate)
library(ggrepel)

### 1.1 Looking at the structure of the data

str(US_tweets)

### 1.2 summary of the data set 

summary(US_tweets)

### 1.3 Viewing head of the data set

head(US_tweets)

### 1.4 Viewing the tail of the data set 

tail(Us_tweets)

### 2. Since on twitter there are options of using different languages, we need to check how many tweets have been tweeted in English and other languages.
### Using kable function to identify tweets and group them by different language and assiging frquency to it
kable(
  US_tweets %>% 
    group_by(lang) %>% 
    count() %>% 
    rename(Language = lang, 'Number of Tweets' = n))

### Filtering out all other languages except english

US_tweets <- US_tweets %>% 
  filter(lang != "es") %>% 
  filter(lang != "da") %>% 
  filter(lang!= "et") %>% 
  filter(lang!= "fi") %>% 
  filter(lang!= "fr") %>% 
  filter(lang!= "tl") %>% 
  filter(lang!= "und")

### Changing name of Downald Trump and clinton for better visualization 

US_tweets$handle <- sub("realDonaldTrump", "Trump", US_tweets$handle)
US_tweets$handle <- sub("HillaryClinton", "Clinton", US_tweets$handle)

### Changing the is_retweet to logical function, so that we can form a table looking at the tweets made by clinton and trump

US_tweets$is_retweet <- as.logical(US_tweets$is_retweet)

#### Grouping by each handler in a frequency table of the number of retweets
kable(
  US_tweets %>% 
    filter(is_retweet==FALSE) %>% 
    group_by(handle) %>% 
    count())
### Whose tweets were retweeted by donlad trump and Hillary Clinton
retweets <- US_tweets %>% 
  filter(original_author != "") %>% 
  group_by(original_author) %>% 
  count() %>% 
  filter(n>=5) %>% 
  arrange(desc(n)) %>% 
  ungroup()

ggplot(retweets, aes(x=reorder(original_author, n), y=n)) +
  geom_bar(stat="identity", fill="darkgreen") + coord_flip() +
  labs(x="", y="number of tweets retweeted by either Trump or Clinton") +
  theme(legend.position = "none")


### Cleaning the text###
#### Removing the following from the text column 
### url link
### Back slashes 
### &amp
### Emojis 

US_tweets$text <- str_replace_all(US_tweets$text, "[\n]" , "")
US_tweets$text <- str_replace_all(US_tweets$text, "&amp", "") 
US_tweets$text <- str_replace_all(US_tweets$text, "http.*" , "")
US_tweets$text <- iconv(US_tweets$text, "latin1", "ASCII", sub="")

### Renaming doc_id to id

US_tweets <- US_tweets %>% 
  rename (doc_id = id)

### Filtering out clintons tweets 

ClintonTweets <- US_tweets %>% 
  filter(is_retweet=="FALSE" & handle=="Clinton")

#### Tokenizing clinton tweets
clinton_token<-ClintonTweets %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>%
  count(word, sort =TRUE)

### Filtering out trumps tweets 

TrumpTweets <- US_tweets %>% 
  filter(is_retweet=="FALSE" & handle=="Trump")

### Tokenizing trump tweets 

trump_token <- TrumpTweets %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>%
  count(word, sort =TRUE) 


### Forming Vcorpus for clinton tweets and trump tweets 
TrumpCorpus <- DataframeSource(TrumpTweets)
TrumpCorpus <- VCorpus(TrumpCorpus)

ClintonCorpus <- DataframeSource(ClintonTweets)
ClintonCorpus <- VCorpus(ClintonCorpus)

### Cleaning the data further and then forming individual word counts each for trump and clinton
### Removing numbers, ounctuations, white space and also removing names 
CleanCorpus <- function(x){
  x <- tm_map(x, content_transformer(tolower))
  x <- tm_map(x, removeNumbers) #remove numbers before removing words. Otherwise "trump2016" leaves "trump"
  x <- tm_map(x, removeWords, tidytext::stop_words$word)
  x <- tm_map(x, removePunctuation)
  x <- tm_map(x, stripWhitespace)
  return(x)
}

RemoveNames <- function(x) {
  x <- tm_map(x, removeWords, c("donald", "hillary", "clinton", "trump", "realdonaldtrump", "hillaryclinton"))
  return(x)
}

### Creating term matrix

CreateTermsMatrix <- function(x) {
  x <- TermDocumentMatrix(x)
  x <- as.matrix(x)
  y <- rowSums(x)
  y <- sort(y, decreasing=TRUE)
  return(y)
}



### Using the above functions to clean the trump corpus first 
TrumpCorpus <- CleanCorpus(TrumpCorpus) 
### Creating the terms matrix for trump corpus
TermFreqTrump <- CreateTermsMatrix(TrumpCorpus)
### first forming a dataframe
TrumpDF <- data.frame(word=names(TermFreqTrump), count=TermFreqTrump)
TrumpDF[1:20,] %>%
  ggplot(aes(x=(reorder(word, count)), y=count)) +
  geom_bar(stat='identity', fill="Red") + coord_flip() + theme(legend.position = "none") +
  labs(x="")

### Removing the names again and then forming word clouds 

set.seed(2018)

TrumpCorpus1 <- RemoveNames(TrumpCorpus)
TermFreqTrump <- CreateTermsMatrix(TrumpCorpus1)
TrumpDF <- data.frame(word=names(TermFreqTrump), count=TermFreqTrump)

### Forming the wordcloud
wordcloud(TrumpDF$word, TrumpDF$count, max.words = 100, scale=c(2.5,.5), random.color = TRUE, colors=brewer.pal(9,"Set1"))
wordcloud2::wordcloud2(TrumpDF[1:100,], color = "random-light", backgroundColor = "grey", shuffle=FALSE, size=0.4)
### Using the above functions to clean clintons corpus 

ClintonCorpus <- CleanCorpus(ClintonCorpus)
TermFreqClinton <- CreateTermsMatrix(ClintonCorpus)

ClintonDF <- data.frame(word=names(TermFreqClinton), count=TermFreqClinton)

ClintonDF[1:20,] %>%
  ggplot(aes(x=(reorder(word, count)), y=count)) +
  geom_bar(stat='identity', fill="#FF1493") + coord_flip() + theme(legend.position = "none") +
  labs(x="")
### Removing the names again and then forming word clouds 
ClintonCorpus1 <- RemoveNames(ClintonCorpus)
TermFreqClinton <- CreateTermsMatrix(ClintonCorpus1)
ClintonDF <- data.frame(word=names(TermFreqClinton), count=TermFreqClinton)

wordcloud(ClintonDF$word, ClintonDF$count, max.words = 100, scale=c(2.5,.5), random.color = TRUE, colors=brewer.pal(9,"Set1"))
wordcloud2::wordcloud2(ClintonDF[1:100,], color = "random-light", backgroundColor = "grey", shuffle=FALSE, size=0.4)

######################################################################################################################

################################################################################
#### Sentiment analysis using all three lexicons #########
################################################################################
### Sentiment analysis of clinton using bing lexicon 

clinton_bing<-ClintonTweets %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>%
  count(word, sort =TRUE) %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE)%>% 
  group_by(sentiment) %>% 
  count() %>% 
  mutate(proportion = n / sum(n))%>%
  ggplot(aes(sentiment, n))+
  geom_col(show.legend=FALSE)+
  geom_bar(stat='identity', fill="light pink")+
  labs(x = "", y ="Relative proportion", title="Positive vs Negative sentiment (Clinton)")+
  coord_flip() +
  theme(legend.position="none")

Clinton_top_bing <- ClintonTweets %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>%
  count(word, sort =TRUE) %>% 
  inner_join(get_sentiments("bing")) %>%
  group_by(sentiment) %>% 
  arrange(desc(n))%>% 
  slice(1:20) %>%
  ggplot(aes(x=reorder(word,n), y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE) +
  coord_flip() +
  facet_wrap(~sentiment, scales="free_y") +
  labs(x="", y="number of times used", title="Hillary Clintons most used words") +
  scale_fill_manual(values = c("positive"="green", "negative"="red"))


#### Sentiment analysis of Trump using bing lexicon

Trump_bing<-TrumpTweets %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>%
  count(word, sort =TRUE) %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>% 
  group_by(sentiment) %>% 
  count() %>% 
  mutate(proportion = n/sum(n)*100)%>%
  ggplot(aes(sentiment, n))+
  geom_col(show.legend=FALSE)+
  geom_bar(stat='identity', fill="light blue")+
  labs(x = "", y ="Relative proportion", title="Positive vs Negative sentiment (Trump)")+
  coord_flip() +
  theme(legend.position="none")


Trump_top_bing<-TrumpTweets %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>%
  count(word, sort =TRUE) %>% 
  inner_join(get_sentiments("bing")) %>%
  group_by(sentiment) %>% 
  arrange(desc(n)) %>% 
  slice(1:20) %>%
  ggplot(aes(x=reorder(word,n), y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE) +
  coord_flip() +
  facet_wrap(~sentiment, scales="free_y") +
  labs(x="", y="number of times used", title="Donald Trump's most used words") +
  scale_fill_manual(values = c("positive"="green", "negative"="red"))

###################################################################################
### Sentiment analysis using nrc lexicon
###################################################################################

### Sentiment analysis of trump nrc

Trump_nrc<-TrumpTweets %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>%
  count(word, sort =TRUE) %>% 
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>% 
  group_by(sentiment) %>% 
  count() %>%
  #mutate(proportion = n/sum(n)) %>%
  ggplot(aes(x=sentiment, y=n, fill=sentiment))+
  geom_col(show.legend=FALSE)+
  geom_bar(stat='identity')+
  labs(x = "", y ="n", title="Trump NRC lexicon")+
  coord_flip() +
  theme(legend.position="none")


### Sentiment analysis of Clinton NRC 

Clinton_nrc<-ClintonTweets %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>%
  count(word, sort =TRUE) %>% 
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>% 
  group_by(sentiment) %>% 
  count() %>%
  mutate(proportion = n/sum(n)) %>%
  ggplot(aes(x=sentiment, y=n, fill=sentiment))+
  geom_col(show.legend=FALSE)+
  geom_bar(stat='identity')+
  labs(x = "", y ="n", title="Clinton NRC lexicon")+
  coord_flip() +
  theme(legend.position="none")

top_nrc_clinton <- TrumpTweets %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>%
  count(word, sort =TRUE) %>%  
  inner_join(nrc, "word") %>%
  count(sentiment, word, sort=TRUE) %>%
  group_by(sentiment) %>%
  #arrange(desc(n)) %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(word, n), y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  facet_wrap(~sentiment, scales="free_y") +
  labs(y="Frequency", x="Words", 
       title="Most frequent terms for each sentiment (Clinton - NRC lexicon)") +
  coord_flip() +
  theme_bw()

################################################################################
### Sentiment analysis using afinn lexicon #####################################
### Trump afinn lexicon

Trump_afinn<-TrumpTweets %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>%
  count(word, sort =TRUE) %>% 
  inner_join(get_sentiments("afinn")) %>%
  count(value, sort=TRUE) %>%
  ggplot(aes(x=value, y=n)) +
  geom_bar(stat="identity", aes(fill=n), show.legend=FALSE, width=0.5) +
  geom_label(aes(label=n)) +
  scale_fill_gradient(low="thistle1", high="thistle4") +
  scale_x_continuous(breaks=seq(-4 ,  4, 1)) +
  labs(x="Score", y="Frequency", title="Trump (AFINN lexicon)") +
  theme_bw()

### Clinton afinn lexicon

Clinton_afinn<-ClintonTweets %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>%
  count(word, sort =TRUE) %>% 
  inner_join(get_sentiments("afinn")) %>%
  count(value, sort=TRUE) %>%
  ggplot(aes(x=value, y=n)) +
  geom_bar(stat="identity", aes(fill=n), show.legend=FALSE, width=0.5) +
  geom_label(aes(label=n)) +
  scale_fill_gradient(low="thistle1", high="thistle4") +
  scale_x_continuous(breaks=seq(-4 ,  4, 1)) +
  labs(x="Score", y="Frequency", title=" Clinton (AFINN lexicon)") +
  theme_bw()

########################################################################################################

###Time series of posititve negative tweets 

#########################################################################################################
trumptweetsdate <- TrumpTweets %>% 
  group_by(time) %>%
  mutate(time = as.Date(time, "%m/%d/%Y")) %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>%
  count(word, sort =TRUE) %>% 
  inner_join(get_sentiments("bing"))%>% 
  count(word, sentiment, sort=TRUE) %>% 
  group_by(sentiment) %>%
  spread(sentiment, n) %>% 
  mutate(positive = replace_na(positive, 0)) %>% 
  mutate(negative = replace_na(negative, 0)) %>% 
  mutate(score=positive-negative) %>%
  
  ggplot(aes(x=time, y=score)) +
  scale_x_date(limits=c(as.Date("2016-01-05"), as.Date("2016-09-27")), date_breaks = "1 month", date_labels = "%b") +
  geom_line(stat="identity", col="orange") + geom_smooth(col="black") + labs(title="Sentiment Donald Trump")

Clintontweetsdate <- ClintonTweets %>% 
  group_by(time) %>%
  mutate(time = as.Date(time, "%m/%d/%Y")) %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>%
  count(word, sort =TRUE) %>% 
  inner_join(get_sentiments("bing"))%>% 
  count(word, sentiment, sort=TRUE) %>% 
  group_by(sentiment) %>%
  spread(sentiment, n) %>% 
  mutate(positive = replace_na(positive, 0)) %>% 
  mutate(negative = replace_na(negative, 0)) %>% 
  mutate(score=positive-negative) %>%
  
  ggplot(aes(x=time, y=score)) +
  scale_x_date(limits=c(as.Date("2016-01-05"), as.Date("2016-09-27")), date_breaks = "1 month", date_labels = "%b") +
  geom_line(stat="identity", col="orange") + geom_smooth(col="black") + labs(title="Sentiment Hillary Clinton")


################################################################################################################
###################### TF IDF Analysis ######################################################################
##############################################################################################################

### First tokeninzing the us_tweets data set 

tweets_token <- US_tweets %>%
  group_by(handle) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>%
  count(word, sort =TRUE) %>% 
  
  total_words <- tweets_token %>% 
  group_by(handle) %>% 
  summarize(total=sum(n))

tweets_words <- left_join(tweets_token, total_words)%>%
  filter(handle %in% c("Clinton", "Trump"))

### Plotting 

ggplot(tweets_words, aes(n/total, fill = handle))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.001) +
  facet_wrap(~handle, ncol=2, scales="free_y")

######################################
########## ZIPF's law ################
######################################

freq_by_rank_Trump <- tweets_words %>%
  group_by(handle) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total) %>% 
  filter(handle == "Trump")
freq_by_rank_Trump

#let's plot ZIPF's Law
freq_by_rank_Trump %>%
  ggplot(aes(rank, `term frequency`, color=handle))+
  #let's add a tangent line , the first derivative, and see what the slop is
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()

freq_by_rank_Clinton <- tweets_words %>%
  group_by(handle) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total) %>% 
  filter(handle == "Clinton")
freq_by_rank_Clinton

#let's plot ZIPF's Law
freq_by_rank_Clinton %>%
  ggplot(aes(rank, `term frequency`, color=handle))+
  #let's add a tangent line , the first derivative, and see what the slop is
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()


###################################################
################# TF_IDF ##########################
###################################################

handle_words <- tweets_words %>%
  bind_tf_idf(word, handle, n)

handle_words # we get all the zeors because we are looking at stop words ... too common

handle_words %>%
  arrange(desc(tf_idf))
#what can we say about these words?

#############
# looking at the graphical apprach:
handle_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(handle) %>%
  top_n(50) %>%
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill=handle))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~handle, ncol=2, scales="free")+
  coord_flip()
#########################################################################################################
#### NGRAMS to analyze the data ####
### Trump trigrams 
trump_trigrams <- US_tweets %>% 
  filter(handle == "Trump") %>% 
  unnest_tokens(bigram, text, token = "ngrams", n= 3) %>% 
  separate(bigram, c("word1", "word2", "word3"), sep = " ") %>% 
  filter(!word1 %in%  stop_words$word) %>%   
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

### trigram word visualization (Trump)

Trump_trigram_graph <- trump_trigrams %>%
  filter(n>10) %>%
  graph_from_data_frame()

ggraph(Trump_trigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha=n), show.legend = FALSE,
                 end_cap=circle(0.07, 'inches'))+
  geom_node_point(color="lightblue", size=3)+
  geom_node_text(aes(label=name), vjust =1, hjust=1)+
  theme_void()
###############################################################################

#### Clinton trigrams

Clinton_trigrams <- US_tweets %>% 
  filter(handle == "Clinton") %>% 
  unnest_tokens(trigram, text, token = "ngrams", n= 3) %>% 
  separate(bigram, c("word1", "word2", "word3"), sep = " ") %>% 
  filter(!word1 %in%  stop_words$word) %>%   
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) %>%
  count(word1, word2, word3,  sort = TRUE)

### Bigram word visualization (clinton)

Clinton_trigram_graph <- Clinton_trigrams %>%
  filter(n>10) %>%
  graph_from_data_frame()

ggraph(Clinton_trigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha=n), show.legend = FALSE,
                 end_cap=circle(0.07, 'inches'))+
  geom_node_point(color="lightblue", size=3)+
  geom_node_text(aes(label=name), vjust =1, hjust=1)+
  theme_void()
###############################################################################
### bidrogram for clinton 

Clinton_birogram <- US_tweets %>% 
  filter(handle == "Clinton") %>% 
  unnest_tokens(biogram, text, token = "ngrams", n= 2) %>% 
  separate(quadrogram, c("word1", "word2",), sep = " ") %>% 
  filter(!word1 %in%  stop_words$word) %>%   
  filter(!word2 %in% stop_words$word) %>% 

  
  count(word1, word2,  sort = TRUE) 


### bigram Visualization (clinton)

Clinton_bigram_graph <- Clinton_bigram %>%
  filter(n>1) %>%
  graph_from_data_frame()

ggraph(Clinton_bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha=n), show.legend = FALSE,
                 end_cap=circle(0.07, 'inches'))+
  geom_node_point(color="lightblue", size=3)+
  geom_node_text(aes(label=name), vjust =1, hjust=1)+
  theme_void()

### bigram for Trump
Trump_quadrogram <- US_tweets %>% 
  filter(handle == "Trump") %>% 
  unnest_tokens(bigram, text, token = "ngrams", n= 2) %>% 
  separate(quadrogram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in%  stop_words$word) %>%   
  filter(!word2 %in% stop_words$word) %>% 

  count(word1, word2,  sort = TRUE) %>% 
 


##### bigram Visualization (clinton)

Trump_bigram_graph <- Trump_bigram %>%
  filter(n>1) %>%
  graph_from_data_frame()

ggraph(Trump_bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha=n), show.legend = FALSE,
                 end_cap=circle(0.07, 'inches'))+
  geom_node_point(color="lightblue", size=3)+
  geom_node_text(aes(label=name), vjust =1, hjust=1)+
  theme_void()

###############################################################################################
### Applying TF IDF framework to Ngrams 
###############################################################################################

##### Trump Ngram TF iDF 

trump_trigram_tfidf <- US_tweets %>% 
  filter(handle == "Trump") %>% 
  unnest_tokens(trigram, text, token = "ngrams", n= 3) %>% 
  separate(bigram, c("word1", "word2", "word3"), sep = " ") %>% 
  filter(!word1 %in%  stop_words$word) %>%   
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) %>%
  unite(trigram, word1, word2, word3, sep=" ") %>% 
  count(handle, trigram) %>%
  bind_tf_idf(trigram, handle, n) %>%
  arrange(desc(tf_idf)) %>% 
  top_n(10) %>% 
  ggplot(aes(trigram,tf_idf,fill=handle))+geom_col(show.legend=FALSE)+labs(x="",y="tf-idf",title="Top 20 Trigrams")+facet_wrap(~handle,ncol=2,scales="free")+coord_flip()


##### clinton Ngram TF iDF 

clinton_trigram_tfidf <- US_tweets %>% 
  filter(handle == "Clinton") %>% 
  unnest_tokens(trigram, text, token = "ngrams", n= 3) %>% 
  separate(bigram, c("word1", "word2", "word3"), sep = " ") %>% 
  filter(!word1 %in%  stop_words$word) %>%   
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) %>%
  unite(bigram, word1, word2, word3 sep=" ") %>% 
  count(handle, trigram) %>%
  bind_tf_idf(trigram, handle, n) %>%
  arrange(desc(tf_idf)) %>% 
  top_n(15) %>% 
  ggplot(aes(trigram,tf_idf,fill=handle))+geom_col(show.legend=FALSE)+labs(x="",y="tf-idf",title="Top 20 Trigrams")+facet_wrap(~handle,ncol=2,scales="free")+coord_flip()

