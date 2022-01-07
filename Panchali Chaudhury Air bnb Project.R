########################################################
######### Project: Data Analysis on Airbnb #############
######### Team: 12
######### Subject: Text Mining & Data Analysis #########
########################################################

#installing and loading the mongolite library to download the Airbnb data
#install.packages("mongolite")
#install.packages("jsonlite")
#install.packages("textcat")
#need to run this line of code only once and then you can comment out

library(mongolite)
library(jsonlite)
library(textcat)
library(tidytext)
library(tm)
library(plyr)
library(dplyr)
library(magrittr)
library(janeaustenr)
library(gutenbergr)
library(base64enc)
library(rvest)
library(tidytext)
library(tidyverse)
library(tidyr)
library(tidytuesdayR)
library(textshape)
library(twitteR)
library(tm)
library(scales)
library(magrittr)
library(Matrix)
library(ggplot2)
library(textdata)
library(igraph)
library(ggraph)
library(topicmodels)
library(gutenbergr) #error
library(quanteda) #error
library(widyr) #error
library(quanteda.textmodels) #error
library(RColorBrewer)
library(textreadr)
library(dplyr)
library(stringr)


# This is the connection_string. You can get the exact url from your MongoDB cluster screen
#replace the <<user>> with your Mongo user name and <<password>> with the mongo password
#lastly, replace the <<server_name>> with your MongoDB server name
connection_string <- 'mongodb+srv://Naila:NeenaNaila@cluster0.pgudw.mongodb.net/myFirstDatabase?retryWrites=true&w=majority'
airbnb_collection <- mongo(collection="listingsAndReviews", db="sample_airbnb", url=connection_string)

#Here's how you can download all the Airbnb data from Mongo
## keep in mind that this is huge and you need a ton of RAM memory

airbnb_all <- airbnb_collection$find()

write_json(airbnb_all, "E:/HULT-SF/Data-Mining-Project/Airbnb.json", pretty= TRUE )

#######################################################
#if you know or want to learn MQL (MongoQueryLanguage), that is a JSON syntax, feel free to use the following:::
######################################################
#1 subsetting your data based on a condition:
mydf <- airbnb_collection$find('{"bedrooms":2, "price":{"$gt":50}}')

#2 writing an analytical query on the data::
mydf_analytical <- airbnb_collection$aggregate('[{"$group":{"_id":"$room_type", "avg_price": {"$avg":"price"}}}]')


##### Cleaning the Data for English #######
na.omit(mydf$summary)
mydf[mydf$summary == " "] <- "NA"

language <- c()
language <- textcat(mydf$summary)

english_df <- mydf %>% 
  filter(language == "english")

View(english_df)

typeof(english_df$address)
my_adress <- as.data.frame(english_df$address)
View(my_adress)


################# trying different logics #################
View(reviews)
reviews <- do.call(rbind.data.frame, english_df$reviews)
colnames(reviews)[6]<- "text"

language2 <- c()
language2 <- textcat(reviews$text)
english_reviews <- reviews %>% 
  filter(language2 == "english")

View(english_reviews)

################# Group work ################################

View(my_adress)
reviews <- do.call(rbind.data.frame, english_df$reviews)
write.csv(english_reviews, "/Users/Neena/Desktop/DATA-MINING-PROJECT/reviews.csv")
write.csv(my_address, "/Users/Neena/Desktop/DATA-MINING-PROJECT/my_address.csv")
description <-english_df$description
View(english_reviews)

address <- do.call(rbind.data.frame, english_df$address)
View(address)

write.csv(english_reviews, "/Users/Neena/Desktop/DATA-MINING-PROJECT/reviews_all_3.csv")

##########################################################
########## Filtering  Countries , To select only 3 of them ######

english_df_US <- english_df %>% 
  filter(my_adress$country == "United States")

#View(english_df_US )

english_df_Aus <- english_df %>% 
  filter(my_adress$country == "Australia")

english_df_Spain <- english_df %>% 
  filter(my_adress$country == "Spain")

#####################################################################
########## Filtering the reviews  #, for the selected countries #####
reviews_US <- do.call(rbind.data.frame, english_df_US$reviews)

reviews_US$country <- "US"
#View(reviews_US)
reviews_Aus <- do.call(rbind.data.frame, english_df_Aus$reviews)
reviews_Aus$country <- "AUS"
#View(reviews_Aus)
re

View(english_reviews)views_Spain <- do.call(rbind.data.frame, english_df_Spain$reviews)
reviews_Spain$country <- "Spain"
#View(reviews_Spain)

review_all1 <- rbind(reviews_US,reviews_Aus,reviews_Spain)
View(review_all)

######## Cleaning the combined file for English language
language2 <- c()
language2 <- textcat(review_all1$comments)
reviews_all <- review_all %>% 
  filter(language2 == "english")


View (reviews_all)
library(tidytext)
library(dplyr)

colnames(reviews_all)[6] <- "text"
#############################################
####We want to combine all the datasets and do frequencies 
#############################################

###########
#USA 
##########

US <- reviews_all %>%
  filter(country %in% "US")

US

tidy_US <- US %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_US)

#counting frequencies for tokens
tidy_US %>%
  count(word, sort=TRUE)

###########
#Spain 
##########

Spain <- reviews_all %>%
  filter(country %in% "Spain")

Spain

tidy_Spain <- Spain %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_Spain)

#counting frequencies for tokens
tidy_Spain %>%
  count(word, sort=TRUE)


###########
#Australia
##########

AUS <- reviews_all %>%
  filter(country %in% "AUS")

AUS


tidy_AUS <- AUS %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_AUS)
#counting frequencies for tokens
tidy_AUS %>%
  count(word, sort=TRUE)

frequency <- bind_rows(mutate(tidy_US, country="US"),
                       mutate(tidy_Spain, country= "Spain"),
                       mutate(tidy_AUS, country="AUS"))%>%#closing bind_rows
  
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(country, word) %>%
  group_by(country) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(country, proportion) %>%
  gather(country, proportion, `Spain`, `AUS`)

#let's plot the correlograms:
library(scales)
ggplot(frequency, aes(x=proportion, y=`US`, 
                      color = abs(`US`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~country, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "US", x=NULL)

##########################################
##doing the cor.test() ################
##########################################

cor.test(data=frequency[frequency$country == "Spain",],
         ~proportion + `US`)

cor.test(data=frequency[frequency$country == "AUS",],
         ~proportion + `US`)



###################################################
################# Bi-Gram  ########################
###################################################



library(dplyr)
library(tidytext)
library(tidyr)

review_bigrams <- reviews_all %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

review_bigrams #We want to see the bigrams (words that appear together, "pairs")

review_bi_fqr <- review_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
bigrams_separated_review <- review_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered_review <- bigrams_separated_review %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigrams_filtered_review

#creating the new bigram, "no-stop-words":
bigram_counts_review <- bigrams_filtered_review %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts_review

# creating a ggraph to see the connections between the different words
library(ggplot2)
visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
} 

# filter out rare combinations, as well as digits
bigram_counts_review %>%
  filter(n > 100,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>%
  visualize_bigrams()
###############################
# United the word 1 and word 2
###############################
bigrams_united <- bigrams_filtered_review %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united
###############################
# 3 gram 
###############################
trigram_counts_review <- reviews_all %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

trigram_counts_review
###############################
# creating a ggraph to see the connections between the different words
###############################
visualize_trigrams <- function(trigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  trigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 3, hjust = 3) +
    theme_void()
} 
###############################
# filter out rare combinations, as well as digits
###############################
trigram_counts_review %>%
  filter(n > 5,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d"),
         !str_detect(word3, "\\d")) %>%
  visualize_trigrams()

###############################
# location word 
###############################
bigrams_filtered_review %>%
  filter(word2 == "location") %>%
  count(reviewer_name, word1, sort = TRUE)
###############################
# tf-idf by name 
###############################
bigram_tf_idf <- bigrams_united %>%
  count(reviewer_name, bigram) %>%
  bind_tf_idf(bigram, reviewer_name, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf
###############################
######Using Bigrams to Provide Context in Sentiment Analysis
###############################
bigrams_separated_review  %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

AFINN <- get_sentiments("afinn")

AFINN
###############################
# not word 
###############################
not_words <- bigrams_separated_review  %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE) %>%
  ungroup()

not_words
###############################
# plot word associate with not 
###############################
not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()

############################################################################
###############################
# negation words 
###############################
negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated_review %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE) %>%
  ungroup()

negated_words
###############################
#### plot Negated words
###############################
negated_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"negation_words\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()


#################################################################################


###############################
# we need to filter for at least relatively common words first
###############################
bigrams_filtered_review
bigrams_united

word_cors <- bigrams_united %>%
  group_by(bigram) %>%
  filter(n() >= 70) %>%
  pairwise_cor(bigram, country, sort = TRUE)

word_cors
###############################
#visualize the correlations and clusters of words by the widyr package
###############################
set.seed(2016)

word_cors %>%
  filter(correlation > .2) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

###################################################
################# TF_IDF             ##############
###################################################

#we're grouping by the country 

# Tokenization 

review_token <- reviews_all %>%
  unnest_tokens(word, text) %>%
  count(country, word, sort=TRUE) %>%
  ungroup()

total_words <- review_token %>%
  group_by(country) %>%
  summarise(total=sum(n))

review_words <- left_join(review_token, total_words)%>%
  filter(country %in% c("US", "Spain", "AUS"))

print(review_words)

country_words <- review_words %>%
  bind_tf_idf(word, country, n)
# we get all the zeors because we are looking at stop words 
country_words 

country_words %>%
  arrange(desc(tf_idf))

# looking at the graphical apprach:

country_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(country) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=country))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~country, ncol=2, scales="free")+
  coord_flip()



################################################################################
############                      Format                  #####################             
##################   Using Sentiment analysises Frameworks  ###################
################################################################################
library(tm)


my_review_corp <- VCorpus(VectorSource(reviews_all$text))

library(tidytext)
library(janeaustenr)
library(dplyr)
library(stringr)
library(tidyr)
library(tidytuesdayR)

############################################################
##### Comparing different sentiment libraries 
############################################################



##################### Sentiment Analysis for   #####################


# regular sentiment score using get_sentiment() 
# each method has a different scale
install.packages("bing")
library(tidytext)
library(textdata)
library(syuzhet)
library(bing)


###############################
# bing method
bing_vector_review <- get_sentiment(my_review_corp , method="bing")
head(bing_vector_review)
summary(bing_vector_review)
###############################
#affin method
afinn_vector_review <- get_sentiment(my_review_corp, method="afinn")
head(afinn_vector_review)
summary(afinn_vector_review)
###############################
#nrc method

d_review <- get_nrc_sentiment(as.vector(as.character(my_review_corp))) 
###############################
# To see top 20 lines of the get_nrc_sentiment dataframe
head (d_review,20)

#transpose
td_review <- data.frame(t(d_review))
# Dimension of the DF
dim(td_review)
# Computing the sums across rows for each variable.
td_new_review <- data.frame(rowSums(td_review[1:10,]))
#Cleaning
names(td_new_review)[1] <- "count"
td_new_review <- cbind("sentiment" = rownames(td_new_review), td_new_review)
rownames(td_new_review) <- NULL
td_new2_review <- td_new_review[1:20,]
#Plot 
quickplot(sentiment, data=td_new2_review, weight=count, geom="bar",
          fill=sentiment, ylab="count")+ggtitle("Survey sentiments")


################################################
###### Latent Dirichlet algorithm ##############
################################################

# There are two principles:
#1. Every document is a combination of multiple topics
#2. Every topic is a combination of multiple words

library(tm)

by_country <- reviews_all %>%
  group_by(country) 

#split into words
by_country_word <- by_country %>%
  unnest_tokens(word, text)

by_country_word

#find document-word counts
word_counts <- by_country_word %>%
  anti_join(stop_words) %>%
  count(country, word, sort = TRUE) %>%
  ungroup()

print(word_counts)
country_dtm <- word_counts %>%
  cast_dtm(country, word, n)


country_lda <- LDA(country_dtm,k=2, control = list(seed=123))
country_lda
# cast the country word frequency into a document-term matrix. 

# tease out the data of the topics
country_topics <- tidy(country_lda, matrix = "beta")
country_topics

# tease out the data of 20 top terms
top_terms <- country_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

# Plot the 20 top terms per topic
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

############################################################################
####### Gamma, to see the probabliity of the words from each Country  ######
country_gamma <- tidy(country_lda, matrix="gamma")
country_gamma




