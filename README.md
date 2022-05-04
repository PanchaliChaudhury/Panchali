Text mining on US Presidential Elections Tweets
1
Text mining on US Presidential Elections Tweets
Hult International Business School
Panchali Chaudhury
Text mining on US Presidential Elections Tweets 2
Introduction
This report demonstrates the application of R using NLP to analyze the tweets posted by Hilary
Clinton and Donald Trump, during the US presidential election, 2016. The total tweets gathered
is 6,445 from September 2016 to January 2017.
We found that Trump tweeted more than Clinton and used the social space to negatively talk
about other candidates. Rather Clinton used this space to talk more about political policies.
The frameworks used are as follows:
1. Cleaning the text
2. Structuring data using tokenization and creating data frames
3. Most used words by each candidate
4. Ngrams
5. Sentiment analysis
6. TF IDF framework
Text mining on US Presidential Elections Tweets 3
Data Preparation
The data was extracted from Ben Hammers github page, which had 6,445 tweets both by Clinton
and Trump. The data was cleaned after importing.
1. Tweets were in different languages, tweets made only in English was filtered out.
2. There are two variables, first handle and the second is original_author. Clinton or Trump
are involved in these tweets with variable name handle. The second variable,
original_author, if an observation is blank, then the original_author is the main handler. If
the original_author observations are not blank, these tweets are the original tweets by the
two handlers.
3. The is_retweet data type was changed to logical. This was done in order to analyze the
actual tweets posted by the two candidates. The total tweets by the two were 5,722 after
filtering
4. Each tweets were messy with html links, emojis, and back slashes. StringR package was
used to replace these characters from the text variable.
Text mining on US Presidential Elections Tweets 4
Data Analysis
1. Tweets in languages other than “English”
Apart from English, there are other languages like Spanish and few other. The table below
reflects the number of languages.
105 tweets made in Spanish, 9 are from other languages and 82 tweets are undefined language.
These tweets were too short for twitter to detect. For further analysis we will be using only the
tweets made in English.
2. Which people were re-tweeted?
Text mining on US Presidential Elections Tweets 5
From the above plot (people who were retweeted at least 5 times), the maximum retweets
were for “TheBriefing2016”, official Clinton campaign account. Both candidates also retweeted
on tweets by Joe Bidden (Democratic), Eric Trump (son of Donald Trump), HFA (NGO
supporting Clinton).
The number of retweets, gives us an idea of the followers and the emotions involved in
the retweets. Further in this study we will analyze the sentiments in the tweets of the two main
presidential candidates.
Text mining on US Presidential Elections Tweets 6
3. Tokenization
Analyzing the most frequent words used by them
3.1 Top 20 words used by Trump
Text mining on US Presidential Elections Tweets 7
Trump tweeted about America and its people. Also, he claims to make America better
again, which came up over 100 times. It is interesting to note that, he uses the word
crooked which indicates undermining his opponent’s character.
3.2 Top 20 words used by Clinton
Text mining on US Presidential Elections Tweets 8
Clinton uses the word “president” the most followed by “America”, as she is addressing
to people of America. The word POTUS (President of the United States) is used over 100
times. It was used to tag it because it’s a separate twitter handler. Unlike her opponent,
she emphasized about policies, such as, “women”, “economy”, “tax”etc.
4. Sentiment Analysis
In order to understand the emotions of the tweet, I have used three tests, first ,bing
lexicon, which categorizes the words in binary fashion. Second, NRC, which categorizes
the words with different categories of emotions, such as joy, surprise, anger etc. Lastly,
the afinn which rates an emotion on the scale of -5 to 5. Also, I have used time series to
understand the time series of emotions used in the tweets.
4.1 Sentiment Analysis using Bing Lexicon
4.1.1 Hillary Clinton
The negative word proportion is about 60%. However, few of the negative words,
indicates social issues or political policies (poverty, racism, crisis, debt) Such words are
classified as negative words, but are few major concerns for any country. Clinton was
trying to discuss about how to deal with such problems
From the positive words, Love, Support, win, stronger are few of them. From these
words, she is appealing for support and votes. Overall if we look at the sentiment behind
her words, she did not use this platform to defame her opponents.
Text mining on US Presidential Elections Tweets 9
Text mining on US Presidential Elections Tweets 10
4.1.2 Donald Trump
Trump used almost 63% negative words. The word “crooked” was used over 150 times to
defame Clinton. Unlike in the case of Clinton, where negative words were mostly used to
address towards issues. Trump used them mostly to criticize someone or the existing system. The
positive words used by him were mostly for his campaigns or supporters.
Text mining on US Presidential Elections Tweets 11
4.2 Sentiment analysis - NRC Lexicon
The NRC lexicon goes beyond positive and negative, and categorizes words under
categories, such as, joy, anger, surprise, disgust, trust, sadness etc.
Both Trump and Clinton used variety of word. They used words to gain trust amongst
the US citizens. Both used words with surprise and sadness.
4.3 Sentiment analysis using the Afinn Lexicon
The Afinn lexicon categorizes words in the range of -5 to 5.
Trump used more negative sentiments. While Clinton had a more positive tone in her
tweets.
4.4 Drawbacks of the sentiment analysis
Text mining on US Presidential Elections Tweets 12
From the framework, we cannot predict the context of the tweets. The lexicon failed
partially to categorize the words properly. For instance, Trumps targeted towards his
opponents in a more sarcastic way. The Sarcasm element is a weakness of this
framework. Also, negative sentiments used by Clinton was used to discuss political
policies and thus cannot be termed under negative sentiments.
4.5 Time series of positive negative tweets of both candidates
Clinton started posting later than Trump. Both does not really have an upward or downward
trend and thus have a neutral line. Negative response from Clinton was likely because of the
series of negative tweet from trump.
Text mining on US Presidential Elections Tweets 13
TF – IDF
Term frequency refers to the number of times a word has occurred in a document. We have seen
the most common words used by both candidates in their tweets. However, we would also like to
know the least words used. Because the word America, or Trump or Hillary or Iamwithyou
appears a lot of times in either of the cases. These words clearly indicate that they are trying to
increase their supporters. We would like to see not so common words.
Here n is the number of times a word has been used in tweet and count is the total number of
words. In the above distribution n/ total for each handle, is the term frequency.
Text mining on US Presidential Elections Tweets 14
The curve is tailed towards the right, indicating these are the most unique words. The most
frequent words are on the left, where lower weights have been assigned. Both show similar
distribution.
Using the idf framework , weights are assigned to the most unique words, which helps us to find
words used least by both handles and if there were any similarities. From previous framework it
Text mining on US Presidential Elections Tweets 15
is evident that Clinton used twitter for talking about political policies, thanking her supporters,
and sometimes using negative tone on her opponents. Whereas Trump used mostly for attacking
opponents. With this analysis I can find out if Trump has ever used this platform to talk about
reforms and policies.
4.6 TF IDF analysis of trump tweets
The style of his tweets – their form as opposed to their actual meanings – has been far
more limited and has tend to focus on more superficial features, such as, insults,
misspellings, nonstandard grammar. For instance, goofy, foxandfreinds, gopdebate etc.
His tweets were not just used to defame opponents, but also to delegitimize the press,,
such as fox news The keywords doesn’t indicate about policies.
Text mining on US Presidential Elections Tweets 16
4.7 TF IDF analysis of Hillary Tweets
Clinton not only addresses on policy more but also possesses a much wider depth in
what she chooses to discuss.
Ngram analysis using TF IDF and pairwise correlation
Text mining on US Presidential Elections Tweets 17
Using ngrams and tf idf on ngrams, our analysis gives us a broader standpoint on the tweets of
Clinton and Trump. It gives a clearer view on how Clinton was clear about using this platform
for campaign updates, political policies, and gathering supporters. On the other hand, Trump
used it o a negative space for delegitimizing and defaming.
Text mining on US Presidential Elections Tweets 18
5. Conclusion
In this report, we evaluated the tweets of Clinton and Trump. The key findings are as
follows.
a. Trump tweeted more than Clinton
b. Trump used twitter to defame opponents, updating about campaigns, defaming news
agencies, praise himself and articulate policy makers. His tweets did not reflect about
making good changes or policies.
c. Clinton tweeted more about critical policies and said that she remains to be on the
positive side. She used @mention plenty of times to tag few important personalities
like Joe bidden or Obama. It was also found that Clinton was in defensive position in
few of her tweets and had to repond to Trumps challenges.
d. The ngram diagrams revealed recurrent topics covered in the tweets of the candidates.
