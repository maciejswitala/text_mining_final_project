#install.packages("syuzhet")
library(ggplot2)
library(syuzhet)
library(tibble)
data <- fromJSON(txt = "/Users/Julian/3_Semester/Text_Mining/text_mining_final_project-master/data/texts.txt")

#preparing data  

####################################################################### same as for clusterin from here: 
cleaned_txt <- Corpus(VectorSource(data))
cleaned_txt <- tm_map(cleaned_txt, content_transformer(tolower))

#removing some additional words which we found that do not remove using stopwords('english')
stop_words = c("’re", "’s", "’m", "’", "…", '“', 'httpstco…', "’r")
cleaned_txt<- tm_map(cleaned_txt, removeWords, c(stopwords("english"), stop_words))
cleaned_txt<- tm_map(cleaned_txt, removePunctuation)
cleaned_txt <- tm_map(cleaned_txt, removeNumbers)
cleaned_txt<- tm_map(cleaned_txt, stripWhitespace)
cleaned_txt <- tm_map(cleaned_txt, stemDocument)



#converting into document term matrix 
dtm <- DocumentTermMatrix(cleaned_txt)   


############################################################################ to here 
as_tidy <- tidy(dtm)


############################################################################ BING
# Creating leixcon: 
bing <- get_sentiments("bing")
as_bing_words <- inner_join(as_tidy,bing,by = c("term"="word"))

index <- as_bing_words%>%mutate(doc=as.numeric(document))
index <- index %>% count(sentiment,doc)
index <- index %>% spread(sentiment,n,fill=0)
index <- index %>% mutate(polarity = positive-negative)
index

####### PLOT 1
ggplot(index, aes(doc, polarity)) +   geom_col(show.legend = FALSE
) + theme_minimal() + theme(legend.position="none", panel.grid.major = element_blank()) + labs( x = "Full Thread", y = "Polarity") + ggtitle("Sentiment of social media posts on depression over time (Bing)") + theme(plot.title = element_text(hjust=0.5))

####### PLOT 2
bing_word_counts <- as_bing_words%>%
  count(term, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ggplot(aes(reorder(term, n), n, fill = sentiment)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment (Bing)", x = NULL) +
  coord_flip()

############################################################################ AFIN

afinn <- get_sentiments("afinn")
as_afinn_words <- inner_join(as_tidy,afinn,by = c("term"="word"))

####### PLOT 1
afinn_word_counts <- as_afinn_words%>%
  count(term, value, sort = TRUE) %>%
  ungroup()

afinn_word_counts %>%
  group_by(n) %>%
  top_n(10) %>%
  ggplot(aes(reorder(term, n), n, fill = n)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~n, scales = "free_y") +
  labs(y = "Contribution to sentiment (Afinn)", x = NULL) +
  coord_flip()


############################################################################ NRC

nrc_data <- get_nrc_sentiment(data)
# Let's look at the corpus as a whole again:
nrc_data <- as.data.frame(colSums(nrc_data))
nrc_data <- rownames_to_column(nrc_data) 
colnames(nrc_data) <- c("emotion", "count")
ggplot(nrc_data, aes(x = emotion, y = count, fill = emotion)) + geom_bar(stat = "identity") + theme_minimal() + theme(legend.position="none", panel.grid.major = element_blank()) + labs( x = "Emotion", y = "Total Count") + ggtitle("Sentiment of social media posts on depression (NRC)") + theme(plot.title = element_text(hjust=0.5))

#Source: https://towardsdatascience.com/a-light-introduction-to-text-analysis-in-r-ea291a9865a8
# https://uc-r.github.io/sentiment_analysis

