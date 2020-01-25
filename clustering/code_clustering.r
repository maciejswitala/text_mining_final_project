setwd("C:\\Users\\Maciek\\Desktop\\TextMiningProject")

library(jsonlite)
library(tm)
library(SnowballC)
library(tidytext)
library(dplyr)
library(wordcloud) 
library(cluster) 
library(fpc)  
library(ggplot2)  

data <- fromJSON(txt = "data\\texts.txt")

#preparing data  
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

#changing format of dtm and then removing some of the characters that couldn't be 
#removed using t_map
xxx <- tidy(dtm)
dim(xxx)
head(xxx)

xxx <- xxx %>% filter(term != '…' & term != '’' & term != '“')

#going back to dtm 
dtm <- xxx %>%
  cast_dtm(document, term, count)

#freq <- colSums(as.matrix(dtm))   
#length(freq)   
#ord <- order(freq)   
#m <- as.matrix(dtm)   
#dim(m)   
#write.csv(m, file="DocumentTermMatrix.csv")   

#Removing sparse terms with the maxium sparsity equal to 0.99
dtms_2 <- removeSparseTerms(dtm, 0.99) 

#Word frequencies for dtm 
freq <- colSums(as.matrix(dtms_2))   

#Brief look at dtms' frequencies  
findFreqTerms(dtms_2, lowfreq=20)  
findFreqTerms(dtms_2, lowfreq=50)  
findFreqTerms(dtms_2, lowfreq=100)  
findFreqTerms(dtms_2, lowfreq=1000)

wf <- data.frame(word=names(freq), freq=freq)   

mean(wf$freq) #280.816
median(wf$freq) #187

#Words with the frequency higher than the mean
p <- ggplot(subset(wf, freq>281), aes(reorder(word, -freq), freq))    
p <- p + geom_bar(stat="identity", fill='#cf5a86')   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   

#Words with the frequency higher than the median
p2 <- ggplot(subset(wf, freq>187), aes(reorder(word, -freq), freq))    
p2 <- p2 + geom_bar(stat="identity", fill='#cf5a86')   
p2 <- p2 + theme(axis.text.x=element_text(angle=45, hjust=1))   
p2   

# Look at the term correlations
findAssocs(dtms_2, c("sad" , "health"), corlimit=0) 
findAssocs(dtms_2, "ppl", corlimit=0.0)  

cor(as.matrix(dtms_2)[,"sad"], as.matrix(dtms_2)[,"play"])

cor(as.matrix(dtms_2)[,"bitch"], as.matrix(dtms_2)[,"fuck"])

cor(as.matrix(dtms_2)[,"sad"], as.matrix(dtms_2)[,"health"])



#The word clouds
#dark2 <- brewer.pal(6, "Dark2")   
set.seed(1234)
wordcloud(names(freq), freq, max.words=100, rot.per=0.35, colors=brewer.pal(8, "Dark2"),
          random.order=FALSE)    


#### CLUSTERING
d <- dist(t(dtms_2), method="euclidian")   
fit <- hclust(d=d, method="complete")    
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=3)  
rect.hclust(fit, k=2, border="red") 

#clustering with earlier defined k (using elbow method)
for (i in 2:29) wss[i] <- sum(kmeans(d,centers=i,nstart=25)$withinss)
plot(2:29, wss[2:29], type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

d <- dist(t(dtms_2), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0, main = 'Cusplot')

d <- dist(t(dtms_2), method="euclidian")   
kfit <- kmeans(d, 3)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)

########Clustering with differnt level of sparsity
#Removing sparse terms with the maxium sparsity equal to 0.97
dtms_3 <- removeSparseTerms(dtm, 0.97) 

#Frequencies of dtm
freq_2 <- colSums(as.matrix(dtms_3))   

wf_2 <- data.frame(word=names(freq_2), freq=freq_2)   

mean(wf_2$freq) #653.8276
median(wf_2$freq) #444

#Words with the frequency higher than the median
p <- ggplot(subset(wf, freq>444), aes(reorder(word, -freq), freq))    
p <- p + geom_bar(stat="identity", fill='#cf5a86')   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   

#Words with the frequency higher than the mean
p2 <- ggplot(subset(wf, freq>654), aes(reorder(word, -freq), freq))    
p2 <- p2 + geom_bar(stat="identity", fill='#cf5a86')   
p2 <- p2 + theme(axis.text.x=element_text(angle=45, hjust=1))   
p2   

#Correlations
findAssocs(dtms_3, c("sad" , "health"), corlimit=0) 
findAssocs(dtms_3, "mental", corlimit=0.0)  

cor(as.matrix(dtms_3)[,"sad"], as.matrix(dtms_3)[,"health"])

#Word cloud
#dark2 <- brewer.pal(6, "Dark2")   
set.seed(1234)
wordcloud(names(freq_2), freq_2, max.words=100, rot.per=0.35, colors=brewer.pal(8, "Dark2"),
          random.order=FALSE)    


# CLUSTERING
d <- dist(t(dtms_3), method="euclidian")   
fit <- hclust(d=d, method="complete")    
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=3)  
rect.hclust(fit, k=2, border="red") 


#clustering with k defined using elbow method
wss <- 2:19
for (i in 2:19) wss[i] <- sum(kmeans(d,centers=i,nstart=25)$withinss)
plot(2:19, wss[2:19], type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

d <- dist(t(dtms_3), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0, main = 'Cusplot')

d <- dist(t(dtms_3), method="euclidian")   
kfit <- kmeans(d, 3)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)

