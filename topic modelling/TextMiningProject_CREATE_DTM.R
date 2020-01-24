
create_dtm <- function(abstracts, dtm_cols=1000, n_grams=3){
  
  #creates an empty data.frame
  df<-as.data.frame(rep(NA,length(abstracts)))
  colnames(df)<-"abstracts"
  df$id<-rep(NA,length(abstracts))
  
  #fills data.frame
  for(i in c(1:length(abstracts))){
    df[i,2]<-i #fills id
    df[i,1]<-abstracts[i] #fills abstracts
  }
  
  #prepares tokenized words
  words <- df %>%
    unnest_tokens(word, abstracts)
  
  #removes numbers
  words$word <- gsub('[[:digit:]]+', '', words$word)
  
  #removes dots
  words$word <- gsub('[[:punct:]]+', '', words$word)
  
  #stemming
  words$word <- wordStem(words$word, language = "en")
  
  #prepares list of stop-words
  stop_words <- stopwords("en")
  stop_words<-append(stop_words,
                     
                     #adds unneeded words to stop-words list
                     c("https","i'm","im"
                     ))
  
  stop_words<-as.data.frame(stop_words)
  colnames(stop_words) <- "word"
  
  #removes stop-words
  words <- words %>%
    anti_join(stop_words)
  
  words <- as.data.frame(words)
  
  #removes empty ones
  tokens <- words %>% 
    filter(!(word==""))
  
  #creates new variable = row number
  tokens <- tokens %>% 
    mutate(ind = row_number())
  
  #table: rows = abstracts, cols = single words
  tokens <- tokens %>% 
    group_by(id) %>% 
    mutate(ind = row_number()) %>%
    tidyr::spread(key = ind, value = word)
  
  #replaces NAs with ""
  tokens[is.na(tokens)] <- ""
  
  #merges columnss, space as separator
  tokens <- tidyr::unite(tokens, text,-id,sep =" " )
  
  #removes citations
  tokens$text <- trimws(tokens$text)
  
  tokens
  
  #removes "" after NAs replacement
  tokens <- tokens %>%
    filter(!(text==""))
  
  tokens
  
  #prepares data for word2vec transformation
  it_train = itoken(tokens$text, 
                    tokenizer = space_tokenizer,
                    ids = tokens$id, 
                    progressbar = FALSE)
  
  #creates vocabulary, takes n=1 for ngrams
  vocab = create_vocabulary(it_train, ngram=c(1,n_grams))
  
  #word2vec transformation
  vectorizer = vocab_vectorizer(vocab)
  dtm_train = text2vec::create_dtm(it_train, vectorizer)
  dim(dtm_train)
  
  words <- tidy(dtm_train)
  colnames(words) <- c("id","word",".")
  
  #prepares TF, IDF, TF-IDF
  words <- words %>%
    select(id, word) %>%
    count(id, word, sort = TRUE)
  
  total_words <- words %>% 
    group_by(id) %>% 
    summarize(total = sum(n))
  
  words <- left_join(words, total_words)
  
  words
  
  words <- words %>%
    bind_tf_idf(word, id, n)
  
  #sorts words in abstracts per IDF
  words <- words %>%
    select(-total) %>%
    arrange(desc(idf))
  
  n_row <- words %>%
    select(word) %>%
    unique() %>%
    nrow()
  
  begin <- floor(n_row/2)-dtm_cols/2
  end <- floor(n_row/2)+dtm_cols/2
  
  chosen_words <- words %>%
    group_by(word) %>%
    summarize(mean(idf)) %>%
    mutate(row = row_number()) %>%
    filter(row >= begin) %>%
    filter(row < end) %>%
    select(word) %>%
    pull()
  
  words <- words %>%
    filter(word %in% chosen_words)
  
  unique(words$word)
  
  words <- words %>%
    select(id, word) %>%
    arrange(id)
  
  words <- as.data.frame(words)
  
  #removes empty ones
  tokens <- words %>% 
    filter(!(word==""))
  
  #creates new variable = row number
  tokens <- tokens %>% 
    mutate(ind = row_number())
  
  #table: rows = abstracts, cols = single words
  tokens <- tokens %>% 
    group_by(id) %>% 
    mutate(ind = row_number()) %>%
    tidyr::spread(key = ind, value = word)
  
  #replaces NAs with ""
  tokens[is.na(tokens)] <- ""
  
  #merges cols, space as separator
  tokens <- tidyr::unite(tokens, text,-id,sep =" " )
  
  #removes citations
  tokens$text <- trimws(tokens$text)
  
  tokens
  
  #removes "" after NAs replacement
  tokens <- tokens %>%
    filter(!(text=="")) #5 378
  
  tokens
  
  it_train = itoken(tokens$text, 
                    tokenizer = space_tokenizer,
                    ids = tokens$id, #5 738
                    progressbar = FALSE)
  
  #creates vocabulary, takes n=1 for ngrams
  vocab = create_vocabulary(it_train) #25 008

  #word2vec transformation
  vectorizer = vocab_vectorizer(vocab)
  dtm_train = text2vec::create_dtm(it_train, vectorizer)
  
  dim(dtm_train)
  str(dtm_train)
  
  #prepares Document Term Matrix
  rowTotals <- apply(dtm_train, 1, sum)
  
  #if there are any empty rows, they are removed
  dtm_train   <- dtm_train[rowTotals> 0, ] 
  
  return(dtm_train)
}

