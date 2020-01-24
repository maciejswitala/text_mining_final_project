
#declares sequences of the parameters for grid search
candidate_observations = seq(5000,25000,5000)
candidate_word2vecs = seq(1,3,1)
candidate_k = seq(2,40,2)

#declares number of folds for cross-validation
folds = 5

#checks all possible combinations of parameters
perplexities = c()
i_number = 0
for(i in candidate_observations){
  
  i_number = i_number + 1
  
  for(j in candidate_word2vecs){
    
    #prepares DTM with chosen parameters
    source("TextMiningProject_CREATE_DTM.R")
    dtm_train <- create_dtm(train, dtm_cols = i, n_grams = j)
    
    #calculates LDA model for multiple topic numbers
    #then calculates perplexity (with cross validation)
    for(k in candidate_k){
      
      n <- nrow(dtm_train)
      splitfolds <- sample(1:folds, n, replace = TRUE)
      
      seed = 12345; burnin = 1000; iter = 1000; keep = 50
      
      for(fold in c(1:folds)){
        print(paste(i,j,k,fold))
        
        train_set <- dtm_train[splitfolds != fold, ]
        valid_set <- dtm_train[splitfolds == fold, ]
        
        model_lda<-LDA(train_set, k = k, method='Gibbs', 
                       list(seed=seed, burnin = burnin, iter = iter, keep = keep))
        
        #saves results
        perplexities <- append(perplexities, topicmodels::perplexity(model_lda,
                                                newdata = as.simple_triplet_matrix(valid_set)))
      }
    }
  }
}

#creates data.frame with results
data = data.frame(perplexity=perplexities,
                  obs=rep(candidate_observations, each=length(perplexities)/length(candidate_observations)),
                  ngrams=rep(candidate_word2vecs, each=length(perplexities)/length(candidate_observations)/length(candidate_word2vecs)),
                  fold=rep(1:folds, times=length(perplexities)/folds),
                  k=rep(candidate_k, times=length(perplexities)/folds/length(candidate_k), each=folds)
           )

#calculates mean perplexity for each cross calidation
data %>% 
  group_by(obs, ngrams, k) %>%
  summarize(mean(perplexity)) %>%
  arrange(`mean(perplexity)`)

#prepares plots
list_of_plots = c()
for(i in candidate_observations){
  for(j in candidate_word2vecs){
    
    data1 = data %>%
      filter(obs == i) %>%
      filter(ngrams == j)
    
    list_of_plots = c(list_of_plots,
                           list(ggplot(data1, aes(x = k, y = perplexity)) +
                             geom_point() +
                             geom_smooth(se = FALSE) +
                             ggtitle("5-fold cross-validation of topic modelling",
                                     paste(i,"obs.; ",j,"n-grams")) +
                             labs(x = "Candidate number of topics", y = "Perplexity when fitting the trained model to the hold-out set")
                           ))
    
  }
}

#plots multiple k vs perplexity plots
#enables choosing optimal parameters
list_of_plots[1]
list_of_plots[2]
list_of_plots[3]

list_of_plots[4]
list_of_plots[5]
list_of_plots[6]

list_of_plots[7]
list_of_plots[8]
list_of_plots[9]

list_of_plots[10]
list_of_plots[11]
list_of_plots[12]

list_of_plots[13]
list_of_plots[14]
list_of_plots[15]
