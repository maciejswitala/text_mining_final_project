#sets working directory
setwd("C:\\Users\\Maciek\\Desktop\\TextMiningProject")

#loads all the previously installed packages into memory
source("TextMiningProject_TECHNICALS.R")

#loads the data from prepared json
data <- fromJSON(file = "texts.txt")
ids <- fromJSON(file = "ids.txt")

#TOPIC MODELLING - PREPROCESSING

#creates train and test subsets
smp_size <- floor(0.6 * length(data))

set.seed(372532)
train_ind <- sample(seq_len(length(data)), size = smp_size)

train <- data[train_ind]
test <- data[-train_ind]

#checks number of documents
length(train)
length(test)

#creates Document Term Matrixes (uses prepared function)
source("TextMiningProject_CREATE_DTM.R")
dtm_train <- create_dtm(train, dtm_cols = 5000, n_grams = 1)
dtm_test <- create_dtm(test, dtm_cols = 5000, n_grams = 1)

#checks dimensions of DTMs
dim(dtm_train)
dim(dtm_test)

#runs LDA model (parameters chosen using optimalization file)
seed = 12345; burnin = 1000; iter = 1000; keep = 50
model_lda<-LDA(dtm_train, k = 21, method='Gibbs', list(seed=seed, burnin = burnin, iter = iter, keep = keep))

#calculates perplexity
topicmodels::perplexity(model_lda, newdata = as.simple_triplet_matrix(dtm_train))
topicmodels::perplexity(model_lda, newdata = as.simple_triplet_matrix(dtm_test))

#prepares visualisation
topics <- tidy(model_lda, matrix = "beta")

ap_top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()
