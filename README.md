# text_mining_final_project
Repository created for storing the text mining final project made by Michalina Cieśliak, Julian Jarząbkowski, Maciej Świtała.
The data come from: https://github.com/AshwanthRamji/Depression-Sentiment-Analysis-with-Twitter-Data.
Please notice that all directories are set in a way that both codes and data need to be unpacked into the same directory.

All the materials consist of following folders/files:

- clustering (prepared by: Michalina Cieślak)
  - code_clustering.r: some initial data preprocessing and analysis; also clustering, wordclouds etc.
  
- data (prepared together)
  - depression data preprocessed.txt - data,
  - ids.txt - preprocessed ids,
  - skrypt.js - java script used for preprocessing data,
  - texts.txt - preprocessed texts.
  
- sentiment analysis (prepared by: Julian Jarząbkowski)
  - sentiment_analysis.r - code donducting some sentiment analysis.
  
- topic modelling (prepared by: Maciej Świtała)
  - TextMiningProject_CREATE_DTM.R - function which creates a dtm with specified number of words and using certain n_grams,
  - TextMiningProject_MAIN_CODE.R - main code, does topic model; calculates perplexity for train/test, presents most important words for each topic,
  - TextMiningProject_OPTIMALIZATION.R - enables optimalization of: number of topics, n_grams, number of words,
  - TextMiningProject_TECHNICALS.R - loads all used packages.
