library(tm)
library(readr)
library(dplyr)
library(tidytext)
library(text2vec)


list <- read_csv("data/listings.csv")
rev <- read_csv("data/reviews.csv")

# SETUP =======================================================================

# Bag of words approach -------------------------------------------------------

# keeping listings with more than three reviews
three_plus <- rev %>%
  count(listing_id) %>%
  filter(n > 3) %>%
  pull(listing_id)
rev <- rev %>%
  filter(listing_id %in% three_plus)

# df w/ one word per row
word_per_row <- rev %>%
  unnest_tokens(word, comments) %>%
  anti_join(stop_words) %>%
  filter(!is.na(word))

# creating dtm
dtm <- word_per_row %>%
  count(listing_id, word) %>%
  cast_dtm(listing_id, word, n)
# inspect(dtm) # uncomment line to view contents of dtm
m <- as.matrix(dtm)
set.seed(4151)
m_samp <- m[sample(1:nrow(m), 100), ] # sample m

# Sentiment & location --------------------------------------------------------

word_sent_score <- get_sentiments("afinn")
list_scores <- word_per_row %>%
  left_join(word_sent_score, by = "word") %>%
  filter(!is.na(score)) %>%
  group_by(listing_id) %>%
  summarise(avg_score = sum(score)/n())
score_loc_df <- list %>% 
  select(id, latitude, longitude) %>%
  right_join(list_scores, by = c("id" = "listing_id")) %>%
  mutate(latitude = scale(latitude), 
         longitude = scale(longitude), 
         avg_score = scale(avg_score))
  

# CLUSTERING ==================================================================

# bag of words clustering -----------------------------------------------------

# calculating distances
cos <- 1 - sim2(m, method = "cosine", norm = "l2")
# jac <- 1 - sim2(as.matrix(dtm), method = "jaccard", norm= "none")

# selecting k
fviz_nbclust(m_samp, hcut, k.max = 20, method = "wss") # no clear elbow
fviz_nbclust(m_samp, hcut, k.max = 30, nboot = 10, method = "gap") # 
fviz_nbclust(m_samp, hcut, k.max = 20, method = "silhouette")

fviz_nbclust(m_samp, kmeans, k.max = 20, method = "wss") # no clear elbow
fviz_nbclust(m_samp, kmeans, k.max = 10, nboot = 10, method = "gap")
fviz_nbclust(m_samp, kmeans, k.max = 20, method = "silhouette")

# clustering based on location and sentiment score -----------------------------

l2dist <- score_loc_df %>%
  select(latitude, longitude, avg_score) %>%
  dist(method = "euclidean") 

# selecting k
fviz_nbclust(l2dist, hcut, k.max = 20, method = "wss") # no clear elbow
fviz_nbclust(m_samp, hcut, k.max = 30, nboot = 10, method = "gap") # 
fviz_nbclust(m_samp, hcut, k.max = 20, method = "silhouette")
