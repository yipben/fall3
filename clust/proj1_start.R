library(tm)
library(cld2)
library(readr)
library(dplyr)
library(tidytext)
library(text2vec)
library(SnowballC)
library(wordcloud)
library(factoextra)


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
  filter(listing_id %in% three_plus) %>%
  mutate(lang = detect_language(comments))

# add specific stop words
airbnb_stop_words <- c("boston", "airbnb", "host", "hosts",
                       "apartment", "house", "stay", "time",
                       "bnb", "city", "street", "night", "stayed")
airbnb_stop_words <- data_frame(word = airbnb_stop_words,
                                lexicon = "custom")
stop_words2 <- rbind(stop_words, airbnb_stop_words)

# df w/ one word per row
word_per_row <- rev %>%
  filter(lang == "en") %>%
  unnest_tokens(word, comments) %>%
  anti_join(stop_words2) %>%
  filter(!is.na(word)) 
stem_per_row <- rev %>%
  filter(lang == "en") %>%
  unnest_tokens(word, comments) %>%
  anti_join(stop_words2) %>%
  filter(!is.na(word)) %>%
  mutate(word = wordStem(word))

# wordcloud
word_counts <- word_per_row %>%
  group_by(word) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
wordcloud(word_counts$word, word_counts$count, max.words = 50)

# creating dtm
dtm <- stem_per_row %>%
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
set.seed(4151)
score_loc_samp <- score_loc_df %>%
  sample_n(200)
  

# CLUSTERING ==================================================================

# bag of words clustering -----------------------------------------------------

# calculating distances
cos <- 1 - sim2(m, method = "cosine", norm = "l2")
# jac <- 1 - sim2(as.matrix(dtm), method = "jaccard", norm = "none")

# selecting k (takes long time to b/c so many cols.)
fviz_nbclust(m_samp, hcut, k.max = 10, method = "wss") 
fviz_nbclust(m_samp, hcut, k.max = 10, nboot = 20, method = "gap") 
fviz_nbclust(m_samp, hcut, k.max = 10, method = "silhouette") 

fviz_nbclust(m_samp, kmeans, k.max = 20, method = "wss") 
fviz_nbclust(m_samp, kmeans, k.max = 10, nboot = 10, method = "gap")
fviz_nbclust(m_samp, kmeans, k.max = 20, method = "silhouette") 

# clustering based on location and sentiment score -----------------------------

l2dist <- score_loc_df %>%
  select(latitude, longitude, avg_score) %>%
  dist(method = "euclidean") 

# selecting k
fviz_nbclust(score_loc_samp[, 2:4], hcut, k.max = 20, method = "wss") # no clear elbow
fviz_nbclust(score_loc_samp[, 2:4], hcut, k.max = 20, method = "gap") # drops after 3
fviz_nbclust(score_loc_samp[, 2:4], hcut, k.max = 20, method = "silhouette") # drop after 3

fviz_nbclust(score_loc_samp[, 2:4], kmeans, k.max = 20, method = "wss") 
fviz_nbclust(score_loc_samp[, 2:4], kmeans, k.max = 20, method = "gap") # drops after 2
fviz_nbclust(score_loc_samp[, 2:4], kmeans, k.max = 20, method = "silhouette") 
