library(tm)
library(cld2)
library(readr)
library(dplyr)
library(tidytext)
library(text2vec)
library(SnowballC)
library(wordcloud)
library(ggfortify)
library(factoextra)


list <- read_csv("data/listings.csv")
rev <- read_csv("data/reviews.csv")

# SETUP =======================================================================

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

# Bag of words approach -------------------------------------------------------

# # creating dtm
# dtm <- stem_per_row %>%
#   count(listing_id, word) %>%
#   cast_dtm(listing_id, word, n)
# # inspect(dtm) # uncomment line to view contents of dtm
# m <- as.matrix(dtm)
# set.seed(4151)
# m_samp <- m[sample(1:nrow(m), 100), ] # sample m

# Sentiment & location --------------------------------------------------------

word_sent_score <- get_sentiments("afinn")
list_scores <- word_per_row %>%
  left_join(word_sent_score, by = "word") %>%
  filter(!is.na(score)) %>%
  group_by(listing_id) %>%
  summarise(avg_score = sum(score)/n())
score_loc_df <- list %>% 
  select(id, latitude, longitude, price, accommodates, review_scores_rating) %>%
  right_join(list_scores, by = c("id" = "listing_id")) %>%
  mutate(latitude = scale(latitude), 
         longitude = scale(longitude), 
         avg_score2 = scale(avg_score),
         price2 = scale(parse_number(price)/accommodates), # normalize by no. guests
         avg_score_raw = avg_score,
         price_raw = parse_number(price),
         review_score2 = scale(review_scores_rating), # air bnb rating
         review_score_raw = review_scores_rating)
set.seed(4151)
score_loc_samp <- score_loc_df %>%
  sample_n(200)

# Review scores (from air bnb) ------------------------------------------------

df2 <- list %>%
  filter(id %in% three_plus) %>%
  select(id, latitude, longitude, price, accommodates, review_scores_rating, review_scores_location) %>%
  mutate(price = parse_number(price), adj_price = price/accommodates)

df2 %>%
  select(latitude, longitude, adj_price, review_scores_rating, review_scores_location) %>%
  scale() %>%
  fviz_nbclust(kmeans, k.max = 20, method = "silhouette")
  

# CLUSTERING ==================================================================

# bag of words clustering -----------------------------------------------------

# # calculating distances
# cos <- 1 - sim2(m, method = "cosine", norm = "l2")
# # jac <- 1 - sim2(as.matrix(dtm), method = "jaccard", norm = "none")
# 
# # selecting k (takes long time b/c so many cols.)
# fviz_nbclust(m_samp, hcut, k.max = 20, method = "wss") 
# fviz_nbclust(m_samp, hcut, k.max = 10, nboot = 20, method = "gap") 
# fviz_nbclust(m_samp, hcut, k.max = 20, method = "silhouette") 
# 
# fviz_nbclust(m_samp, kmeans, k.max = 20, method = "wss") 
# fviz_nbclust(m_samp, kmeans, k.max = 10, nboot = 10, method = "gap")
# fviz_nbclust(m_samp, kmeans, k.max = 20, method = "silhouette") 

# clustering based location, sentiment score, and price -----------------------

# selecting k
score_loc_df %>%
  select(latitude, longitude, price2, review_score2) %>%
  fviz_nbclust(hcut, k.max = 20, method = "silhouette") 
score_loc_df %>%
  select(latitude, longitude, price2, avg_score2) %>%
  fviz_nbclust(kmeans, k.max = 20, method = "silhouette")

# create clusters
l2dist <- score_loc_df %>%
  select(latitude, longitude, price2, review_score2) %>%
  dist(method = "euclidean") 

hier_full <- hclust(l2dist, method = "ward.D2")
hier <- cutree(hier_full, 3)

km <- score_loc_df %>%
  select(latitude, longitude, price2, review_score2) %>%
  kmeans(4)

# output results for plotting in Tableau
clusters <- data_frame(id = score_loc_df$id, 
                       km = km$cluster,
                       hier = hier,
                       score = score_loc_df$avg_score_raw,
                       price = score_loc_df$price_raw)
write_csv(clusters, "clusters.csv")

words_clusters <- word_per_row %>%
  left_join(clusters, by = c("listing_id" = "id")) %>%
  inner_join(word_sent_score, by = "word")
km_word_counts <- words_clusters %>%
  group_by(km, word) %>%
  summarise(count = n())
hier_word_counts <- words_clusters %>%
  group_by(hier, word) %>%
  summarise(count = n())

par(mfrow = c(2, 2), mar = rep(0, 4))
km_word_counts %>%
  filter(km == 1) %>%
  with(wordcloud(word, count, max.words = 20))
km_word_counts %>%
  filter(km == 2) %>%
  with(wordcloud(word, count, max.words = 20))
km_word_counts %>%
  filter(km == 3) %>%
  with(wordcloud(word, count, max.words = 20))
km_word_counts %>%
  filter(km == 4) %>%
  with(wordcloud(word, count, max.words = 20))
km_word_counts %>%
  filter(km == 5) %>%
  with(wordcloud(word, count, max.words = 20))
dev.off()

par(mfrow = c(2, 3), mar = rep(0, 4))
hier_word_counts %>%
  filter(hier == 1) %>%
  with(wordcloud(word, count, max.words = 20))
hier_word_counts %>%
  filter(hier == 2) %>%
  with(wordcloud(word, count, max.words = 20))
hier_word_counts %>%
  filter(hier == 3) %>%
  with(wordcloud(word, count, max.words = 20))
hier_word_counts %>%
  filter(hier == 4) %>%
  with(wordcloud(word, count, max.words = 20))
hier_word_counts %>%
  filter(hier == 5) %>%
  with(wordcloud(word, count, max.words = 20))

summary(clusters)

clusters %>%
  group_by(km) %>%
  summarise(avg_price = mean(price), avg_score = mean(score))

clusters %>%
  group_by(hier) %>%
  summarise(avg_price = mean(price), avg_score = mean(score))

score_loc_df %>%
  select(avg_score2, review_scores_rating) %>%
  cor
  
