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

# wordcloud
word_counts <- word_per_row %>%
  group_by(word) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
wordcloud(word_counts$word, word_counts$count, max.words = 50)

# Review scores (from air bnb) ------------------------------------------------

df2 <- list %>%
  filter(id %in% three_plus) %>%
  select(id, latitude, longitude, price, accommodates, review_scores_rating, review_scores_location) %>%
  mutate(price = parse_number(price), adj_price = price/accommodates)

df2 %>%
  select(latitude, longitude, adj_price, review_scores_rating, review_scores_location) %>%
  scale() %>%
  fviz_nbclust(kmeans, k.max = 20, method = "silhouette")

df2 %>%
  select(latitude, longitude, adj_price, review_scores_rating, review_scores_location) %>%
  scale() %>%
  fviz_nbclust(hcut, k.max = 20, method = "silhouette")

km <- df2 %>%
  select(latitude, longitude, adj_price, review_scores_rating, review_scores_location) %>%
  scale() %>%
  kmeans(3) %>%
  `$`(., "cluster")
hier9 <- df2 %>%
  select(latitude, longitude, adj_price, review_scores_rating, review_scores_location) %>%
  scale() %>%
  dist(method = "euclidean") %>%
  hclust(method = "ward.D2") %>%
  cutree(9)
hier3 <- df2 %>%
  select(latitude, longitude, adj_price, review_scores_rating, review_scores_location) %>%
  scale() %>%
  dist(method = "euclidean") %>%
  hclust(method = "ward.D2") %>%
  cutree(3)

clusters <- df2 %>% mutate(km = km, hier3 = hier3, hier9 = hier9)
write_csv(clusters, "clusters.csv")

clusters %>%
  group_by(km) %>%
  summarise(avg_price_per = mean(adj_price), avg_rating = mean(review_scores_rating),
            avg_loc_rating = mean(review_scores_location), count = n())

clusters %>%
  group_by(hier3) %>%
  summarise(avg_price_per = mean(adj_price), avg_rating = mean(review_scores_rating),
            avg_loc_rating = mean(review_scores_location), count = n())

clusters %>%
  group_by(hier9) %>%
  summarise(avg_price_per = mean(adj_price), avg_rating = mean(review_scores_rating),
            avg_loc_rating = mean(review_scores_location), count = n())
