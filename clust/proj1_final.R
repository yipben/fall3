library(cld2)
library(purrr)
library(readr)
library(dplyr)
library(tidytext)
library(wordcloud)
library(factoextra)

# *edit these file paths to run*
list <- read_csv("data/listings.csv")
rev <- read_csv("data/reviews.csv")


# DATA MANIPULATION -----------------------------------------------------------

# keeping listings with more than three reviews
three_plus <- rev %>%
  count(listing_id) %>%
  filter(n > 3) %>%
  pull(listing_id)
rev <- rev %>%
  filter(listing_id %in% three_plus) %>%
  mutate(lang = detect_language(comments)) # add language col

# create df for clustering
df2 <- list %>%
  filter(id %in% three_plus) %>%
  select(id, latitude, longitude, price, accommodates, review_scores_rating, review_scores_location) %>%
  mutate(price = parse_number(price), adj_price = price/accommodates)


# CLUSTERING ------------------------------------------------------------------

# find k
df2 %>%
  select(latitude, longitude, adj_price, review_scores_rating, review_scores_location) %>%
  scale() %>%
  fviz_nbclust(kmeans, k.max = 20, method = "silhouette")
df2 %>%
  select(latitude, longitude, adj_price, review_scores_rating, review_scores_location) %>%
  scale() %>%
  fviz_nbclust(hcut, k.max = 20, method = "silhouette")

# create clusters
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

# output clusters for plotting in Tableau
clusters <- df2 %>% mutate(km = km, hier3 = hier3, hier9 = hier9)
write_csv(clusters, "clusters.csv")

# summary tables
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


# WORDCLOUDS ------------------------------------------------------------------

# Boston/air bnb-specific stop words
airbnb_stop_words <- c("boston", "airbnb", "host", "hosts",
                       "apartment", "house", "stay", "time",
                       "bnb", "city", "street", "night", "stayed",
                       "staying", tolower(list$host_name))
airbnb_stop_words_df1 <- data_frame(word = airbnb_stop_words,
                                    lexicon = "custom")
stop_words2 <- rbind(stop_words, airbnb_stop_words_df1)

# df w/ one word per row
word_per_row <- rev %>%
  filter(lang == "en") %>%
  unnest_tokens(word, comments) %>%
  anti_join(stop_words2) %>%
  filter(!is.na(word)) 

# remove top 20 common words to create more interesting clouds
common_words <- word_per_row %>%
  left_join(clusters, by = c("listing_id" = "id")) %>% 
  group_by(word) %>%
  summarise(count = n(), n_clust = length(unique(hier9))) %>%
  arrange(desc(count)) %>%
  filter(n_clust == 9) %>%
  select(word) %>%
  pull()
word_per_row2 <- word_per_row %>%
  filter(!(word %in% common_words[1:20]))

# df's for comparing wordclouds
hier9_words1 <- clusters %>%
  select(id, hier9) %>%
  inner_join(word_per_row, by = c("id" = "listing_id")) %>%
  select(id, word, hier9) %>%
  group_by(hier9, word) %>%
  summarise(count = n())
hier9_words2 <- clusters %>%
  select(id, hier9) %>%
  inner_join(word_per_row2, by = c("id" = "listing_id")) %>%
  select(id, word, hier9) %>%
  group_by(hier9, word) %>%
  summarise(count = n())

# function for plotting wordclouds
plot_wordcloud <- function(x, color = T, remove_common = T) {
  if (remove_common) {
    data <- hier9_words2 
  } else {
    data <- hier9_words1
  }
  if (color) {
    hier9_words %>%
      filter(hier9 == x) %>%
      with(wordcloud(word, count, max.words = 30, colors = brewer.pal(9,"BuGn")))
  } else {
    hier9_words %>%
      filter(hier9 == x) %>%
      with(wordcloud(word, count, max.words = 30))
  }  
}

# plot
par(mfrow = c(3, 3), mar = rep(0, 4)) # plot all in one pane 
map(as.list(1:9), ~ plot_wordcloud(.x, color = T))

par(mfrow = c(1, 1), mar = rep(0, 4)) # plot one at a time
map(as.list(1:9), ~ plot_wordcloud(.x, color = T))





