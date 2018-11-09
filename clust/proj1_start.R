library(tm)
library(readr)
library(dplyr)
library(tidytext)
library(text2vec)


list <- read_csv("data/listings.csv")
rev <- read_csv("data/reviews.csv")


# keep listings with more than three reviews
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


# create dtm
dtm <- word_per_row %>%
  count(listing_id, word) %>%
  cast_dtm(listing_id, word, n)
# inspect(dtm) # uncomment line to view contents of dtm
m <- as.matrix(dtm)


# OPTIONAL: join lat and lon before calculating dist
# Note: unsure if this is valid or not!
rw_nms <- rownames(m)
lat_lon <- list %>%
  filter(id %in% rw_nms) %>%
  select(latitude, longitude) %>%
  as.matrix() %>%
  scale()
m_lat_lon <- cbind(m, lat_lon)


# calculate distances
cos <- 1 - sim2(m, method = "cosine", norm = "l2")
cos_lat_lon <- 1 - sim2(m_lat_lon, method = "cosine", norm = "l2")
# jac <- 1 - sim2(as.matrix(dtm), method = "jaccard", norm= "none")


# cluster away!
test <- hclust(as.dist(cos), method = "complete")
plot(test)
