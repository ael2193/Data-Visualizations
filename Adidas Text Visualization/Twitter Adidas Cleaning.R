library(stringr)
library(tidyverse)
library(readr)
library(tidyr)
library(tidytext)

df <- read.csv("adidas_sa.csv")
df <- df %>%
  mutate(text = str_replace_all(text, #hastags
                                pattern = "#[[:alnum:]]+", 
                                replacement = "")) %>%
  mutate(text = str_replace_all(text,
                                pattern = "@[0-9A-Za-z_]+", 
                                replacement = "")) %>%
  mutate(has_URL = str_detect(text, 
                              pattern = "http[s]*://t.co/[[:alnum:]]+")) %>%
  mutate(text = str_replace_all(text,
                                pattern = "http[s]*://t.co/[[:alnum:]]+",
                                replacement = "")) %>%
  mutate(text = iconv(text, from = "UTF-8", to = "ASCII", sub = "byte")) %>%
  mutate(has_emoji = str_detect(text, pattern = "<[[:alnum:]]+>")) %>%
  mutate(text = str_replace_all(text, pattern = "(<[[:alnum:]]+>)+", 
                                replacement = ""))
tidy_df <- df %>% unnest_tokens(output = word, input = text) %>% 
  anti_join(bind_rows(stop_words, data.frame(word = c("rt", "https"), 
                                             lexicon = "TWITTER")), 
            by = "word") %>%
  mutate(word =  gsub("[[:punct:][:blank:]]+", "", word)) %>%
  mutate(word = gsub("[0-9]+", "", word)) %>%
  mutate(word =  gsub(" *\\b[[:alpha:]]{1}\\b *", " ", word)) %>%
  mutate(word = gsub("^ +| +$|( ) +", "\\1", word)) 

is.na(tidy_df$word) <- tidy_df$word==''
write_csv(tidy_df, "clean_tidy.csv")
