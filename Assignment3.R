veriler<-stringr::words
library(tidyverse)
library(dplyr)

set.seed(2016555014)
#1 
words_df<-sample(veriler ,100)
head(words_df)

tb_words<-tibble(word = words_df)
#2

words_df[str_detect(words_df, "^a[a-z]*e$" )]

# 3 
length(words_df [str_count(words_df, "[aeiou]") >3])


#4 List the five longest word in your data


data<-tb_words %>%
  mutate(
    vowels = str_count(words_df,"[aeiou]"),
    consonants = str_count(words_df,"(?i)([b-z&&[^eiou]])"),
    length = vowels + consonants
  )
head(data)
five_longest <- head(arrange(data,desc(length)))
five_longest


#5 

contain_word <- c("age", "any", "day", "exp", "her", "pro","the")
word_match <- str_c(contain_word, collapse = "|")
has_word <- str_subset(words_df, word_match) 
head(has_word)


















