library(tidyverse)
library(ds4psy)
library(syuzhet)

male_pattern <- "\\b(?:man|men|male|males|daddy|dad|father|masculine|boy|boys|gentleman|gentlemen|guy|guys|husband|he|him|his|sir|brother|son|uncle|grandfather|grandpa|boyfriend|mr|mr\\.|dude|bloke|chap|fella|lad)\\b"
female_pattern <- "\\b(?:woman|women|female|females|mama|mother|feminine|girl|girls|lady|ladies|gal|gals|wife|she|her|hers|madam|ms|ms\\.|mrs|mrs\\.|miss|sister|daughter|aunt|grandmother|grandma|girlfriend|missus|dame|lass)\\b"

read_sentences <- function(filepath) {
  readLines(filepath, warn = FALSE) %>%
    paste(., collapse = " ") %>%
    gsub("\\s+", " ", .) %>%
    unlist(strsplit(., "(?<=[.!?])\\s+", perl = TRUE))  
}

male_female_separation_aux <- function(male_sentence, female_sentence) {
  both_sentence <- male_sentence & female_sentence
  
  list(
    male = male_sentence & (!both_sentence),
    female = female_sentence & (!both_sentence)
  )
}

male_female_separation <- function(sentence) {
  male_female_separation_aux(
    male_sentence = lapply(sentence, function(x) 
      grepl(male_pattern, x, ignore.case = TRUE)) %>%
      unlist(),
    
    female_sentence = lapply(sentence, function(x) 
      grepl(female_pattern, x, ignore.case = TRUE)) %>%
      unlist()
  )
}

global_sentiment <- function(sentence) {
  group <- male_female_separation(sentence)
  
  list(
    X = sapply(with(group, sentence[male]), function(x)
      get_sentiment(char_v=x,
                    method = "syuzhet", language = "english", lowercase=FALSE)
    ) %>% as.numeric(),
    
    Y = sapply(with(group, sentence[female]), function(x)
      get_sentiment(char_v=x,
                    method = "syuzhet", language = "english", lowercase=FALSE)
    ) %>% as.numeric()  
  )
}
  
sentiment <- read_sentences("../../../datasets/scum/scum_manifesto.txt") %>%
  text_to_sentences %>% tolower %>% global_sentiment()

set.seed(123)
# X < Y
with(sentiment, wilcox.test(x = X, y = Y,
                          alternative = "less",
                          mu = 0, paired = FALSE, exact = NULL, correct = TRUE,
                          conf.int = FALSE, conf.level = 0.95))

data <- data.frame(
  value = c(sentiment$X, sentiment$Y),
  group = c(rep("X", length(sentiment$X)), rep("Y", length(sentiment$Y)))
)

ggplot(data, aes(x = value, fill = group)) +
  geom_histogram(data = subset(data, group == "X"), 
                 aes(y = ..density..), 
                 binwidth = 0.5, 
                 alpha = 0.5, 
                 color = "blue", 
                 fill = "blue") +
  geom_histogram(data = subset(data, group == "Y"), 
                 aes(y = ..density..), 
                 binwidth = 0.5, 
                 alpha = 0.5, 
                 color = "pink", 
                 fill = "pink") +
  labs(title = "Sentiment Distribution by Gender-Related Terms",
       subtitle = "Scapegoat (males): Blue,  Target Audience (females): Pink",
       x = "Sentiment Score",
       y = "Density",
       caption = "Source: Valerie Solanas's \"SCUM Manifesto\"") +
  theme_minimal()

