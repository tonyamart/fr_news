library(tidyverse)
library(tidytext)
library(openxlsx)
library(topicmodels)

theme_set(theme_minimal())
library(pals)

# script to execute
# the code works only if the workind directory is set correctly
# to see where you're : 
getwd()
# to set working directory: setwd("Path/To/The/Folder/With/Data")
# to see what's inside the folder you're now:
dir()


#################################
######## Run this part if you want to work with word frequencies 
#################################
##### Load texts #####
datt <- read_csv("lemonde_lemmas.csv")
str(datt)

dat <- datt[,-1]
str(dat)
rm(datt)

##### Stopwords #####

stopwords <- c("le", "de", "à", "être", "avoir", "un", "une", 
               "et", "qui", "que", "sur", "par", "d", "l", "ce", 
               "en", "y", "son", "qu", "dans", "c", "pour", "sur", 
               "il", "plus", "rien", "personne", "tout", "pas", "avec", 
               "mais", "ne", "n", "s", "se", "faire", "pouvoir", "on", 
               "ou", "aussi", "lui", "comme")


##### Count word frequencies #####
word_freq <- dat %>% 
  unnest_tokens(input = text, output = word, token = "words") %>% 
  # to count ngrams, set: (input = text, output = ngram, token = "ngrams", n = 2) 
  # n = 2 for bigrams, 3 for 3-grams etc. ; for more run: ?unnest_tokens
  
  group_by(mois) %>% 
  count(word, sort = T)

head(word_freq)

#####
#####
#####
#################################
######## Run this part if you want to work with topic models ##########
#################################
##### Look at a topic model #####

# step 1: load a file with a model

load("model_10t.rda") # will load model with 10 topics
# OR
load("model_25t.rda") # will load model with 25 topics

# look at the most probable words in each topic
top_words <- beta %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>% # n = 20 -- 20 most probable words, number can be changed
  ungroup() %>%
  arrange(topic, -beta)

# plot
top_words %>% 
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# top words in a selected topic:
top_words %>% 
  filter(topic == 8) # the 8th topic

# gamma: distribution of topics in documents (months)
gamma %>% 
  group_by(document, topic) %>% 
  summarise(avrg = mean(gamma)) %>% 
  separate(document, into = c("n", "month"), sep = "_") %>% 
  mutate(n = as.numeric(n)) %>% 
  filter(n != 1) %>% 
  ggplot(aes(x = as.factor(n), y = avrg, fill = as.factor(topic))) + geom_col() + 
  scale_fill_manual(values = as.vector(alphabet(n = 25))) + 
  labs(x = "Month", 
       y = "Distribution of topics", 
       fill = "Number of topic")

### Thoughts about models:
# a model with 10 topics shows similar homogeneous thematics for Feb, but results technically seems more relatable (some topics are distributed in more than 2 months, like the 8th)
# top_words %>% 
#   filter(topic == 8)

# for 25 topics it's pretty messy: no consistent theme is evolved overtime (to me), some are present for a couple of months (like dark-pink n13 in May and June)
# to see the topwords for a topic:
# top_words %>% 
#   filter(topic == 13)