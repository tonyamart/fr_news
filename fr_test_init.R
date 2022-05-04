library(tidyverse)
library(tidytext)
library(openxlsx)
library(topicmodels)

theme_set(theme_minimal())
library(pals)

# setwd("../help_fr/") 
dir()


##### reading & lemmatizing texts from Le Monde ####

dat <- read_file(file = "LeMonde_aasta_2020.txt")

dat <- tibble(mois = "", 
              text = dat)

test <- dat %>% 
  separate_rows(text, sep = "\\*\\*\\*\\* \\*journal_LeMonde ") %>% 
  mutate(mois = str_extract(text, "\\*mois_\\w+")) %>% 
  mutate(mois = str_remove_all(mois, "\\*mois_")) %>% 
  mutate(path = paste0("lemonde/", row_number(), "_", mois, ".txt"))

str(test)

# write separate file for each month
for (i in 1:length(test$text)) {
  write_file(x = test$text[i], file = test$path[i])
}

files <- list.files(path = "lemonde/")
files[1]

library(udpipe)
# test library #
# fr <- udpipe_download_model(language = "french")
# #str(fr)
# fr_model <- udpipe_load_model(file = "french-gsd-ud-2.5-191206.udpipe")
# 
# t <- c("je n'étais pas sûre si vous pouvez lire ce texte. mais on verra",
#        "il n'y a qu'un exemple")
# x <- udpipe_annotate(fr_model, x = t, tagger = "default", parser = "none")
# x <- as.data.frame(x)
# str(x)
#
# fev <- read_file("lemonde/2_Février.txt")
# x <- udpipe_annotate(fr_model, x = fev, tagger = "default", parser = "none")
# x <- as.data.frame(x)
# str(x)
# x <- x %>% filter(upos != "PUNCT")
# y <- x$lemma[1:20]
# y <- paste(y, collapse = " ")
# y
# write_file(y, file = "lemonde/test.txt")

### lemmatization w/udpipe
# lemmatized files written in the folder "lemmatise/" as lem_*num_mois*.txt

for (i in 2:length(files)) {
  t <- read_file(paste0("lemonde/", files[i]))
  x <- udpipe_annotate(fr_model, x = t, tagger = "default", parser = "none")
  x <- as.data.frame(x)
  x <- x %>% filter(upos != "PUNCT")
  y <- x$lemma
  y <- paste(y, collapse = " ")
  write_file(y, file = paste0("lemonde/lemmatise/lem_", files[i]))
}

##### Write lemmatized texts #####

dat <- tibble(id = list.files(path = "lemonde/lemmatise/", 
                              pattern = ".*?\\.txt$", full.names = T),
              text = sapply(fh, read_file))
str(dat)

dat <- dat %>% 
  mutate(mois = str_replace_all(id, "lemonde/lemmatise//lem_(.*?).txt", "\\1")) %>% 
  mutate(text = str_remove_all(text, "\\*mois_\\w+"))

write.csv(dat, file = "lemonde_lemmas.csv")

##### Load texts #####

datt <- read_csv("lemonde_lemmas.csv")
str(datt)

dat <- datt[,-1]
str(dat)
rm(datt)
##### stopwords #####

stopwords <- c("le", "de", "à", "être", "avoir", "un", "une", 
  "et", "qui", "que", "sur", "par", "d", "l", "ce", 
  "en", "y", "son", "qu", "dans", "c", "pour", "sur", 
  "il", "plus", "rien", "personne", "tout", "pas", "avec", 
  "mais", "ne", "n", "s", "se", "faire", "pouvoir", "on", 
  "ou", "aussi", "lui", "comme")

##### Word frequencies ####

word_freq <- dat %>% 
  unnest_tokens(input = text, output = word, token = "words") %>% 
  group_by(mois) %>% 
  count(word, sort = T)

word_count <- word_freq %>% 
  group_by(mois) %>% 
  summarise(total_n_mois = sum(n))

word_freq <- left_join(word_freq, word_count, by = "mois")

head(word_freq)

word_freq <- word_freq %>% 
  ungroup() %>% 
  mutate(perc_mois = n/total_n_mois*100)


##### tf-idf bind ####

month_tf_idf <- bind_tf_idf(word_freq, word, mois, n)

head(month_tf_idf)

tfidf <- month_tf_idf %>% 
  filter(!word %in% stopwords) %>% 
  select(-total_n_mois) %>% 
  slice_max(tf_idf, n = 100) %>% 
  arrange(desc(tf_idf))

write.csv(tfidf, file = "top100_word_montly_tf_idf.csv")


##### bigram frequencies #####
  
bigram_freq <- dat %>% 
  unnest_tokens(input = text, output = ngram, token = "ngrams", n = 2) %>% 
  group_by(mois) %>% 
  count(ngram, sort = T)
head(bigram_freq)

bigram_count <- bigram_freq %>% 
  group_by(mois) %>% 
  summarise(total_n_mois = sum(n))

bigram_freq <- left_join(bigram_freq, bigram_count, by = "mois")

head(bigram_freq)

bigram_freq <- bigram_freq %>% 
  ungroup() %>% 
  mutate(perc_mois = n/total_n_mois*100)

##### write frequencies to excel #####

# split the table into a list of lists
temp <- split(bigram_freq, bigram_freq$mois)

# function to attach lists to export to excel file
files_to_excel <- list()
to_excel <- function(temp, file) {
  append_list <- NULL
  for (i in 1:length(temp)) {
    append_list <- list(x = temp[[i]])
    file <- c(file, append_list)
  }
  #str(files_to_excel)
  
  list_count <- seq(1,length(temp))
  names(file) <- list_count
  file
}

# apply funciton
excel_file <- to_excel(temp, files_to_excel)

# write
# nb change filename
#write.xlsx(excel_file, file = "lemonde_bigram_frequencies.xlsx")













##### simple topic models building (10 and 25 topics) #####

word_freq <- dat %>% 
  unnest_tokens(input = text, output = word, token = "words") %>% 
  filter(!word %in% stopwords) %>% 
  group_by(mois) %>% 
  count(word, sort = T)

glimpse(word_freq)

# create a matrix of words appearances
dtm <- word_freq %>% cast_dtm(mois, word, n)

######### NB model building may take a lot of time
# month_model <- LDA(dtm, k = 25, control = list(seed = 1234))
# month_model

# beta -- probabilities of words in topics (sorted, top 10 words selected)
beta <- tidy(month_model, matrix = "beta")

top_words <- beta %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>% 
  ungroup() %>%
  arrange(topic, -beta)

head(top_words)

top_words %>% 
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# gamma: probability of topics in documents (months in our case)
gamma <- tidy(month_model, matrix = "gamma")

# February is fully described by topic 5, which is about the appearance of covid
gamma %>% 
  filter(document == "2_Février")
top_words %>% 
  filter(topic == 5)

# distribution of topics over time
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

# save(month_model, beta, gamma, file = "model_25t.rda")
# save(month_model, beta, gamma, file = "model_10t.rda")

# write.csv(top_words, "beta_25t.csv")
