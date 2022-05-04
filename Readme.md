### About

For now I've made some preliminary work with the articles from LeMonde:
* texts are separated one month = one .txt file (I can send you these files if you want);
* lemmatization with UDPipe:lemmatized texts are in the folder `lemonde/lemmatise` ;  
  
All what I did is written in R and can be found in `fr_test_init.R`
  
#### Word frequencies & more
I've written excel files with word and bigram frequencies for all words (lemmatized) by months. These are written in the files:
* `lemonde_word_frequencies.xlsx`
* `lemonde_bigram_frequencies.xlsx`

I've also written a table with the most prominent words in each months according to tf-idf index, here a small stoplist was applied (see in the code). Some description of what is tf-idf (term frequency inverse document frequency) can be found [here](https://www.tidytextmining.com/tfidf.html), my code is almost the same; the same book has some info about topic modeling if you want to refresh the idea.      
Table with top 100 tf-idf words: `top100_word_monthly_tf_idf.csv`
  

#### Topic modeling
As a super fast exploration I did two topic models: for 10 and 25 topics. You can see the results yourself if you want, running the code `fr_lemonde_test.R`, in the last part it has a code for loading the models to see the most probable words etc.
I've also saved the images on the distribution of topics in months, but they are to be interpreted with the most probable words in topics 10 / 25, which are saved as `beta_10t.csv` or `beta_25t_csv` accordingly.

