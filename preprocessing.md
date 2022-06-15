# Overview:

Operations were performed on version 5.0 of the United Nations General
Debate Corpus (UNGDC)

This section includes pre-processing steps for the UNGDC corpus. The
steps include:

1)  Load packages to library
2)  Load the UNGDC corpus
3)  Create `iso` and `year` variables from document titles.
4)  Remove speeches from 1970 because of incomplete data
5)  Transliteration text to ensure all characters are in ASCII
6)  Create unnested token object; remove stopwords and numbers

The original data can be downloaded from the following link:
<https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/0TJX8Y>

# 1)

This step loads the necessary packages into the R library.

``` r
pacman::p_load(tidyverse, magrittr, stringi, tidytext)
```

# 2)

This step loads the base corpus, UNGDC version 5.0

``` r
ungdc18_base <- read_csv("ungdc18_base.csv", 
    col_types = cols(...1 = col_skip()))
```

    ## New names:
    ## * `` -> ...1

# 3)

This step splits the document titles to create `iso` and `year`
variables, so that the data can be subset by state and time.

``` r
ungdc18_base %<>% 
  separate(col = doc_id, into = c("iso", "session", "year", sep = "_"), remove = FALSE) %>% 
  select(index, doc_id, iso, year, text)
```

    ## Warning: One or more parsing issues, see `problems()` for details

# 4)

We remove the 1970 data because it is incomplete.

``` r
ungdc18_base$year <- as.numeric(ungdc18_base$year) 
ungdc18_base %<>% filter(year > 1970)
```

# 5)

This step ensures that all characters are in a unifirm, ASCII encoding.

``` r
ungdc18_base %<>%
  mutate(translit = stri_trans_general(text, "Latin-ASCII"))
```

# 6)

This step tokenizes the text, removes stopwords from the `tidytext`
list, and removes number tokens. This is the object that will be used to
compute Concept Mover Distance values.

``` r
ungdc_unnest <- ungdc18_base %>%
   unnest_tokens(word, translit, to_lower = TRUE) %>% # tokenizes corpus
   anti_join(stop_words) %>% # rempval of `tidytext` stop words
   filter(!str_detect(word, "[0-9]+") ) %>% # removal of numbers
   count(doc_id, word) %>% # counts word frequencies and appends to `doc_id`
   bind_tf_idf(word, doc_id, n) # computes term frequency - inverse document frequency for each term per document
```

    ## Joining, by = "word"
