# Overview:

Operations were performed on version 5.0 of the United Nations General
Debate Corpus (UNGDC)

This section includes pre-processing steps for the UNGDC corpus. The
steps include:

1.  Loading the corpus.
2.  Building `iso` (or country) and `year` variables from the document
    titles.
3.  Filtering out the speeches from 1970, since that year is incomplete.
4.  Transliterating the speech text so that all characters are
    ASCII-encoded.

The data can be downloaded from the following link:
<https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/0TJX8Y>

# Load packages to library

``` r
pacman::p_load(tidyverse, magrittr, stringi)
```

# Load and Pre-process the UNGDC data

``` r
# Load UNGDC data ----
ungdc18_base <- read_csv("ungdc18_base.csv", 
    col_types = cols(...1 = col_skip()))
```

    ## New names:
    ## * `` -> ...1

``` r
# Create `iso` and `year` variables from document titles ----
ungdc18_base %<>% 
  separate(col = doc_id, into = c("iso", "session", "year", sep = "_"), remove = FALSE) %>% 
  select(index, doc_id, iso, year, text)
```

    ## Warning: One or more parsing issues, see `problems()` for details

``` r
# Remove speeches from 1970 because of incomplete data ----
ungdc18_base$year <- as.numeric(ungdc18_base$year) 

ungdc18_base %<>% filter(year > 1970)
```

# Transliteration text to ensure all characters are in ASCII

``` r
# Transliterate the text using `stri_trans_general` from the `stringi` package
ungdc18_base %<>%
  mutate(translit = stri_trans_general(text, "Latin-ASCII"))
```

# Conclusion:

The result of these processes is a version of the UNGDC corpus that
contains the following variables: 1) `index` : the numeric position of
each speech. 2) `doc_id`: the title of each document, as listed in the
UNGDC corpus. 3) `iso`: the state that delivers each speech. 4) `year`:
the year in which the speech was delivered. 5) `text`: the raw text of
the speech. 6) `translit`: the ASCII-transliterated text of each speech.
