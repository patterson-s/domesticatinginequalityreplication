``` r
pacman::p_load(tidyverse, magrittr)
```

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
