# Overview

This section illustrates the computation of Concept Mover Distance (CMD)
values for terms chosen from three source texts. We proceed in the
following steps:

1)  Load packages to library

2)  Load the `ungdc18` and `ungdc_unnest` objects built in the
    “preprocessing” stage.

3)  load the `fastText` word embedding file used by Stoltz & Taylor
    (2019).

4)  Compute CMD values for selected concepts

5)  Add identifying variable to identify source text of each concept
    from part 4

# OUTPUT objects:

`un_long.csv`

# 1)

``` r
pacman::p_load(tidyverse,tidytext, magrittr, CMDist,text2vec, tm)
set.seed(42)
```

# 2)

``` r
# ungdc18
ungdc18 <- read_csv("ungdc18.csv", col_types = cols(...1 = col_skip(), 
    index = col_skip()))
```

    ## New names:
    ## * `` -> ...1

``` r
# ungdc_unnest
ungdc_unnest <- read_csv("ungdc_unnest.csv", 
    col_types = cols(...1 = col_skip()))
```

    ## New names:
    ## * `` -> ...1

``` r
# external word embeddings 
load("/Users/spatt/Documents/GitHub/inequality_vectors/inequality_vectors/fastText_embeddings.RData")
```

# 3)

Note: We used the following script - taken from the replication page for
Stoltz & Taylor (2019) - to load these embeddings: library(googledrive)
\# (see <https://googledrive.tidyverse.org/>) temp \<- tempfile()
drive_download(as_id(“17H4GOGedeGo0urQdDC-4e5qWQMeWLpGG”), path = temp,
overwrite = TRUE) my.wv \<- readRDS(temp) saveRDS(my.wv,
“data/ft.cc.en.300D.2M.Rds”)

The replication page can be found at this address:
<https://culturalcartography.gitlab.io/text2map/articles/CMDist-concept-movers-distance.html>

``` r
# external word embeddings 
load("/Users/spatt/Documents/GitHub/inequality_vectors/inequality_vectors/fastText_embeddings.RData")
```

# 4)

NIEO term list:

economic order based on equity sovereign equality redress existing
injustices self-determination cooperation based on equity regulation of
transnational corporations subordination under foreign control
unsatisfactory terms of trade preferential treatment transfer of
financial resources transfer of technology gap between the developed and
the developing countries progress of all people alien domination
neo-colonialism perpetuates inequality prevailing disparities effective
control of natural resources assistance to developing countries full and
equal participation

Reagan term list:

responsible economic realities incentive individual owners liberalize
trade opportunities overspent overtaxed overregulated sound economic
policies reduce inflation reduce interest rates increase foreign
investment raising productivity credit worthiness access to markets
stimulate growth opening up markets private capital flows political
freedom economic opportunity

Brandt term list:

gap between rich and poor countries escaping from poverty international
economic reform dialogue between north and south interdependence mutual
interests of north and south spirit of partnership just and humane
society economic growth domestic economic efficiency international
collaboration expanding trade increasing financial assistance repay its
debts development aid primary responsibility for development shared
responsibility free market economy international social justice official
development assistance price stabilization

``` r
# generate cmds ----
ungdc18$economicorderbasedonequity <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('economic order based on equity'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                                          
ungdc18$sovereignequality <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('sovereign equality'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                                                            
ungdc18$redressexistinginjustices <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('redress existing injustices'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                                            
ungdc18$selfdetermination <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('self-determination'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                                                            
ungdc18$cooperationbasedonequity <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('cooperation based on equity'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                                              
ungdc18$regulationoftransnationalcorporations <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('regulation of transnational corporations'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                    
ungdc18$subordination <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('subordination'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                                                                    
ungdc18$underforeigncontrol <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('under foreign control'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                                                        
ungdc18$unsatisfactorytermsoftrade <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('unsatisfactory terms of trade'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                                          
ungdc18$preferentialtreatment <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('preferential treatment'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                                                    
ungdc18$transferoffinancialresources <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('transfer of financial resources'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                                      
ungdc18$transferoftechnology <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('transfer of technology'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                                                      
ungdc18$gapbetweenthedevelopedandthedevelopingcountries <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('gap between the developed and the developing countries'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()

ungdc18$progressofallpeople <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('progress of all people'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                                                        
ungdc18$aliendomination <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('alien domination'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                                                                
ungdc18$neocolonialism <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('neo-colonialism'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                                                                  
ungdc18$perpetuatesinequality <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('perpetuates inequality'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                                                    
ungdc18$prevailingdisparities <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('prevailing disparities'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                                                    
ungdc18$effectivecontrolofnaturalresources <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('effective control of natural resources'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                          
ungdc18$assistancetodevelopingcountries <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('assistance to developing countries'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                                
ungdc18$fullandequalparticipation <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('full and equal participation'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
 

ungdc18$responsible <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('responsible'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                            
ungdc18$economicrealities <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('economic realities'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                
ungdc18$incentive <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('incentive'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                                
ungdc18$individualowners <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('individual owners'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                  
ungdc18$liberalize <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('liberalize'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                              
ungdc18$tradeopportunities <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('trade opportunities'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
              
ungdc18$overspent <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('overspent'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                                
ungdc18$overtaxed <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('overtaxed'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                                
ungdc18$overregulated <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('overregulated'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                        
ungdc18$soundeconomicpolicies <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('sound economic policies'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
        
ungdc18$reduceinflation <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('reduce inflation'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                    
ungdc18$reduceinterestrates <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('reduce interest rates'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
            
ungdc18$increaseforeigninvestment <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('increase foreign investment'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()

ungdc18$raisingproductivity <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('raising productivity'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
            
ungdc18$creditworthiness <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('creditworthiness'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                  
ungdc18$accesstomarkets <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('access to markets'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                    
ungdc18$stimulategrowth <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('stimulate growth'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                    
ungdc18$openingupmarkets <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('opening up markets'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                  
ungdc18$privatecapitalflows <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('private capital flows'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
            
ungdc18$politicalfreedom <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('political freedom'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                  
ungdc18$economicopportunity <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('economic opportunity'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()


ungdc18$gapbetweenrichandpoorcountries <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('gap between rich and poor countries'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
          
ungdc18$escapingfrompoverty <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('escaping from poverty'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                                
ungdc18$internationaleconomicreform <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('international economic reform'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                
ungdc18$dialoguebetweennorthandsouth <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('dialogue between north and south'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
              
ungdc18$interdependence <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('interdependence'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                                        
ungdc18$mutualinterestsofnorthandsouth <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('mutual interests of north and south'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
          
ungdc18$spiritofpartnership <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('spirit of partnership'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                                
ungdc18$justandhumanesociety <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('just and humane society'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                              
ungdc18$economicgrowth <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('economic growth'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                                          
ungdc18$domesticeconomicefficiency <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('domestic economic efficiency'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                  
ungdc18$internationalcollaboration <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('international collaboration'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                  
ungdc18$expandingtrade <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('expanding trade'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                                          
ungdc18$increasingfinancialassistance <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('increasing financial assistance'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
            
ungdc18$repayitsdebts <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('repay its debts'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                                            
ungdc18$developmentaid <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('development aid'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                                          
ungdc18$primaryresponsibilityfordevelopment <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('primary responsibility for development'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()

ungdc18$sharedresponsibility <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('shared responsibility'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                              
ungdc18$freemarketeconomy <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('free market economy'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                                    
ungdc18$internationalsocialjustice <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('international social justice'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
                  
ungdc18$officialdevelopmentassistance <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('official development assistance'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
            
ungdc18$pricestabilization <- ungdc_unnest %>% cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTf) %>% removeSparseTerms(.999) %>% CMDist(cw=c('price stabilization'),wv=my.wv) %>% select(,2) %>% unlist() %>% as.numeric()
```

# 5)

``` r
# nieo termlist ----
nieo_terms <- c("economic order based on equity", "sovereign equality", "redress existing injustices", "self-determination", "cooperation based on equity", "regulation of transnational corporations", "subordination", "under foreign control", "unsatisfactory terms of trade", "preferential treatment", "transfer of financial resources", "transfer of technology", "gap between the developed and the developing countries", "progress of all people", "alien domination", "neo-colonialism", "perpetuates inequality", "prevailing disparities", "effective control of natural resources", "assistance to developing countries", "full and equal participation")
# remove spaces and dashes
nieo_terms <- str_replace_all(nieo_terms, pattern = " ", replacement = "")
nieo_terms <- str_replace_all(nieo_terms, pattern = "-", replacement = "")

# reagan termlist ----
reagan_terms <- c("responsible", "economic realities", "incentive", "individual owners", "liberalize", "trade opportunities", "overspent", "overtaxed", "overregulated", "sound economic policies", "reduce inflation", "reduce interest rates", "increase foreign investment", "raising productivity", "credit worthiness", "access to markets", "stimulate growth", "opening up markets", "private capital flows", "political freedom", "economic opportunity")
# remove spaces and dashes
reagan_terms <- str_replace_all(reagan_terms, pattern = " ", replacement = "")
reagan_terms <- str_replace_all(reagan_terms, pattern = "-", replacement = "")

# brandt1 ----
brandt1_terms <- c("gap between rich and poor countries", "escaping from poverty", "international economic reform", "dialogue between north and south", "interdependence", "mutual interests of north and south", "spirit of partnership", "just and humane society", "economic growth", "domestic economic efficiency", "international collaboration", "expanding trade", "increasing financial assistance", "repay its debts", "development aid", "primary responsibility for development", "shared responsibility", "free market economy", "international social justice", "official development assistance", "price stabilization")
# remove spaces and dashes
brandt1_terms <- str_replace_all(brandt1_terms, pattern = " ", replacement = "")
brandt1_terms <- str_replace_all(brandt1_terms, pattern = "-", replacement = "")

# Convert corpus to long form ----

un_long <- ungdc18 %>% 
  pivot_longer(cols = 6:68, names_to = "cmd", values_to = "value")

# add variable for sourcetext ----
un_long %<>% 
  mutate(sourcetext = case_when(
    cmd %in% nieo_terms ~ "nieo",
    cmd %in% reagan_terms ~ "reagan",
    cmd %in% brandt1_terms ~ "brandt1"
  ))

# reduce size ----
un_long %<>% select(-text, -translit)

# Save object for fast loading
#write.csv(un_long, file = "un_long.csv")
```
