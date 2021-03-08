# !diagnostics off

## ---------------------------
##
## Script name: xx-lookup-to-finalize.R
##
## Project: Tracing Thick Concepts
##
## Purpose of script: Extract lexical and syntactic matches and create single corpus (local)
##
## Author: Lucien Baumgartner
##
## Date created: 14.09.2020
##
## Email: lucienbaumgartner@philos.uzh.ch
##
## ---------------------------
##
## Notes:
##    DO NOT RUN
##
## ---------------------------

## ---------------------------
######## 1 Libraries #########
## ---------------------------
library(dplyr)
library(quanteda)
rm(list=ls())

## ---------------------------
## 2 Set working directory ###
## ---------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#setwd('~/tc_methods_paper/src')
getwd()

## ---------------------------
######## 2 Load data #########
## ---------------------------
## Keywords
kw <- read.table('../input/dict_rerun_02_09_20.txt', stringsAsFactors = F, sep=',', header = T) %>% rename(TARGET = word)
## sentiWords dict
load('../res/sentiWords-db.RDS')
## Sweep data
plist <- list()
for(i in list.files('/Volumes/INTENSO/methods_paper/output/00-sweep/', full.names = T)){
  plist[[i]] <- read.csv(i, stringsAsFactors = F)
}
df <- do.call(rbind, plist)
#df <- select(df, -id, -created_utc)
#df <- mutate(df, context = 'reddit')

## ---------------------------
## Traditional calls
load('/Volumes/INTENSO/methods_paper/output/02-finalized-corpora/baseline/reddit/new-reddit.RDS')
names(reddit)
names(df)

## ---------------------------
## Combine
df <- rbind(df, reddit) %>% as_tibble
#save(df, file = '/Volumes/INTENSO/methods_paper/output/02.5-makro/makro.RDS', compress = 'gzip')

## ---------------------------
### 4 Select corpus entries ##
## ---------------------------
## filter out observations containing negating adverbs
prblm <- c('too', 'not', 'less', 'both', 'equally', 'through', 'most', 'many', 
           'more', 'non', 'other', 'last', 'overall', 'much', 'idk', 'holy', 'such')
df <- df %>% filter(!(TARGET_mod%in%prblm|ADV%in%prblm|ADJ%in%prblm)) # NOTE: df becomes dfx !!!

## ---------------------------
### 5 Sentiment annotation ###
## ---------------------------
## For conjoined adjectives
annot <- tokens(df$ADJ)
annot <- tokens_lookup(annot, dictionary = sentiWords$num)
annot <- sapply(annot, function(x) mean(as.numeric(unlist(x)),  na.rm = T))
df$sentiWords <- annot
## For target adjectives (only dichot. polarity)
annot <- tokens_lookup(tokens(unique(df$TARGET)), dictionary = sentiWords$dichot)
annot <- tibble(TARGET = unique(df$TARGET), TARGET_pol = sapply(annot, function(x) x[1]))
neutrals <- kw %>% filter(cat == 'descriptive_concepts') %>% pull(TARGET) # isolate descriptive concepts
annot <- annot %>% mutate(TARGET_pol = ifelse(TARGET %in% neutrals, 'neutral', TARGET_pol)) # give them a neutral polarity
df <- left_join(df, annot) # join annot to corpus
## Join keyword data
df <- left_join(df, kw)
## Sanitize CCONJ
df <- df %>% mutate(CCONJ = ifelse(is.na(CCONJ), 'but', CCONJ))
## filter out adjectives with a sentiWords score of 0 or NA
df <- filter(df, !(sentiWords == 0 | is.na(sentiWords)))
## Make dummy for modifier
df <- df %>% mutate(ADV_dummy = ifelse(is.na(ADV), 0, 1),
                    TARGET_pol_mod_dummy = ifelse(is.na(TARGET_mod), 0, 1))
## filter out other problematic structures
problemos <- df %>% 
  rowwise %>% 
  mutate(
    prblm = case_when(
      first == 1 ~ paste0(paste('(both|(at the same time))', switch(!(is.na(TARGET_mod)+1),TARGET_mod,NULL), TARGET, switch(!(is.na(comma)+1), comma, NULL), CCONJ, switch(!(is.na(ADV)+1),ADV,NULL), ADJ, collapse = ' '), '(at the same time)?'),
      first == 0 ~ paste0(paste('(both|(at the same time))', switch(!(is.na(ADV)+1),ADV,NULL), ADJ, switch(!(is.na(comma)+1), comma, NULL), CCONJ, switch(!(is.na(TARGET_mod)+1),TARGET_mod,NULL), TARGET, collapse = ' '), '(at the same time)?')
    ),
    prblm = gsub('\\s+', '\\\\\\s', prblm),
    ADJ_pol = ifelse(sentiWords > 0, 'positive', 'negative')
  ) %>% 
  filter(grepl(prblm, corpus) & !(ADJ_pol == TARGET_pol))
df <- anti_join(df, problemos)

df %>% 
  group_by(TARGET) %>% 
  summarise(n = n()) %>% 
  print(n = 200)

save(df, file = '../output/03-complete_corpus/sweep-only.RDS')
save(df, file = '/Volumes/INTENSO/methods_paper/output/02-finalized-corpora/baseline/reddit/corpus_redone.RDS')
load('/Volumes/INTENSO/methods_paper/output/02-finalized-corpora/baseline/reddit/corpus_redone.RDS')
rm(annot)
rm(sentiWords)


tp <- table(dfx$TARGET_mod) %>% sort %>% tail(., 30) %>% as.data.frame() %>% arrange(desc(Freq)) %>% rename(modifierTARGET = Var1)
write.table(tp, '../output/metainfo_wordlists/operation_pony_top30_modifiers_of_target_ADJ_SAMPLE.txt', quote = F, row.names = F, sep = ';')
tp <- table(dfx$ADJ) %>% sort %>% tail(., 30) %>% as.data.frame() %>% arrange(desc(Freq)) %>% rename(ADJ = Var1)
write.table(tp, '../output/metainfo_wordlists/operation_pony_top30_ADJ_SAMPLE.txt', quote = F, row.names = F, sep = ';')

dfx <- left_join(dfx, kw)
dfx <- filter(dfx, !is.na(sentiWords))
means <- dfx %>% group_by(TARGET, CCONJ, cat) %>% 
  summarise(sentiWords = mean(sentiWords, na.rm = T))
means

# annotate the conjuncts
annot <- tokens_lookup(tokens(unique(dfx$TARGET)), dictionary = sentiWords$dichot)
annot <- tibble(TARGET = unique(dfx$TARGET), TARGET_pol = sapply(annot, function(x) x[1]))
neutrals <- kw %>% filter(cat == 'descriptive_concepts') %>% pull(TARGET)
#valAssocs <- kw %>% filter(cat == 'descriptive_concepts') %>% pull(TARGET)
annot <- annot %>% mutate(TARGET_pol = ifelse(TARGET %in% neutrals, 'neutral', TARGET_pol))
dfx <- left_join(dfx, annot)
means <- left_join(means, annot)

# make dumme for modifier
dfx <- dfx %>% mutate(ADV_dummy = ifelse(is.na(ADV), 0, 1),
                      TARGET_pol_mod_dummy = ifelse(is.na(TARGET_mod), 0, 1))

# annotate the modifiers
annot <- tokens_lookup(tokens(unique(dfx$ADV)), dictionary = sentiWords$num)
annot <- tibble(ADV = unique(dfx$ADV), ADV_pol = as.numeric(sapply(annot, function(x) x[1])))
dfx <- left_join(dfx, annot)
annot <- tokens_lookup(tokens(unique(dfx$TARGET_mod)), dictionary = sentiWords$num)
annot <- tibble(TARGET_mod = unique(dfx$TARGET_mod), TARGET_mod_pol = as.numeric(sapply(annot, function(x) x[1])))
dfx <- left_join(dfx, annot)

save(dfx, file = '/Volumes/INTENSO/methods_paper/output/02-finalized-corpora/baseline/reddit/ML_corpus.RDS')

