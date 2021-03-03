# !diagnostics off

## ---------------------------
##
## Script name: xx-make-corpus.R
##
## Project: Tracing Thick Concepts
##
## Purpose of script: Sentiment annotation and final corpus generation (local)
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
df <- select(df, -id, -created_utc)
df <- mutate(df, context = 'reddit')

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
prblm <- c('too', 'not', 'less')
df <- df %>% filter(!(TARGET_mod%in%prblm|ADV%in%prblm)) # NOTE: df becomes dfx !!!
## filter out adjectives that are not actually adjectives
prblm <- c('most', 'many', 'more', 'non', 'other', 'last', 'overall', 'much', 'idk', 'holy', 'such')
df <- df %>% filter(!ADJ%in%prblm)

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
## For both modifier positions
annot <- tokens_lookup(tokens(unique(df$ADV)), dictionary = sentiWords$num)
annot <- tibble(ADV = unique(df$ADV), ADV_pol = as.numeric(sapply(annot, function(x) x[1])))
df <- left_join(df, annot)
annot <- tokens_lookup(tokens(unique(df$TARGET_mod)), dictionary = sentiWords$num)
annot <- tibble(TARGET_mod = unique(df$TARGET_mod), TARGET_mod_pol = as.numeric(sapply(annot, function(x) x[1])))
df <- left_join(df, annot)

## ---------------------------
#### 6 Additional changes ####
## ---------------------------
## Join keyword data
df <- left_join(df, kw)
## Sanitize CCONJ
df <- df %>% mutate(CCONJ = ifelse(is.na(CCONJ), 'but', CCONJ))
## filter out adjectives with a sentiWords score of 0 or NA
df <- filter(df, !(sentiWords == 0 | is.na(sentiWords)))
# Make dummy for modifier
df <- df %>% mutate(ADV_dummy = ifelse(is.na(ADV), 0, 1),
                    TARGET_pol_mod_dummy = ifelse(is.na(TARGET_mod), 0, 1))

## ---------------------------
########## 7 Specs ###########
## ---------------------------
tp <- table(df$TARGET_mod) %>% sort %>% tail(., 30) %>% as.data.frame() %>% arrange(desc(Freq)) %>% rename(modifierTARGET = Var1)
write.table(tp, '../output/metainfo_wordlists/top30_modifiers_of_target_ADJ.txt', quote = F, row.names = F, sep = ';')
tp <- table(df$ADJ) %>% sort %>% tail(., 30) %>% as.data.frame() %>% arrange(desc(Freq)) %>% rename(ADJ = Var1)
write.table(tp, '../output/metainfo_wordlists/top30_conj_ADJ.txt', quote = F, row.names = F, sep = ';')

## ---------------------------
########### 8 Save ###########
## ---------------------------
save(df, file = '/Volumes/INTENSO/methods_paper/output/02-finalized-corpora/baseline/reddit/ML_corpus.RDS')

