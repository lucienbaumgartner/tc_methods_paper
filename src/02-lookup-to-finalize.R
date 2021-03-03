# !diagnostics off

## ---------------------------
##
## Script name: 02-lookup-to-finalize.R
##
## Project: Tracing Thick Concepts
##
## Purpose of script: Extract lexical and syntactic matches and create single corpus (local)
##
## Author: Lucien Baumgartner
##
## Date created: 300.06.2020
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
library(stringr)
library(spacyr)
library(gtools)
library(tokenizers)
library(pbmcapply)
library(stringr)
rm(list=ls())

## ---------------------------
## 2 Set working directory ###
## ---------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#setwd('~/tc_methods_paper/src')
getwd()

## ---------------------------
######## 3 Load data #########
## ---------------------------
datasets <- list.files('/Volumes/INTENSO/methods_paper/output/01-reduced-corpora/baseline/reddit', full.names = T, pattern = 'new')
search.terms <- read.table('../input/dict_rerun_02_09_20.txt', header = T, stringsAsFactors = F, sep=',')

## ---------------------------
########### 4 UDFs ###########
## ---------------------------
## formulate the regex to extract specific regex matches from responses
## according to PoS-tags
syntax.regex <- '(ADV\\s)?ADJ\\s(PUNCT\\s)?CCONJ\\s(ADV\\s)?ADJ'
make_regex <- function(INDEX){
  TARGET = paste0('\\\\b',df$TARGET[INDEX], '\\\\b')
  LEMMA = paste0(txtparsed[[INDEX]], collapse = ' ')
  SYNTAX = paste0(names(txtparsed[[INDEX]]), collapse = ' ')
  SYNTAX <- unlist(str_extract(SYNTAX, syntax.regex))
  SYNTAX <- unique(SYNTAX)
  tmp <- str_replace_all(SYNTAX, c(
    'ADV' = '\\\\w+',
    'CCONJ' = '(and|but)',
    'PUNCT' = '\\\\,'
  ))
  regex1 <- sub('ADJ', TARGET, tmp)
  regex1 <- str_replace_all(regex1, c(
    ' ' = '\\\\s',
    'ADJ' = '\\\\w+'
  ))
  regex2 <- stri_replace_last(tmp, replacement = TARGET, regex = 'ADJ')
  regex2 <- str_replace_all(regex2, c(
    ' ' = '\\\\s',
    'ADJ' = '\\\\w+'
  ))
  match1 <- unlist(str_extract_all(LEMMA, regex1))
  match2 <- unlist(str_extract_all(LEMMA, regex2))
  MATCH <- unlist(c(match1, match2))
  tryCatch(tmp <- lapply(MATCH, function(y){
    POS <- spacy_parse(y, pos = T)
    if(paste0(POS$pos, collapse = ' ') %in% SYNTAX){
      ADJ <- filter(POS, pos == 'ADJ' & !lemma == df$TARGET[INDEX])$lemma
      TARGET_mod <- filter(POS, (pos == 'ADJ' & lemma == df$TARGET[INDEX]) | pos == 'ADV')
      TARGET_mod <- TARGET_mod$lemma[TARGET_mod$pos == 'ADV' & TARGET_mod$token_id == TARGET_mod$token_id[TARGET_mod$pos == 'ADJ'] - 1]
      TARGET_mod <- ifelse(identical(TARGET_mod, character(0)), NA, TARGET_mod)
      
      ADV <- filter(POS, (pos == 'ADJ' & !lemma == df$TARGET[INDEX]) | pos == 'ADV')
      ADV <- ADV$lemma[ADV$pos == 'ADV' & ADV$token_id == ADV$token_id[ADV$pos == 'ADJ'] - 1]
      ADV <- ifelse(identical(ADV, character(0)), NA, ADV)
      
      first <- filter(POS, pos == 'ADJ')
      first <- ifelse(first$token_id[first$lemma == df$TARGET[INDEX]] < first$token_id[!first$lemma == df$TARGET[INDEX]], 1, 0)
      dta <- tibble(match = y, ADJ, TARGET_mod, ADV, first)
      return(dta)
    }
  }), warning = function(e) print(INDEX))
  if(is.list(tmp)) tmp <- do.call(rbind, tmp)
  if(is.list(tmp)|is.null(tmp)) return(tmp)
}

## ---------------------------
###### 4 Annotate Data #######
## ---------------------------
for(i in datasets){
  #i=datasets[1]
  load(i)
  print(i)
  ## Annotate lexical data
  df <- mutate(df, TARGET = strsplit(gsub('\\,', '', match), '\\s'))
  df <- mutate(df, CCONJ = sapply(TARGET, function(x) return(x[x %in% c('and', 'but')][1])))
  df <- mutate(df, TARGET = sapply(TARGET, function(x) return(x[x %in% search.terms$word][1])))
  df <- mutate(df, comma = grepl('\\,', match))
  ## Extract matches
  txtparsed <- spacy_parse(tolower(df$corpus), pos = TRUE)
  txtparsed <- split(txtparsed, txtparsed$doc_id, lex.order = F)
  txtparsed <- txtparsed[mixedsort(names(txtparsed))]
  txtparsed <- pbmclapply(txtparsed, function(x) x$lemma %>% setNames(., x$pos), mc.cores = 4)
  txtparsed_adj <- pbmclapply(1:length(txtparsed), make_regex, mc.cores=4)
  ## Reduce initial response according to the occurence of target structures (nrow X n matches)
  reps <- unlist(lapply(sapply(txtparsed_adj, nrow), function(x) ifelse(is.null(x), 0, x)))
  df <- df[rep(1:nrow(df), reps),]
  txtparsed_adj <- do.call(rbind, txtparsed_adj)
  df <- rename(df, match_first = match)
  df <- cbind(df, txtparsed_adj)
  df <- as_tibble(df)
  df <- filter(df, TARGET%in%search.terms$word)
  ## Save data
  out <- paste0('/Volumes/INTENSO/methods_paper/output/02-finalized-corpora/baseline/reddit/', gsub('.*\\/', '', i))
  save(df, file = out)
}

## ---------------------------
#### 5 Generate One Corpus ###
## ---------------------------
## Get filepaths
fileslist <- list.files('/Volumes/INTENSO/methods_paper/output/02-finalized-corpora/baseline/reddit/', full.names = T, pattern = 'new\\-')
reddit <- pbmclapply(fileslist, function(x){
  load(x)
  return(df)
}, mc.cores = 4)
## Collect data
reddit <- do.call(rbind, reddit)
reddit <- as_tibble(reddit)
reddit <- mutate(reddit, context = 'reddit')
## Save data
save(reddit, file = '/Volumes/INTENSO/methods_paper/output/02-finalized-corpora/baseline/reddit/new-reddit.RDS', compress = T)
