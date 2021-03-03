## ---------------------------
##
## Script name: 00b-sweep.R
##
## Project: Tracing Thick Concepts
##
## Purpose of script: Collect data from raw API responses (local)
##
## Author: Lucien Baumgartner
##
## Date created: 12.11.2021
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
datasets <- list.files('/Volumes/INTENSO/methods_paper/output/00-bulk-data/baseline/reddit/raw_aggr', full.names = T, pattern = '\\%22')
search.terms <- read.table('../input/dict_rerun_02_09_20.txt', header = T, stringsAsFactors = F, sep=',')

## ---------------------------
####### 4 Collect Data #######
## ---------------------------
for(i in datasets){
  #i = datasets[1]
  load(i)
  df <- tibble(txt=dta)
  ## Formulate regex
  .lookup <- paste0(paste0('(\\w+(\\,)?\\s\\b(and|but)\\b\\s\\b', 
                           search.terms$word, '\\b', ')|(\\b', 
                           search.terms$word, '\\b'), 
                    '(\\,)?\\s\\b(and|but)\\b\\s\\w+)', collapse = '|')
  ## Extract regex matches
  corpus <- pbmclapply(df$txt, function(x){
    tmp <- tokenizers::tokenize_sentences(x)
    tmp <- unlist(tmp)
    tmp <- tolower(tmp)
    tmp <- tmp[grepl(.lookup, tolower(tmp), perl = T)]
    tmp <- unname(tmp)
    return(tmp)
  }, mc.cores = 4)
  ## Reduce initial response according to the occurence of target structures (nrow X n matches)
  df <- cbind(df[rep(1:nrow(df), lengths(corpus)),], corpus=unlist(corpus)) %>% as_tibble
  rm(corpus)
  df <- mutate(df, corpus = as.character(corpus))
  ## Extract matches and add them to df
  reg_matches <- pbmclapply(df$corpus, function(x) str_extract_all(x, .lookup), mc.cores=4)
  reg_matches <- unlist(reg_matches, recursive=F)
  df <- cbind(df[rep(1:nrow(df), lengths(reg_matches)),], match=unlist(reg_matches)) %>% as_tibble
  df <- mutate(df, match = as.character(match))
  df <- as_tibble(df)
  ## Save data
  out <- paste0('/Volumes/INTENSO/methods_paper/output/01-reduced-corpora/baseline/reddit/new-', gsub('.*\\/', '', i))
  save(df, file = out, compress = 'gzip')
}
