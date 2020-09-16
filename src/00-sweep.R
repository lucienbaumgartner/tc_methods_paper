library(dplyr)
library(pbmcapply)
library(httr)
library(lubridate)
library(anytime)
library(stringi)
library(stringr)
library(rlist)
library(Hmisc)
library(spacyr)
rm(list=ls())

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('~/tc_methods_paper/src')
getwd()

search.terms <- read.table('../input/dict_rerun_02_09_20.txt', header = T, stringsAsFactors = F, sep=',')
search.terms <- 
  tibble(word=c(paste0('%22', capitalize(search.terms$word), '%20and%22'), 
                paste0('%22', capitalize(search.terms$word), '%20but%22')),
         cat=rep(search.terms$cat, 2),
         cancel=ifelse(grepl('20and%', word), 0, 1))

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

today <- as.Date('13-09-2020', '%d-%m-%Y')
end <- as.Date('31-08-2020', '%d-%m-%Y')
start <- as.Date('01-01-2018', '%d-%m-%Y')
range_end <- abs(difftime(start, end))
range_lag <- abs(difftime(today, end))


Lurls <- lapply(search.terms$word, function(x){
  paste0(
    'https://api.pushshift.io/reddit/search/comment/?q=',
    x,
    #'&',
    #'subreddit=',
    #subreddit,
    '&',
    'after=',
    (range_end+range_lag+1):(range_lag+1),
    'd&before=',
    (range_end+range_lag):range_lag,
    'd&sort=dsc&size=499'
  )})

names(Lurls) <- search.terms$word

for(m in search.terms$word[-c(1:51)]){
  #m <- search.terms$word[1]
  urls <- Lurls[[m]]
  print(m)
  rowLOG <- T
  its <- 0
  while(rowLOG){
    ITERATION <- 1 + its
    i = try(urls[ITERATION])
    if(ITERATION>length(urls)) break
    #i <- urls[1]
    #print(i)
    #check <- today()-1-as.numeric(stri_extract_first_regex(i, '[0-9]+'))
    #check <- paste0(m, '_', check, '.RDS') %in% list.files('../output/raw/and_but/')
    #if(check) next
    response <- try(httr::GET(i))
    if(!'try-error' %in% class(response)){
      response <- try(httr::content(response))
      if(!'try-error' %in% class(response)&!identical(response, list())){
        df <- try(do.call(rbind, response$data) %>% as_tibble)
        if(!'try-error' %in% class(df)){
          selcols <- list.common(lapply(response$data, names))
          df <- lapply(response$data, function(x) x[selcols])
          df <- do.call(rbind, df) %>% as_tibble
          if(nrow(df)>0){
            df <- select(df, c('id', 'body', 'created_utc'))
            df <- rename(df, txt = body)
            df <- mutate_all(df, .funs = function(x){
              x[sapply(x, is.null)] <- NA
              unlist(x)
            })
            .word <- tolower(stri_extract(regex='(?<=22)(.*)(?=\\%20)',str=i))
            .lookup <- paste0(paste0('(\\w+(\\,)?\\s\\b(and|but)\\b\\s\\b', .word, '\\b', ')|(\\b', .word, '\\b'), '(\\,)?\\s\\b(and|but)\\b\\s\\w+)', collapse = '|')
            corpus <- mclapply(unlist(df$txt), function(x){
              #print(x)
              tmp <- tokenizers::tokenize_sentences(x)
              tmp <- unlist(tmp)
              tmp <- tolower(tmp)
              tmp <- tmp[grepl(.lookup, tolower(tmp), perl = T)]
              tmp <- unname(tmp)
              return(tmp)
            }, mc.cores = 4)
            
            df <- cbind(df[rep(1:nrow(df), lengths(corpus)),], corpus=unlist(corpus)) %>% as_tibble
            rm(corpus)
            df <- mutate(df, corpus = as.character(corpus))
            reg_matches <- mclapply(df$corpus, function(x) stringr::str_extract_all(x, .lookup), mc.cores=4)
            reg_matches <- unlist(reg_matches, recursive=F)
            df <- cbind(df[rep(1:nrow(df), lengths(reg_matches)),], match=unlist(reg_matches)) %>% as_tibble
            df <- mutate(df, match = as.character(match))
            df <- as_tibble(df)
            
            df <- mutate(df, TARGET = strsplit(gsub('\\,', '', match), '\\s'))
            df <- mutate(df, CCONJ = sapply(TARGET, function(x) return(x[x %in% c('and', 'but')][1])))
            df <- mutate(df, TARGET = sapply(df$TARGET, function(x) return(x[x %in% .word][1])))
            df <- mutate(df, comma = grepl('\\,', match))
            
            txtparsed <- spacy_parse(tolower(df$corpus), pos = TRUE)
            txtparsed <- split(txtparsed, txtparsed$doc_id, lex.order = F)
            txtparsed <- txtparsed[gtools::mixedsort(names(txtparsed))]
            txtparsed <- mclapply(txtparsed, function(x) x$lemma %>% setNames(., x$pos), mc.cores = 4)
            
            system.time(txtparsed_adj <- mclapply(1:length(txtparsed), make_regex, mc.cores=4))
            #system.time(txtparsed_adj <- lapply(1:length(txtparsed), make_regex))
            
            #df <- rename(df, comma_check = comma, TARGET_check = TARGET, CCONJ_check = CCONJ)
            reps <- unlist(lapply(sapply(txtparsed_adj, nrow), function(x) ifelse(is.null(x), 0, x)))
            df <- df[rep(1:nrow(df), reps),]
            txtparsed_adj <- do.call(rbind, txtparsed_adj)
            #txtparsed_adj <- mutate(txtparsed_adj, id=df$id)
            #txtparsed_adj <- select(txtparsed_adj, -id)
            df <- rename(df, match_first = match)
            df <- cbind(df, txtparsed_adj)
            df <- as_tibble(df)
            #table(df$TARGET)
            df <- filter(df, TARGET%in%.word)
            out <- paste0('/Volumes/INTENSO/methods_paper/output/00-sweep/', m, '.csv')
            if(!file.exists(out)){
              write.csv(df, file=out, row.names = F)
              rowLOG <- T
            }else{
              df2 <- read.csv(out) %>% as_tibble
              df <- rbind(df2, df)
              df <- filter(df, !duplicated(id))
              write.csv(df, file=out, row.names = F)
              #print(nrow(df))
              rowLOG <- !nrow(df) >= 5000
            }
            its <- ITERATION
          }
        }
      }
    }
  }
}

