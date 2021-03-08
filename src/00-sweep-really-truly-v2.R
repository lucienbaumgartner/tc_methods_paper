# !diagnostics off
library(dplyr)
library(httr)
library(lubridate)
library(anytime)
library(stringi)
library(stringr)
library(rlist)
library(Hmisc)
library(spacyr)
library(jsonlite)
library(utc)
library(doParallel)
rm(list=ls())

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('~/tc_methods_paper/src')
getwd()

search.terms <- read.table('../input/dict_rerun_02_09_20.txt', header = T, stringsAsFactors = F, sep=',')
search.terms <- search.terms %>% filter(word %in% c('bad', 'honest', 'empty', 'short', 'stupid', 'generous'))

study <- 'study2'

search.terms <- 
  tibble(word=c(paste0('(%22truly%20', capitalize(search.terms$word), '%22)'),
                paste0('(%22really%20', capitalize(search.terms$word), '%22)'),
                paste0(search.terms$word)),
         original = rep(search.terms$word, 3),
         modifier = c(rep('really', nrow(search.terms)), rep('truly', nrow(search.terms)), rep(NA, nrow(search.terms))),
         cat=rep(search.terms$cat, 3))

Lurls <- lapply(search.terms$word, function(x){
  paste0(
    'https://api.pushshift.io/reddit/search/comment/?q=',
    x,
    '&size=1000&before='
  )})

names(Lurls) <- search.terms$word

#cl <- parallel::makeForkCluster(2)
#doParallel::registerDoParallel(cl)

#foreach(i=1:length(search.terms$word)[1:4])
#for(j in 1:4){
#length(search.terms$word)[1:4]
#foreach(i=1:4) 

limit <- 200
log <- gsub('\\.csv', '', list.files(paste0('../output/00-sweep/', study)))
log <- (1:length(search.terms$word))[!search.terms$word %in% log]
for(j in log){
  #m <- search.terms$word[13]
  # extract root of API call
  m <- search.terms$word[j]
  urls <- Lurls[[m]]
  print(m)
  # reset parameters used in the loop
  rowLOG <- T # maintains the outer while loop
  its <- 0 # iteration index
  # time index
  time.index <- toUTC(as.Date('31-08-2020', '%d-%m-%Y')) 
  time.index <- as.numeric(time.index)
  
  # saving process and meta-updates
  out <- paste0('../output/00-sweep/', study, '/',  m, '.csv')
  
  if(file.exists(out)){
    df <- read.csv(out) %>% as_tibble
    its <- 0
  }
  
  while(rowLOG){
    if(its == 0){geturl <- paste0(urls, time.index)}else{
      #df <- read.csv(out) %>% as_tibble
      time.index <- min(df$created_utc)
      }
    #print(time.index)
    geturl <- paste0(urls, time.index)
    
    # actual API call
    TRYING <- T
    while(TRYING){
      response <- try(jsonlite::fromJSON(geturl))
      TRYING <- 'try-error' %in% class(response)
    }
    # coerce to tibble
    df <- as_tibble(response$data)
    
    # if the API call is empty, skip to next url
    if(nrow(df)==0) break
    # test whether all the variables we need are present
    if(!'body'%in%names(df)){
      df <- mutate(df, body = NA)
    }else if(!'id'%in%names(df)){
      df <- mutate(df, id = NA)
    }else if(!'created_utc'%in%names(df)){
      df <- mutate(df, created_utc = NA)
    }
    # mutate df
    df <- rename(df, txt = body)
    df <- select(df, id, txt, created_utc)
    if(grepl('%22(truly|really)', geturl)){
      df <- mutate(df, match = stringr::str_extract_all(tolower(txt), tolower(stringr::str_replace_all(gsub('.*\\(|\\).*', '', geturl), c("%22" = "", "%20" = "\\\\s")))))
    }else{
      df <- mutate(df, match = stringr::str_extract_all(tolower(txt), gsub('.*q\\=|\\&.*', '', geturl)))
    }
    df <- df[lengths(df$match)>0,]
    df <- df %>% rowwise %>% mutate(match = paste0(match, collapse = ';'))
    
    if(!file.exists(out)){
      write.csv(df, file=out, row.names = F)
      rowLOG <- T
    }else{
      # append data
      df2 <- read.csv(out) %>% as_tibble
      df <- rbind(df2, df)
      df <- filter(df, !duplicated(id))
      if(!grepl('%22truly', geturl)){
        check <- read.csv(paste0('../output/00-sweep/study2/(%22truly%20', stringr::str_extract(geturl, 
                              paste0(c('Bad', 'Honest', 'Empty', 'Short', 'Stupid', 'Generous', 
                                       'bad', 'honest', 'empty', 'short', 'stupid', 'generous'),
                                     collapse = '|')), '%22).csv')) %>% as_tibble
        check <- min(check$created_utc)
        rowLOG <- !any(df$created_utc < check)
        df <- filter(df, created_utc > check)
      }else{
        rowLOG <- !nrow(df) >= limit
        if(rowLOG == F){
          df <- df[1:limit,]
        }
      }
      write.csv(df, file=out, row.names = F)
      #print(nrow(df))
    }
    its <- its+1
  }
}
  
  
df <- lapply(
  list.files(paste0('../output/00-sweep/', study), full.names = T),
  function(x) read.csv(x, stringsAsFactors = F, header = T)
)
df <- do.call(rbind, df)
df <- as_tibble(df)
df <- mutate(df, TARGET = stringr::str_extract(match, paste0(c('bad', 'honest', 'empty', 'short', 'stupid', 'generous'), collapse = '|')),
             mod = stringr::str_extract(match, 'really|truly'),
             mod = ifelse(is.na(mod), 'none', mod))
df <- filter(df, !is.na(TARGET))
aggr <- df %>% 
  group_by(TARGET, mod) %>% 
  summarise(n = n())

aggr <- reshape2::dcast(aggr, TARGET ~ mod, value.var = 'n')
aggr <- aggr %>% mutate(fsum = none + really + truly,
                        preally = really/fsum,
                        ptruly = truly/fsum)

dart <- read.csv('../output/00-sweep/study2/short.csv', stringsAsFactors = F) %>% as_tibble
anker <- read.csv('../output/00-sweep/study2/(%22truly%20Short%22).csv', stringsAsFactors = F) %>% as_tibble
min(dart$created_utc)
min(anker$created_utc)
already <- as.numeric(toUTC(as.Date('31-08-2020', '%d-%m-%Y')))-min(dart$created_utc)
haveto <- min(dart$created_utc) - min(anker$created_utc)
already
haveto
haveto/already
