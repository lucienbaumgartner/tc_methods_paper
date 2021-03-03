## ---------------------------
##
## Script name: 00a-reddit-API.R
##
## Project: Tracing Thick Concepts
##
## Purpose of script: Handler for API calls
##
## Author: Lucien Baumgartner
##
## Date created: 30.06.2020
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
library(pbmcapply)
library(httr)
library(lubridate)
library(anytime)
library(stringi)
library(rlist)
library(Hmisc)
rm(list=ls())

## ---------------------------
## 2 Set working directory ###
## ---------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

## ---------------------------
######## 3 Load data #########
## ---------------------------
search.terms <- read.table('../input/dict_rerun_02_09_20.txt', header = T, stringsAsFactors = F, sep=',')
search.terms <- 
  tibble(word=c(paste0('%22', capitalize(search.terms$word), '%20and%22'), 
                paste0('%22', capitalize(search.terms$word), '%20but%22')),
         cat=rep(search.terms$cat, 2),
         cancel=ifelse(grepl('20and%', word), 0, 1))

## ---------------------------
##### 4 Formulate Calls ######
## ---------------------------
## Set date range
today <- as.Date('02-09-2020', '%d-%m-%Y')
end <- as.Date('31-08-2020', '%d-%m-%Y')
start <- as.Date('31-12-2020', '%d-%m-%Y')
range_end <- difftime(start, end)
range_lag <- difftime(today, end)

## ---------------------------
## URL wrapper
Lurls <- lapply(search.terms$word, function(x){
  paste0(
    'https://api.pushshift.io/reddit/search/comment/?q=',
    x,
    '&',
    'after=',
    (range_end+range_lag+1):(range_lag+1),
    'd&before=',
    (range_end+range_lag):range_lag,
    'd&sort=dsc&size=1000'
  )})
## name URL list
names(Lurls) <- search.terms$word

## ---------------------------
## API calls
for(m in search.terms$word){
  #m <- search.terms$word[1]
  urls <- Lurls[[m]] # extract set uof urls
  print(m)
  for(i in urls){ # loop over set of urls
    #i <- urls[1]
    #print(i)
    response <- try(GET(i)) # get response
    if(!'try-error' %in% class(response)){ # extract response
      response <- try(content(response))
      if(!'try-error' %in% class(response)&!identical(response, list())){
        df <- try(do.call(rbind, response$data) %>% as_tibble) # easy binder
        if('try-error' %in% class(df)){
          selcols <- list.common(lapply(response$data, names))
          df <- lapply(response$data, function(x) x[selcols]) # conditional binder based on the most common columns in the response
          df <- do.call(rbind, df) %>% as_tibble
        }
        utc <- stri_extract(i, regex = '(?<=before\\=)([0-9]+)') # date of the call as defined in the URL
        if(nrow(df)>0){
          out <- paste0('/Volumes/INTENSO/methods_paper/output/00-bulk-data/baseline/reddit/raw/', m, '_', utc, '.RDS')
          save(df, file = out)
        }
      }
    }
  }
}

## ---------------------------
####### 5 Compile Data #######
## ---------------------------
file_paths <- list.files('/Volumes/INTENSO/methods_paper/output/00-bulk-data/baseline/reddit/raw/', full.names = T)
df <- pbmclapply(file_paths, function(x){
  load(x)
  return(df$body)
}, mc.cores = 4)
names(df) <- gsub('\\_.*','', list.files('/Volumes/INTENSO/methods_paper/output/00-bulk-data/baseline/reddit/raw/'))
for(i in unique(names(df))){
  print(i)
  dta <- unlist(df[grepl(i, names(df))])
  if(!is.null(dta)){
    names(dta) <- i
    save(dta, file = paste0('/Volumes/INTENSO/methods_paper/output/00-bulk-data/baseline/reddit/raw_aggr/', i, '.RDS'))
  }
}

