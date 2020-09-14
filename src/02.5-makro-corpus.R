library(dplyr)

plist <- list()
for(i in list.files('/Volumes/INTENSO/methods_paper/output/00-sweep/', full.names = T)){
  plist[[i]] <- read.csv(i, stringsAsFactors = F)
}

df <- do.call(rbind, plist)
df <- select(df, -id, -created_utc)
df <- mutate(df, context = 'reddit')
load('/Volumes/INTENSO/methods_paper/output/02-finalized-corpora/baseline/reddit/new-reddit.RDS')
names(reddit)
names(df)

df <- rbind(df, reddit) %>% as_tibble
save(df, file = '/Volumes/INTENSO/methods_paper/output/02.5-makro/makro.RDS', compress = 'gzip')
