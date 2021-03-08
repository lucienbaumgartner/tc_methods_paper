## ---------------------------
##
## Script name: 05-study-1.R
##
## Project: Tracing Thick Concepts
##
## Purpose of script: Study 1
##
## Author: Lucien Baumgartner
##
## Date created: 25.02.2021
##
## Email: lucienbaumgartner@philos.uzh.ch
##
## ---------------------------
##
## Notes:
##    If not otherwise noted,
##    we use ANOVA models
##    followed by estimated
##    marginal means with 
##    pairwise contrasts
##
## ---------------------------


## ---------------------------
######## 1 Libraries #########
## ---------------------------
library(dplyr)
library(emmeans)
library(nnet)
library(viridis)
library(Ckmeans.1d.dp) # univariate k-means
library(factoextra)
library(quanteda) # remove again later
library(MNLpred)
library(purrr)
rm(list =ls())


## ---------------------------
## 2 Set working directory ###
## ---------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

## ---------------------------
########## 3 UDFs ############
## ---------------------------
abbrv <- function(x, width = 200) lapply(strwrap(x, width, simplify = FALSE), paste, collapse="\n")

## ---------------------------
######## 4 Load data #########
## ---------------------------
#load('../output/03-corpus/ML_corpus.RDS')
load('../output/03-complete_corpus/sweep-only.RDS')
dfx <- df

dfx %>% 
  group_by(TARGET) %>% 
  summarise(n=n()) %>% 
  pull(n)

## ---------------------------
######### 5 Models  ##########
## ---------------------------

## ---------------------------
## m1: sentiment ~ polarity (pos/neg/neutral)
nrow(dfx)
m1 <- aov(sentiWords ~ TARGET_pol, data = dfx)
FittedMeans.m1 <- emmeans(m1, ~TARGET_pol)
FittedPairs.m1 <- pairs(FittedMeans.m1, adjust="bon")
FittedMeans.m1
summary(FittedPairs.m1)
## plot
annot <- as.data.frame(summary(FittedMeans.m1))

p <- ggplot(dfx, aes(x=TARGET_pol, y=sentiWords)) +
  geom_boxplot(outlier.alpha = 0) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  geom_point(data = annot, aes(x=TARGET_pol, y=emmean)) +
  geom_line(data = annot, aes(x=TARGET_pol, y=emmean, group = 1)) +
  labs(
    y = "sentiWords Score of\nConjoined Adjectives",
    x = "Target Adjective Polarity"
  ) +
  theme_bw()
  #*geom_errorbar(data = annot, aes(x=TARGET_pol, y=emmean, ymin = lower.CL, ymax = upper.CL))
p
ggsave('../output/paper/Figure1_new.pdf', width = 6, height = 4) # rename figure 1

## ---------------------------
## m2: abs(sentiment) ~ concept classes (NTC/NVAC/NThin/DC/PThin/PVAC/PTC) [implicitly: polarity]
# create new concept class vector
dfx$cat2 <- recode(gsub('_(non-)?moral', '',dfx$cat),
                   'value-associated_concepts_NEG' = 'NVAC',
                   'value-associated_concepts_POS' = 'PVAC',
                   descriptive_concepts = 'DC',
                   thick_concepts_NEG = 'NTC',
                   thick_concepts_POS = 'PTC',
                   thin_concepts_POS = 'PThin',
                   thin_concepts_NEG = 'NThin'
)
dfx$cat2 <- as.factor(dfx$cat2)
unique(dfx$cat2)
levels(dfx$cat2)
m2 <- aov(abs(sentiWords) ~ cat2, data = dfx)
FittedMeans.m2 <- emmeans(m2, ~ cat2)
FittedPairs.m2 <- pairs(FittedMeans.m2, adjust="bon")
summary(FittedPairs.m2)

## ---------------------------
## m3: sentiment ~ non-descriptive concept classes (TC/Thin/VAC) * polarity
dfx$cat3 <- gsub('^(N|P)', '', dfx$cat2)
unique(dfx$cat3)
m3 <- aov(sentiWords ~ cat3*TARGET_pol, data = dfx[!dfx$cat3 == 'DC',])
FittedMeans.m3 <- emmeans(m3, ~ cat3|TARGET_pol)
FittedPairs.m3 <- pairs(FittedMeans.m3, adjust="bon")
FittedMeans.m3
summary(FittedPairs.m3)

## ---------------------------
## m4: sentiment ~ non-descriptive concept classes (moralTC/non-moralTC/Thin/VAC) * polarity
dfx$cat4 <- gsub('_(NEG|POS)', '', dfx$cat)
unique(dfx$cat4)
m4 <- aov(sentiWords ~ cat4*TARGET_pol, data = dfx[!dfx$cat3 == 'DC',])
FittedMeans.m4 <- emmeans(m4, ~ cat4|TARGET_pol)
FittedPairs.m4 <- pairs(FittedMeans.m4, adjust="bon")
FittedMeans.m4
summary(FittedPairs.m4)

## ---------------------------
## m5: sentiment ~ non-descriptive concept classes (VAC/evaluative) * polarity
dfx$eval <- ifelse(dfx$cat3 %in% c('VAC', 'DC'), 'non-eval', 'eval') 
unique(dfx$eval)
m5 <- aov(sentiWords ~ eval*TARGET_pol, data = dfx[!dfx$cat3 == 'DC',])
FittedMeans.m5 <- emmeans(m5, ~ eval|TARGET_pol)
FittedPairs.m5 <- pairs(FittedMeans.m5, adjust="bon")
FittedMeans.m5
summary(FittedPairs.m5)

## ---------------------------
## m6: multinomial logit model to estimate concept class membership probabilities based on sentiment
dfx <- as_tibble(dfx)
dfx$cat2 <- relevel(factor(dfx$cat2), ref = "DC")
m6 <- nnet::multinom(cat2 ~ sentiWords, Hess = T, data = dfx)
summary(m6)
preds <- MNLpred::mnl_pred_ova(
  model = m6, 
  data = dfx,
  x = 'sentiWords',
  by = 0.1,
  seed = 176,
  nsim = 10,
  probs = c(0.025, 0.975)
)

preds$plotdata <- preds$plotdata %>% mutate(cat2 = factor(cat2, levels=unique(preds$plotdata$cat2)[c(2,3,4,1,7,6,5)]))
p <- ggplot(preds$plotdata, aes(x=sentiWords, y=mean, ymin = lower, ymax = upper)) +
  geom_ribbon(aes(group = cat2), fill = 'grey') +
  geom_line(aes(lty = cat2), lwd=0.2) +
  theme_bw() +
  theme(
    plot.title = element_text(face = 'bold')
  ) +
  scale_y_continuous(limits = c(0,.75), labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = abbrv('Predicted Probabilities for Class Membership based on sentiWords', width = 50),
    x = 'Eval',
    y = 'Probability'
  )
p

## ---------------------------
##### 6 Cluster Analysis  ####
## ---------------------------
set.seed(1234)
aggr <- dfx %>% 
  mutate(
    cat2 = factor(cat2, levels = c('NTC', 'NVAC', 'NThin', 'DC', 'PThin', 'PVAC', 'PTC'))
  ) %>% 
  group_by(cat2, TARGET) %>% 
  summarise(mean = mean(sentiWords, na.rm = T))
clusdata <- data.frame(sentiScore = aggr[, 'mean'])
rownames(clusdata) <- aggr$TARGET
res2 <- eclust(clusdata, FUNcluster = 'hclust', hc_metric = "euclidean", hc_method = "ward.D2", stand = F, k = 7,
               gap_maxSE = list(d.power = 2, method = 'firstSEmax', spaceH0 = 'scaledPCA', B = 2))
res2 <- data.frame(res2 = res2$cluster, TARGET = names(res2$cluster))

aggr <- left_join(aggr, res2)
p <- t(t(table(aggr$cat2, aggr$res2))/colSums(table(aggr$cat2, aggr$res2))*100)
p[p == 0] <- NA
xtable::xtable(p)

## sentiScore based clustering
dat <- read.csv('../input/study2_results.csv', header = T, stringsAsFactors = F)
dat <- rename(dat, sentiScore = 'Sentiment.Value.from.SentiWords')
dat <- mutate(dat,
              Type2 = case_when(
                Type == 'Value-Associated' & sentiScore < 0 ~ 'NVAC',
                Type == 'Value-Associated' & sentiScore > 0 ~ 'PVAC',
                Type == 'Thin' & sentiScore < 0 ~ 'NThin',
                Type == 'Thin' & sentiScore > 0 ~ 'PThin',
                Type == 'Descriptive' ~ 'DC',
                Type %in% c('Thick Moral', 'Thick and Thin Non-Moral') & sentiScore < 0 ~ 'NTC',
                Type %in% c('Thick Moral', 'Thick and Thin Non-Moral') & sentiScore > 0 ~ 'PTC'
              ),
              Type2 = factor(Type2, levels = c('NTC', 'NVAC', 'NThin', 'DC', 'PThin', 'PVAC', 'PTC'))
)
nonaggr <- data.frame(sentiScore = dat[, 'sentiScore'])
rownames(nonaggr) <- dat$Adjective

res <- eclust(clusdata, FUNcluster = 'kmeans', nstart = 25, seed = 1234, k = 7)
res <- data.frame(res = res$cluster, Adjective = names(res$cluster))
nonaggr <- left_join(dat, res)

t(t(table(nonaggr$Type2, nonaggr$res))/colSums(table(nonaggr$Type2, nonaggr$res))*100)

## ---------------------------
## 6 Sentiment distributions #
## ---------------------------
## Sentiment distributions overall
p <- c(0.2, 0.5, 0.8)
p_names <- map_chr(p, ~paste0(.x*100, "%"))

p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)

distrib <- dfx %>% 
  group_by(TARGET, cat2) %>% 
  summarize_at(vars(sentiWords), funs(!!!p_funs, mean)) %>% 
  arrange(cat2, TARGET)
print(xtable::xtable(distrib, digits =4), include.rownames =F)
nrow(distrib)

filter(distrib, m)

p <- dfx %>% 
  filter(TARGET == 'bad' & sentiWords > 0.5) 
head(p, 200) %>% 
  pull(corpus)
table(p$ADJ)
table(p$TARGET_mod)
p <- filter(p, ADJ == 'good')
table(p$ADV) %>%  sort
head(p$corpus, 200)

p %>% 
  rowwise %>% 
  mutate(
    prblm = case_when(
      first == 1 ~ paste0(paste('(both|(at the same time))', switch(!(is.na(TARGET_mod)+1),TARGET_mod,NULL), TARGET, switch(!(is.na(comma)+1), comma, NULL), CCONJ, switch(!(is.na(ADV)+1),ADV,NULL), ADJ, collapse = ' '), '(at the same time)?'),
      first == 0 ~ paste0(paste('(both|(at the same time))', switch(!(is.na(ADV)+1),ADV,NULL), ADJ, switch(!(is.na(comma)+1), comma, NULL), CCONJ, switch(!(is.na(TARGET_mod)+1),TARGET_mod,NULL), TARGET, collapse = ' '), '(at the same time)?')
    ),
    prblm = gsub('\\s+', '\\\\\\s', prblm),
    ADJ_pol = ifelse(sentiWords > 0, 'positive', 'negative')
  ) %>% 
  #pull(prblm) 
  filter(grepl(prblm, corpus) & !(ADJ_pol == TARGET_pol))

head(p)$corpus
switch((!is.na(1))+1,NA,NULL)
