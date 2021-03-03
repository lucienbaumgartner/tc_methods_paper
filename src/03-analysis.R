library(quanteda)
library(ggplot2)
library(dplyr)
library(pbmcapply)
library(lubridate)
library(viridis)
library(ggwordcloud)
library(car)
library(nortest)
library(fastDummies)
library(pbapply)
library(tm)
library(scales)
library(emmeans)
rm(list=ls())

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
kw <- read.table('../input/dict_rerun_02_09_20.txt', stringsAsFactors = F, sep=',', header = T) %>% rename(TARGET = word)
load('/Volumes/INTENSO/methods_paper/output/02.5-makro/makro.RDS')

#load('/Volumes/INTENSO/methods_paper/output/02-finalized-corpora/baseline/reddit/new-reddit.RDS')
#df <- reddit
#rm(reddit)
load('../res/sentiWords-db.RDS')

annot <- tokens(df$ADJ)
annot <- tokens_lookup(annot, dictionary = sentiWords$num)
#rm(sentiWords)
annot <- sapply(annot, function(x) mean(as.numeric(unlist(x)),  na.rm = T))
df$sentiWords <- annot
rm(annot)

df <- df %>% mutate(CCONJ = ifelse(is.na(CCONJ), 'but', CCONJ))

save(df, file = '/Volumes/INTENSO/methods_paper/output/02-finalized-corpora/baseline/reddit/corpus_redone.RDS')
load('/Volumes/INTENSO/methods_paper/output/02-finalized-corpora/baseline/reddit/corpus_redone.RDS')

############################################################################################### 
#################################### HANDLE DATA ##############################################
###############################################################################################

prblm <- c('too', 'not', 'less')
#nrow(filter(df, modifier%in%prblm))/nrow(df)
#nrow(filter(df, modifier%in%c('so', 'very', 'really')))/nrow(df)
dfx <- df %>% filter(!(TARGET_mod%in%prblm|ADV%in%prblm))
#nrow(filter(df, ADV%in%prblm))/nrow(df)
#dfx <- dfx %>% filter(!ADV%in%prblm)
prblm <- c('most', 'many', 'more', 'non', 'other', 'last', 'overall', 'much', 'idk', 'holy', 'such')
nrow(filter(df, ADJ%in%prblm))/nrow(df)
dfx <- dfx %>% filter(!ADJ%in%prblm)
dfx <- filter(dfx, !sentiWords == 0)

#df %>% filter(!is.na(CCONJ)) %>% filter(CCONJ%in%c('and', 'but')) %>% nrow/nrow(df)
#dfx <- dfx %>% filter(!is.na(CCONJ)|CCONJ%in%c('and', 'but'))

tp <- table(dfx$TARGET_mod) %>% sort %>% tail(., 30) %>% as.data.frame() %>% arrange(desc(Freq)) %>% rename(modifierTARGET = Var1)
write.table(tp, '../output/metainfo_wordlists/operation_pony_top30_modifiers_of_target_ADJ_SAMPLE.txt', quote = F, row.names = F, sep = ';')
tp <- table(dfx$ADJ) %>% sort %>% tail(., 30) %>% as.data.frame() %>% arrange(desc(Freq)) %>% rename(ADJ = Var1)
write.table(tp, '../output/metainfo_wordlists/operation_pony_top30_ADJ_SAMPLE.txt', quote = F, row.names = F, sep = ';')

dfx <- left_join(dfx, kw)
dfx <- filter(dfx, !is.na(sentiWords))
means <- dfx %>% group_by(TARGET, CCONJ, cat) %>% 
  summarise(sentiWords = mean(sentiWords, na.rm = T))
#dfx <- dfx %>% filter(!(is.na(sentiWords)|is.na(cat)|is.na(CCONJ)))

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

table(is.na(dfx$TARGET_mod_pol))/nrow(dfx)
table(is.na(dfx$ADV_pol))/nrow(dfx)

# subsample to look at the effect of the modifiers
mod_subsample <- filter(dfx, !(is.na(ADV)))
mod_subsample <- filter(mod_subsample, !((!is.na(ADV))&is.na(ADV_pol)))
mod_subsample <- filter(mod_subsample, (!ADV_pol == 0))
mod_subsample <- filter(mod_subsample, !is.na(ADV))
mod_subsample <- mod_subsample %>% mutate(
  ADV_pol_mod = sentiWords*(ifelse(ADV_pol>0, 1+ADV_pol, -1-ADV_pol))
)
head(mod_subsample$sentiWords, 20)
(head(mod_subsample$ADV_pol, 20)+1) * head(mod_subsample$sentiWords, 20)
head(mod_subsample$ADV_MOD_FACTOR, 20)
ggplot(mod_subsample %>% filter(CCONJ =='but')) +
  geom_boxplot(aes(x=TARGET, y = ADJ_pol_mod, fill = CCONJ)) +
  facet_grid(~cat, scales = 'free_x')

mean_comp <- mod_subsample %>% filter(CCONJ =='but') %>% group_by(cat) %>% summarise(mean(sentiWords), mean(ADV_pol_mod))
ggplot(mod_subsample %>% filter(CCONJ =='but')) +
  geom_point(aes(x=cat, y = mean(ADJ_pol_mod), fill = CCONJ)) +
  geom_point(aes(x=cat, y = mean(sentiWords), fill = CCONJ))

# draw sample
set.seed(7826)
dfx_sample <- dfx %>% group_by(TARGET, CCONJ) %>% sample_n(1000, replace = T)
dfx_sample <- dfx_sample %>% mutate(GEN = gsub('\\_(NEG|POS)', '', cat))
means_OV <- dfx_sample %>% group_by(GEN, CCONJ) %>% 
  summarise(sentiWords = mean(abs(sentiWords), na.rm = T))

# Medians for Kevin's Cluster-Analysis
medians <- dfx_sample %>% filter(CCONJ == 'and') %>% group_by(cat) %>% summarise(.median=boxplot.stats(sentiWords)$stats[3])
write.csv(medians, file = '../output/metainfo_wordlists/AND_medians_clusterAnalysis_incl_TARGET_pol.csv', quote = F, row.names = F)
medians <- dfx_sample %>% filter(CCONJ == 'and', TARGET_pol=='positive') %>% group_by(GEN) %>% summarise(.median=median(sentiWords))
write.csv(medians, file = '../output/metainfo_wordlists/AND_medians_clusterAnalysis_excl_TARGET_pol.csv', quote = F, row.names = F)
boxplot.stats()

boxplot(dfx_sample$sentiWords[dfx_sample$CCONJ=='and' & dfx_sample$TARGET_pol=='positive' & dfx_sample$GEN=='thick_moral_concepts'])

ggplot(dfx_sample %>% filter(CCONJ == 'but')) +
  geom_boxplot(aes(x = as.factor(first), y = sentiWords, fill=TARGET_pol)) 
  #+geom_point(data = dfx_sample %>% filter(CCONJ == 'but') %>% group_by(TARGET_pol, first) %>% summarise(AVG = mean(sentiWords)), aes(x = as.factor(first), y = AVG, colour = TARGET_pol))

dfx_sample <- dfx_sample %>% mutate(first = as.factor(first))
m1 <- aov(sentiWords ~ TARGET_pol*first, data = dfx_sample %>% filter(CCONJ == 'but'))
summary(m1)

FittedMeans.m1 <- emmeans(m1, ~first|TARGET_pol)
FittedMeans.m1

dfx_sample <- dfx_sample %>% mutate(first = as.factor(first))
m1 <- aov(sentiWords ~ TARGET_pol*TARGET_pol_mod_dummy + ADV_dummy + first*TARGET_pol_mod_dummy, data = dfx_sample %>% filter(CCONJ == 'and'))
summary(m1)

FittedMeans.m1 <- emmeans(m1, ~TARGET_pol_mod_dummy|TARGET_pol)
FittedMeans.m1

m1 <- aov(sentiWords ~ TARGET_pol*TARGET_pol_mod_dummy + ADV_dummy + first*TARGET_pol_mod_dummy, data = dfx_sample %>% filter(CCONJ == 'but'))
summary(m1)

FittedMeans.m1 <- emmeans(m1, ~TARGET_pol_mod_dummy|TARGET_pol)
FittedMeans.m1

############################################################################################### 
################################## OVERVIEW GRAPHIC ###########################################
###############################################################################################

cols <- hue_pal()(2)
vec_cols <- c(cols[2], cols[1], 'lightgrey', 'lightgrey')
newcols <- viridis(2, alpha = 0.7)
p <- ggplot(dfx_sample %>% filter(CCONJ == 'and'), aes(y=abs(sentiWords), x=GEN
                            #, fill =CCONJ
                            )) +
  geom_boxplot() +
  geom_point(data = means_OV %>% filter(CCONJ == 'and'), aes(y=sentiWords, x=GEN
                                  #, colour=CCONJ
                                  )) +
  theme(plot.title = element_text(face = 'bold')) +
  #scale_colour_manual(values = rev(newcols)) +
  #scale_fill_manual(values = rev(newcols)) +
  labs(x = 'Category',
       title = 'Overall Absolute Sentiment Distribution per Category')

p

ggsave(p, filename = '../output/plots/SUBSAMPLE_overview_only_02_09_20.png', width = 11, height = 6)

############################################################################################### 
################################ DISTRIBUTION PER TARGET ######################################
###############################################################################################

vec <- c('positive', 'negative', 'neutral')
.labs <- list(
  title = c('Observed Sentiment Distribution: Positive Target Adjectives', 
            'Observed Sentiment Distribution: Negative Target Adjectives',
            'Observed Sentiment Distribution: Neutral Target Adjectives'),
  caption = c(
      abbrv(
        paste0('NUMBER OF NON-UNIQUE ADJ:   ',
               paste0(
                 names(table(dfx$TARGET[dfx$TARGET_pol == 'positive'])),
                 ': ',
                 format(as.character(table(dfx$TARGET[dfx$TARGET_pol == 'positive']), big.mark = "'")),
                 collapse = '; '
               )
        )
        , width = 170),
      abbrv(
        paste0('NUMBER OF NON-UNIQUE ADJ:   ',
               paste0(
                 names(table(dfx$TARGET[dfx$TARGET_pol == 'negative'])),
                 ': ',
                 format(as.character(table(dfx$TARGET[dfx$TARGET_pol == 'negative']), big.mark = "'")),
                 collapse = '; '
               )
        )
        , width = 170),
      abbrv(
        paste0('NUMBER OF NON-UNIQUE ADJ:   ',
               paste0(
                 names(table(dfx$TARGET[dfx$TARGET_pol == 'neutral'])),
                 ': ',
                 format(as.character(table(dfx$TARGET[dfx$TARGET_pol == 'neutral']), big.mark = "'")),
                 collapse = '; '
               )
        )
        , width = 70)
  )
)
plist <- list()
for(i in 1:3){
  if(i == 3)  plist[[i+1]] <- ggplot(dfx_sample %>% filter(TARGET_pol == vec[i], CCONJ == 'and'), aes(y=sentiWords, x=TARGET, fill=CCONJ)) + 
      geom_hline(aes(yintercept=0), lty='dashed') +
      geom_boxplot(outlier.shape = NA) + 
      geom_point(data = means %>% filter(TARGET_pol == vec[i], CCONJ == 'and'), aes(y=sentiWords, x=TARGET
                                                                    #, colour=CCONJ
                                                                    )) +
      geom_point(data = means %>% filter(TARGET_pol == vec[i], CCONJ == 'and'), aes(y=sentiWords, x=TARGET), shape=1) +
      facet_grid(~cat, scales = 'free_x', drop = T) +
      #scale_color_manual(values = rev(newcols)) +
      #scale_fill_manual(values = rev(newcols)) +
      guides(color = FALSE) +
      theme(
        plot.title = element_text(face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = alpha(vec_cols[i], 0.2)),
        legend.position = 'top'
      ) +
      labs(
        #fill = 'CCONJ',
        y = 'sentiWords Score\nfor lemma#pos:#a (adjectives)'
      )
  plist[[i]] <- ggplot(dfx_sample %>% filter(TARGET_pol == vec[i]), aes(y=sentiWords, x=TARGET
                                                                        #, fill=CCONJ
                                                                        )) + 
    geom_hline(aes(yintercept=0), lty='dashed') +
    geom_boxplot(outlier.shape = NA) + 
    geom_point(data = means %>% filter(TARGET_pol == vec[i], CCONJ == 'and'), aes(y=sentiWords, x=TARGET
                                                                  #, colour=CCONJ
                                                                  )) +
    geom_point(data = means %>% filter(TARGET_pol == vec[i], CCONJ == 'and'), aes(y=sentiWords, x=TARGET), shape=1) +
    facet_grid(~cat, scales = 'free_x', drop = T) +
    scale_color_manual(values = rev(newcols)) +
    scale_fill_manual(values = rev(newcols)) +
    guides(color = FALSE) +
    theme(
      plot.title = element_text(face= 'bold'),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.background = element_rect(fill = alpha(vec_cols[i], 0.2)),
      #legend.position = 'right'
    ) +
    labs(x="Target Adjective",  y = 'sentiWords Score\nfor lemma#pos:#a (adjectives)',
         title = .labs[['title']][i]
        # ,caption = .labs[['caption']][i]
         )
}

g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  legend
}
legend <- g_legend(plist[[4]])
q <- gridExtra::grid.arrange(plist[[1]], plist[[2]], plist[[3]],
                             legend,
                             heights = c(1,2,2,2),
                             layout_matrix = rbind(
                               c(NA,4,4,NA),
                               c(1,1,1,1),
                               c(2,2,2,2),
                               c(NA,3,3,NA)
))
#ggsave(plist[[1]], file='../output/plots/POS_only_02_09_20.png', height = 3.5, width = 10)
#ggsave(plist[[2]], file='../output/plots/NEG_only_02_09_20.png', height = 3.5, width = 10)
#ggsave(plist[[3]], file='../output/plots/VALASSOC_only.png', height = 4, width = 6)
#ggsave(plist[[3]], file='../output/plots/NEUTRAL_only_02_09_20.png', height = 4, width = 6)

ggsave(plist[[1]], file='../output/plots/SUBSAMPLE_POS_only_02_09_20.png', height = 3.5, width = 10)
ggsave(plist[[2]], file='../output/plots/SUBSAMPLE_NEG_only_02_09_20.png', height = 3.5, width = 10)
#ggsave(plist[[3]], file='../output/plots/VALASSOC_only.png', height = 4, width = 6)
ggsave(plist[[3]], file='../output/plots/SUBSAMPLE_NEUTRAL_only_02_09_20.png', height = 4, width = 6)

# imgs Kevin paper only and
ggsave(plist[[1]], file='../output/plots/AND-SUBSAMPLE_POS_only_01_10_20.png', height = 3.5, width = 10)
ggsave(plist[[2]], file='../output/plots/AND-SUBSAMPLE_NEG_only_01_10_20.png', height = 3.5, width = 10)
#ggsave(plist[[3]], file='../output/plots/VALASSOC_only.png', height = 4, width = 6)
ggsave(plist[[3]], file='../output/plots/AND-SUBSAMPLE_NEUTRAL_only_01_10_20.png', height = 4, width = 6)

# data subsets
vecs <- c('insane', 'rude', 'generous', 'funny')
for(i in vecs){
  g <- filter(dfx_sample, TARGET == i, CCONJ == 'but')
  write.csv(g, file = paste0('../output/metainfo_wordlists/', i, '.csv'), row.names = F)
}

############################################################################################### 
################################# DISTRIBUTION PER CAT ########################################
###############################################################################################
means_OV <- dfx_sample %>% group_by(GEN, TARGET_pol, CCONJ) %>% 
  summarise(sentiWords = mean(sentiWords, na.rm = T))
plist <- list()
for(i in 1:3){
  if(i == 3)  plist[[i+1]] <- ggplot(dfx_sample %>% filter(TARGET_pol == vec[i], CCONJ == 'and'), aes(y=sentiWords, x=GEN, fill=CCONJ)) + 
      geom_hline(aes(yintercept=0), lty='dashed') +
      geom_boxplot(outlier.shape = NA) + 
      geom_point(data = means_OV %>% filter(TARGET_pol == vec[i], CCONJ == 'and'), aes(y=sentiWords, x=GEN
                                                                                    #, colour=CCONJ
      )) +
      geom_point(data = means_OV %>% filter(TARGET_pol == vec[i], CCONJ == 'and'), aes(y=sentiWords, x=GEN), shape=1) +
      #scale_color_manual(values = rev(newcols)) +
      #scale_fill_manual(values = rev(newcols)) +
      guides(color = FALSE) +
      theme(
        plot.title = element_text(face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = alpha(vec_cols[i], 0.2)),
        legend.position = 'top'
      ) +
      labs(
        #fill = 'CCONJ',
        y = 'sentiWords Score\nfor lemma#pos:#a (adjectives)'
      )
  plist[[i]] <- ggplot(dfx_sample %>% filter(TARGET_pol == vec[i], CCONJ=='and'), aes(y=sentiWords, x=GEN
                                                                        #, fill=CCONJ
  )) + 
    geom_hline(aes(yintercept=0), lty='dashed') +
    geom_boxplot(outlier.shape = NA) + 
    geom_point(data = means_OV %>% filter(TARGET_pol == vec[i], CCONJ == 'and'), aes(y=sentiWords, x=GEN
                                                                                  #, colour=CCONJ
    )) +
    geom_point(data = means_OV %>% filter(TARGET_pol == vec[i], CCONJ == 'and'), aes(y=sentiWords, x=GEN), shape=1) +
    #facet_grid(~cat, scales = 'free_x', drop = T) +
    scale_color_manual(values = rev(newcols)) +
    scale_fill_manual(values = rev(newcols)) +
    guides(color = FALSE) +
    theme(
      plot.title = element_text(face= 'bold'),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.background = element_rect(fill = alpha(vec_cols[i], 0.2)),
      #legend.position = 'right'
    ) +
    labs(x="Target Adjective Category",  y = 'sentiWords Score\nfor lemma#pos:#a (adjectives)',
         title = abbrv(.labs[['title']][i], width = ifelse(vec[i]=='neutral', 30,50))
         # ,caption = .labs[['caption']][i]
    )
}

ggsave(plist[[1]], file='../output/plots/AND-SUBSAMPLE_CAT_POS_only_09_10_20.png', height = 3.5, width = 6)
ggsave(plist[[2]], file='../output/plots/AND-SUBSAMPLE_CAT_NEG_only_09_10_20.png', height = 3.5, width = 6)
#ggsave(plist[[3]], file='../output/plots/VALASSOC_only.png', height = 4, width = 6)
ggsave(plist[[3]], file='../output/plots/AND-SUBSAMPLE_CAT_NEUTRAL_only_09_10_20.png', height = 4, width = 4)

############################################################################################### 
################################### MEANS PER TARGET ##########################################
###############################################################################################

dfx_sample %>% 
  group_by(GEN, TARGET, CCONJ) %>% 
  summarise(avg = mean(sentiWords), n = n()) %>% 
  print(n=200)

############################################################################################### 
####################################### H1a-d #################################################
###############################################################################################

## normality test
# Ahapiro wilk test
set.seed(1234)
res.aov <- aov(sentiWords ~ TARGET_pol*CCONJ, data = dfx_sample[sample(1:nrow(dfx_sample), 5000),])
shapiro.test(x = residuals(res.aov))
# Anderson-darling test
ad.test(dfx_sample$sentiWords)

## homogenity of variance test
leveneTest(sentiWords ~ TARGET_pol*CCONJ, data = dfx_sample)

## ANOVA
dfx_sample <- mutate(dfx_sample, TARGET_mod_dummy = ifelse(!is.na(TARGET_mod), 1, 0), JOINED_ADJ_mod_dummy = ifelse(!is.na(ADV), 1, 0))
res.aov <- aov(sentiWords ~ TARGET_pol*CCONJ + JOINED_ADJ_mod_dummy + TARGET_mod_dummy + first, data = dfx_sample)
# effect size
summary(res.aov)
FittedMeans.m1 <- emmeans(res.aov, ~TARGET_pol|CCONJ)
FittedMeans.m1
test(FittedMeans.m1, null = 0, side = ">")
test(FittedMeans.m1, null = 0, side = "<")
FittedPairs.m1 <- pairs(FittedMeans.m1, adjust="bon")
FittedPairs.m1

emmip(res.aov, ~TARGET_pol|CCONJ, CIs = T)

means_OV <- dfx_sample %>% group_by(TARGET_pol, CCONJ) %>% summarise(sentiWords = mean(sentiWords, na.rm = T))
p <- ggplot(dfx_sample %>% filter(CCONJ == 'and'), aes(x=TARGET_pol, y=sentiWords
                            ###, fill = CCONJ
                            )) +
  geom_hline(yintercept = 0, lty = 'dashed') +
  geom_boxplot(outlier.shape = NA) +
  #scale_x_continuous(labels = unique(gsub('_(NEG|POS)', '', levels(FittedMeans.m1$cat)))) +
  geom_point(data = means_OV %>% filter(CCONJ == 'and'), aes(y=sentiWords, x=TARGET_pol
                                                             #, colour=CCONJ
                                                             )) +
  geom_point(data = means_OV %>% filter(CCONJ == 'and'), aes(y=sentiWords, x=TARGET_pol), shape=1) +
  scale_color_manual(values = rev(newcols)) +
  scale_fill_manual(values = rev(newcols)) +
  theme(
    axis.text.x = element_text(angle=45, hjust=1),
    plot.title = element_text(face = 'bold')
  ) +
  labs(
    title = abbrv('Observed Sentiment Distribution: Basis for Testing Overall Valence-Effect', width=60),
    y = 'sentiWords Score',
    x = 'Target Polarity'
  )
p

ggsave(p, file='../output/plots/AND-SUBSAMPLE_H1_02_10_20.png', height = 6, width = 6)

############################################################################################### 
################### Estimated Means: Interaction of categories with CCONJ #####################
###############################################################################################
dfx_sample <- mutate(dfx_sample, GEN_X_CCONJ = paste0(GEN, '_', CCONJ))
#cell.means <- matrix(with(dfx_sample, tapply(sentiWords, interaction(GEN, CCONJ), mean)), nrow = 5)
#with(dfx_sample, tapply(sentiWords, interaction(GEN_X_CCONJ, TARGET), mean))
#cell.means
#freqs <- with(dfx_sample, table(GEN, CCONJ))
#freqs
#freqs[1, 1] * cell.means[1, 1] / freqs[1, 1]
m1 <- aov(abs(sentiWords) ~ GEN*CCONJ + JOINED_ADJ_mod_dummy + TARGET_mod_dummy, data = dfx_sample)
summary(m1)
FittedMeans.m1 <- emmeans(m1, ~GEN|CCONJ)
FittedMeans.m1
#FittedPairs.m1 <- contrast(regrid(FittedMeans.m1))
FittedPairs.m1 <- pairs(FittedMeans.m1)
FittedPairs.m1
FittedMeans.m1 <- summary(FittedMeans.m1)
FittedMeans.m1 <- as.data.frame(FittedMeans.m1)
p <- ggplot(FittedMeans.m1, aes(x=as.numeric(GEN), y=emmean)) +
  geom_rect(aes(ymin=lower.CL, ymax=upper.CL, xmax=Inf, xmin=-Inf), fill='red', alpha=.2) +
  geom_point() +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width = 0.2) +
  facet_wrap(~ CCONJ) +
  scale_x_continuous(labels = levels(FittedMeans.m1$GEN)) +
  theme(
    axis.text.x = element_text(angle=45, hjust=1),
    plot.title = element_text(face = 'bold')
  ) +
  labs(
    title = 'Estimated Means: Interaction of categories with CCONJ on Absolute Sentiment',
    y = 'sentiWords Score',
    x = 'Adj. Categories'
  )
p
ggsave(p, filename = '../output/plots/SUBSAMPLE_emmeans_catxCCONJ_02_09_20.png', width = 8, height = 8)
#ggsave(p, filename = '../output/plots/SAMPLE_emmeans_catxCCONJ_02_09_20.png', width = 8, height = 8)

############################################################################################### 
################## Estimated Means: Interaction Target polarity with CCONJ ####################
############################################################################################### 
m1 <- aov(sentiWords ~ cat*CCONJ, data = dfx_sample[!dfx_sample$cat == 'descriptive_concepts',])
FittedMeans.m1 <- emmeans(m1, ~cat|CCONJ)
FittedPairs.m1 <- pairs(FittedMeans.m1, adjust="bon")
FittedMeans.m1 <- summary(FittedMeans.m1)
FittedMeans.m1 <- as.data.frame(FittedMeans.m1)
FittedMeans.m1 <- mutate(FittedMeans.m1, fill = gsub('.*_(NEG|POS)', '\\1', cat))
p <- ggplot(FittedMeans.m1, aes(x=cat, y=emmean)) +
  geom_rect(aes(ymin=lower.CL, ymax=upper.CL, xmax=Inf, xmin=-Inf, fill=CCONJ), alpha=.6) +
  geom_point() +
  geom_hline(aes(yintercept = 0), lty='dashed') +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width = 0.2) +
  facet_wrap(~ fill, scales = 'free_x') +
  #scale_x_continuous(labels = unique(gsub('_(NEG|POS)', '', levels(FittedMeans.m1$cat)))) +
  scale_color_manual(values = rev(newcols)) +
  scale_fill_manual(values = rev(newcols)) +
  theme(
    axis.text.x = element_text(angle=45, hjust=1),
    plot.title = element_text(face = 'bold')
  ) +
  labs(
    title = abbrv('Estimated Means: Interaction Target polarity with CCONJ per Category', width=300),
    y = 'sentiWords Score',
    x = 'Target Polarity'
  )
p

ggsave(p, filename = '../output/plots/SUBSAMPLE_emmeans_TARGET_polxCCONJ_02_09_20.png', width = 8, height = 8)

############################################################################################### 
####### Estimated Means: Interaction Target polarity with CCONJ and category with CCONJ ####### 
############################################################################################### 

set.seed(1235)
dfx_sample <- dfx %>% group_by(TARGET) %>% sample_n(5000, replace = T)
m1 <- aov(sentiWords ~ TARGET_pol*CCONJ + cat*CCONJ, data = dfx_sample)
FittedMeans.m1 <- emmeans(m1, ~TARGET_pol|CCONJ + cat|CCONJ)
FittedPairs.m1 <- pairs(FittedMeans.m1, adjust="bon")
summary(m1)

FittedMeans.m1 <- summary(FittedMeans.m1)
FittedMeans.m1 <- as.data.frame(FittedMeans.m1)
p <- ggplot(FittedMeans.m1, aes(x=as.numeric(cat), y=emmean)) +
  geom_rect(aes(ymin=lower.CL, ymax=upper.CL, xmax=Inf, xmin=-Inf,  fill=TARGET_pol), alpha=.2) +
  geom_point() +
  geom_hline(aes(yintercept = 0), lty='dashed') +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width = 0.2) +
  facet_grid( ~ CCONJ) +
  scale_x_continuous(labels = levels(FittedMeans.m1$cat)) +
  theme(
    axis.text.x = element_text(angle=45, hjust=1),
    plot.title = element_text(face = 'bold')
  ) +
  labs(
    title = 'Estimated Means: Interaction Target polarity with CCONJ and category with CCONJ',
    subtitle = 'Based on pooled random subsamples of 5000 conjoined adjectives per target adjective.',
    y = 'sentiWords Score',
    x = 'Adj. Categories'
  )
p
ggsave(p, filename = '../output/plots/emmeans_TARGET_polxCCONJ__catxCCONJ.png', width = 8, height = 8)

############################################################################################### 
################################## Inquiry: Effect of modifier ################################ 
############################################################################################### 

# most modifiers 
annot <- dfx$modifier
annot <- removePunctuation(unique(annot))
annot <- spacy_parse(annot,pos=F)
annot <- annot$lemma
annot <- tokens(annot)
annot2 <- tokens_lookup(annot, dictionary = sentiWords$num)
annot2 <- sapply(annot2, function(x) mean(as.numeric(unlist(x)),  na.rm = T))
annot <- unlist(annot)
annot <- tibble(id=names(annot), modifier=annot) %>%  left_join(., tibble(id=names(annot2), modSent=annot2)) %>%  select(- id)

dfx <- left_join(dfx, annot)

plot(density(abs(na.omit(dfx$modSent))))

poi <- c('truly', 'genuinely', 'very', 'so', 'insanely', 'really', 'damn', 'real', 'super', 'extremely', 'pretty', 'fucking')
dfx <- dfx %>% mutate(TARGET_intensifier = ifelse(!modifier %in% poi, NA, modifier),
                      TARGET_intensifier = ifelse(is.na(modifier) & is.na(TARGET_intensifier), 0, TARGET_intensifier),
                      TARGET_intensifier = factor(TARGET_intensifier),
                      ABS_sentiWords = abs(sentiWords))

m1 <- aov(sentiWords ~ TARGET_intensifier*TARGET_pol, data = dfx)
FittedMeans.m1 <- emmeans(m1, ~TARGET_intensifier|TARGET_pol)
FittedPairs.m1 <- pairs(FittedMeans.m1, adjust="bon")

FittedMeans.m1 <- summary(FittedMeans.m1)
FittedMeans.m1 <- as.data.frame(FittedMeans.m1)
p <- ggplot(filter(FittedMeans.m1, !TARGET_intensifier == 0), aes(x=as.numeric(TARGET_pol), y=emmean)) +
  geom_rect(aes(ymin=lower.CL, ymax=upper.CL, xmax=Inf, xmin=-Inf, fill=TARGET_intensifier), alpha=.2) +
  geom_hline(data = filter(FittedMeans.m1, TARGET_intensifier == 0) %>% select(emmean), aes(yintercept = emmean), alpha=.2) +
  geom_point(data = filter(FittedMeans.m1, TARGET_intensifier == 0) %>% select(emmean, TARGET_pol), aes(y = emmean), alpha=.2) +
  geom_point() +
  geom_hline(aes(yintercept = 0), lty='dashed') +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL, colour=TARGET_intensifier), width = 0.2) +
  scale_x_continuous(labels = levels(FittedMeans.m1$TARGET_pol), breaks=c(1,2,3,4)) +
  facet_grid(~TARGET_intensifier) +
  theme(
    axis.text.x = element_text(angle=45, hjust=1),
    plot.title = element_text(face = 'bold')
  ) +
  labs(
    title = 'Estimated Means: Interaction of Target modifier with Target Polarity',
    y = 'sentiWords Score',
    x = 'Target polarity'
  )
p

ggsave(p, filename = '../output/plots/emmeans_TARGET_polxTARGET_modifier.png', width = 15, height = 8)

baseline <- table(dfx$modifier) %>% sort %>% tail(100) %>% names
ref <- dfx %>% 
  mutate(modifier_dummy = ifelse(is.na(modifier)|!modifier %in% baseline, 'no modifier present', 'modifier present')) %>% 
  group_by(cat, modifier_dummy) %>% 
  summarise(n=n()) %>% 
  mutate(perc=n/sum(n))

pdata <- dfx %>% 
  mutate(truly = ifelse(modifier=='truly'&!is.na(modifier), 'truly', 'another modifier'),
         truly = ifelse(is.na(modifier), 'no modifier', truly)) %>% 
  group_by(truly, cat) %>% 
  summarise(n=n()) %>% 
  mutate(perc=n/sum(n))

ggplot(pdata %>% filter(truly=='truly'), aes(y=perc, x=cat)) +
  geom_bar(data = filter(ref, modifier_dummy == 'modifier present'), stat = 'identity', alpha = 0.2) +
  geom_bar(aes(fill=truly), stat = 'identity', position = 'dodge', alpha=0.4)

poi <- c('truly', 'genuinely', 'very', 'so', 'insanely', 'really', 'damn', 'real', 'super', 'extremely', 'pretty', 'fucking')
dfx <- dfx %>% mutate(TARGET_intensifier = factor(ifelse(!modifier %in% poi, 0, modifier)), 
                      ABS_sentiWords = abs(sentiWords),
                      ABS_modifier = abs(modSent))

m1 <- lm(sentiWords ~ ABS_modifier*TARGET_pol, data = dfx)

newData <- data.frame(ABS_modifier=rep(seq(0,.998, 0.001), 3), TARGET_pol=rep(unique(dfx$TARGET_pol), each=999))
pred <- predict(m1, newdata = newData, interval = "confidence")
newData <- cbind(newData, pred)
ggplot(newData) +
  #geom_segment(aes(x=ABS_modifier, y=lwr, yend=upr, xend=ABS_modifier, colour=TARGET_pol)) +
  geom_hline(aes(yintercept=0), lty='dashed') +
  geom_line(aes(x=ABS_modifier, y=fit, colour = TARGET_pol)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, x=ABS_modifier, fill=TARGET_pol))

ggplot(dfx, aes(x=modSent, y=sentiWords, colour=TARGET_pol)) +
  geom_smooth() +
  facet_wrap(~CCONJ)


FittedMeans.m1 <- summary(FittedMeans.m1)
FittedMeans.m1 <- as.data.frame(FittedMeans.m1)
p <- ggplot(filter(FittedMeans.m1, !TARGET_intensifier == 0), aes(x=as.numeric(TARGET_pol), y=emmean)) +
  geom_rect(aes(ymin=lower.CL, ymax=upper.CL, xmax=Inf, xmin=-Inf, fill=TARGET_intensifier), alpha=.2) +
  geom_hline(data = filter(FittedMeans.m1, TARGET_intensifier == 0) %>% select(emmean), aes(yintercept = emmean), alpha=.2) +
  geom_point(data = filter(FittedMeans.m1, TARGET_intensifier == 0) %>% select(emmean, TARGET_pol), aes(y = emmean), alpha=.2) +
  geom_point() +
  geom_hline(aes(yintercept = 0), lty='dashed') +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL, colour=TARGET_intensifier), width = 0.2) +
  scale_x_continuous(labels = levels(FittedMeans.m1$TARGET_pol), breaks=c(1,2,3)) +
  facet_grid(~TARGET_intensifier) +
  theme(
    axis.text.x = element_text(angle=45, hjust=1),
    plot.title = element_text(face = 'bold')
  ) +
  labs(
    title = 'Estimated Means: Interaction of Target modifier with Target Polarity',
    y = 'sentiWords Score',
    x = 'Target polarity'
  )
p


dfx <- dfx %>% mutate(TARGET_intensifier = ifelse(!modifier %in% poi, NA, 1),
                      TARGET_intensifier = ifelse(is.na(modifier) & is.na(TARGET_intensifier), 0, TARGET_intensifier),
                      TARGET_intensifier = factor(TARGET_intensifier),
                      ABS_sentiWords = abs(sentiWords))
m1 <- lm(sentiWords ~ cat*CCONJ + TARGET_pol + TARGET_intensifier -1, data = dfx)
summary(m1)

dfx <- dummy_cols(dfx, select_columns = c('TARGET_pol')) 
summary(lm(TARGET_pol_neutral ~ TARGET_pol_negative,data=dfx))



.add <- strsplit(as.character(FittedPairs.m1$contrast), '-') %>% 
  lapply(., function(x) gsub('.*\\,|\\s', '', x)) %>% 
  do.call(rbind, .) %>% as.data.frame %>% 
  setNames(., c('CCONJ1', 'CCONJ2'))
FittedPairs.m1 <- cbind(FittedPairs.m1, .add)
FittedPairs.m1 <- FittedPairs.m1 %>% filter(CCONJ1 == CCONJ2)
FittedPairs.m1 <- FittedPairs.m1 %>% 
  mutate(contrast = gsub('\\,(and|but)?', '', contrast)) %>% 
  select(contrast, CCONJ1, estimate, SE, df, t.ratio, p.value) %>% 
  rename(CCONJ = CCONJ1)

newData <- expand.grid(cat=unique(dfx$cat), CCONJ=unique(dfx$CCONJ), TARGET_pol=unique(dfx$TARGET_pol))
newData <- newData %>% filter(!((TARGET_pol=='neutral'&!cat=='desc')|(!TARGET_pol=='neutral'&cat=='desc')))
pred <- predict(m1, newdata = newData, interval = "confidence")
newData <- cbind(newData, pred)
ggplot(newData) +
  geom_segment(aes(x=TARGET_pol, y=lwr, yend=upr, xend=TARGET_pol, colour=CCONJ)) +
  geom_hline(aes(yintercept=0), lty='dashed') +
  geom_point(aes(x=TARGET_pol, y=fit, colour=CCONJ)) +
  geom_point(aes(x=TARGET_pol, y=fit), shape=1) +
  scale_colour_manual(values=rev(newcols)) +
  facet_wrap(~cat, nrow=1, scales = 'free_x')

dfx <- dfx %>% rowwise() %>% mutate(CatPolCCONJ=paste0(cat, ':', TARGET_pol,':', CCONJ),
                                    PolCCONJ=paste0(TARGET_pol,':', CCONJ)) %>% ungroup
pairwise.wilcox.test(dfx$sentiWords, dfx$PolCCONJ, p.adjust.method = "BH")
p <- pairwise.wilcox.test(dfx$sentiWords, dfx$CatPolCCONJ, p.adjust.method = "BH")

pvalr <- function(pvals, sig.limit = .05, digits = 3, html = FALSE) {
  
  roundr <- function(x, digits = 1) {
    res <- sprintf(paste0('%.', digits, 'f'), x)
    zzz <- paste0('0.', paste(rep('0', digits), collapse = ''))
    res[res == paste0('-', zzz)] <- zzz
    res
  }
  
  sapply(pvals, function(x, sig.limit) {
    if (x < sig.limit)
      if (html)
        return(sprintf('&lt; %s', format(sig.limit))) else
          return(sprintf('< %s', format(sig.limit)))
    if (x > .1)
      return(roundr(x, digits = 2)) else
        return(roundr(x, digits = digits))
  }, sig.limit = sig.limit)
}

ps <- t(p$p.value)

lala <- lapply((ncol(ps)-4):ncol(ps), function(y) sapply(ps[,y], function(x) if(!is.na(x)) pvalr(x, digits = 2) else x))
lala <- do.call(cbind, lala)
colnames(lala) <- colnames(t(p$p.value))[(ncol(ps)-4):ncol(ps)]
cat(lala)
pvalr(.00003, digits=3)

set.seed(123)
for(i in unique(dfx$TARGET)){
  print(i)
  q <- dfx[dfx$TARGET == i & dfx$modifier %in% poi,]
  q <- q[sample(1:nrow(q), ifelse(nrow(q) < 50, nrow(q), 50)),]
  q <- dplyr::select(q, TARGET, CCONJ, ADJ, modifier, corpus)
  write.table(q, file = paste0('../output/final_corpus/and_but/metainfo_wordlists/', i, '_w_modifier.txt'), sep = ';')
}


############################################################################################### 
#################################### WORDCLOUDS ###############################################
###############################################################################################


dfx2 <- dfx %>% group_by(TARGET, CCONJ, ADJ, cat, sentiWords) %>% 
  summarise(n=n()) %>% group_by(TARGET, CCONJ) %>% top_n(20,wt=n) %>%
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40)))

for(i in unique(dfx$cat)){
  p1 <- ggplot(filter(dfx2, cat==i & CCONJ=='and' & !is.na(sentiWords)), aes(label=ADJ)) + 
    geom_text_wordcloud_area(aes(size=n, colour = sentiWords), 
                             eccentricity = 1, show.legend = TRUE, 
                             family="Roboto Bold") +
    facet_grid(CCONJ~TARGET, scales = 'free') +
    scale_size(range=c(4,7)) +
    scale_colour_gradient2(
      low = 'red',
      mid = 'white',
      high = 'green',
      midpoint = 0,
      limits= c(-1,1),
      na.value = "grey50",
      guide = "colourbar",
    ) +
    guides(size = FALSE) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill='darkgrey'),
          strip.text = element_text(size = 12))
  
  p2 <- ggplot(filter(dfx2, cat==i & CCONJ=='but' & !is.na(sentiWords)), aes(label=ADJ)) + 
    geom_text_wordcloud_area(aes(size=n, colour = sentiWords), 
                             eccentricity = 1, show.legend = TRUE, 
                             family="Roboto Bold") +
    facet_grid(CCONJ~TARGET, scales = 'free') +
    scale_size(range=c(4,7)) +
    scale_colour_gradient2(
      low = 'red',
      mid = 'white',
      high = 'green',
      midpoint = 0,
      limits= c(-1,1),
      na.value = "grey50",
      guide = "colourbar",
    ) +
    guides(size = FALSE) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill='darkgrey'),
          strip.text = element_text(size = 12))
  
  p <- gridExtra::arrangeGrob(p1,p2, nrow=2)
  
  ggsave(p, filename = paste0('../output/plots/clouds', i,'.png'), height=9, width = 15)
}
