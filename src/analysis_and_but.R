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
rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

abbrv <- function(x, width = 200) lapply(strwrap(x, width, simplify = FALSE), paste, collapse="\n")
kw <- read.table('../input/dict_and_but.txt', stringsAsFactors = F, sep=',', header = T) %>% rename(TARGET = word)

df <- pblapply(list.files('../output/curated/and_but', full.names = T, pattern = '\\.RDS'), 
             function(x){
               load(x)
               return(corpus2)
             })
df <- do.call(rbind, df) %>% as_tibble
df <- df %>% mutate(TARGET = gsub('.*(\\;|\\(|\\)|\\~|「|\\+|\\%|\\||\\”|\\—)|\\s', '', TARGET),
                    TARGET = removePunctuation(TARGET),
                    TARGET = ifelse(TARGET == "︎friendly", "friendly", TARGET))
df <- df %>% filter(!TARGET%in%c('courageous', 'egoistic'))


load('../res/sentiWords-db.RDS')

annot <- tokens(df$ADJ)
annot <- tokens_lookup(annot, dictionary = sentiWords$num)
#rm(sentiWords)
annot <- sapply(annot, function(x) mean(as.numeric(unlist(x)),  na.rm = T))
df$sentiWords <- annot
rm(annot)

save(df, file = '../output/final_corpus/corpus_redone.RDS')
load('../output/final_corpus/corpus_redone.RDS')

prblm <- c('dishonest', 'dishonesty', 'impermanent', 'louder', 'rudeness', 'rudest', 'stupidity', 'stupidly', 'uncruel', 'unfair', 'unfriendly', 'unfunny', 'honesty', 'crude', 'selfishness', 'unselfish')
nrow(filter(df, TARGET%in%prblm))/nrow(df)
dfx <- df %>% filter(!TARGET%in%prblm)

prblm <- c('too', 'not', 'less')
nrow(filter(df, modifier%in%prblm))/nrow(df)
nrow(filter(df, modifier%in%c('so', 'very', 'really')))/nrow(df)
dfx <- dfx %>% filter(!modifier%in%prblm)
nrow(filter(df, ADV%in%prblm))/nrow(df)
dfx <- dfx %>% filter(!ADV%in%prblm)
prblm <- c('most', 'many', 'more', 'non', 'other', 'last', 'overall', 'much', 'idk', 'holy', 'such')
nrow(filter(df, ADJ%in%prblm))/nrow(df)
dfx <- dfx %>% filter(!ADJ%in%prblm)

df %>% filter(!is.na(CCONJ)) %>% filter(CCONJ%in%c('and', 'but')) %>% nrow/nrow(df)
dfx <- dfx %>% filter(!is.na(CCONJ)|CCONJ%in%c('and', 'but'))

tp <- table(dfx$modifier) %>% sort %>% tail(., 30) %>% as.data.frame() %>% arrange(desc(Freq)) %>% rename(modifierTARGET = Var1)
write.table(tp, '../output/metainfo_wordlists/operation_pony_top30_modifiers_of_target_ADJ_SAMPLE.txt', quote = F, row.names = F, sep = ';')
tp <- table(dfx$ADJ) %>% sort %>% tail(., 30) %>% as.data.frame() %>% arrange(desc(Freq)) %>% rename(ADJ = Var1)
write.table(tp, '../output/metainfo_wordlists/operation_pony_top30_ADJ_SAMPLE.txt', quote = F, row.names = F, sep = ';')

dfx <- left_join(dfx, kw)
means <- dfx %>% group_by(TARGET, CCONJ, cat) %>% 
  summarise(sentiWords = mean(sentiWords, na.rm = T))
dfx <- dfx %>% filter(!(is.na(sentiWords)|is.na(cat)|is.na(CCONJ)))
library(scales)
annot <- tokens_lookup(tokens(unique(dfx$TARGET)), dictionary = sentiWords$dichot)
annot <- tibble(TARGET = unique(dfx$TARGET), TARGET_pol = sapply(annot, function(x) x[1]))
annot <- annot %>% mutate(TARGET_pol = ifelse(TARGET %in% c('loud', 'short', 'permanent', 'narrow'), 'neutral', TARGET_pol),
                          TARGET_pol = ifelse(TARGET %in% c('empty', 'sunny', 'young', 'closed'), 'val.-associated', TARGET_pol))
dfx <- left_join(dfx, annot)
means <- left_join(means, annot)

cols <- hue_pal()(2)
p <- ggplot(dfx, aes(y=sentiWords, x=TARGET, fill=CCONJ)) + 
  geom_hline(aes(yintercept=0), lty='dashed') +
  geom_boxplot(outlier.shape = NA) + 
  geom_point(data = means %>% filter(TARGET_pol == 'negative'), aes(y=sentiWords, x=TARGET, colour=CCONJ)) +
  geom_point(data = means %>% filter(TARGET_pol == 'negative'), aes(y=sentiWords, x=TARGET), shape=1) +
  facet_grid(TARGET_pol~cat, scales = 'free_x', drop = T) +
  scale_color_manual(values = rev(cols)) +
  scale_fill_manual(values = rev(cols)) +
  guides(color = FALSE) +
  labs(
    title = 'Sentiment Distribution of Adjective Conjunctions',
    subtitle = abbrv(paste0('The data consists of ', 
                      format(as.character(length(dfx$TARGET[!is.na(dfx$sentiWords)])), big.mark = "'"),
                      ' TARGET ADJ + CCONJ + ADJ constructions from reddit comments. The data spans over the following time period: ',
                      today()-2,
                      ' to ',
                      today()-2-100,
                      ' (100 days). The boxes represent the quartiles, the whiskers +/-1.5*IQR, the horizontal line the median, and the dots the means.'
                      ), width = 130),
    fill = 'CCONJ',
    y = 'sentiWords Score\nfor lemma#pos:#a (adjectives)',
    caption = 
      abbrv(
        paste0('NUMBER OF NON-UNIQUE ADJ:   ',
               paste0(
                 names(table(dfx$TARGET[!is.na(dfx$sentiWords)])),
                 ': ',
                 format(as.character(table(dfx$TARGET[!is.na(dfx$sentiWords)]), big.mark = "'")),
                 collapse = '; '
               )
        )
      , width = 170)
  ) +
  theme(
    plot.title = element_text(face= 'bold'),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
p
ggsave(p, filename = '../output/plots/overview.png', width = 11, height = 6)

vec <- c('positive', 'negative', 'neutral')
vec_cols <- c(cols[2], cols[1], 'lightgrey')
newcols <- viridis(2, alpha = 0.7)
.labs <- list(
  title = c('Sentiment Distribution: Positive Target Adjectives', 
            'Sentiment Distribution: Negative Target Adjectives',
            'Sentiment Distribution: Neutral Target Adjectives'),
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
  if(i == 3)  plist[[i+1]] <- ggplot(dfx %>% filter(TARGET_pol == vec[i]), aes(y=sentiWords, x=TARGET, fill=CCONJ)) + 
      geom_hline(aes(yintercept=0), lty='dashed') +
      geom_boxplot(outlier.shape = NA) + 
      geom_point(data = means %>% filter(TARGET_pol == vec[i]), aes(y=sentiWords, x=TARGET, colour=CCONJ)) +
      geom_point(data = means %>% filter(TARGET_pol == vec[i]), aes(y=sentiWords, x=TARGET), shape=1) +
      facet_grid(~cat, scales = 'free_x', drop = T) +
      scale_color_manual(values = rev(newcols)) +
      scale_fill_manual(values = rev(newcols)) +
      guides(color = FALSE) +
      theme(
        plot.title = element_text(face= 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = alpha(vec_cols[i], 0.2)),
        legend.position = 'top'
      ) +
      labs(
        fill = 'CCONJ',
        y = 'sentiWords Score\nfor lemma#pos:#a (adjectives)'
      )
  plist[[i]] <- ggplot(dfx %>% filter(TARGET_pol == vec[i]), aes(y=sentiWords, x=TARGET, fill=CCONJ)) + 
    geom_hline(aes(yintercept=0), lty='dashed') +
    geom_boxplot(outlier.shape = NA) + 
    geom_point(data = means %>% filter(TARGET_pol == vec[i]), aes(y=sentiWords, x=TARGET, colour=CCONJ)) +
    geom_point(data = means %>% filter(TARGET_pol == vec[i]), aes(y=sentiWords, x=TARGET), shape=1) +
    facet_grid(~cat, scales = 'free_x', drop = T) +
    scale_color_manual(values = rev(newcols)) +
    scale_fill_manual(values = rev(newcols)) +
    guides(color = FALSE) +
    theme(
      plot.title = element_text(face= 'bold'),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.background = element_rect(fill = alpha(vec_cols[i], 0.2)),
      legend.position = 'right'
    ) +
    labs(x="Target Adjective",  y = 'sentiWords Score\nfor lemma#pos:#a (adjectives)',
         title = .labs[['title']][i],
         caption = .labs[['caption']][i])
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
ggsave(plist[[1]], file='../output/plots/POS_only.png', height = 3.5, width = 10)
ggsave(plist[[2]], file='../output/plots/NEG_only.png', height = 3.5, width = 10)
ggsave(plist[[3]], file='../output/plots/NEUTRAL_only.png', height = 4, width = 6)

#table(df$NOUN[!is.na(df$sentiWords)])

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


#####
library(emmeans)
set.seed(1234)
res.aov <- aov(sentiWords ~ cat*CCONJ, data = dfx[sample(1:nrow(dfx), 5000),])
leveneTest(sentiWords ~ cat*CCONJ, data = dfx)
shapiro.test(x = residuals(res.aov))

annot <- tokens_lookup(tokens(unique(dfx$TARGET)), dictionary = sentiWords$dichot)
annot <- tibble(TARGET = unique(dfx$TARGET), TARGET_pol = sapply(annot, function(x) x[1]))
annot <- annot %>% mutate(TARGET_pol = ifelse(TARGET %in% c('loud', 'short', 'permanent', 'narrow'), 'neutral', TARGET_pol)) 
dfx <- left_join(dfx, annot)
dfx <- dummy_cols(dfx, select_columns = c('cat', 'CCONJ')) 
cor(dfx[,grep('cat_.*|CCONJ$', names(dfx), value = T)] %>% mutate(CCONJ = ifelse(CCONJ == 'and', 0, 1)))
fml <- as.formula(paste0('sentiWords ~', paste0(grep('cat_.*|CCONJ$|comma', names(dfx), value = T), collapse = '+')))
fml <- as.formula('sentiWords ~ cat*CCONJ')
m1 <- lm(fml, data=dfx)
summary(m1)

############################################################################################### 
################### Estimated Means: Interaction of categories with CCONJ #####################
############################################################################################### 

m1 <- aov(abs(sentiWords) ~ cat*CCONJ, data = dfx)
FittedMeans.m1 <- emmeans(m1, ~cat|CCONJ)
FittedMeans.m1 <- summary(FittedMeans.m1)
FittedMeans.m1 <- as.data.frame(FittedMeans.m1)
p <- ggplot(FittedMeans.m1, aes(x=as.numeric(cat), y=emmean)) +
  geom_rect(aes(ymin=lower.CL, ymax=upper.CL, xmax=Inf, xmin=-Inf), fill='red', alpha=.2) +
  geom_point() +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width = 0.2) +
  facet_wrap(~ CCONJ) +
  scale_x_continuous(labels = levels(FittedMeans.m1$cat)) +
  theme(
    axis.text.x = element_text(angle=45, hjust=1),
    plot.title = element_text(face = 'bold')
  ) +
  labs(
    title = 'Estimated Means: Interaction of categories with CCONJ',
    y = 'sentiWords Score',
    x = 'Adj. Categories'
  )
p
ggsave(p, filename = '../output/plots/emmeans_catxCCONJ.png', width = 8, height = 8)

############################################################################################### 
################## Estimated Means: Interaction Target polarity with CCONJ ####################
############################################################################################### 
set.seed(1235)
dfx_sample <- dfx %>% group_by(TARGET) %>% sample_n(5000, replace = T)
m1 <- aov(sentiWords ~ TARGET_pol*CCONJ, data = dfx_sample)
FittedMeans.m1 <- emmeans(m1, ~TARGET_pol|CCONJ)
FittedPairs.m1 <- pairs(FittedMeans.m1, adjust="bon")
summary(m1)
FittedMeans.m1 <- summary(FittedMeans.m1)
FittedMeans.m1 <- as.data.frame(FittedMeans.m1)
p <- ggplot(FittedMeans.m1, aes(x=as.numeric(TARGET_pol), y=emmean)) +
  geom_rect(aes(ymin=lower.CL, ymax=upper.CL, xmax=Inf, xmin=-Inf), fill='red', alpha=.2) +
  geom_point() +
  geom_hline(aes(yintercept = 0), lty='dashed') +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width = 0.2) +
  facet_wrap(~ CCONJ) +
  scale_x_continuous(labels = levels(FittedMeans.m1$TARGET_pol), breaks = c(1,2,3)) +
  theme(
    axis.text.x = element_text(angle=45, hjust=1),
    plot.title = element_text(face = 'bold')
  ) +
  labs(
    title = abbrv('Estimated Means: Interaction Target polarity with CCONJ', width=50),
    y = 'sentiWords Score',
    x = 'Target Polarity'
  )

ggsave(p, filename = '../output/plots/emmeans_TARGET_polxCCONJ.png', width = 5, height = 8)

############################################################################################### 
####### Estimated Means: Interaction Target polarity with CCONJ and category with CCONJ ####### 
############################################################################################### 

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

