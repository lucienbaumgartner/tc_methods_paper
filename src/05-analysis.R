library(dplyr)
library(emmeans)
library(nnet)
library(viridis)

rm(list =ls())
abbrv <- function(x, width = 200) lapply(strwrap(x, width, simplify = FALSE), paste, collapse="\n")

load('/Volumes/INTENSO/methods_paper/output/02-finalized-corpora/baseline/reddit/ML_corpus.RDS')
m1 <- aov(sentiWords ~ TARGET_pol, data = dfx)
FittedMeans.m1 <- emmeans(m1, ~TARGET_pol)
FittedPairs.m1 <- pairs(FittedMeans.m1, adjust="bon")
FittedMeans.m1
summary(FittedPairs.m1)

dfx$cat2 <- recode(gsub('_(non-)?moral', '',dfx$cat),
                   'value-associated_concepts_NEG' = 'NVAC',
                   'value-associated_concepts_POS' = 'PVAC',
                   descriptive_concepts = 'DC',
                   thick_concepts_NEG = 'NTC',
                   thick_concepts_POS = 'PTC',
                   thin_concepts_POS = 'PThin',
                   thin_concepts_NEG = 'NThin'
)
unique(dfx$cat2)
m2 <- aov(abs(sentiWords) ~ cat2, data = dfx)
FittedMeans.m2 <- emmeans(m2, ~ cat2)
FittedPairs.m2 <- pairs(FittedMeans.m2, adjust="bon")
summary(FittedPairs.m2)

# TC vs Thin vs VAC by polarity
dfx$cat3 <- gsub('^(N|P)', '', dfx$cat2)
unique(dfx$cat3)
m3 <- aov(sentiWords ~ cat3*TARGET_pol, data = dfx[!dfx$cat3 == 'DC',])
FittedMeans.m3 <- emmeans(m3, ~ cat3|TARGET_pol)
FittedPairs.m3 <- pairs(FittedMeans.m3, adjust="bon")
FittedMeans.m3
summary(FittedPairs.m3)

# TC moral vs TC non-moral vs Thin vs VAC by polarity
dfx$cat4 <- gsub('_(NEG|POS)', '', dfx$cat)
unique(dfx$cat4)
m4 <- aov(sentiWords ~ cat4*TARGET_pol, data = dfx[!dfx$cat3 == 'DC',])
FittedMeans.m4 <- emmeans(m4, ~ cat4|TARGET_pol)
FittedPairs.m4 <- pairs(FittedMeans.m4, adjust="bon")
FittedMeans.m4
summary(FittedPairs.m4)

# VAC vs all evaluative classes by polarity
dfx$eval <- ifelse(dfx$cat3 %in% c('VAC', 'DC'), 'non-eval', 'eval') 
unique(dfx$eval)
m5 <- aov(sentiWords ~ eval*TARGET_pol, data = dfx[!dfx$cat3 == 'DC',])
FittedMeans.m5 <- emmeans(m5, ~ eval|TARGET_pol)
FittedPairs.m5 <- pairs(FittedMeans.m5, adjust="bon")
FittedMeans.m5
summary(FittedPairs.m5)

m6 <- multinom(cat2 ~ sentiWords, Hess = T, data = dfx)
preds <- mnl_pred_ova(
  model = m6, 
  data = dfx,
  x = 'sentiWords',
  by = 0.1,
  seed = 176,
  nsim = 10,
  probs = c(0.025, 0.975)
)

preds$plotdata <- preds$plotdata %>% mutate(cat2 =factor(cat2, levels=unique(preds$plotdata$cat2)[c(2,3,4,1,7,6,5)]))
p <- ggplot(preds$plotdata, aes(x=sentiWords, y=mean, ymin = lower, ymax = upper, colour = cat2)) +
  #geom_ribbon(alpha = 0.1) +
  geom_line(lwd=1) +
  #facet_wrap(cat2~., ncol = 2) +
  theme_bw() +
  #scale_color_viridis(discrete = T) +
  theme(
    plot.title = element_text(face = 'bold')
  ) +
  scale_y_continuous(limits = c(0,.75), labels = percent_format(accuracy = 1)) +
  labs(
    title = abbrv('Predicted Probabilities for Class Membership based on sentiWords', width = 50),
    x = 'Eval',
    y = 'Probability'
  )
p
