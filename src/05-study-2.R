## ---------------------------
##
## Script name: 05-study-2.R
##
## Project: Tracing Thick Concepts
##
## Purpose of script: Study 2
##
## Author: Lucien Baumgartner
##
## Date created: 23.02.2021
##
## Email: lucienbaumgartner@philos.uzh.ch
##
## ---------------------------
##
## Notes:
##    Not all of the analyses
##    and results in this 
##    script were used in the
##    final paper
##
## ---------------------------


## ---------------------------
######## 1 Libraries #########
## ---------------------------
library(quanteda)
library(dplyr)
library(openxlsx)
library(factoextra) # hcla, loads ggplot2
library(stringr)
library(reshape2)
library(xtable)
library(Ckmeans.1d.dp) # univariate k-means
library(utc)
library(nnet)
library(MNLpred)
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
kdata <- read.xlsx('../input/Study 2 Results.xlsx') %>% .[-ncol(.)] %>%  as_tibble()

## ---------------------------
######### 5 Analysis #########
## ---------------------------
## Complete Type var
index <- c(grep('.*', kdata$Type), length(kdata$Type)+1)
reps <- na.omit(abs(lag(index)-index))
repl <- rep(na.omit(kdata$Type), reps)
length(repl) == nrow(kdata)
cbind(repl, kdata$Type)
kdata$Type <- repl
write.table(kdata, file = '../input/study2_results.csv', sep = ',', row.names = F)

## ---------------------------
## Hierarchical cluster analysis & dendogram
varsel <- grep('Eval\\.', colnames(kdata), value = T)
clusdata <- kdata[, varsel] %>%  as.data.frame()
rownames(clusdata) <- kdata$Adjective
?cluster::maxSE
res <- eclust(clusdata, FUNcluster = 'hclust', hc_method = 'ward.D', hc_metric = 'euclidean', seed = 2, stand = F,
              gap_maxSE = list(d.power = 2, method = 'firstmax', spaceH0 = 'scaledPCA'), k = 3)
col_vec <- ifelse(kdata$Type %in% c('Value-Associated', 'Descriptive'), 'darkgrey', 'black')
col_vec <- col_vec[res$order]
p <- fviz_dend(res, phylo_layout = 'layout.gem', k_colors ="black", 
               label_cols = col_vec, cex = 0.9, rect = T, lower_rect = -7,
               #horiz = T, 
               lwd = 0.5,
               main = 'Dendogramm using Ward Linkage')
p
p_coord <- ggplot_build(p)$data[[4]]
p <- p + 
  #geom_text(aes(x = p_coord$xmax, y = p_coord$ymin, label = 1:2, hjust = 2, vjust = -.9)) +
  #geom_text(aes(x = p_coord$xmin[1], y = p_coord$ymin[1], label = 'Cluster:', hjust = -.2, vjust = -.9)) +
  theme(plot.title = element_text(face = 'bold'))
p
ggsave(p, filename = '../output/paper/paper_study2_dendogramm.pdf', height = 6, width = 10)

## ---------------------------
## Univariate k-means (does not work better)
## Might be removed in the final version, since it is not part of the results
kdata
uvres <- Ckmeans.1d.dp(kdata$Eval.Weight)

aggr <- cbind(kdata, uvres = uvres$cluster)

p <- ggplot(aggr, aes(x = Eval.Weight, y = Adjective)) +
  geom_text(aes(label = uvres)) +
  #scale_x_continuous(limits = c(-.5,.5)) +
  facet_grid(Type~., scales = 'free_y', space = "free_y", drop = T) +
  labs(
    y = 'Target Ajective',
    x = 'Eval.Weight',
    caption = abbrv('NTC: Neg. Thick Concepts; NVAC: Neg. Value-Assciated Concepts; NThin: Neg. Thin Concepts; DC: Descriptive Concepts; PThin: Pos. Thin Concepts; PVAC: Pos. Value-Associated Concepts; PTC: Pos. Thick Concepts.', width = 75)
  ) 
p

## ---------------------------
## Multinomial probit model
kdata$Type2 <- relevel(factor(kdata$Type), ref = "Value-Associated")
kdata <- filter(kdata, !Adjective == c('awful', 'disgusting'))
range(kdata$Eval.Weight)
m1 <- multinom(Type2 ~ Eval.Weight, data = kdata, Hess = T)
## Predictions
preds <- mnl_pred_ova(
  model = m1, 
  data = kdata,
  x = 'Eval.Weight',
  by = 0.01,
  seed = 176,
  nsim = 100,
  probs = c(0.025, 0.975)
)
## Plot loop for plot panel
plist <- list()
for(i in 1:length(levels(kdata$Type2))){
  index <- levels(kdata$Type2)[i]
  plist[[i]] <- ggplot(filter(preds$plotdata, Type2==index), aes(x=Eval.Weight, y=mean, ymin = lower, ymax = upper)) +
    geom_ribbon(alpha = 0.1, colour = NA) +
    geom_line(linetype = c('solid', 'dashed', '12345678', 'dotdash', 'dotted')[i]) +
    facet_wrap(Type2~., ncol = 2) +
    theme_bw() +
    theme(
      plot.title = element_text(face = 'bold') 
    ) +
    scale_y_continuous(limits = c(0,1), labels = percent_format(accuracy = 1)) +
    labs(
      x='',
      y=''
    )
}
plist[[1]]
plist[[6]] <-
  ggplot(preds$plotdata, aes(x=Eval.Weight, y=mean, group=Type2)) +
  #geom_ribbon(alpha = 0.1, colour = NA) +
  geom_line(aes(lty=Type2)) +
  theme_bw() +
  theme(
    plot.title = element_text(face = 'bold'),
    legend.position = 'none'
  ) +
  scale_y_continuous(limits = c(0,1), labels = percent_format(accuracy = 1)) +
  scale_linetype_manual(values = c('solid', 'dashed', '12345678', 'dotdash', 'dotted')) +
  labs(
    x='',
    y=''
    #lty='Class'
  )
p <- grid.arrange(
  textGrob(abbrv("Predicted Predicted Probabilities for Class Membership based on Eval", width = 60), x=0, y=0.9, vjust = 1, hjust = 0, gp = gpar(fontface = "bold", cex = 1)),
  plist[[1]], plist[[2]], plist[[3]], plist[[4]], plist[[5]], plist[[6]],
  ncol = 2, 
  bottom = textGrob("Eval", x = 0.55),
  left = textGrob('Probability', rot = 90, vjust = 1),
  layout_matrix = rbind(c(1,1),
                        c(2,3),
                        c(4,5),
                        c(6,7)),
  heights = c(0.5,2,2,2)
)

ggsave(p, filename = '../output/paper/paper_study2_class_membership_MNL.pdf', width = 5, height = 7)

## ---------------------------
## First version of the panel, might be removed in the final version
p <- ggplot(preds$plotdata, aes(x=Eval.Weight, y=mean, ymin = lower, ymax = upper)) +
  geom_ribbon(alpha = 0.1, colour = NA) +
  geom_line() +
  facet_wrap(Type2~., ncol = 2) +
  theme_bw() +
  theme(
    plot.title = element_text(face = 'bold') 
  ) +
  scale_y_continuous(limits = c(0,1), labels = percent_format(accuracy = 1)) +
  labs(
    title = abbrv('Predicted Probabilities for Class Membership based on Eval', width = 50),
    x = 'Eval',
    y = 'Probability'
  )
p

## ---------------------------
## Eval vs non-eval logit model
kdata$evalType <- factor(ifelse(kdata$Type %in% c('Value-Associated', 'Descriptive'), 'non-eval', 'eval'))
kdata$evalType <- relevel(kdata$evalType, ref = 'non-eval')
kdata <- filter(kdata, !Adjective == c('awful', 'disgusting'))
range(kdata$Eval.Weight)
m2 <- glm(evalType ~ Eval.Weight, data = kdata, family = "binomial")
## Predictions
newData <- data.frame(Eval.Weight = seq(0, max(kdata$Eval.Weight), 0.01))
newData <- cbind(newData, predict(m2, newData, type = 'link', se =T))
newData <- within(newData, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
## Plot
p <- ggplot(newData, aes(x = Eval.Weight, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.2) + 
  geom_line() +
  theme_bw() +
  scale_y_continuous(limits = c(0,1), labels = percent_format(accuracy = 1)) +
  theme(
    plot.title = element_text(face = 'bold')
  ) +
  labs(
    x = 'Eval',
    y = 'Probability',
    title = abbrv('Predicted Probabilities for Membership in Evaluative Concept Class compared to a Non-evaluative Class based on Eval', width = 80)
  )
ggsave(p, filename = '../output/paper/paper_study2_EvalvsNonEval_logit.pdf', width = 8, height = 5)

## ---------------------------
## Robustness-checks for truly/really ratios
## load data
rm(list=ls())
files <- list.files('../output/00-sweep/study2/', full.names = T)
files <- files[!grepl('(s|S)hiny', files)]
length(files)==6*3

df <- lapply(files, function(x){
  tmp <- read.csv2(x, stringsAsFactors = F, sep = ',') %>% as_tibble
  tmp <- mutate(tmp, fileName = gsub('.*\\/|%22|\\(|\\)|\\.csv', '', x) %>% gsub('%20', '_', .) %>% tolower)
  tmp <- select(tmp, -txt)
  return(tmp)
})

## ---------------------------
## we had issues with short! we will thus approximate the data
df <- do.call(rbind, df)
df <- as_tibble(df)
df <- mutate(df, mod = gsub('\\_.*', '', fileName), target = gsub('.*\\_', '', fileName))
df <- mutate(df, mod = ifelse(mod == target, 'none', mod))
## sanitize short
short <- filter(df, target == 'short')
short <- short %>% 
  group_by(mod, created_utc) %>% 
  summarise(n=n())
shortmean <- mean(short$n[short$mod=='none'])
shortsd <- sd(short$n[short$mod=='none'])
start <- min(short$created_utc)
end <- max(short$created_utc)
expandr <- tibble(created_utc = start:end)
expandr <- expandr[!expandr$created_utc %in% short$created_utc[short$mod=='none'],]
expandr$n <- rnorm(nrow(expandr), mean = shortmean, sd = shortsd)
expandr$mod <- 'none'
expandr$target <- 'short'
shortAdd <- expandr %>% group_by(target, mod) %>% summarise(n = round(sum(n)))

## ---------------------------
## create table
fin <- df %>% 
  group_by(target, mod) %>% 
  summarise(n = n())
fin <- dcast(fin, 'target~mod')
## add short
fin[fin$target == 'short', 'none'] <- (fin[fin$target == 'short', 'none'] + shortAdd$n)
fin <- mutate(fin, perc_really = really/none*100, perc_truly = truly/none*100, Eval.Weight = perc_really+perc_truly, none = as.integer(none))
## add timeframe
time <- df %>% group_by(target) %>% summarise(t2 = min(created_utc), t1 = max(created_utc))
time <- time %>% mutate(t2 = as.POSIXct(t2, origin='1970-01-01'), t1 = as.POSIXct(t1, origin='1970-01-01'), t_diff = difftime(t1,t2,units = 'days'))
fin <- left_join(fin, time)
fin <- mutate(fin, t1 = as.character(t1), t2 = as.character(t2), t_diff = as.character(t_diff))
print(xtable(fin[, -c(8,9)], digits = 10), include.rownames = F)
