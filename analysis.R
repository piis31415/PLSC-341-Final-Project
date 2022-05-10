rm(list = ls())

pwd <- getwd()

library(tidyverse)
library(ggrepel)
library(ggthemes)
library(patchwork)
library(randomizr)
library(texreg)
library(estimatr)

timeAnalysis <- T
failureValue <- 7 #note: some ylims may need to be changed if this value is not 7.

dat <- readRDS("clean.rds")
if (timeAnalysis) { 
  figName <- "time"
  dat <-readRDS("clean.rds") %>%
    mutate(Y = Y_t) 
} else {
  figName <- "score"
  dat <-readRDS("clean.rds") %>%
  mutate(Y = if_else(Y_s == 'X', failureValue, as.numeric(Y_s)))
}  

##### Summaries #######

# Group Means 

group_means <- dat %>%
  group_by(Z) %>%
  summarise(
    tidy(lm_robust(Y ~ 1, data = cur_data()))
  )

# Group Means by Block
# There are six blocks (see IDs)

block_group_means <- dat %>%
  group_by(Z, block) %>%
  summarise(
    tidy(lm_robust(Y ~ 1, data = cur_data()))
  )

# Difference in Means (DIMs)

dim_4 <- dat %>%
  filter(Z %in% c("4","5")) %>%
  summarise(
    tidy(difference_in_means(Y ~ l4, data = cur_data()))
  )

dim_6 <- dat %>%
  filter(Z %in% c("6","5")) %>%
  summarise(
    tidy(difference_in_means(Y ~ l6, data = cur_data()))
  )

dim_7 <- dat %>%
  filter(Z %in% c("7","5")) %>%
  summarise(
    tidy(difference_in_means(Y ~ l7, data = cur_data()))
  )

dims = bind_rows(dim_4, dim_6, dim_7)

# Conditional Average Treatment Effects
# (DIMs for block X=1 and block X=0)

cates_4 <- dat %>%
  filter(Z %in% c("4", "5")) %>%
  group_by(block) %>%
  summarise(
    tidy(difference_in_means(Y ~ l4, data = cur_data()))
  )

cates_6 <- dat %>%
  filter(Z %in% c("6", "5")) %>%
  group_by(block) %>%
  summarise(
    tidy(difference_in_means(Y ~ l6, data = cur_data()))
  )

cates_7 <- dat %>%
  filter(Z %in% c("7", "5")) %>%
  group_by(block) %>%
  summarise(
    tidy(difference_in_means(Y ~ l7, data = cur_data()))
  )

cates = bind_rows(cates_4, cates_6, cates_7)

group_means <- dat %>%
  group_by(Z) %>%
  summarise(
    tidy(lm_robust(Y ~ 1, data = cur_data()))
  )

####### Visualizing Data ###########

# Template 1: Scatterplot with summary overlays

fig_scatterplot <- ggplot(data = dat,
                          aes(x = jitter(Z, 0.4), y = if (timeAnalysis) Y else jitter(Y, 0.2))) + geom_point() +
  geom_point(data = group_means,
             aes(x = Z, y = estimate),
             color = "red",
             size = 3) +
  geom_linerange(
    data = group_means,
    aes(
      x = Z,
      y = estimate,
      ymin = conf.low,
      ymax = conf.high
    ),
    color = "red"
  ) +
  ylim(if(timeAnalysis) c(0, 1750) else c(1, failureValue+1)) +
  xlab("Wordle Length") +
  ylab(if(timeAnalysis) "Time Taken (s)" else "Score") +
  theme_bw()

# Template 2: Coefficient plots

fig_coefplot <- ggplot(data = dims,
                       aes(x = term, y = estimate)) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  ylim(if(timeAnalysis) c(-110, 430) else c(-1,1.5)) +
  xlab("Treatment-Control Comparison") +
  ylab("Difference in Means") +
  theme_bw()

# Combining the two figures using ggpatchwork

fig_results <- fig_scatterplot + fig_coefplot

# save figures

ggsave(fig_scatterplot, file=gsub("/", "\\\\", paste(pwd, "/Figures/",figName,"scatter.png",sep="")), 
       width=5, height=4, dpi=300)

ggsave(fig_coefplot, file=gsub("/", "\\\\", paste(pwd, "/Figures/",figName,"coef.png",sep="")), 
       width=5, height=4, dpi=300)

ggsave(fig_results, file=gsub("/", "\\\\", paste(pwd, "/Figures/",figName,"results.png",sep="")), 
       width=7, height=4, dpi=300)

# Template 3: Coefficient plots with block-level ATEs

ATE_coefplots <- ggplot(data = cates, # we use cates, not dims
       aes(x = term, y = estimate)) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  ylim(if(timeAnalysis) c(-550, 1500) else c(-7,7)) +
  xlab("Treatment-Control Comparison") +
  ylab("Difference in Means") +
  labs(caption = paste("l4, l6, l7 correspond to the length of the Wordle.", 
                       (if(timeAnalysis)"\nNote: the CI for ID 29, treatment l6 is [-1686, 1878] and is too large to display comfortably with the other data." else "")
                       ,sep=""))  +
  facet_wrap( ~ block, nrow = 1) + # This is the crucial step where we create sep. coef plots for each block
  theme_bw()

ggsave(ATE_coefplots, file=gsub("/", "\\\\", paste(pwd, "/Figures/",figName,"ATEcoef.png",sep="")), 
       width=10, height=4, dpi=300)

####### Making Regression Tables #######

# Specify models using lm_robust or difference_in_means in estimatr package

m_5_v_6 <-
  lm_robust(Y ~ l6, data = dat, subset = Z %in% c("5", "6"))

m_5_v_7 <-
  lm_robust(Y ~ l7, data = dat, subset = Z %in% c("5", "7"))

m_5_v_4 <-
  lm_robust(Y ~ l4, data = dat, subset = Z %in% c("5", "4"))

m_4_v_7 <-
  lm_robust(Y ~ l7, data = dat, subset = Z %in% c("4", "7"))

m_4_v_6 <-
  lm_robust(Y ~ l6, data = dat, subset = Z %in% c("4", "6"))

m_6_v_7 <-
  lm_robust(Y ~ l7, data = dat, subset = Z %in% c("6", "7"))

# Template 3: Make the table using texreg, copy-paste the latex code into your Tex word processor

texreg(
  list(m_5_v_6, m_5_v_7, m_5_v_4, m_4_v_7, m_4_v_6, m_6_v_7),
  include.ci = FALSE,
  custom.model.names = c("6 v. 5", "7 v. 5", "4 v. 5", "4 v. 7", "4 v. 6", "6 v. 7"),
  caption = "Difference in Means",
  caption.above = TRUE,
  booktabs = TRUE,
  digits = 3
)

