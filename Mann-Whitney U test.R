library(tidyverse)
library(dplyr)
library(ggplot2)
library(rstatix)
library(ggpubr)
library(coin)
library(car)
library(broom)
library(reshape2)
setwd("/Users/limk/Desktop/Ongoing manuscript/new copepod/R script")
copepoda<-read.csv("copepod infestation rate.csv")
## Testing whether there is evidence to support the idea that the presence of copepod infestation varies significantly across different Provinces.
# Create a contingency table
cont_table <- table(copepoda$Copepod, copepoda$Province)

# Perform Fisher's exact test
fisher_test <- fisher.test(cont_table)
fisher_test$p.value
print(fisher_test)

#post-hoc test
pairwise.fisher.test <- pairwise_fisher_test(cont_table, p.adjust.method = "bonferroni")
print(pairwise.fisher.test)

# Example calculating odds ratios
odds_ratios <- sapply(2:ncol(cont_table), function(i) {
  fisher.test(cont_table[, c(1, i)])$estimate
})
print(odds_ratios)

bubplot<-ggplot(pairwise.fisher.test, aes(x = group1, y = group2, size = n, fill = p.adj.signif)) +
  geom_point(shape = 21, colour = "black") +
  scale_size_area(max_size = 15) +
  labs(title = "Pairwise Fisher's Exact Test Adjusted p-values and Sample Size",
       x = "Group 1", y = "Group 2", size = "Sample Size") +
  scale_fill_manual(values = c("ns" = "gray", "*" = "blue", "**" = "green", "***" = "purple", "****" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

copepoda %>%
  group_by(Copepod) %>%
  get_summary_stats(SL, type = "median_iqr")
bxp <- ggboxplot(
  copepoda, x = "Copepod", y = "SL", 
  ylab = "Standard Length (mm)", xlab = "Copepod", add = "jitter"
)
bxp
stat.test <- copepoda %>% 
  rstatix::wilcox_test(SL ~ Copepod) %>%
  add_significance()
stat.test
copepoda %>% wilcox_effsize(SL ~ Copepod)
stat.test <- stat.test %>% add_xy_position(x = "Copepod")
bxp + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))

copepoda %>%
  group_by(Copepod) %>%
  get_summary_stats(Weight, type = "median_iqr")
bxp2 <- ggboxplot(
  copepoda, x = "Copepod", y = "Weight", 
  ylab = "Wet weight (g)", xlab = "Copepod", add = "jitter"
)
bxp2
stat.test2 <- copepoda %>%
  rstatix::wilcox_test(Weight ~ Copepod) %>%
  add_significance()
stat.test2
copepoda %>% wilcox_effsize(Weight ~ Copepod)
stat.test2 <- stat.test2 %>% add_xy_position(x = "Copepod")
bxp2 + 
  stat_pvalue_manual(stat.test2, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test2, detailed = TRUE))

rate<-read.csv("new.csv")
kruskal_test <- kruskal.test(Rate ~ Province, data = rate)
print(kruskal_test)

# Create a boxplot with individual data points
rate$Province<-factor(rate$Province, levels=c("North", "North-Central", "Central", "South-Central", "South"))
ggplot(rate, aes(x = Province, y = Rate)) +
  geom_boxplot(aes(x = Province), alpha = 0.6) +
  geom_jitter(width = 0.2, aes(x = Province), size = 2) +
  labs(title = "Infestation Rates by Province",
       x = "Province",
       y = "Infestation Rate (%)") +
  theme_minimal()
