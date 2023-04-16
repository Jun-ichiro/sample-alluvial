
library(tidyverse)
library(ggalluvial)

# READ csv file
res <- read_csv("res.csv")

# Number of respondents
n_id <- nrow(res)

# Unit of items  
items <- c(1,2,3)

# 
res.2 <- res %>% count(res[items[1]], res[items[2]], res[items[3]])
res.3 <- res.2 %>% mutate(res.2, i1=paste0("(", res.2[[1]], ")"), i2=paste0("(", res.2[[2]], ")"), i3=paste0("(", res.2[[3]], ")"))

# Cutoff to hidden data
n_cutoff <- 0

# DRAW alluvial diagram
p <- res.3 %>% 
    filter(n > n_cutoff) %>% 
    ggplot(aes(y = n, axis1 = i1, axis2 = i2, axis3 = i3, label = after_stat(stratum))) +
    geom_alluvium(aes(fill = i1), aes.bind='flows', width = 4/12) +
    geom_stratum(width = 5/16, fill = "white", color = "black") +
    geom_text(stat = "stratum") +
    scale_x_discrete(limits = c("item1_0", "item1_1", "item1_2"),
                     expand = c(.05, .05)) +
    scale_y_continuous(breaks = seq(0, n_id, length.out = 11), label = scales::percent_format(scale = 100 / n_id)) + 
    scale_fill_manual(values = c("red", "orange", "green", "blue", "purple")) +
    labs(y = "Percentage") +
    theme_minimal() +
    theme(legend.position = "none", axis.text.x = element_text(size = 14)) 

print(p)

ggsave(sprintf('alluvial_%d-%d-%d.png', items[1], items[2], items[3]), plot=p, dpi = 500, width = 4.5, height = 4.0)











