# script to visualize gene expression data (GSE183947)
# setwd('C:/Users/angel/Desktop/Rthings/Gene Expression')

# load libraries
library(tidyverse)
library(ggplot2)

# data
# dat.long to be used from C:\Users\angel\Desktop\Rthings\Gene Expression

# basic ggplot2 format:
# ggplot(data, aes(x = variable1, y= variable2)) +
#   geom_col()

# 1.barplot
dat.long %>%
  #head()
  filter(gene == 'BRCA1') %>%
  #head()
  ggplot(., aes(x = samples, y = FPKM, fill = tissue)) + 
  geom_col() # the dot means that we want ggplot to take the output of the previous line as a parameter (data)

# 2.density
dat.long %>%
  filter(gene == 'BRCA1') %>%
  ggplot(., aes(x = FPKM, fill = tissue)) +
  geom_density(alpha = 0.3)   # alpha is opacity

# 3.boxplot
dat.long %>%
  filter(gene == 'BRCA1') %>%
  ggplot(., aes(x = metastasis, y = FPKM)) +
  #geom_boxplot()
  geom_violin()

# 4.scatterplot
dat.long %>%
  filter(gene == 'BRCA1' | gene == 'BRCA2') %>%
  spread(key = gene, value = FPKM) %>%
  #head()
  ggplot(., aes(x = BRCA1, y=BRCA2, color = tissue)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

# 5. heatmap
genes.of.interest <- c('BRCA1', 'BRCA2', 'TP53', 'ALK', 'MYCN')

p <- dat.long %>%
  filter(gene %in% genes.of.interest) %>%
  #head()
  ggplot(., aes(x = samples, y = gene, fill = FPKM))+
  geom_tile() +
  scale_fill_gradient(low = 'white', high = 'red')

ggsave(p, filename = 'heatmap01.png', width = 10, height = 8)  # dimensions default is in inches

