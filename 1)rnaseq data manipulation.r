# script to manipulate gene expression data

# set/check working directory (getwd()/setwd())

# load libraries
library(dplyr)
library(tidyverse)
library(GEOquery)

data <- read.csv(file = "GSE183947_fpkm.csv")
dim(data)

# get metadata
gse <- getGEO(GEO = "GSE183947", GSEMatrix = TRUE)
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 1000)

metadata <- pData(phenoData(gse[[1]]))
head(metadata)

# get a subset of the metadata (keep only columns of interest) (the dumb way)
metadata.subset <- select(metadata, c(1,10,11,17))

# get a subset of the metadata (keep only columns of interest) (the smart way)
metadata.modified <- metadata %>%
  select(1,10,11,17) %>%
  rename(tissue = characteristics_ch1) %>%              # change the title of columns
  rename(metastasis = characteristics_ch1.1) %>%
  mutate(tissue = gsub("tissue: ", "", tissue)) %>%  # delete the unnecessary "tissue: " string
  mutate(metastasis = gsub("metastasis: ", "", metastasis))
  # head() (to run the head() function use %>% on the the above row)

head(data)

# reshape data
data.long <- data %>%
  rename(gene = X) %>%
  gather(key = 'samples', value = 'FPKM', -gene)
  # head()

# join the dataframes (data.long + metadata.modified)
data.long <- data.long %>%
  left_join(., metadata.modified, by = c("samples" = "description")) 
  # head()

# explore data
data.long %>%
  filter(gene == "BRCA1" | gene == "BRCA2")%>%    # check the mean FPKM value for these genes
  group_by(gene, tissue) %>%                      # in each tissue
  summarize(mean_FPKM=mean(FPKM))%>%
  # head()
  arrange(mean_FPKM) # ascending order (use arrage(-mean_FPKM) for descending order))
  
  
  
  
  