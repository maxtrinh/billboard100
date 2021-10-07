# Lecture 5 with github version control
library(tidyverse)

billboard <- read_csv('https://clanfear.github.io/CSSS508/Lectures/Week5/data/billboard.csv')

str(billboard[,65:ncol(billboard)])

# Manually column types as an option
bb_types <- paste(c('icccD', rep('i', 76)), collapse = '')
billboard <- read_csv('https://clanfear.github.io/CSSS508/Lectures/Week5/data/billboard.csv', col_types = bb_types)

#alternate solution

billboard <- read_csv('https://clanfear.github.io/CSSS508/Lectures/Week5/data/billboard.csv', guess_max = 5000)

# Pivot longer 
df <- billboard %>% 
    pivot_longer(cols = starts_with('wk'),
                 names_to = 'week',
                 values_to = 'rank',
                 values_drop_na = T)

dim(df)




