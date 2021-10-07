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
summary(df$rank)
summary(df$week)

# Parse number of week column into number only
df <- df %>% 
    mutate(week = parse_number(week))

# Separate time into length
df <- df %>% 
    separate(time, into = c("minute", "second"), sep = ":",
             convert = T) %>% 
    mutate(length = minute + second / 60) %>% 
    select(-minute, -second)
    
# Charts of 2000: Data Prep
df <- df %>% 
    group_by(artist, track) %>% 
    mutate('Weeks at #1' = sum(rank == 1),
           'Peak Rank' = ifelse(any(rank == 1),
                                'Hit #1',
                                "Didn't hit #1")) %>% 
    ungroup()

# Charts of 2000
df %>% 
    ggplot(aes(x = week, y = rank, group = track,
               color = `Peak Rank`)) +
    geom_line(aes(size = `Peak Rank`), alpha = 0.4) +
    scale_y_reverse() +
    scale_x_log10(breaks = seq(0, 70, 10)) +
    theme_classic() +
    labs(x = 'Week', y = 'Rank') +
    scale_size_manual(values = c(0.25, 1)) +
    scale_color_manual(values = c('black', 'red')) +
    theme(legend.position = c(0.9, 0.25),
          legend.background = element_rect(fill = 'transparent'))
    













