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

ggsave('Chart2000b.png')

# Which were #1 the most?
df %>% 
    distinct(artist, track, `Weeks at #1`) %>% 
    arrange(desc(`Weeks at #1`)) %>% 
    head(7)

# Getting Usable Dates
df <- df %>% 
    mutate(date = date.entered + (week - 1) * 7)

df %>% 
    arrange(artist, track, week) %>% 
    select(artist, date.entered, week, date, rank) %>% 
    head(5)

plot_by_date <- 
    ggplot(df, aes(x = date, y = rank,
                   group = track)) +
    geom_line(size = 0.25, alpha = 0.4) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    scale_y_reverse() +
    theme_bw() +
    geom_vline(xintercept = as.numeric(as.Date("2000-01-01", "%Y-%m-%d")),
               col = 'red') +
    geom_vline(xintercept = as.numeric(as.Date("2001-01-01", "%Y-%m-%d")),
               col = 'red') +
    labs(x = 'Month', y = 'Rank')

plot_by_date












    





    














