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

# Seattle police data
spd <- read_csv("https://raw.githubusercontent.com/clanfear/CSSS508/master/Seattle_Police_Department_911_Incident_Response.csv")

glimpse(spd)
str(spd$`Event Clearance Date`)

library(lubridate)
spd <- spd %>% 
    mutate(`Event Clearance Date` = 
               mdy_hms(`Event Clearance Date`,
                       tz = 'America/Los_Angeles')) 

str(spd$`Event Clearance Date`)

demo_dts <- spd$`Event Clearance Date`[1:2]
demo_dts
(date_only <- as.Date(demo_dts))
(day_of_week_only <- weekdays(demo_dts))
(one_hour_later <- demo_dts + dhours(1))

spd_times <- spd %>% 
    select(`Initial Type Group`, `Event Clearance Date`) %>% 
    mutate(hour = hour(`Event Clearance Date`))

spd_times %>% 
    ggplot(aes(x = hour)) +
    geom_histogram(binwidth = 2) +
    facet_wrap(~ `Initial Type Group`) +
    theme_minimal() +
    theme(strip.text.x = element_text(size = rel(0.6))) +
    labs(y = 'Count of Incidence', x = 'Hour of Day')

# Managing Factor Variables
# Using forcats that is part of the tidyverse package
str(spd$`Initial Type Group`)

spd_times <- spd_times %>% 
    mutate(`Initial Type Group` = factor(`Initial Type Group`))

head(spd_times$`Initial Type Group`)

head(as.numeric(spd_times$`Initial Type Group`))

# Releveing by Frequency
spd_times <- spd_times %>% 
    mutate(`Initial Type Group` = 
               fct_infreq(`Initial Type Group`))

head(levels(spd_times$`Initial Type Group`))

spd_times %>% 
    ggplot(aes(x = hour)) +
    geom_histogram(binwidth = 2) +
    facet_wrap(~ `Initial Type Group`) +
    theme_minimal() +
    theme(strip.text.x = element_text(size = rel(0.6))) +
    labs(y = 'Count of Incidence', x = 'Hour of Day')

# Jay-Z Reorder example
jayz <- df %>% 
    filter(artist == 'Jay-Z') %>% 
    mutate(track = factor(track))

jayz_bad_legend <- 
    ggplot(data = jayz, aes(x = week, y = rank,
                            group = track, color = track)) +
    geom_line() +
    theme_bw() +
    scale_y_reverse(limits = c(100,0)) +
    theme(legend.position = c(0.8, 0.25),
          legend.background = element_rect(fill = 'transparent'))

jayz_bad_legend

jayz <- jayz %>% 
    filter(artist == 'Jay-Z') %>% 
    mutate(track = fct_reorder(track, rank, min))

jayz_good_legend <- 
    ggplot(data = jayz, aes(x = week, y = rank,
                            group = track, color = track)) +
    geom_line() +
    theme_bw() +
    scale_y_reverse(limits = c(100,0)) +
    theme(legend.position = c(0.8, 0.25),
          legend.background = element_rect(fill = 'transparent'))






























    





    














