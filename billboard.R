# Lecture 5 with github version control
library(tidyverse)

billboard <- read_csv('https://clanfear.github.io/CSSS508/Lectures/Week5/data/billboard.csv')

str(billboard[,65:ncol(billboard)])
