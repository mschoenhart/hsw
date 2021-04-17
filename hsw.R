#' Hungry Shark World
#' https://www.ubisoft.com/de-de/game/hungry-shark/world
#' Find the best possible combinations of items and sets (including set bonus) for your shark with brute force grid search
#' hsw.R generates a list of all possible combinations and sorts it according to your preferences
#' Implement your own utility functions for getting best results

# packages
library(tidyverse)
#library(readxl)

# read items and sets
# d <- read_excel("hsw.xlsx", sheet = 1)
# sets <- read_excel("hsw.xlsx", sheet = 2)
d <- read.csv2("items.csv", header = T, encoding = "UTF-8")
sets <- read.csv2("setbonus.csv", header = T, encoding = "UTF-8")

setitems <- sets[, 12:ncol(sets)]

# create grid
g <- expand.grid(
  tail = unique(d$Item[d$Position == "Tail"]),
  pect = unique(d$Item[d$Position == "Pectoral"]),
  chest = unique(d$Item[d$Position == "Chest"]),
  dors = unique(d$Item[d$Position == "Dorsal"]),
  hat = unique(d$Item[d$Position == "Hat"]),
  nose = unique(d$Item[d$Position == "Nose"]),
  stringsAsFactors = F
)

# create progress bar
total <- nrow(g)
pb <- txtProgressBar(min = 0, max = total, style = 3)

# search grid
cstab <- data.frame()
for (i in 1:nrow(g)) {
  # item i
  rowi <- g[i, ]
  
  # item colsum
  cs <- d %>%
    filter(Item %in% rowi) %>%
    select(-Item,-Position,-Set)
  
  # check for set bonus
  setbonus <- which(rowSums(matrix(
    is.element(unlist(setitems), rowi), nrow = nrow(setitems)
  ) | is.na(setitems)) == ncol(setitems))
  if (length(setbonus) == 1) {
    cs <- rbind(cs, sets[setbonus, 2:11]) # bind set bonus
    # cat("SetBonus\n")
  }
  
  cstab <- rbind(cstab, colSums(cs, na.rm = T))
  
  # update progress bar
  setTxtProgressBar(pb, i)
}
close(pb) # progress bar
names(cstab) <- names(d[, 4:13])

# bind grid results
res <- cbind(g, cstab) %>%
  mutate(
    Utility = `Boost Refill` / max(`Boost Refill`) * .3 +
      HealthDrain / max(HealthDrain) * .4 +
      Health / max(Health) * .1 +
      Food / max(Food * .2)
  ) %>%
  arrange(-HealthDrain,-`Boost Refill`,-Food,-Health,-GoldRush)

# view resulting list
view(res)
