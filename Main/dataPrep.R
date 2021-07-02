# Préparer les données afin de les utiliser dans la rédaction
setwd("C:/Users/Axel-Cleris Gailloty/Desktop/M2 IEE S2/MéthodesEvaluations/Projet/Main")
source("nationality.R")

output_path <- "C:/Users/Axel-Cleris Gailloty/Desktop/M2 IEE S2/MéthodesEvaluations/Projet/Main/final"

library(readr)
library(dplyr)

# Importing the data and merging 
fifa21 <- read_csv("players_21.csv")
league_nationality <- strsplit(fifa21$league_name, split = " ")


league_nat <- tolower(league_nationality %>% sapply(function(x) x[[1]][1]))
country_adj <- read_csv("demonyms.csv")
colnames(country_adj) <- c("adjective", "country")
country_adj$adjective <- tolower(country_adj$adjective)
country_adj$country <- tolower(country_adj$country)

find_country <- function(adj) {
  ladj <- tolower(adj)
  if (! ladj %in% country_adj$adjective) {
    if (ladj %in% country_adj$country) {
      tools::toTitleCase(country_adj$country[which(country_adj$country == ladj)][1])
    } else {
      paste0("NF_", adj)
    }
  } else {
    tools::toTitleCase(country_adj$country[which(adj == country_adj$adjective)][1])
  }
}

league_nat_country <- sapply(league_nat, find_country)
fifa21$league_nationality <- league_nat_country

# Player nationality == club nationality ?

fifa21$foreigner <- ifelse(fifa21$nationality == fifa21$league_nationality, 0, 1)

write.csv(x = fifa21, file = paste0(output_path, "/fifa21.csv"), row.names = FALSE)
