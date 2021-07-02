setwd("/cloud/project/Main")

league_nat <- tolower(readRDS("/cloud/project/Main/league_nat.rds"))
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
