# The goal here is to reduce the dimensions of the dataset by throwing out useless columns and 
# transforming the numeric columns with PCA
source("helper.R")

library(dplyr)
library(FactoMineR)

fifa21 <- read.csv("final/fifa21.csv")
pca_data <- fifa21[,columns_for_pca]
pca_data$short_name <- make.unique(fifa21$short_name) # Make sure all names are unique'
row.names(pca_data) <- pca_data$short_name
player_names <- pca_data$short_name

pca_data <- subset(pca_data, select = - c(short_name, defending_marking))

acp <- PCA(pca_data, scale.unit = TRUE, ncp = 10, graph = FALSE)

players <- data.frame(acp$ind$coord)

# Combine the PCA coordinates with the main dataset and remove the columns used in the PCA

players <- cbind(fifa21[, columns_to_keep], players[, 1:4])

# Keep only clubs from these countries : ["Spain", "Italy", "Germany", "France", "England"]
selected_countries <- c("Spain", "Italy", "Germany", "France", "England", "Portugal")

cleaned_data <- filter(players, league_nationality %in% selected_countries)

# Calculate contract length : date joining and date end contract
library(lubridate)

cleaned_data <- cleaned_data %>%
  mutate(contract_length = difftime(ymd(contract_valid_until, truncated = 2L), 
                                    as_date(joined), units = "days")/(365.25/12))

write.csv(players, file = "final/players.csv", row.names = FALSE)
write.csv(cleaned_data, file = "final/cleaned.csv", row.names = FALSE)

