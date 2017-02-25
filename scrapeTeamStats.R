library(rvest)
library(data.table)
library(taRifx)


scrapeTeamStats <- function() {
  team_off_url <- 'http://www.espn.com/nba/statistics/team/_/stat/offense-per-game'
  team_def_url <- 'http://www.espn.com/nba/statistics/team/_/stat/defense-per-game'
  
  data_sets <- data.table(dataset=c('off', 'def'), url=c(team_off_url, team_def_url))
  
  # Initialize a team_stats list to hold offense & defense data tables
  team_stats <- list()
  
  for(i in 1:nrow(data_sets)) {
    # Download HTML data
    team_data_html <- read_html(data_sets[['url']][i])
    
    # Parse HTML data
    team_data <- team_data_html %>%
      html_nodes("table") %>%
      .[[1]] %>%
      html_table()
    
    # Set data frame column names
    colnames(team_data) <- team_data[1,]
    
    # Remove placeholder data
    team_data <- team_data[team_data['TEAM'] != 'TEAM',]
    
    # Reindex rows
    row.names(team_data) <- 1:nrow(team_data)
    
    # Transform numeric rows to numeric type
    team_data <- japply( team_data, which(!names(team_data) %in% c('TEAM', 'RK')), as.numeric )
    
    # Add data to the team_stats list
    team_stats[data_sets[['dataset']][i]] <- list(team_data)
  }
  
  team_stats
}