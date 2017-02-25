library(rvest)
library(data.table)

scrapeMatchups <- function() {
  matchups_url <- 'http://www.espn.com/nba/schedule'
  
  # Download HTML data
  matchups_html <- read_html(matchups_url)
  
  # Find available schedule dates in h2 nodes
  raw_dates <- matchups_html %>%
    html_nodes("h2") %>%
    html_text()
  
  # Parse the available schedule dates and omit the unparseables
  schedule_dates = na.omit(as.Date(raw_dates, '%a, %b %d'))
  
  matchup_sections <- matchups_html %>%
    html_nodes("table") %>%
    html_table(header=FALSE, fill=TRUE)
  
  # Number of schedule dates found should match the number of matchup sections
  stopifnot(length(schedule_dates) == length(matchup_sections))
  
  # Create an output data table
  matchups <- data.table(date=as.Date(character()), home=character(), away=character())
  
  for(i in 1:length(schedule_dates)) {
    # Extract relevant data from first two columns, removing the header row
    daily_matchups <- as.data.frame(matchup_sections[i])[-1,1:2]
    
    # Remove "Eastern Conf" and "Western Conf"
    # daily_matchups <- daily_matchups[!grep("Conf", paste(daily_matchups$X1, daily_matchups$X2)), c(1,2)]
    
    daily_matchup_count <- nrow(daily_matchups)
    
    if (daily_matchup_count == 0) { next; }
    
    # Clean up team abbreviations
    daily_matchups <- as.data.frame(as.list(apply(daily_matchups[,c(1,2)], 2, function(x) gsub(' \\w*$', '', x))))
    
    # Change "Los Angeles" to "LA Lakers"
    daily_matchups <- as.data.frame(as.list(apply(daily_matchups[,c(1,2)], 2, function(x) gsub('^Los Angeles$', 'LA Lakers', x))))
    
    # Change "LA" to "LA Clippers"
    daily_matchups <- as.data.frame(as.list(apply(daily_matchups[,c(1,2)], 2, function(x) gsub('^LA$', 'LA Clippers', x))))
    
    # Convert back to a data.table
    daily_matchups <- as.data.table(daily_matchups)
    
    # Prepend a column with the current schedule date 
    daily_matchups <- cbind(schedule_dates[i], daily_matchups)
    
    # Set dailyMatchup column names
    colnames(daily_matchups) <- c('date', 'home', 'away')
    
    matchups <- rbind(matchups, daily_matchups)
  }
  
  matchups
}
