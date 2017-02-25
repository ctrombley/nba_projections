source('scrapeMatchups.R')
source('scrapeTeamStats.R')


calcProjections = function() {
  # Scrape matchup data
  matchups <- scrapeMatchups()
  
  # Scrape team stats
  team_stats <- scrapeTeamStats()
  
  # Create projections table
  options(stringsAsFactors = FALSE)
  projections <- NULL
  
  for(i in 1:nrow(matchups)) {
    home_team <-  matchups[i]$home;
    away_team <- matchups[i]$away;
    
    column_exclusions = c('TEAM')
    
    home_off <- team_stats$off[which(team_stats$off['TEAM'] == home_team), 
                               !(names(team_stats$off) %in% column_exclusions)]
    colnames(home_off) <- paste("HOME_OFF", colnames(home_off), sep = "_")
    
    home_def <- team_stats$def[which(team_stats$def['TEAM'] == home_team),
                               !(names(team_stats$def) %in% column_exclusions)]
    colnames(home_def) <- paste("HOME_DEF", colnames(home_def), sep = "_")
    
    away_off <- team_stats$off[which(team_stats$off['TEAM'] == away_team),
                               !(names(team_stats$off) %in% column_exclusions)]
    colnames(away_off) <- paste("AWAY_OFF", colnames(away_off), sep = "_")
    
    away_def <- team_stats$def[which(team_stats$def['TEAM'] == away_team),
                               !(names(team_stats$def) %in% column_exclusions)]
    colnames(away_def) <- paste("AWAY_DEF", colnames(away_def), sep = "_")
    
    current_proj <- data.table(home_off, home_def, away_off, away_def)
    
    if (nrow(current_proj) == 0 ) { next; }
     
    current_proj <- cbind(DATE=matchups[i]$date, HOME_TEAM=home_team, AWAY_TEAM=away_team, current_proj)
    
    # Calculate AVG_FGM
    current_proj <- cbind(current_proj, 
                          HOME_AVG_FGM=(current_proj$HOME_OFF_FGM + current_proj$AWAY_DEF_FGM) / 2, 
                          AWAY_AVG_FGM=(current_proj$AWAY_OFF_FGM + current_proj$HOME_DEF_FGM) / 2)
     
    # Calculate AVG_3PM
    current_proj <- cbind(current_proj, 
                          HOME_AVG_3PM=(current_proj$HOME_OFF_3PM + current_proj$AWAY_DEF_3PM) / 2, 
                          AWAY_AVG_3PM=(current_proj$AWAY_OFF_3PM + current_proj$HOME_DEF_3PM) / 2)
    
    # Calculate AVG_FTA
    current_proj <- cbind(current_proj, 
                          HOME_AVG_FTA=(current_proj$HOME_OFF_FTA + current_proj$AWAY_DEF_FTA) / 2, 
                          AWAY_AVG_FTA=(current_proj$AWAY_OFF_FTA + current_proj$HOME_DEF_FTA) / 2)
    
    # Calculate AVG_FTP
    current_proj <- cbind(current_proj, 
                          `HOME_AVG_FT%`=(current_proj$`HOME_OFF_FT%` * current_proj$HOME_AVG_FTA), 
                          `AWAY_AVG_FT%`=(current_proj$`AWAY_OFF_FT%` * current_proj$AWAY_AVG_FTA))
    
    # Calculate PROJ
    current_proj <- cbind(current_proj, 
                          HOME_PROJ=(current_proj$HOME_AVG_FGM * 2) + current_proj$HOME_AVG_3PM + current_proj$`HOME_AVG_FT%`, 
                          AWAY_PROJ=(current_proj$AWAY_AVG_FGM * 2) + current_proj$AWAY_AVG_3PM + current_proj$`AWAY_AVG_FT%`)
  
    # Add the row to the table
    if(is.null(projections)) {
      projections <- current_proj
    }
    else {
      projections <- rbind(projections, current_proj)
    }
  }
  
  projections
}
