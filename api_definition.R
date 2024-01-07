#Run packages
{
  library(plumber)
  library(jsonlite)
  library(lavaan)
  library(corrplot)
  library(ggplot2)
  library(psych)
  #library(tidyr)
  library(dplyr)
  #library(gridExtra)
  #library(spotifyr)
}
#Run custom functions
{
  # F1 turn json into R DF
  F1_json_df <- function(input_data) {
    df1 <- fromJSON(input_data)
    return(df1)
  }
  
  # F2 turns F1 output into a list containing a summary statistics
  # as an R table (summary_table) and as a JSON (summary_json)
  F2_summary_table <- function(df1) {
    component_names <- c(
      "Acousticness",
      "Danceability",
      "Liveness",
      "Instrumentalness",
      "Energy",
      "Loudness",
      "Speechiness",
      "Tempo",
      "Valence"
    )
    
    summary_table <- data.frame(
      Components = character(),
      Median = numeric(),
      Mean = numeric(),
      Q1 = numeric(),
      Q1low30pc = numeric(),
      Q1low20pc = numeric(),
      Q1low10pc = numeric(),
      Q1high30pc = numeric(),
      Q1high20pc = numeric(),
      Q1high10pc = numeric(),
      Q3 = numeric(),
      Q3low30pc = numeric(),
      Q3low20pc = numeric(),
      Q3low10pc = numeric(),
      Q3high30pc = numeric(),
      Q3high20pc = numeric(),
      Q3high10pc = numeric(),
      stringsAsFactors = FALSE
    )
    
    for (i in 1:length(component_names)) {
      column <- df1[[component_names[i]]]
      column_median <- median(column, na.rm = TRUE)
      column_mean <- mean(column, na.rm = TRUE)
      column_q1 <- quantile(column, 0.25, na.rm = TRUE)
      column_q1low30pc <- column_q1 * 0.7
      column_q1low20pc <- column_q1 * 0.8
      column_q1low10pc <- column_q1 * 0.9
      column_q1high30pc <- column_q1 * 1.3
      column_q1high20pc <- column_q1 * 1.2
      column_q1high10pc <- column_q1 * 1.1
      column_q3 <- quantile(column, 0.75, na.rm = TRUE)
      column_q3low30pc <- column_q3 * 0.7
      column_q3low20pc <- column_q3 * 0.8
      column_q3low10pc <- column_q3 * 0.9
      column_q3high30pc <- column_q3 * 1.3
      column_q3high20pc <- column_q3 * 1.2
      column_q3high10pc <- column_q3 * 1.1
      
      new_row <- list(
        Components = component_names[i],
        Median = column_median,
        Mean = column_mean,
        Q1 = column_q1,
        Q1low30pc = column_q1low30pc,
        Q1low20pc = column_q1low20pc,
        Q1low10pc = column_q1low10pc,
        Q1high30pc = column_q1high30pc,
        Q1high20pc = column_q1high20pc,
        Q1high10pc = column_q1high10pc,
        Q3 = column_q3,
        Q3low30pc = column_q3low30pc,
        Q3low20pc = column_q3low20pc,
        Q3low10pc = column_q3low10pc,
        Q3high30pc = column_q3high30pc,
        Q3high20pc = column_q3high20pc,
        Q3high10pc = column_q3high10pc
      )
      summary_table <- rbind(summary_table, new_row)
    }
    
    # Convert data frame to JSON
    summary_json <- toJSON(summary_table, pretty = TRUE)
    
    return(list(summary_table = summary_table, summary_json = summary_json))
  }
  
  # F3 turns JSON into ...
  F3_factor_analysis_varimax <- function(df1) {
    #keep only these columns
    Component_names <- c(
      "Acousticness",
      "Danceability",
      "Liveness",
      "Instrumentalness",
      "Energy",
      "Loudness",
      "Speechiness",
      "Tempo",
      "Valence"
    )
    df1 <- df1 %>% #drop columns
      select(one_of(Component_names))
    
    Clusters1 <- df1[, Component_names]
    CORClusters1 <- cor(Clusters1, use = "complete.obs")
    
    # Perform factor analysis
    FA <- fa.parallel(CORClusters1, n.obs = nrow(Clusters1))
    PCA1 <- principal(CORClusters1, nfactors = FA$nfact, rotate = "varimax", scores = TRUE)
    
    # Convert loadings to a data frame
    Profile1 <- PCA1$loadings
    # Cut off anything below "Valence" row
    Profile1 <- Profile1[1:which(trimws(rownames(PCA1$loadings)) == "Valence"), , drop = FALSE]
    Profile1 <- as.data.frame(Profile1)
    
    # Prepare table for Spotify metrics using .4 as rule of thumb
    Profile1 <- Profile1 %>%
      mutate_all(~ case_when(
        . < -0.39 ~ -100, #-100 for negative + 'significant' correlation
        . > 0.39 ~ 100, #100 for postive + 'significant' correlation
        TRUE ~ 0 #0 for 'non-significant' correlation
      ))
    
    # Run Summary table function to get profile values as table
    result <- F2_summary_table(df1)
    summary_table <- result$summary_table
    summary_table <- summary_table %>%
      select(Components, Q1, Q3)
    
    # Quick function to replace values marked as strong correlations
    mutate_profile1 <- function(value, q1, q3) {
      case_when(
        value == 0 ~ NA_real_,
        value == -100 ~ q1,
        value == 100 ~ q3,
        TRUE ~ value
      )
    }
    
    # Use quick function to add columns with Q1 or Q3 values to be sent to Spotify API
    Profile1 <- Profile1 %>%
      mutate(across(everything(), ~ mutate_profile1(.x, summary_table$Q1, summary_table$Q3), .names = "new_{.col}"))
    
    #return(list(PCA1 = PCA1, FA = FA, num_factors = num_factors, factor_loadings = factor_loadings))
    
    # Convert data frame to JSON
    profile_json <- toJSON(Profile1, pretty = TRUE)
    
    return(list(PCA1 = PCA1, profile_json = profile_json))
  }
  
}
#Test area
{
  #Test_DF <- F1_json_df("~/Documents/GitHub/r-api/output.json")
  #Test_Sum_Table <- F2_summary_table(Test_DF)
  #View(Test_Sum_Table[["summary_table"]])
  #result <- F1_json_df("~/Documents/GitHub/r-api/output.json")
  #profile_cfa <- F3_factor_analysis_varimax(result)
  #profile_cfa$profile_json
}


#* @get /summary_table
#* Spotify top 100 as JSON
#* @param input_data Input top 100 as JSON
#* @serializer json
function(input_data) {
  result <- F1_json_df(input_data)
  SumTableAndJSON <- F2_summary_table(result)
  SumTableAndJSON$summary_table
  #SumTableAndJSON$summary_json
}

#* @get /cfa
#* Profile through CFA analysis based on Spotify top 100
#* @param input_data Input top 100 as JSON
#* @serializer json
function(input_data) {
  result <- F1_json_df(input_data)
  profile_cfa <- F3_factor_analysis_varimax(result)
  profile_cfa$profile_json
}
