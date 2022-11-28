Get_Pitch_Stats_Table <- function(name_last, name_first, date_start = "2022-03-01", date_end = "2022-12-01") {
  
  library(tidyverse)
  library(baseballr)
  library(gt)
  
  player_id <- playerid_lookup(first_name = name_first, last_name = name_last) %>%
    dplyr::filter(!is.na(fangraphs_id)) %>%
    pull(mlbam_id)
  
  player_data <- baseballr::statcast_search_pitchers(start_date = date_start,
                                                     end_date = date_end,
                                                     pitcherid = player_id)
  
  player_cleaned_data <- player_data %>%
    dplyr::filter(!is.na(pfx_x), !is.na(pfx_z),
                  game_type == "R") %>%
    dplyr::mutate(pfx_x_in_pv = -12 * pfx_x,
                  pfx_z_in = 12 * pfx_z)
  
  year <- unique(stringr::str_sub(player_cleaned_data$game_date, 1, 4))
  
  Player_Pitch_Stats <- player_cleaned_data %>%
    filter(is.na(release_speed)==F,
           is.na(pfx_z_in)==F,
           is.na(pfx_x_in_pv)==F,
           is.na(spin_axis)==F) %>%
    mutate(Strike = ifelse(description %in% c("foul", "called_strike", "swinging_strike",
                                              "foul_tip", "swinging_strike_blocked"), 1, 0),
           CalledStrike = ifelse(description %in% c("called_strike"), 1, 0),
           Whiff = ifelse(description %in% c("swinging_strike", "swinging_strike_blocked"), 1, 0),
           BattedBalls = ifelse()) %>%
    rename(`Pitch Type` = pitch_type) %>%
    group_by(player_name, `Pitch Type`) %>%
    summarise(Count = n(),
              Velocity = mean(release_speed),
              IVB = mean(pfx_z_in),
              HB = mean(pfx_x_in_pv),
              SpinAxis = mean(spin_axis),
              CalledStrikes = sum(CalledStrike),
              Whiffs = sum(Whiff),
              Strike = sum(Strike),
              `CStr%` = sum(CalledStrike) / Count,
              `SwStrike%` = sum(Whiff) / Count,
              `CSW%` = (sum(CalledStrike) + sum(Whiff)) / Count) %>%
    mutate(Velocity = round(Velocity, 1),
           IVB = round(IVB, 1),
           HB = round(HB, 1),
           SpinAxis = round(SpinAxis),
           `CStr%` = round(`CStr%`, 3)*100,
           `SwStrike%` = round(`SwStrike%`, 3)*100,
           `CSW%` = round(`CSW%`, 3)*100) %>%
    ungroup() %>%
    select(-player_name, -CalledStrikes, -Whiffs, -Strike) %>%
    arrange(desc(Count))
  
  stats_table <- Player_Pitch_Stats %>% gt(caption = paste(name_first,name_last,"Pitch Stats",year,"Season")) %>%
    tab_footnote(footnote = "Movement (in.) from Pitcher's Perspective")
  
  return(stats_table)
  
}

Get_Pitch_Stats_Table(name_first = "Jalen", name_last = "Beeks", date_start = "2022-03-01", date_end = "2022-12-01")

# Save plots 600 x 375
