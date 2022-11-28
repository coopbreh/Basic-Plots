Get_Pitch_Plots <- function(name_last, name_first, date_start = "2022-03-01", date_end = "2022-12-01", pitch_type) {
  
  library(tidyverse)
  library(baseballr)
  
  pitch_colors <- c("4-Seam Fastball" = "red",
                    "2-Seam Fastball" = "blue",
                    "Sinker" = "cyan",
                    "Cutter" = "violet",
                    "Fastball" = "black",
                    "Curveball" = "green",
                    "Knuckle Curve" = "pink",
                    "Slider" = "orange",
                    "Changeup" = "gray50",
                    "Split-Finger" = "gold",
                    "Knuckleball" = "beige")
  
  player_id <- baseballr::playerid_lookup(last_name = name_last, first_name = name_first) %>%
    dplyr::filter(!is.na(fangraphs_id)) %>%
    pull(mlbam_id)
  
  player_data <- baseballr::statcast_search_pitchers(start_date = date_start,
                                                     end_date = date_end,
                                                     pitcherid = player_id)
  
  player_cleaned_data <- player_data %>%
    dplyr::filter(!is.na(pfx_x), !is.na(pfx_z),
                  game_type == "R") %>%
    dplyr::mutate(pfx_x_inches_pv = -12 * pfx_x,
                  pfx_z_inches = 12 * pfx_z,
                  plate_x_pv = -1 * plate_x)
  
  if (pitch_type == "Breaking Ball" | pitch_type == "breaking ball") {
    pitch_data <- player_cleaned_data 
      %>% dplyr::filter(pitch_name %in% c("Slider","Curveball","Knuckle Curve"))
  } else if (pitch_type == "Fastball" | pitch_type == "fastball") {
    pitch_data <- player_cleaned_data %>% 
      dplyr::filter(pitch_name %in% c("4-Seam Fastball","Sinker","Cutter"))    
  } else if (pitch_type == "Offspeed" | pitch_type == "offspeed") {
    pitch_data <- player_cleaned_data %>% 
      dplyr::filter(pitch_name %in% c("Changeup","Split-Finger"))
  } else if (pitch_type == "All" | pitch_type == "all") {
    pitch_data <- player_cleaned_data
  } else if (pitch_type == "Changeup" | pitch_type == "Changeup") {
    pitch_data <- player_cleaned_data %>% 
      dplyr::filter(pitch_name %in% c("Changeup","Split-Finger"))
  }
  
  player_pitch_types <- unique(pitch_data$pitch_name)
  
  ggplot2::ggplot(pitch_data, aes(plate_x_pv, plate_z, 
                                           color = pitch_name, size = release_speed)) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::scale_color_manual(values = pitch_colors, limits = player_pitch_types) +
    ggplot2::xlim(-2, 2) +
    ggplot2::ylim(0, 5) +
    ggplot2::labs(title = paste(name_first,name_last, "Pitch Location"),
                  subtitle = paste(str_sub(player_cleaned_data$game_date, 1, 4), "Season"),
                  x = "Pitcher's Perspective",
                  y = "",
                  color = "Pitch Name", size = "Velocity") +
    ggplot2::theme_bw() +
    ggplot2::coord_fixed() +
    annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.01)
  
}

Get_Pitch_Plots(name_first = "Jalen", name_last = "Beeks", pitch_type = "Fastballs")
