Get_Movement_Plots <- function(name_last, name_first, date_start, date_end) {
  
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
    pull(mlbam_id)
  
  player_data <- baseballr::statcast_search_pitchers(start_date = date_start,
                                          end_date = date_end,
                                          pitcherid = player_id)
  
  player_cleaned_data <- player_data %>%
    dplyr::filter(!is.na(pfx_x), !is.na(pfx_z),
                  game_type == "R") %>%
    dplyr::mutate(pfx_x_inches_pv = -12 * pfx_x,
                  pfx_z_inches = 12 * pfx_z)
  
  player_pitch_types <- unique(player_cleaned_data$pitch_name)
  
  player_pitch_avgs <- player_cleaned_data %>%
    dplyr::group_by(pitch_name) %>%
    dplyr::summarise(pfx_z_inches = mean(pfx_z_inches),
                     pfx_x_inches_pv = mean(pfx_x_inches_pv))
  
  ggplot2::ggplot(player_cleaned_data, aes(pfx_x_inches_pv, pfx_z_inches, 
                                           color = pitch_name, size = release_speed)) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::geom_point(data = player_pitch_avgs, size = 7, color = "black") +
    ggplot2::scale_color_manual(values = pitch_colors, limits = player_pitch_types) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::xlim(-25, 25) +
    ggplot2::ylim(-25, 25) +
    ggplot2::labs(title = paste(name_first,name_last, "Pitch Movement"),
                  subtitle = paste(str_sub(player_cleaned_data$game_date, 1, 4), "Season"),
                  x = "Horizontal Break",
                  y = "Induced Vertical Break",
                  color = "Pitch Name", size = "Velocity") +
    ggplot2::theme_bw()
  
}

Get_Movement_Plots(name_first = "Alex", name_last = "Cobb", 
                   date_start = "2022-03-01", date_end = "2022-12-01")
