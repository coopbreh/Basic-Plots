Get_Pitch_Heatmap <- function(name_last, name_first, date_start = "2022-03-01", date_end = "2022-12-01", pitch_type) {
  
  library(tidyverse)
  library(baseballr)
  
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
    pitch_data <- player_cleaned_data %>% 
      dplyr::filter(pitch_name %in% c("Slider","Curveball","Knuckle Curve"))
  } else if (pitch_type == "Fastball" | pitch_type == "fastball") {
    pitch_data <- player_cleaned_data %>% 
      dplyr::filter(pitch_name %in% c("4-Seam Fastball","Sinker","Cutter"))    
  } else if (pitch_type == "Offspeed" | pitch_type == "offspeed") {
    pitch_data <- player_cleaned_data %>% 
      dplyr::filter(pitch_name %in% c("Changeup","Split-Finger"))
  } else if (pitch_type == "All" | pitch_type == "all") {
    pitch_data <- player_cleaned_data
  } else if (pitch_type == "Four Seam" | pitch_type == "4-Seam Fastball") {
    pitch_data <- player_cleaned_data %>% 
      dplyr::filter(pitch_name == "4-Seam Fastball")
  } else if (pitch_type == "Sinker" | pitch_type == "sinker") {
    pitch_data <- player_cleaned_data %>% 
      dplyr::filter(pitch_name == "Sinker")
  } else if (pitch_type == "Cutter" | pitch_type == "cutter") {
    pitch_data <- player_cleaned_data %>% 
      dplyr::filter(pitch_name == "Cutter")
  } else if (pitch_type == "Slider" | pitch_type == "slider") {
    pitch_data <- player_cleaned_data %>%
      dplyr::filter(pitch_name == "Slider")
  } else if (pitch_type == "Curveball" | pitch_type == "curveball") {
    pitch_data <- player_cleaned_data %>%
      dplyr::filter(pitch_type %in% c("Curveball", "Knuckle Curve"))
  } else if (pitch_type == "Changeup" | pitch_type == "Changeup") {
    pitch_data <- player_cleaned_data %>% 
      dplyr::filter(pitch_name %in% c("Changeup","Split-Finger"))
  }
  
  ggplot(pitch_data, aes(x=plate_x_pv ,y=plate_z)) +
    stat_density_2d(aes(fill = ..density..), geom="raster", contour = F) +
    scale_fill_distiller(type = "div", palette = "RdYlBu") +
    xlim(c(-2,2)) +
    ylim(c(0,5)) +
    annotate('rect', xmin = -0.85, xmax = 0.85, ymin = 1.6, ymax = 3.5, fill = 'black', color = 'black', alpha = 0.1) +
    theme_bw() +
    labs(title = paste(name_first,name_last,pitch_type,"Location"),
         subtitle = paste(str_sub(player_cleaned_data$game_date, 1, 4), "Season"),
         x = "Pitcher's Perspective",
         y = "",
         fill = "% Thrown") +
    coord_equal()
  
}

library(patchwork)

p1<-Get_Pitch_Heatmap(name_first = "Wandy", name_last = "Peralta", pitch_type = "Breaking Balls",
                      date_start = "2020-03-01", date_end = "2020-12-01")
p2<-Get_Pitch_Heatmap(name_first = "Wandy", name_last = "Peralta", pitch_type = "Offspeed")
p1+p2

Get_Pitch_Heatmap(name_first = "Jalen", name_last = "Beeks", pitch_type = "Changeup")

playerid_lookup(first_name = "A.J.", last_name = "Puk")
