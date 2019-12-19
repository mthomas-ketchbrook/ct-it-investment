library(RSocrata)
library(tidyverse)
library(stringi)
library(gganimate)

options(scipen = 999)

data <- RSocrata::read.socrata("https://data.ct.gov/resource/g24u-uzzf.csv")

# Data is based upon https://portal.ct.gov/OPM/Fin-CFO/ITIM/IT-Capital-Investment-Program

last_four <- function(x) stringi::stri_sub(x, -4, -1)

plot.df <- data %>% 
  dplyr::select_at(
    dplyr::vars(-dplyr::contains("requested"))) %>% 
  dplyr::select(
    -c(
    agency_name, 
    project_name, 
    project_description, 
    total_capital_funds_allocation, 
    bond_commission_approval
  )) %>% 
  dplyr::rename_at(
    dplyr::vars(dplyr::contains("allocated")), 
    last_four) %>% 
  dplyr::select(
    -c(
      `2021`, 
      `2022`
    )) %>% 
  tidyr::pivot_longer(cols = -c(function_of_government, 
                                application_phase), 
                      names_to = "year", 
                      values_to = "amount_allocated") %>% 
  dplyr::filter(!application_phase %in% c("Cancelled", 
                                         "LEAN", 
                                         "Pre-Selection", 
                                         "Identification")) %>% 
  tidyr::replace_na(list(amount_allocated = 0)) %>% 
  dplyr::group_by(function_of_government, 
                  year) %>% 
  dplyr::summarise(amount_allocated = sum(amount_allocated)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(amount_allocated = amount_allocated / 1000000, 
                year = as.integer(year)) %>% 
  dplyr::group_by(year) %>% 
  mutate(rank = dplyr::row_number(-amount_allocated) * 1) %>%
  ungroup() %>% 
  dplyr::mutate(function_of_government = dplyr::case_when(
    function_of_government == "Conservation and Development" ~ "Conservation/Development", 
    TRUE ~ function_of_government
  ))
  
# Working basic version using geom_bar:

# ggplot2::ggplot(plot.df, 
#                 aes(x = function_of_government, 
#                     y = amount_allocated)) + #, 
#   # ggplot2::geom_rect()
#                     #fill = `Application Phase`)) + 
#   ggplot2::geom_bar(stat = "identity") + 
#   # ggplot2::scale_y_continuous(labels = scales::comma) + 
#   ggplot2::ylab("$ Allocated (Millions)") + 
#   ggplot2::xlab("") + 
#   # ggthemes::theme_few() + 
#   ggplot2::coord_flip() + 
#   # ggplot2::facet_wrap(. ~year) + 
#   labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
#   transition_time(year) +
#   ease_aes('linear')


theme_set(theme_classic())

p <- ggplot(plot.df, aes(rank, group = function_of_government, 
                         fill = as.factor(function_of_government), color = as.factor(function_of_government))) +
  geom_tile(aes(y = amount_allocated/2,
                height = amount_allocated,
                width = 0.9), alpha = 0.8, color = NA) +
  
  # text in x-axis (requires clip = "off" in coord_*)
  # paste(function_of_government, " ")  is a hack to make pretty spacing, since hjust > 1 
  #   leads to weird artifacts in text spacing.
  geom_text(aes(y = 0, label = paste(function_of_government, " ")), vjust = 0.2, hjust = 1, size = 3) +
  
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  
  labs(title='{closest_state}', x = "", y = "$ Allocated (Millions)") +
  theme(plot.title = element_text(hjust = 0, size = 22),
        axis.ticks.y = element_blank(),  # These relate to the axes post-flip
        axis.text.y  = element_blank(),  # These relate to the axes post-flip
        plot.margin = margin(1,1,1,4, "cm")) +
  
  transition_states(year, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')

# Create a gif
# animate(p, fps = 25, duration = 20, width = 800, height = 600)

# Create an mp4
a <- animate(p, fps = 25, duration = 20, width = 800, height = 600, renderer = av_renderer())

gganimate::anim_save("chart_vid.mp4", a)

