library(tidyverse)
library(lubridate)
library(waffle)
library(hrbrthemes)
library(extrafont)
loadfonts(device = "win", quiet = TRUE)

# Create the data-----
life_data <-
  tibble(months = factor(rep(month.abb[1:12], 82), levels=month.abb[1:12])) %>%   ## make months
  slice(4:(n()-9)) %>%  ## remove months in 1996 before my birth month (April)
  tibble(
    age = rep(0:80, each = 12) ## age range: 1-80
  ) %>%
  rowid_to_column("row_name")  ## add column for row number

## add the "eras" to be colored in the waffle chart
life_data <- life_data %>%
  mutate(era = fct_inorder(case_when(row_name < 161 ~ "Childhood - US",
                         row_name < 207 ~ "High School - PH",
                         row_name < 255 ~ "College - PH",
                         row_name < 285 ~ "Adulting - PH",
                         row_name < ((year(Sys.Date()) - 1996)*12) + (month(Sys.Date()) - 3) ~ "Adulting - US", ## months into "Adulting - US" based on current month
                         TRUE ~ "Time left")))

# Waffle chart-----
life_in_months <- life_data %>%
  count(era) %>% ## the count of each era is the number of months in that era
  ggplot(aes(fill = era, values = n)) +
  geom_waffle(color = "#F7F7F7", n_rows = 12, size = 1, flip = TRUE) + ## make each row a year/12 months
  scale_fill_manual(name = "", values = c("#EF476F","#FCA311","#FFD166","#0EAD69","#4ECDC4","#118AB2")) +  ## assign colors to the eras
  coord_equal() +
  theme_ipsum(grid = "") +
  theme(legend.text = element_text(family = "Cooper Lt BT", size = 40),
        plot.background = element_rect(fill = "#F7F7F7", color = "#F7F7F7")) +
  theme_enhance_waffle()

life_in_months

# Save the chart
life_in_months + ggsave("life_in_months.png", device = "png", type = "cairo", width = 15, height = 25, dpi = 300)
