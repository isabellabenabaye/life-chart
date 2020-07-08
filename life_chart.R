library(tidyverse)
library(lubridate)
library(waffle)
library(hrbrthemes)

life_data <-
  tibble(months = factor(rep(month.abb[1:12], 82), levels=month.abb[1:12])) %>%   ## make months
  slice(4:(n()-9)) %>%  ## remove months in 1996 before my birth month (April)
  tibble(
    age = rep(0:80, each = 12) ## age range: 1-80
  ) %>%
  rowid_to_column("row_name")  ## add column for row number

life_data <- life_data %>%
  mutate(era = fct_inorder(case_when(row_name < 161 ~ "Childhood - US",
                         row_name < 207 ~ "High School - PH",
                         row_name < 255 ~ "College - PH",
                         row_name < 285 ~ "Adulting - PH",
                         row_name < ((year(Sys.Date()) - 1996)*12) + (month(Sys.Date()) - 3) ~ "Adulting - US", ## months into "Adulting - US" based on current month
                         TRUE ~ "Time left")))

life_data %>%
  count(era) %>%
  ggplot(aes(fill = era, values = n)) +
  geom_waffle(color = "white", n_rows = 12, flip = TRUE) +
  scale_fill_manual(name = "", values = c("#EF476F","#FCA311","#FFD166","#0EAD69","#4ECDC4","#118AB2")) +
  coord_equal() +
  theme_ipsum(base_family = "IBM Plex Sans", grid = "") +
  theme_enhance_waffle()
