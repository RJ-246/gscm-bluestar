library(tidyverse)
library(tidymodels)
library(dplyr)
library(lubridate)

# Dallas
# I broke the excel sheets into two CSV UT8 files. The function file.choose() 
# lets me choose the file easily if you wanted to use it to we wouldn't need to change it every time
file_path_shipments <- file.choose()

df_shipments <- read_csv(file_path_shipments)

file_path_carriers <- file.choose()

df_carriers <- read_csv(file_path_carriers)

merged_df <- left_join(df_shipments, df_carriers, by = "SCAC") %>% 
  janitor::clean_names() %>% 
  mutate(ship_date = mdy(ship_date)) %>% 
  mutate(freight_paid = parse_number((freight_paid)))

merged_df %>% slice_max(ship_date)
# 4 months of data

glimpse(merged_df)


merged_df %>% unique("Ship Date")
# Gavin

# Derek

# Ryan