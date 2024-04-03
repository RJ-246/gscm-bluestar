library(tidyverse)
library(tidymodels)
library(dplyr)
library(lubridate)

# Dallas
# I broke the excel sheets into two CSV UT8 files. You can use the links to download it 

df_shipments <- read_csv('https://www.dropbox.com/scl/fi/kycaltu2y4e4k2jno8gmu/Blue-Star-2019-1-Shipments.csv?rlkey=fw60qiu5y9ciniheuyub44phz&dl=1')

df_carriers <- read_csv('https://www.dropbox.com/scl/fi/zly5n0tuzpmceczh9dpct/Blue-Star-2019-1-Carriers.csv?rlkey=nz5h6wg2y5fthyy0juw1o36xo&dl=1')

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