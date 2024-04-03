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

annual_total_freight_paid <- sum(merged_df$freight_paid)*3
annual_total_freight_paid

avg_LTL_payment <- merged_df %>% group_by(carrier_type) %>% 
  filter(carrier_type == 'LTL') %>% 
  summarize(avg_LTL_payment = mean(freight_paid))
avg_LTL_payment

# Checking cities in different states with same name
merged_df %>%
  group_by(origin_city) %>%
  summarize(distinct_state_count = n_distinct(origin_state)) %>%
  filter(distinct_state_count > 1)
merged_df %>%
  group_by(dest_city) %>%
  summarize(distinct_state_count = n_distinct(dest_state)) %>%
  filter(distinct_state_count > 1)
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 8 Cities that exist in different states few so have to group by dest_state as well

top_dest_org_pair <- merged_df %>% group_by(carrier_type, origin_city, dest_city, dest_state) %>% 
  filter(carrier_type == 'LTL') %>% 
  summarize(dest_org_pair_count = n()) %>% 
  arrange(desc(dest_org_pair_count))

# Gavin

# Derek

# Ryan