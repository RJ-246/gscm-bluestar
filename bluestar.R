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

library(tidyverse)
library(tidymodels)
library(dplyr)
library(lubridate)

df_shipments <- read_csv('https://www.dropbox.com/scl/fi/kycaltu2y4e4k2jno8gmu/Blue-Star-2019-1-Shipments.csv?rlkey=fw60qiu5y9ciniheuyub44phz&dl=1')

df_carriers <- read_csv('https://www.dropbox.com/scl/fi/zly5n0tuzpmceczh9dpct/Blue-Star-2019-1-Carriers.csv?rlkey=nz5h6wg2y5fthyy0juw1o36xo&dl=1')

# 1. How much does BlueStar annually spend on transportation?
# First, let's take a look at the unique values in the `Freight Paid` column to see if there's any pattern that's causing the issue.
unique_values_in_freight_paid <- unique(df_shipments$`Freight Paid`)

# Clean the `Freight Paid` column by removing dollar signs and commas, then convert to numeric
df_shipments$`Freight Paid Clean` <- as.numeric(gsub(",", "", gsub("\\$", "", df_shipments$`Freight Paid`)))

# Check again for any NA values that might have been introduced and what the original `Freight Paid` values were
na_values <- df_shipments %>% 
  filter(is.na(`Freight Paid Clean`)) %>% 
  select(`Freight Paid`, `Freight Paid Clean`)

# Now, filter the annual shipments for 2015 and sum the cleaned `Freight Paid` column
annual_shipments <- df_shipments %>% 
  filter(year(`Ship Date`) == 2015)

annual_transportation_cost <- sum(annual_shipments$`Freight Paid Clean`, na.rm = TRUE)


# 2. What are some of the key metrics for LTL shipments?
# Assuming LTL shipments can be filtered by a specific condition (e.g., Volume < some threshold)
ltl_shipments <- df_shipments %>% 
  filter(Volume < 1500)  # Example volume threshold for LTL

# Calculating LTL metrics
ltl_metrics <- ltl_shipments %>%
  summarise(
    On_Time_Delivery_Rate = mean(`On-Time`),
    Freight_Damage_Rate = mean(`Damage Free`),
    Billing_Accuracy_Rate = mean(`Billed Accurately`)
  )

# 3. What are the top origin-destination pairs for LTL shipments?
# Further filter LTL shipments if needed and then group by origin-destination pair
top_od_pairs <- ltl_shipments %>%
  group_by(`Origin City`, `Dest City`) %>%
  summarise(Total_Shipments = n()) %>%
  arrange(desc(Total_Shipments))

# 4. How many carriers currently serve the top TL (Truckload) origin-destination pairs?
# Assuming TL shipments can be distinguished in a similar way to LTL
tl_shipments <- df_shipments %>%
  filter(Volume >= 1500)  # Example volume threshold for TL

# Join the shipments data with the carriers data on the SCAC code
tl_shipments_with_carriers <- tl_shipments %>%
  inner_join(df_carriers, by = "SCAC")

# Count distinct carriers for the top origin-destination pairs
top_tl_od_pairs <- tl_shipments_with_carriers %>%
  group_by(`Origin City`, `Dest City`) %>%
  summarise(Carrier_Count = n_distinct(`Carrier Name`)) %>%
  arrange(desc(Carrier_Count))

# 5. What is the average shipment size and length of haul for each mode?
# Join the shipments data with the carriers data on the SCAC code for overall use
shipments_with_carriers <- df_shipments %>%
  inner_join(df_carriers, by = "SCAC")

# Now you can proceed with question #5 using the above-defined object
average_shipment_size_length <- shipments_with_carriers %>%
  group_by(`Carrier Type`) %>%
  summarise(
    Average_Weight = mean(Weight, na.rm = TRUE),
    Average_Miles = mean(Miles, na.rm = TRUE)
  )


# Calculate the average Weight and Miles for each Carrier Type
average_shipment_size_length <- shipments_with_carriers %>%
  group_by(`Carrier Type`) %>%
  summarise(
    Average_Weight = mean(Weight),
    Average_Miles = mean(Miles)
  )

# 6. Who are the top TL carriers?
# Assuming top means carriers with the most shipments
top_tl_carriers <- tl_shipments_with_carriers %>%
  group_by(`Carrier Name`) %>%
  summarise(Total_Shipments = n()) %>%
  arrange(desc(Total_Shipments))



# 7. What are your criteria for LTL consolidation?





# Derek

# Ryan
library(tidyverse)
library(tidymodels)
library(dplyr)
library(lubridate)
#install.packages("units", "sf", "tigris", "raster", "tidycensus", "zipcodeR")
#install.packages('units')
#install.packages("sf", type = "source", configure.args = "--with-proj-lib=$(brew --prefix)/lib/")
#install.packages('tigris')
#install.packages('raster')
#install.packages('tidycensus')
#install.packages('zipcodeR')
library('zipcodeR')

df_shipments <- read_csv('https://www.dropbox.com/scl/fi/kycaltu2y4e4k2jno8gmu/Blue-Star-2019-1-Shipments.csv?rlkey=fw60qiu5y9ciniheuyub44phz&dl=1')

df_carriers <- read_csv('https://www.dropbox.com/scl/fi/zly5n0tuzpmceczh9dpct/Blue-Star-2019-1-Carriers.csv?rlkey=nz5h6wg2y5fthyy0juw1o36xo&dl=1')

merged_df <- left_join(df_shipments, df_carriers, by = "SCAC") %>% 
  janitor::clean_names() %>% 
  mutate(ship_date = mdy(ship_date)) %>% 
  mutate(freight_paid = parse_number((freight_paid)))
merged_df %>% glimpse()

#clean destination zip codes
merged_df <- merged_df %>% 
  mutate(dest_zip_clean = str_extract(dest_zip, "^\\d{5}"))

#Generate all possible pairings of origin and destinations
ori_dest_pairs <- merged_df %>% 
  expand(origin_zip, dest_zip_clean)

#Calculate distances between all possible combinations and select the shortest 3 for each destination
#creates 264 NA values because of lack of info about certain zip codes
shortest_pairs <- ori_dest_pairs %>% 
  mutate(distance = (zipcodeR::zip_distance(origin_zip, dest_zip_clean)$distance)) %>% 
  group_by(dest_zip_clean) %>% 
  drop_na(distance) %>% 
  slice_min(n=1, order_by = distance)

distance_plot <- shortest_pairs %>% 
  ggplot(mapping = aes(x = distance))+
  geom_histogram()


#Calculate average cost/mile for each individual carrier
TL_carriers_ppm <- merged_df %>% 
  group_by(scac) %>% 
  filter(carrier_type == "TL") %>% 
  summarize(TL_price_per_mile = mean(freight_paid / miles)) %>% 
  arrange(TL_price_per_mile)

#has negative averages, need to figure out why thats happening
LTL_carriers_ppm <- merged_df %>% 
  group_by(scac) %>% 
  filter(carrier_type == "LTL") %>% 
  summarize(TL_price_per_mile = mean(freight_paid / miles / (weight /100))) %>% 
  arrange(TL_price_per_mile)

merged_df %>% 
  filter(scac == "TWEX") %>% 
  select(freight_paid, miles, weight) %>% 
  arrange(freight_paid)

