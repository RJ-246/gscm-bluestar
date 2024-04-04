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

merged_df <- merged_df %>% # Changes negative weights to null because they're obvious errors and I don't want to screw up the data
  mutate(weight = ifelse(weight <= 0, NA, weight)) %>%
  na.omit()

merged_df %>% slice_max(ship_date)
# 4 months of data

glimpse(merged_df)

annual_total_freight_paid <- sum(merged_df$freight_paid)*3
annual_total_freight_paid

avg_LTL_metrics <- merged_df %>% group_by(carrier_type) %>% 
  filter(carrier_type == 'LTL') %>% 
  summarize(avg_LTL_payment = mean(freight_paid),
            On_Time_Delivery_Rate = mean(on_time),
            Freight_Damage_Rate = mean(damage_free),
            Billing_Accuracy_Rate = mean(billed_accurately),
            LTL_price_per_mile = mean((freight_paid / miles) / (weight /100))
            )

avg_TL_metrics <- merged_df %>% group_by(carrier_type) %>% 
  filter(carrier_type == 'TL') %>% 
  summarize(avg_LTL_payment = mean(freight_paid),
            On_Time_Delivery_Rate = mean(on_time),
            Freight_Damage_Rate = mean(damage_free),
            Billing_Accuracy_Rate = mean(billed_accurately),
            mile_per_price = mean(freight_paid/miles)
  )
glimpse(avg_LTL_metrics)
glimpse(avg_TL_metrics)

merged_df <- merged_df %>% 
  mutate(rate = if_else(carrier_type == 'LTL', 
                        (freight_paid / miles) / (weight /100), 
                        freight_paid/miles))


plot_2 <- merged_df %>% 
  filter(carrier_type %in% c("TL")) %>% 
  group_by(scac) %>% 
  filter(n() > 100) %>% 
  ggplot(aes(x = miles, y = freight_paid, size = weight)) +
  geom_point()+
  facet_wrap(~scac)
plot_2


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

# Now, filter the annual shipments for 2015 and sum the cleaned `Freight Paid` column
annual_shipments <- df_shipments %>% 
  filter(year(`Ship Date`))

annual_transportation_cost <- sum(annual_shipments$`Freight Paid Clean`, na.rm = TRUE)


# 2. What are some of the key metrics for LTL shipments?
# Assuming LTL shipments can be filtered by a specific condition (e.g., Volume < some threshold)
ltl_shipments <- merged_df %>% 
  filter(carrier_type == 'LTL') 

# Calculating LTL metrics
ltl_metrics <- ltl_shipments %>%
  summarise(
    On_Time_Delivery_Rate = mean(`on_time`),
    Freight_Damage_Rate = mean(`damage_free`),
    Billing_Accuracy_Rate = mean(`billed_accurately`)
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
  filter(carrier_type == 'LTL')  # Example volume threshold for TL

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

# Load necessary libraries
library(ggplot2)

summary(merged_df)

merged_df <- merged_df %>% # Changes negative weights to null because they're obvious errors and I don't want to screw up the data
  mutate(weight = ifelse(weight < 0, NA, weight)) %>%
  na.omit()

# Handle dates - ensuring ship_date is in the correct format
merged_df$ship_date <- ymd(merged_df$ship_date) # Modify if the format is different

# Data visualization to understand factors influencing freight cost
# Relationship between weight and freight_paid
ggplot(merged_df, aes(x = weight, y = freight_paid)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Freight Cost by Weight")

# Relationship between volume and freight_paid
ggplot(merged_df, aes(x = volume, y = freight_paid)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Freight Cost by Volume")

# Identifying anomalies/outliers in freight cost
# Boxplot for freight_paid to identify outliers
ggplot(merged_df, aes(y = freight_paid)) + 
  geom_boxplot() +
  labs(title = "Boxplot for Freight Costs")

# Exploring categorical variables such as carrier_type
ggplot(merged_df, aes(x = carrier_type, y = freight_paid)) + 
  geom_boxplot() +
  labs(title = "Freight Cost by Carrier Type")

# Correlation matrix to see which variables are most related to freight_paid
# "Miles" is hands down the biggest indicator of cost
numeric_vars <- merged_df %>% select_if(is.numeric)
correlation_matrix <- cor(numeric_vars, use = "complete.obs")
print(correlation_matrix)

# Freight paid over time--shows some anomalies towards the beginning
ggplot(merged_df, aes(x = ship_date, y = freight_paid)) + 
  geom_line(stat = "summary", fun.y = "mean") +
  labs(title = "Trend of Freight Cost Over Time")





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
#install.packages('geosphere')
#install.packages('ggmap')
library(zipcodeR)
library(geosphere)
library(ggmap)

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
  group_by(dest_zip_clean)

# to_geocode <- shortest_pairs %>% 
#   filter(is.na(distance) == TRUE) %>% 
#   distinct(dest_zip_clean) %>% 
#   inner_join(merged_df, by="dest_zip_clean") %>% 
#   distinct(dest_zip_clean, dest_state, dest_city)

#ggmap::register_google(key = "AIzaSyBdUgUeiKptV_THCrLVEch0B_jN0EbJ9_k")
#geocoded_zips <- to_geocode %>% 
#  unite("location", dest_city, dest_state, dest_zip_clean, sep = " ") %>% 
#  ggmap::mutate_geocode(location)

#write_delim(geocoded_zips, file= "/Users/rjackso3/Documents/School_Stuff/Winter_2024/GSCM_530/gscm-bluestar/geocoded_zips.csv", delim = ",")

geocoded_zips <- read_csv("/Users/rjackso3/Documents/School_Stuff/Winter_2024/GSCM_530/gscm-bluestar/geocoded_zips.csv")

#takes zip codes back out of geocoded
geocoded_dests <- geocoded_zips %>% 
  mutate(location = str_extract(location, "\\d{5}$")) %>% 
  rename(dest_zip = location, dest_lon = lon, dest_lat = lat)

# geocodes the origin zips
distinct_origin_zips <- merged_df %>% 
  distinct(origin_zip) %>% 
  apply(MARGIN =1 , FUN = geocode_zip) %>% 
  bind_rows() %>% 
  rename(origin_zipcode = zipcode)

#joins both tibbles
joined_zips_coords <- crossing(dest_zip = geocoded_dests$dest_zip, origin_zipcode = distinct_origin_zips$origin_zipcode) %>% 
  left_join(geocoded_dests, by = "dest_zip") %>% 
  left_join(distinct_origin_zips, by = "origin_zipcode")
#calculates distances between origins and destinations
missing_distances <- joined_zips_coords %>% 
  rowwise() %>% 
  mutate(distance = raster::pointDistance(c(dest_lon, dest_lat), c(lng, lat), lonlat=TRUE) * 0.00062137)
#removes duplicates (i dont know why there are duplicates)
missing_distances <- missing_distances %>%
  relocate(origin_zipcode) %>% 
  arrange(origin_zipcode, dest_zip) %>% 
  group_by(origin_zipcode, dest_zip) %>% 
  slice_min(distance, n=1) %>% 
  select(-c('lat', 'dest_lon', 'dest_lat','lng'))

#replace NAs in original distance calculation with new ones
shortest_pairs <- shortest_pairs %>% left_join(missing_distances, by = c('origin_zip' = "origin_zipcode", "dest_zip_clean" = "dest_zip")) %>% 
  mutate(distance.x = if_else(is.na(distance.x) == TRUE, distance.y, distance.x)) %>% 
  rename(distance = distance.x) %>% 
  select(-distance.y)

#get just the shortest distance between each origin/dest pair
shortest_pairs <- shortest_pairs %>% 
  group_by(dest_zip_clean) %>% 
  slice_min(n=1, order_by = distance)

distance_plot <- shortest_pairs %>% 
  ggplot(mapping = aes(x = distance))+
  geom_histogram()

###################

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

