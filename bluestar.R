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
                        (freight_paid / miles / (weight /100)), 
                        freight_paid/miles))

merged_df$id <- seq_len(nrow(merged_df))

trucks_only_df <- merged_df %>% filter(carrier_type %in% c("TL","LTL"))

plot_2 <- merged_df %>% 
  filter(carrier_type %in% c("TL")) %>% 
  group_by(scac) %>% 
  filter(n() > 100) %>% 
  ggplot(aes(x = miles, y = freight_paid)) +
  geom_point()+
  geom_smooth()+
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

summarized_df <- merged_df %>%
  filter(carrier_type %in% c("TL","LTL")) %>% 
  group_by(origin_city, dest_city, dest_state, ship_date, carrier_type) %>%
  summarize(weight_sum = sum(weight),
            mean_rate = mean(rate),
            mean_miles = mean(miles))


filtered_df <- summarized_df %>%
  mutate(
    trucks = ceiling(weight_sum/45000),
    left_over = weight_sum %% 45000,
    left_over = if_else(left_over <= 887, 0, left_over),
    trucks = if_else(left_over == 0, trucks - 1, trucks),
    reduced_price = 
      (if_else(
    left_over = weight_sum - ((trucks-1)*45000),
    reduced_price = (if_else(
      carrier_type == 'LTL',
      (mean_miles * mean_rate) / (45000 / 100),
      mean_miles * mean_rate
    ) * (trucks-1)) +
  if_else(
    carrier_type == 'LTL',
    (mean_miles * mean_rate) / (left_over / 100),
    mean_miles * mean_rate
  ))))



# Modify the summarized_df to include ship_date in grouping
summarized_df <- merged_df %>%
  filter(carrier_type %in% c("TL", "LTL")) %>%
  group_by(origin_city, dest_city, dest_state, ship_date, carrier_type) %>%
  summarize(weight_sum = sum(weight),
            mean_rate = mean(rate),
            mean_miles = mean(miles))

filtered_df <- summarized_df %>%
  arrange(ship_date) %>%
  mutate(trucks = ceiling(weight_sum / 45000),
         left_over = weight_sum %% 45000)

filtered_df$id <- seq_len(nrow(filtered_df))


for (i in 1:(nrow(filtered_df))) {
  if (filtered_df$left_over[i] > 0) {
    later_shipments <- filtered_df %>%
      filter(origin_city == filtered_df$origin_city[i] &
               dest_city == filtered_df$dest_city[i] &
               dest_state == filtered_df$dest_state[i] &
               carrier_type == filtered_df$carrier_type[i] &
               ship_date > filtered_df$ship_date[i])
    total_left_over <- filtered_df$left_over[i]
    
    for (j in 1:nrow(later_shipments)) {
      if(total_left_over + later_shipments$left_over[j] > 1000) {
        filtered_df$left_over[i] = 0
        later_shipments$left_over[j] = total_left_over + later_shipments$left_over[j]
        filtered_df$left_over[later_shipments$id[j]] = later_shipments$left_over[j]
        total_left_over = 0
        print(total_left_over)
        break
      }
      total_left_over <- total_left_over + later_shipments$left_over[j]
      later_shipments$left_over[j] = 0
      filtered_df$left_over[later_shipments$id[j]] = 0
      last_id <- later_shipments$id[j]
    }
    
    if (total_left_over > 0) {
      extra_shipment <- filtered_df[i]  
      extra_shipment$id <- max(filtered_df$id) + 1  
      extra_shipment$weight <- total_left_over
      extra_shipment$ship_date <- filtered_df$ship_date[last_id]+1
      filtered_df <- rbind(filtered_df, extra_shipment)
    }
  }
  print(1)
}

# Calculate the reduced price based on the adjusted trucks and leftover weight
filtered_df <- filtered_df %>%
  mutate(
    reduced_price = (if_else(carrier_type == 'LTL',
                             (mean_rate * mean_miles * (45000 / 100)),
                             mean_miles * mean_rate) * (trucks - 1)) +
      (if_else(carrier_type == 'LTL',
               (mean_miles * mean_rate * (left_over / 100)),
               mean_miles * mean_rate))
  )


sum(trucks_only_df$freight_paid)

filtered_df
sum(merged_df$freight_paid) 
trucks_only_df <- merged_df %>% filter(carrier_type %in% c("TL","LTL"))
sum(trucks_only_df$freight_paid) 

sum(filtered_df$reduced_price)



filtered_df %>% select(reduced_price, origin_city, dest_city, dest_state, ship_date, carrier_type) %>% 
  arrange(ship_date, origin_city)

# Gavin

library(tidyverse)
library(tidymodels)
library(dplyr)
library(lubridate)

df_shipments <- read_csv('https://www.dropbox.com/scl/fi/kycaltu2y4e4k2jno8gmu/Blue-Star-2019-1-Shipments.csv?rlkey=fw60qiu5y9ciniheuyub44phz&dl=1')

df_carriers <- read_csv('https://www.dropbox.com/scl/fi/zly5n0tuzpmceczh9dpct/Blue-Star-2019-1-Carriers.csv?rlkey=nz5h6wg2y5fthyy0juw1o36xo&dl=1')

# 1. How much does BlueStar annually spend on transportation?
annual_total_freight_paid <- sum(merged_df$freight_paid)*3

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
# Note that some destination cities are repeated and so this will need to be changed
top_od_pairs <- ltl_shipments %>%
  group_by(origin_city, dest_city) %>%
  summarise(total_shipments = n()) %>%
  arrange(desc(total_shipments))

# 4. How many carriers currently serve the top TL (Truckload) origin-destination pairs?
# Needs update to account for serving the top TL
tl_shipments <- merged_df %>%
  filter(carrier_type == 'TL')

# Count distinct carriers for the top origin-destination pairs
top_tl_od_pairs <- tl_shipments %>%
  group_by(origin_city, dest_city) %>%
  summarise(carrier_count = n_distinct(`carrier_name`)) %>%
  arrange(desc(carrier_count))

# 5. What is the average shipment size and length of haul for each mode?

# Calculate the average Weight and Miles for each Carrier Type
average_shipment_size_length <- merged_df %>%
  group_by(carrier_type) %>%
  summarise(
    Average_Weight = mean(weight),
    Average_Miles = mean(miles)
  )

# 6. Who are the top TL carriers?



# 7. What are your criteria for LTL consolidation?





# Who is making the most mistakes?

# How many companies are there?
#70
number_of_unique_scac <- n_distinct(merged_df$scac)
print(number_of_unique_scac)

# What are their names
unique_scac_values <- unique(merged_df$scac)
print(unique_scac_values)


# Top Five Worst Companies for not being on time
scac_not_on_time_ratio <- merged_df %>%
  group_by(scac) %>%
  summarise(
    total_shipments = n(),
    not_on_time_count = sum(on_time == 0),
    ratio_not_on_time = not_on_time_count / total_shipments
  ) %>%
  arrange(desc(ratio_not_on_time)) %>%
  slice_head(n = 5)

print(scac_not_on_time_ratio)



# Top Five Worst Companies for not completing delivery
scac_not_complete_ratio <- merged_df %>%
  group_by(scac) %>%
  summarise(
    total_shipments = n(),
    not_complete_count = sum(delivered_complete == 0),
    ratio_not_complete = not_complete_count / total_shipments
  ) %>%
  arrange(desc(ratio_not_complete)) %>%
  slice_head(n = 5)

print(scac_not_complete_ratio)


# Top Five Worst Companies for having damaged goods
scac_damaged_ratio <- merged_df %>%
  group_by(scac) %>%
  summarise(
    total_shipments = n(),
    damage_count = sum(damage_free == 0),
    ratio_damaged = damage_count / total_shipments
  ) %>%
  arrange(desc(ratio_damaged)) %>%
  slice_head(n = 5)

print(scac_damaged_ratio)


# Top Five Worst Companies for not billing accurately
scac_not_billing_accurately_ratio <- merged_df %>%
  group_by(scac) %>%
  summarise(
    total_shipments = n(),
    not_billing_accurately_count = sum(billed_accurately == 0),
    ratio_not_billing_accurately = not_billing_accurately_count / total_shipments
  ) %>%
  arrange(desc(ratio_not_billing_accurately)) %>%
  slice_head(n = 5)

print(scac_not_billing_accurately_ratio)



print(scac_not_on_time_ratio)

print(scac_not_complete_ratio)

print(scac_damaged_ratio)

print(scac_not_billing_accurately_ratio)



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


#geocoded_zips <- read_csv("/Users/rjackso3/Documents/School_Stuff/Winter_2024/GSCM_530/gscm-bluestar/geocoded_zips.csv")
#geocoded_zips <- read_csv("/Users/dalla/Documents/Assorted BYU School stuff/Winter 2024/IS 555/gscm-bluestar/geocoded_zips.csv")
#geocoded_zips <- read_csv("C:/Users/derek/OneDrive/Desktop/School/MISM 2/GSCM/gscm-bluestar/geocoded_zips.csv")


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
  slice_min(distance, n=1) #%>% 
  #select(-c('lat', 'dest_lon', 'dest_lat','lng'))

#replace NAs in original distance calculation with new ones
shortest_pairs <- shortest_pairs %>% left_join(missing_distances, by = c('origin_zip' = "origin_zipcode", "dest_zip_clean" = "dest_zip")) %>% 
  mutate(distance.x = if_else(is.na(distance.x) == TRUE, distance.y, distance.x)) %>% 
  rename(distance = distance.x) %>% 
  select(-distance.y)

#get just the shortest distance between each origin/dest pair
shortest_pairs <- shortest_pairs %>% 
  group_by(dest_zip_clean) %>% 
  slice_min(n=1, order_by = distance)

merged_df %>% count(dest_zip == "01020")
430 * (49 * 16.9)
430 * (164 *2.07)
430 * (446 * 1.44)

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
  summarize(LTL_price_per_mile = mean(freight_paid / miles / (weight /100))) %>% 
  arrange(LTL_price_per_mile)

#calulcate rates based on distance groupings to find cheapest distance
binned_distances <- merged_df %>% 
  mutate(distance_bins = if_else(miles < 50, "<50", if_else(miles < 100, "50-100", if_else(miles<250, "100-250", "250+")))) %>%
  mutate(rate = if_else(carrier_type == "LTL", (freight_paid/miles/(weight/100)), freight_paid/miles)) %>% 
  group_by(distance_bins) 
  
binned_rates <- binned_distances %>% 
  summarize(avg_rate = mean(rate))

mile_values = seq(0,500, by=5)

short_cost <- if_else(mile_values < 50, mile_values * (binned_rates %>% filter(distance_bins == "<50") %>% select(avg_rate) %>% pull(avg_rate)), NA) 
medium_short_cost <- if_else(mile_values >=50 & mile_values < 100, mile_values * (binned_rates %>% filter(distance_bins == "50-100") %>% select(avg_rate) %>% pull(avg_rate)), NA) 
medium_long_cost <- if_else(mile_values >=100 & mile_values < 250, mile_values * (binned_rates %>% filter(distance_bins == "100-250") %>% select(avg_rate) %>% pull(avg_rate)) , NA)
long_cost <- if_else(mile_values >= 250, mile_values * (binned_rates %>% filter(distance_bins == "250+") %>% select(avg_rate) %>% pull(avg_rate)), NA) 

costs = tibble(miles = mile_values, short_cost = short_cost, medium_short_cost, medium_long_cost, long_cost)

costs_binned_range <- costs %>% 
  ggplot(mapping = aes(x = miles))+
  geom_line(aes(y = short_cost), color = "red")+
  geom_line(aes(y = medium_short_cost), color = "green")+
  geom_line(aes(y = medium_long_cost), color = "blue")+
  geom_line(aes(y = long_cost), color = "purple")+
  scale_x_continuous(breaks = seq(0,500, by = 10), labels = seq(0,500, by =10))

binned_distances %>% 
  ggplot(mapping = aes(x =miles, y = rate, color = distance_bins))+
  geom_point()

#graph to show rate variance
merged_df %>%
  filter(carrier_type == "AIR") %>% 
  ggplot(mapping = aes(x = (freight_paid/miles), color = carrier_type)) +
  geom_boxplot()+
  facet_grid(~ carrier_type, scales='free')

merged_df %>%
  filter(carrier_type == "TL" & (freight_paid/miles) < 5) %>% 
  ggplot(mapping = aes(x = (freight_paid/miles), color = carrier_type)) +
  geom_boxplot()+
  facet_grid(~ carrier_type, scales='free')
merged_df %>%
  filter(carrier_type == "LTL" & (freight_paid/miles/(weight/100)) <1) %>% 
  ggplot(mapping = aes(x = (freight_paid/miles/ (weight/100)), color = carrier_type)) +
  geom_boxplot()+
  facet_grid(~ carrier_type, scales='free')

merged_df %>% 
  group_by(scac) %>% 
  filter(carrier_type == "TL") %>% 
  mutate(rate = (freight_paid/miles)) %>% 
  summarize(rate_variance = var(rate),
            std_dev = sd(rate)) %>% 
  arrange(desc(std_dev)) %>% 
  ggplot(mapping = aes(x = scac, y = std_dev))+
  geom_point()

#carrier proliferations

#Z Scores?

#Variance based on carrier type
merged_df %>% 
  group_by(carrier_type) %>% 
  mutate(rate = if_else(carrier_type == "LTL", (freight_paid/miles/(weight/100)), freight_paid/miles)) %>% 
  mutate(zRate = scale(rate, center=TRUE, scale=TRUE)) %>% 
  summarize(rate_std_dev =sd(rate), mean_rate = mean(rate))

# Total variance
merged_df %>% 
  mutate(rate = if_else(carrier_type == "LTL", (freight_paid/miles/(weight/100)), freight_paid/miles)) %>% 
  mutate(zRate = scale(rate, center=TRUE, scale=TRUE)) %>% 
  summarize(rate_std_dev =sd(rate), mean_rate = mean(rate))

rate_variance_TL <-  merged_df %>% 
  filter(carrier_type %in% c("TL")) %>% 
  mutate(rate = freight_paid/miles) %>% 
  #filter(rate <250) %>% 
  group_by(scac) %>% 
  #filter(n() > 100) %>% 
  ggplot(aes(x = miles, y = log(rate))) +
  geom_point(aes(color = weight))+
  #geom_smooth(method="lm")+
  labs(
    title="Log of Rate and Miles, colored by Weight (TL Shipments)",
    x = "Miles",
    y = "Log(Rate)",
    color = "Weight"
  )+theme_bw()


rate_variance_LTL <- merged_df %>% 
  filter(carrier_type %in% c("LTL")) %>% 
  mutate(rate = freight_paid/miles/(weight/100)) %>% 
  #filter(rate <100 &  miles < 6000) %>% 
  group_by(scac) %>% 
  filter(n() > 100) %>% 
  ggplot(aes(x = miles, y = log(rate))) +
  geom_point(aes(color = weight))+
  #geom_smooth(method = "lm")+
  labs(
    title="Log of Rate and Miles, colored by Weight (LTL Shipments)",
    x = "Miles",
    y = "Log(Rate)",
    color = "Weight"
  )+theme_bw()

rate_variance_AIR <- merged_df %>% 
  filter(carrier_type %in% c("AIR")) %>% 
  mutate(rate = freight_paid/miles) %>% 
  #filter(rate <250) %>% 
  group_by(scac) %>% 
  filter(n() > 100) %>% 
  ggplot(aes(x = miles, y = log(rate))) +
  geom_point(aes(color = weight))+
  #geom_smooth(method="lm", se=FALSE)
  labs(
    title="Log of Rate and Miles, colored by Weight (Air Shipments)",
    x = "Miles",
    y = "Log(Rate)",
    color = "Weight"
  )+theme_bw()
#calculates average cost per mile for LTL
LTL_avg_ppm <- LTL_carriers_ppm %>% 
  summarize(avg_ppm = mean(LTL_price_per_mile))

LTL_min_ppm <- LTL_carriers_ppm %>% 
  summarize(min_ppm = min(LTL_price_per_mile))

#calculates average cost per mile for TL
TL_avg_ppm <- TL_carriers_ppm %>% 
  summarize(avg_ppm = mean(TL_price_per_mile))

TL_min_ppm <- TL_carriers_ppm %>% 
  summarize(min_ppm = min(TL_price_per_mile))
#generates truck weight values
weight_values = seq(0,45000, by=100)
#grid of LTL costs
avg_cost_of_LTL <- (weight_values /100) * 500 * LTL_avg_ppm$avg_ppm
min_cost_of_LTL <- (weight_values /100) * 500 * LTL_min_ppm$min_ppm
#grid of TL costs
avg_cost_of_TL <-  (weight_values / weight_values) * 500 * TL_avg_ppm$avg_ppm
min_cost_of_TL <-  (weight_values / weight_values) * 500 * TL_min_ppm$min_ppm

avg_costs_for_500_miles <- tibble(weight = weight_values, LTL_cost = avg_cost_of_LTL, TL_cost = avg_cost_of_TL)
min_costs_for_500_miles <- tibble(weight = weight_values, LTL_cost = min_cost_of_LTL, TL_cost = min_cost_of_TL)

avg_intersection_weight <- avg_costs_for_500_miles$weight[which.min(abs(avg_costs_for_500_miles$LTL_cost - avg_costs_for_500_miles$TL_cost))]
avg_intersection_y <- avg_costs_for_500_miles$LTL_cost[which.min(abs(avg_costs_for_500_miles$LTL_cost - avg_costs_for_500_miles$TL_cost))]

min_intersection_weight <- min_costs_for_500_miles$weight[which.min(abs(min_costs_for_500_miles$LTL_cost - min_costs_for_500_miles$TL_cost))]
min_intersection_y <- min_costs_for_500_miles$LTL_cost[which.min(abs(min_costs_for_500_miles$LTL_cost - min_costs_for_500_miles$TL_cost))]

avg_cost_plot <- avg_costs_for_500_miles %>% 
  ggplot(mapping =  aes(x = weight)) +
  geom_line(aes(y = LTL_cost, color = "red"))+
  geom_line(aes(y = TL_cost, color = "blue"))+
  geom_point(x = avg_intersection_weight, y = avg_intersection_y, size = 3)+
  annotate("text", x = avg_intersection_weight, y = avg_intersection_y, label = paste("Weight = ", avg_intersection_weight, "lbs"), vjust= 1.25, hjust=-0.1)+
  labs(
    x= "Shipment Weight (lbs)",
    y = "Cost of Shipment in USD",
    title = "Cost of Shipment by Weight, LTL and TL (Average, 500 Miles)"
  )+
  scale_color_manual(name = "Type of Shipment", values = c("red" = "red", "blue" = 'blue'), labels = c('TL Shipment', "LTL Shipment"))

min_cost_plot <- min_costs_for_500_miles %>% 
  ggplot(mapping = aes(x = weight)) +
  geom_line(aes(y = LTL_cost, color = "red"))+
  geom_line(aes(y = TL_cost, color = "blue"))+
  geom_point(x = min_intersection_weight, y = min_intersection_y, size = 3)+
  annotate("text", x = min_intersection_weight, y = min_intersection_y, label = paste("Weight = ", min_intersection_weight, "lbs"), vjust= 1.25, hjust=-0.1)+
  labs(
    x= "Shipment Weight (lbs)",
    y = "Cost of Shipment in USD",
    title = "Cost of Shipment by Weight, LTL and TL (Minimum, 500 Miles)"
  )+
  scale_color_manual(name = "Type of Shipment", values = c("red" = "red", "blue" = 'blue'), labels = c('TL Shipment', "LTL Shipment"))

merged_df %>% 
  filter(scac == "TWEX") %>% 
  select(freight_paid, miles, weight) %>% 
  arrange(freight_paid)


# Investigating best carriers

# Getting best TL carrier
good_tl_carriers <- merged_df %>%
  filter(carrier_type == "TL") %>%
  group_by(scac, carrier_type) %>%
  summarize(
    shipments = n(),
    on_time_rate = mean(on_time),
    complete_rate = mean(delivered_complete),
    undamaged_rate = mean(damage_free),
    billed_accurate_rate = mean(billed_accurately),
    avg_rate = mean(rate), #(mean(freight_paid)/ mean(miles))
    quality = ((on_time_rate + complete_rate + undamaged_rate + billed_accurate_rate) /4)
  ) %>%
  arrange(desc(quality), avg_rate) %>%
  filter(shipments > 25)
#arrange(desc(complete_rate), desc(undamaged_rate), desc(billed_accurate_rate)) %>%
#print(n=50)
good_tl_carriers %>% arrange(desc(shipments))

# Plotting TL Carriers
best_tl_carriers_plot <- good_tl_carriers %>%
  ggplot(mapping = aes(x = avg_rate, y = quality))+
  geom_point(mapping = aes(color = scac))+
  geom_smooth(method = 'lm', se = FALSE)+
  labs(
    title= "Relationship of Average Quality and Average Rate for TL Carriers",
    color = "Carrier",
    x = "Average Rate",
    y = "Average Quality"
  )

best_tl_carriers_plot

# Getting best LTL carriers
good_ltl_carriers <- merged_df %>%
  filter(carrier_type == "LTL") %>%
  group_by(scac, carrier_type) %>%
  summarize(
    shipments = n(),
    on_time_rate = mean(on_time),
    complete_rate = mean(delivered_complete),
    undamaged_rate = mean(damage_free),
    billed_accurate_rate = mean(billed_accurately),
    avg_rate =mean(rate),#(mean(freight_paid)/ mean(miles) / mean(weight/100)),
    quality = ((on_time_rate + complete_rate + undamaged_rate + billed_accurate_rate) /4)
  ) %>%
  arrange(desc(quality), avg_rate) %>%
  filter(shipments > 25)
#arrange(desc(complete_rate), desc(undamaged_rate), desc(billed_accurate_rate)) %>%
#print(n=50)
good_ltl_carriers %>% arrange(desc(shipments))

# Plotting LTL carriers
best_ltl_carriers_plot <- good_ltl_carriers %>%
  ggplot(mapping = aes(x = avg_rate, y = quality))+
  geom_point(mapping = aes(color = scac))+
  geom_smooth(method='lm', se = FALSE)+
  labs(
    title= "Relationship of Average Quality and Average Rate for LTL Carriers",
    color = "Carrier",
    x = "Average Rate",
    y = "Average Quality"
  )

best_ltl_carriers_plot

# Getting best air carrier
good_air_carriers <- merged_df %>%
  filter(carrier_type == "AIR") %>%
  group_by(scac, carrier_type) %>%
  summarize(
    shipments = n(),
    on_time_rate = mean(on_time),
    complete_rate = mean(delivered_complete),
    undamaged_rate = mean(damage_free),
    billed_accurate_rate = mean(billed_accurately),
    avg_rate = mean(rate),#(mean(freight_paid)/ mean(miles) / mean(weight/100)),
    quality = ((on_time_rate + complete_rate + undamaged_rate + billed_accurate_rate) /4)
  ) %>%
  arrange(desc(quality), avg_rate)
#filter(shipments > 25) %>%
#arrange(desc(complete_rate), desc(undamaged_rate), desc(billed_accurate_rate)) %>%
#print(n=50)


# Plotting air carriers
best_air_carriers_plot <- good_air_carriers %>%
  ggplot(mapping = aes(x = avg_rate, y = quality))+
  geom_point(mapping = aes(color = scac))+
  geom_smooth(method='lm', se = FALSE)+
  labs(
    title= "Relationship of Average Quality and Average Rate for AIR Carriers",
    color = "Carrier",
    x = "Average Rate",
    y = "Average Quality"
  )

best_air_carriers_plot


# Given rate, quality, and a proven number of shipments, these companies are preferred for each carrier_type:

# Best TL Carriers: MER1, CRSE, WSKT, HJBT, FTPC

# Best LTL Carriers: SMTL, YFSY, WWAT, RETL, PITD

# Best AIR Carrier: EUSA


# Confirmation of LTL Carrier choice
# Calculate the mean weight for LTL shipments and sort by SCAC
ltl_mean_weight_by_scac <- merged_df %>%
  filter(carrier_type == "LTL") %>%
  group_by(scac) %>%
  summarise(mean_weight = mean(weight, na.rm = TRUE)) %>%
  arrange(scac)

# View the results
print(ltl_mean_weight_by_scac)


# Mean rate calculation by carrier_type
mean_rate_by_carrier_type <- merged_df %>%
  group_by(carrier_type) %>%
  summarise(mean_rate = mean(rate, na.rm = TRUE))

# View the results
print(mean_rate_by_carrier_type)



# Preferred TL companies mean rate

# List of specific SCACs
specific_scacs <- c("MER1", "WSKT", "FTPC", "FAKF")#"HJBT","CRSE")

# Filter merged_df for the specific SCACs and calculate the mean rate
mean_rate_specific_scacs_TL <- merged_df %>%
  filter(scac %in% specific_scacs) %>%
  summarise(mean_rate = mean(rate, na.rm = TRUE))

# View the result
print(mean_rate_specific_scacs_TL)


# Preferred LTL companies mean rate
library(dplyr)

# List of specific SCACs
specific_scacs <- c("SMTL", "WWAT", "YFSY")#, "RETL", "PITD")

# Filter merged_df for the specific SCACs and calculate the mean rate
mean_rate_specific_scacs_LTL <- merged_df %>%
  #group_by(scac) %>% 
  filter(scac %in% specific_scacs) %>%
  summarise(mean_rate = mean(rate, na.rm = TRUE))

# View the result
print(mean_rate_specific_scacs_LTL)


# Preferred AIR companies mean rate

library(dplyr)

# List of specific SCACs
specific_scacs <- c("EUSA")

# Filter merged_df for the specific SCACs and calculate the mean rate
mean_rate_specific_scacs_AIR <- merged_df %>%
  filter(scac %in% specific_scacs) %>%
  summarise(mean_rate = mean(rate, na.rm = TRUE))

# View the result
print(mean_rate_specific_scacs_AIR)



# Cost estimate code

# Assuming merged_df is your data frame
total_freight_by_carrier_type <- merged_df %>%
  group_by(carrier_type) %>%
  summarise(total_freight = sum(freight_paid, na.rm = TRUE))

# View the results
print(total_freight_by_carrier_type)


# Cost Savings statements if we used preferred providers
merged_df %>% 
  group_by(carrier_type) %>% 
  mutate(optimized_rate_freight_paid = if_else(carrier_type == "LTL", miles * (weight/100) * pull(mean_rate_specific_scacs_LTL), if_else(carrier_type == "TL", miles * pull(mean_rate_specific_scacs_TL), miles * pull(mean_rate_specific_scacs_AIR)))) %>% 
  #select(carrier_type, freight_paid, optimized_rate_freight_paid, miles, weight)
  mutate(unoptimized_rate_freight_paid = if_else(carrier_type == "LTL", 0.542 * miles * (weight/100), if_else(carrier_type == "TL", miles * 6.83, miles * 10.7))) %>% 
  summarise(optimized_freight_paid = sum(optimized_rate_freight_paid, na.rm = TRUE),
            unoptimized_freight_paid = sum(unoptimized_rate_freight_paid),
            savings = unoptimized_freight_paid - optimized_freight_paid)


# TL

# By using the lower rate of $6.43 for TL shipments calculated from the mean rate of the preferred providers, the total cost was 
# $19,702,587.59, approximately $1,225,666.41 would have been saved compared to the total freight cost of $20,928,254 incurred at 
# the mean rate of $6.83.

# LTL

# By applying the new rate of $0.211, the total freight charges would have been approximately $29,245,982.40 instead of the $75,124,751 
# paid at the mean rate of $0.542. This means we would have saved approximately $45,878,768.60 if the new rate had been applied to the 
# previous charges.

# AIR

# By applying the lower rate of $3.19 for AIR shipments, the total freight cost would have been approximately $1,457,349.71. This means 
# we would have saved about $3,430,939.29 if the new rate of 3.19 had been used instead of the actual mean rate of 10.7 which had a total
# cost of $4,888,289.


(1225666.41 + 45878768.60 + 3430939.29)

# Total savings by prioritizing better companies is $50,535,374


# added by Derek



data("zip_code_db")


unique_origin_zips <- merged_df %>% pull(origin_zip) %>% unique() %>% na.omit()
unique_dest_zips <- merged_df %>% pull(dest_zip_clean) %>% unique() %>% na.omit()

# Create a data frame of unique zip codes from your dataset for both origin and destination
origin_zip_df <- data.frame(origin_zip = unique_origin_zips)
dest_zip_df <- data.frame(dest_zip_clean = unique_dest_zips)

# Merge with zip_code_database to get lat and long
origin_coordinates <- left_join(origin_zip_df, zip_code_db, by = c("origin_zip" = "zipcode"))
dest_coordinates <- left_join(dest_zip_df, zip_code_db, by = c("dest_zip_clean" = "zipcode"))

# Select only the relevant columns and rename them for clarity
origin_coordinates <- origin_coordinates %>% select(origin_zip, origin_lat = lat, origin_long = lng)
dest_coordinates <- dest_coordinates %>% select(dest_zip_clean, dest_lat = lat, dest_long = lng)

# Merge the coordinates back into the main dataframe
merged_df <- merged_df %>%
  left_join(origin_coordinates, by = 'origin_zip') %>%
  left_join(dest_coordinates, by = 'dest_zip_clean')


# Join shortest_pairs with latitude and longitude coordinates


origin_coordinates <- zip_code_db %>%
  select(zipcode, lat, lng) %>%
  rename(origin_zip = zipcode, lat = lat, lng = lng)

dest_coordinates <- zip_code_db %>%
  select(zipcode, lat, lng) %>%
  rename(dest_zip_clean = zipcode, dest_lat = lat, dest_long = lng)

# Merge latitude and longitude information
shortest_pairs <- shortest_pairs %>%
  left_join(origin_coordinates, by = "origin_zip") %>%
  left_join(dest_coordinates, by = "dest_zip_clean")

shortest_pairs <- shortest_pairs %>%
  mutate(origin_lat = ifelse(is.na(lat.x), lat.y, lat.x),
         origin_long = ifelse(is.na(lng.x), lng.y, lng.x),
         dest_lat = ifelse(is.na(dest_lat.x), dest_lat.y, dest_lat.x),
         dest_long = ifelse(is.na(dest_lon), dest_long, dest_lon))


# Select only the required columns
shortest_pairs_final <- shortest_pairs %>%
  select(origin_zip, dest_zip_clean, distance, origin_lat, origin_long, dest_lat, dest_long)




shortest_pairs_final <- shortest_pairs_final %>% 
  rename(optimized_origin_zip = origin_zip, 
         optimized_origin_lat = origin_lat, 
         optimized_origin_long = origin_long)

# Now join this data with merged_df based on dest_zip_clean
merged_df <- merged_df %>%
  left_join(shortest_pairs_final %>% 
              select(dest_zip_clean, optimized_origin_zip, optimized_origin_lat, optimized_origin_long),
            by = 'dest_zip_clean')

count_of_optimized_shipments <- merged_df %>% group_by(optimized_origin_zip, dest_zip_clean) %>% 
  summarize(num_shipments = n())

total_milage_optimized <- count_of_optimized_shipments %>% left_join((shortest_pairs_final %>% select(-c(optimized_origin_lat:dest_long))), by= c('optimized_origin_zip', "dest_zip_clean")) %>% 
  mutate(distance = if_else(distance == 0, 10, distance)) %>% 
  mutate(total_milage = num_shipments * distance)

mean_rate <- merged_df %>% 
  filter(carrier_type == "TL") %>% 
  summarize(mean_rate = mean(freight_paid/miles)) %>% 
  pull(mean_rate)

total_milage_optimized %>% 
  mutate(total_cost = total_milage * mean_rate) %>% 
  ungroup() %>% 
  summarize(
    gross_cost = sum(total_cost)
  )
merged_df %>% 
  ungroup() %>% 
  summarize(gross_cost = sum(freight_paid))


# Fill missing destination lat/long





# Your existing analysis code goes here...

# Export the dataframe with the new latitude and longitude columns

write_csv(merged_df, "optimized_routing.csv")

