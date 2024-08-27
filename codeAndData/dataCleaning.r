require(dplyr)
require(DataExplorer)
require(readxl)
require(here)

# todo
# - Clean out empty price
# - generalize format of size
# - impute missing values with dataExplorer
# - mutate and clean with functions


data <- read.csv(here("dataset", "location_carpark_type_filtered.csv"))

#initial analysis
"summary"
summary(data) # nolint
"class" # nolint
class(data) # nolint
"first 10" # nolint
head(data, 10) # nolint
"last 5" # nolint
tail(data, 5) # nolint
"Number of rows" # nolint
nrow(data) # nolint
"Number of columns" # nolint
ncol(data) # nolint
"Dims" # nolint
dim(data) # nolint
"Column names" # nolint
colnames(data) # nolint
glimpse(data) # nolint

#create a variable to check all the different types of property
property_type <- data %>% 
  group_by(Property.Type) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency))
#impute property type with NA values with "NA"
data <- replace(data, data =='', NA)

proptype <- data %>% 
  group_by(Location,Property.Type) %>%
  summarise(frequency = n()) %>%
  arrange(Location, desc(frequency))
mode_property_type <- proptype %>%
  group_by(Location) %>%
  filter(frequency == max(frequency))
nrow(mode_property_type)
# Keep only the first mode for each Location
mode_property_type <- mode_property_type %>%
  group_by(Location) %>%
  slice(1)%>%
  select(Location, Property.Type)
nrow(mode_property_type)
# Join the data frames
data <- left_join(data, mode_property_type, by = "Location", suffix = c("", ".mode"))

# Replace NA values in Property.Type with the mode of Property.Type for each Location
data[is.na(data$Property.Type),]
data$Property.Type[is.na(data$Property.Type)] <- data$Property.Type.mode[is.na(data$Property.Type)]

# Remove the extra column
data$Property.Type.mode <- NULL
data[is.na(data$Property.Type),]

#impute property type with NA string with mode of property type for each location
write.csv(data, here("dataset", "location_carpark_type_filtered.csv"))
