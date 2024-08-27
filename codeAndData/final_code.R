# import packages
require(stringr)
require(dplyr)
require(tidyr)
require(stats)
require(DataExplorer)
require(here)

# clean dataset (nrow = 53883)
# load dataset (replace with ur own file path)
data = read.csv(here("kl_property_data.csv"))
# filter unwanted columns
data <- data %>% 
  select(Location, Price, Rooms, Car.Parks, Property.Type, Size)

# clean location (nrow = 53857) - Chew Jin Ni
# remove comma and words behind comma
data$Location = gsub(",.*", "", data$Location)
# make the location data lowercase
data$Location = tolower(data$Location)
# replace redundant data
data$Location = ifelse(data$Location == "taman yarl oug", "taman yarl", data$Location)
data$Location = ifelse(data$Location == "jalan u-thant", "u-thant", data$Location)
data$Location = ifelse(data$Location == "sri kembangan", "seri kembangan", data$Location)
data$Location = ifelse(data$Location == "jalan klang lama (old klang road)", "old klang road", data$Location)
data$Location = ifelse(data$Location == "bukit tunku (kenny hills)", "bukit tunku", data$Location)
data$Location = ifelse(data$Location %in% c("kl city", "city central", "kuala lumpur", "city centre"), "klcc", data$Location)
data$Location = ifelse(data$Location == "sri damansara", "bandar sri damansara", data$Location)
data$Location = ifelse(data$Location == "desa parkcity", "adiva desa parkcity", data$Location)
data[data$Location == "bukit  persekutuan", "Location"] <- "Bukit Persekutuan"
# remove area not in KL
data <- data %>%
  filter(!(Location %in% c("other", "rawang", "singapore", "landed sd", "petaling jaya", "cyberjaya")))
# make the location data to title case
data$Location = str_to_title(data$Location)
# arrange the data based on location name
data <- arrange(data, data$Location)

# clean car park (nrow = 53857) - Ho Rong Wei
# find the mean number of car parks
mean_carpark = ceiling(mean(data$Car.Parks, na.rm=TRUE))

# group the data by location, find the mean number of car park for each group of location
group_data <- data %>% 
  select(Location, Car.Parks) %>%
  mutate(Car.Parks = ifelse(is.na(Car.Parks), 0, Car.Parks)) %>%
  group_by(Location) %>% 
  summarise(
    num_of_car_parks = sum(Car.Parks, na.rm=TRUE),
    freq = n()
  ) %>% 
  mutate(
    mean_val = ceiling(num_of_car_parks / freq),
    mean_val = ifelse(mean_val == 0, mean_carpark, mean_val)
  )

# map the processed car park number to each location, filter out unwanted columns
data <- data %>%
  left_join(group_data, by = "Location") %>%
  mutate(
    Car.Parks = ifelse(is.na(Car.Parks), mean_val, Car.Parks)
  ) %>%
  select(-c(num_of_car_parks, freq, mean_val))

# clean price (nrow = 53498)
data$Price = tolower(data$Price)
data$Price <- sub("^rm", "", data$Price)
data$Price <- gsub(",", "", data$Price)
data$Price <- as.numeric(data$Price)
q1 <- quantile(as.integer(data$Price), probs = 0.25, na.rm=TRUE)
avg_price <- mean(data$Price, na.rm=TRUE)
data$Price[is.na(data$Price)] <- avg_price
data$Price <- ifelse(data$Price < q1, q1, data$Price)

data[data$Location=="Taman Duta" & data$Price == 1980000000, "Price"] <- quantile(data[data$Location == "Taman Duta", "Price"], probs = 0.75, na.rm=TRUE)
data[data$Location=="Mont Kiara" & data$Price == 1600000000, "Price"] <- quantile(data[data$Location == "Mont Kiara", "Price"], probs = 0.75, na.rm=TRUE)
data[data$Location=="Bukit Jalil" & data$Price == 1123000000, "Price"] <- quantile(data[data$Location == "Bukit Jalil", "Price"], probs = 0.75, na.rm=TRUE)
data[data$Location=="Country Heights Damansara" & data$Price == 145926000, "Price"] <- quantile(data[data$Location == "Country Heights Damansara", "Price"], probs = 0.75, na.rm=TRUE)
data[data$Location=="Pantai" & data$Price == 138000000, "Price"] <- quantile(data[data$Location == "Pantai", "Price"], probs = 0.75, na.rm=TRUE)
data[data$Location=="Titiwangsa" & data$Price == 79714800, "Price"] <- quantile(data[data$Location == "Titiwangsa", "Price"], probs = 0.75, na.rm=TRUE)
data[data$Location=="Adiva Desa Parkcity" & data$Price == 112000000, "Price"] <- quantile(data[data$Location == "Adiva Desa Parkcity", "Price"], probs = 0.75, na.rm=TRUE)

# clean property type - Damon Ng Khai Weng (nrow = 53857)
#create a variable to check all the different types of property
property_type <- data %>% 
  group_by(Property.Type) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency))
#impute property type with empty string values with NA
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
data$Property.Type[is.na(data$Property.Type)] <- data$Property.Type.mode[is.na(data$Property.Type)]

# Remove the extra column and variables
data$Property.Type.mode <- NULL
mode_property_type <- NULL
property_type <- NULL
proptype <- NULL
# clean property size - Aryssa Goh May Lynn (nrow = 53480)

#Split Size Column into 2 using tidyr 
data = separate(data, col = Size, into = c("Land.Type", "Land.Size.Sq.ft"), sep = ":")

# Create a new column Size_Unit and extract non-numeric characters
data$Size_Unit <- gsub("[^a-zA-Z. ]+", "", data$Land.Size.Sq.ft)

# Remove 'x' or 'X' from Size_Unit
data$Size_Unit <- gsub("[xX]", "", data$Size_Unit)

# Check the number of NA rows in the Size column
na_size_count <- sum(is.na(data$Land.Size.Sq.ft))
cat("Number of NA rows in Size column:", na_size_count, "\n")

# Show unique values of 'Size_Unit'
unique_size_units <- unique(data$Size_Unit)
cat("Size Units Available:\n")
for (unit in unique_size_units) {
  cat("-", unit, "\n")
}

# Perform calculation on size that have mathematical operators
# Extract numeric and symbolic characters into Land.Size.Sq.ft
data$Land.Size.Sq.ft <- gsub("[^0-9xX+\\-\\/]+", "", data$Land.Size.Sq.ft) 
# Replace 'x' or 'X' with '*' in the 'Land.Size.Sq.ft' column
data$Land.Size.Sq.ft <- gsub("[xX]", "*", data$Land.Size.Sq.ft)
# Define a function to evaluate expressions
evaluate_expression <- function(expr) {
  if (grepl("[+*/-]", expr)) {
    result <- tryCatch(eval(parse(text = gsub("[xX]", "*", expr))), error = function(e) NA)
    if (!inherits(result, "error")) {
      return(result)
    }
  }
  as.numeric(expr)
}

# Apply the function to the 'Land.Size.Sq.ft' column
data$Land.Size.Sq.ft <- sapply(data$Land.Size.Sq.ft, evaluate_expression)

# Convert Acre into Square feet
data$Land.Size.Sq.ft <- ifelse(grepl("(?i)acres?", data$Size_Unit),
                               as.numeric(gsub("[^0-9.]", "", data$Land.Size.Sq.ft)) * 43560,
                               data$Land.Size.Sq.ft)

# Convert Hectare into Square feet
data$Land.Size.Sq.ft <- ifelse(grepl("(?i)hectares?", data$Size_Unit),
                               as.numeric(gsub("[^0-9.]", "", data$Land.Size.Sq.ft)) * 107639,
                               data$Land.Size.Sq.ft)

# Convert sq. m. into Square feet
data$Land.Size.Sq.ft <- ifelse(grepl("sq\\. m\\.", data$Size_Unit, ignore.case = TRUE),
                               data$Land.Size.Sq.ft * 10.764,  
                               data$Land.Size.Sq.ft)


# Replace size that are less than q1 in every location based on property type with q1 
data <- data %>%
  group_by(Location, Property.Type) %>%
  mutate(Q1 = quantile(Land.Size.Sq.ft, 0.25, na.rm = TRUE),
         Land.Size.Sq.ft = ifelse(Land.Size.Sq.ft < Q1, Q1, Land.Size.Sq.ft)) %>%
  ungroup()


# Calculate price per sq ft
data <- data %>%
  group_by(Location) %>%
  mutate(Price_Per_SqFt = Price / Land.Size.Sq.ft) %>%
  ungroup()

# Calculate mean price per sq ft and impute into the missing values of price per sq ft
data <- data %>%
  group_by(Location) %>%
  mutate(Price_Per_SqFt = ifelse(is.na(Price_Per_SqFt), mean(Price_Per_SqFt, na.rm = TRUE), Price_Per_SqFt)) %>%
  ungroup()

# Impute the missing values (NA) in Size with the estimated Size by using Price
data <- data %>%
  mutate(Land.Size.Sq.ft = ifelse(is.na(Land.Size.Sq.ft), Price / Price_Per_SqFt, Land.Size.Sq.ft))

# Convert Land.Size.Sq.ft into integer
data$Land.Size.Sq.ft <- as.integer(data$Land.Size.Sq.ft)

# Delete rows that size is 0
data <- data %>%
  filter(Land.Size.Sq.ft>0)

# Delete rows that size is more than 37000
data <- data %>%
  filter(Land.Size.Sq.ft <= 37000) %>% 
  select(-c("Size_Unit", "Q1", "Price_Per_SqFt"))

# Impute "Unknown" into NA values in Land.Type
data$Land.Type <- ifelse(is.na(data$Land.Type), "Unknown", data$Land.Type)



# clean number of rooms- Damon Ng Khai Weng (nrow = 53480)
sum(is.na(datadup$Rooms))
#check the unique values of rooms
nrow(data$Rooms[data$Rooms == 1])
#count number of Rooms where it is 9
sum(datadup$Rooms == '20 ', na.rm = TRUE)
unique(datadup$Rooms)

#check number of rows of rooms with NA
datadup<- data
#convert all values in rooms to lowercase
datadup$Rooms <- tolower(datadup$Rooms)
#create extra column to store extra rooms data for Rooms, if contains string "+" or "+1" or " Above"then it has 1 extra room
#else if it has "+2" then it has 2 extra rooms else it has 0 extra rooms
datadup$Extra_Rooms <- ifelse(
  grepl("\\+1", datadup$Rooms), 1,
  ifelse(grepl("\\+2", datadup$Rooms), 2,0)
)
#convert records where values in rooms have + as the last character ,
#to a new value where it is the numeric value of rooms incremented by 1
datadup[grepl("\\+$", datadup$Rooms),] <- datadup[grepl("\\+$", datadup$Rooms),] %>%
  rowwise() %>%
  mutate(Rooms = as.character(as.numeric(gsub("\\+$", "", Rooms)) + 1))
#remove the characters + and any other character after + in the Rooms column
datadup$Rooms <- gsub("\\+.*", "", datadup$Rooms)
#remove the characters "above" in the Rooms column
datadup$Rooms <- gsub("above", "", datadup$Rooms)
#change studio to 1
datadup$Rooms <- gsub("studio", "1", datadup$Rooms)
#convert values in rooms to numeric
datadup$Rooms <- as.numeric(datadup$Rooms)
#convert the values in rooms to numeric
data$Rooms <- as.numeric(data$Rooms)
#find the mode for rooms in the dataset
mode <- function(records) {
  # Filter out NA values and get unique non-NA values
  unique_records <- unique(records[!is.na(records)])
  # Create a table of frequencies for each unique value in the records
  frequencies <- tabulate(match(records, unique_records))
  # Find the index of the maximum frequency
  max_frequency_index <- which.max(frequencies)
  # Return the mode
  return(unique_records[max_frequency_index])
}
#find the property types with NA values in rooms, summarize with the frequency of 
#NA values, mean, lower limit ,q1,q2,q3, upper limit and  mode of rooms for each property type,
#round off the values, replace negative values in rooms_summary with 0
rooms_summary <- datadup %>%
  group_by(Property.Type) %>%
  summarise(
    mean_val = round(mean(Rooms, na.rm = TRUE)),
    q1 = round(quantile(Rooms, 0.25, na.rm = TRUE)),
    q3 = round(quantile(Rooms, 0.75, na.rm = TRUE)),
    mode_val = mode(Rooms),
    lower_limit = round(q1 - 1.5 * IQR(Rooms, na.rm = TRUE)),
    upper_limit = round(q3 + 1.5 * IQR(Rooms, na.rm = TRUE))
  )%>%
  mutate(lower_limit = ifelse(lower_limit < 0, 0, lower_limit))
price_summary <- datadup %>%
  group_by(Property.Type) %>%
  summarise(mean_val = round(mean(Price, na.rm = TRUE)),
    q1 = round(quantile(Price, 0.25, na.rm = TRUE)),
    q3 = round(quantile(Price, 0.75, na.rm = TRUE)),
    mode_val = mode(Price),
    lower_limit = round(q1 - 1.5 * IQR(Price, na.rm = TRUE)),
    upper_limit = round(q3 + 1.5 * IQR(Price, na.rm = TRUE)))%>%
  mutate(lower_limit = ifelse(lower_limit < 0, 0, lower_limit))
#impute rooms with NA values based on the prices for each property type, if price is less than lower_limit of price, impute with room lower limit,
#else less than q1 of price, impute with room q1,
#else if price is less than mean of price, impute with room mean,
#else if price is less than q3 of price, impute with room q3,
#else if price is less than upper_limit of price, impute with room upper limit,
#else impute with room mode
datadup <- datadup %>%
  left_join(rooms_summary, by = "Property.Type") %>%
  left_join(price_summary, by = "Property.Type") %>%
  mutate(
    Rooms = ifelse(is.na(Rooms),
                   ifelse(Price < lower_limit.x, lower_limit.y,
                          ifelse(Price < q1.x, q1.y,
                                 ifelse(Price < mean_val.x, mean_val.x,
                                        ifelse(Price < q3.x, q3.x,
                                               ifelse(Price < upper_limit.x, upper_limit.x,
                                                      mode_val.x
                                               )
                                        )
                                 )
                          )
                   ), Rooms
    )
  )
#find the number of rows with NA values in rooms
sum(is.na(datadup$Rooms))
#convert the values in rooms to integer
datadup$Rooms <- as.integer(datadup$Rooms)
#remove the extra columns
datadup$mean_val.x <- NULL
datadup$q1.x <- NULL
datadup$q3.x <- NULL
datadup$mode_val.x <- NULL
datadup$lower_limit.x <- NULL
datadup$upper_limit.x <- NULL
datadup$mean_val.y <- NULL
datadup$q1.y <- NULL
datadup$q3.y <- NULL
datadup$mode_val.y <- NULL
datadup$lower_limit.y <- NULL
datadup$upper_limit.y <- NULL

data <- datadup

datadup<-NULL
nrow(data)
# compile to csv file (replace with ur own file path)
write.csv(data,here("kl_property_data_filtered.csv"))

# data_cleaned_percentage = (53883 - 53480)/ 53883 * 100 = 0.7479%