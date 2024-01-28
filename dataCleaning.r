require(dplyr)
require(DataExplorer)
require(readxl)

# todo
# - Clean out empty price
# - generalize format of size
# - impute missing values with dataExplorer
# - mutate and clean with functions

data <- readxl::read_excel("dataset/kl_property_data.xlsx",
  sheet = 1, 
  col_names = TRUE, 
  na = c("NA", "-"))

#initial analysis
print("summary") # nolint
print(summary(data)) # nolint
print("class\n") # nolint
print(class(data)) # nolint
print("first 10") # nolint
print(head(data, 10)) # nolint
print("last 5") # nolint
print(tail(data, 5)) # nolint
print("Number of rows") # nolint
print(nrow(data)) # nolint
print("Number of columns") # nolint
print(ncol(data)) # nolint
print("Dims") # nolint
print(dim(data)) # nolint
print("Column names") # nolint
print(colnames(data)) # nolint

#data cleaning
data <- data[complete.cases(data), ] ##remove NA
print(nrow(data))

# convert_to_square_feet <- function(measurement) {
#  # Check if the measurement is in the format 'width x height'
#  if (grepl("x", measurement)) {
#    # Extract the width and height
#    parts <- strsplit(measurement, "x")[[1]]
#    width <- as.numeric(parts[1])
#    height <- as.numeric(parts[2])
#    # Multiply the width and height to get the total area in square feet
#    area <- width * height
#  } else {
#    # If the measurement is just a number followed by 'sq. ft.', extract the number
#    area <- as.numeric(strsplit(measurement, " ")[[1]][1])
#  }
#  return(area)
# }

# data <- data %>%
#   mutate(AreaInSqFt = convert_to_square_feet(Size))
