rm(list = ls())

#-----------------------
#Data Processing
#------------------------


### Required package at first
packages <- c("tidyverse", "lubridate", "readxl", "stringr")
installed <- packages[!(packages %in% installed.packages()[,"Package"])]
if (length(installed)) install.packages(installed)
lapply(packages, library, character.only = TRUE)


### Working directories
dir <- list()
dir$root <- dirname(getwd()) # root directory of the project

# figures directory
dir.create(paste0(dir$root, "/figures/"))
dir$fig <- paste0(dir$root, "/figures/")
# unprocessed data folder
dir.create(paste0(dir$root, "/data/unprocessed data/"))
dir$rawdata <- paste0(dir$root, "/data/unprocessed data/")
# clean data folder
dir.create(paste0(dir$root, "/data/clean data/"))
dir$clndata <- paste0(dir$root, "/data/clean data/")



# Import the datasets--------------------------
hay_data <- read_excel(file.path(dir$rawdata, "base_dataset.xlsx")) %>%
  rename(date = ReportPeriod) %>%
  filter(str_detect(Commodity, "Hay"), Class == "Alfalfa")

# View the structure of the imported dataset
str(hay_data)
head(hay_data)
colnames(hay_data)


# Keep only specified package types and states
wanted_packages <- c("Large Square 3x4", "Large Square 4x4", "Small Square 3 Tie", "Small Square")
wanted_states <- c("Arizona", "Idaho", "Oregon","Montana", "Colorado", "Utah", "California", "Washington", "New Mexico")

hay_data <- hay_data %>%
  filter(Package %in% wanted_packages) %>%
  filter(state %in% wanted_states)

#------------------------------------------------------------------------
# Dependent Variable: Assign Average Price Based on unit and convert to $/ton
#------------------------------------------------------------------------

#check unique category of price unit
unique(hay_data$PriceUnit)
#remove uncommon price unit for better analysis
hay_data <- hay_data %>%
  filter(PriceUnit != "Per Point/Ton")

# Convert Price to Per Ton
bale_weights <- c("Small Square" = 50, "Large Square 3x4" = 1300, "Large Square 4x4" = 1600, "Small Square 3 Tie" = 120)
convert_to_per_ton <- Vectorize(function(price, package) {
  if (!is.na(package) && package %in% names(bale_weights)) {
    2000 / bale_weights[[package]] * price
  } else NA_real_
})

# Apply conversion function to price
hay_data <- hay_data %>%
  mutate(
    priceAvgPerTon = case_when(
      PriceUnit == "Per Bale" ~ convert_to_per_ton(PriceAvg, Package),
      PriceUnit == "Per Ton"  ~ PriceAvg,
      TRUE                    ~ NA_real_
    )
  )


# Round to 2 decimal places
hay_data$priceAvgPerTon <- round(hay_data$priceAvgPerTon, 2)  



#                     -----------------------------------------
# Independent Variables:
# Add data set for each factor : 
# Environmental variables: Temperature , Precipitation , Drought
# Supply side variables : Quality characteristics , Packaging type
# Demand Side Variables : Export Quantity, Corn Price, Feeder Cattle Price
#                    -------------------------------------------


#------------------------------------------------------------------------------------------
## Merge Environmental Data (Drought, Precipitation, Temperature)
#-------------------------------------------------------------------------------------------
#import datasets

drought <- read.csv(file.path(dir$rawdata, "drought DSCI.csv")) %>%
  mutate(date = as.Date(as.character(MapDate), format = "%Y%m%d"),
         YearMonth = floor_date(date, "month")) %>%
  group_by(Name, YearMonth) %>%
  summarise(AvgDSCI = mean(DSCI, na.rm = TRUE), .groups = "drop")

precip <- read.csv(file.path(dir$rawdata, "monthly precip.csv")) %>%
  pivot_longer(-c(year, month), names_to = "state", values_to = "precipitation") %>%
  mutate(YearMonth = ymd(paste(year, month, "01", sep = "-")))

temp <- read.csv(file.path(dir$rawdata, "monthly temp.csv")) %>%
  pivot_longer(-c(year, month), names_to = "state", values_to = "temperature") %>%
  mutate(YearMonth = ymd(paste(year, month, "01", sep = "-")))

# Fix inconsistent state names
precip$state <- gsub("New.Mexico", "New Mexico", precip$state)
temp$state <- gsub("New.Mexico", "New Mexico", temp$state)

# Merge into Hay Dataset
hay_data <- hay_data %>%
  mutate(YearMonth = floor_date(as.Date(date), "month")) %>%
  left_join(drought, by = c("state" = "Name", "YearMonth")) %>%
  left_join(precip, by = c("state", "YearMonth")) %>%
  left_join(temp, by = c("state", "YearMonth"))

sum(is.na(hay_data$temperature))   #check for missing value after merge


#----------------------------------------------------------------------------------
# Merge Market Data (Export Quantity, Corn Price, Feeder Cattle Price)
#----------------------------------------------------------------------------------

#----Export Quantity----
export_q <- read_csv(file.path(dir$rawdata, "export china quantity.csv")) %>%
  pivot_longer(-Year, names_to = "month_name", values_to = "Export_Quantity") %>%
  mutate(month = match(month_name, month.name),
         YearMonth = ymd(paste(Year, month, "01", sep = "-"))) %>%
  select(YearMonth, Export_Quantity)

export_q$Export_Quantity <- export_q$Export_Quantity/1000



# ---- Stock (inventory) ----
stock <- read_csv(file.path(dir$rawdata, "stock.csv")) %>%
  mutate(YearMonth = ymd(paste(year, month, "01", sep = "-")),
         stock = `stock1000tons`) %>%
  select(state, YearMonth, stock)


# ---- Corn  ----
corn <- read.csv(file.path(dir$rawdata, "corn price.csv"))
colnames(corn)[1:2] <- c("MonthRaw", "CornPrice")
corn <- corn %>%
  filter(!is.na(MonthRaw) & MonthRaw != "-") %>%
  mutate(
    Month = as.Date(paste0("01-", MonthRaw), format = "%d-%y-%b"),
    Month = format(Month, "%Y-%m")
  ) %>%
  select(Month, CornPrice)

# ---- Feeder Cattle  ----
feeder <- read.csv(file.path(dir$rawdata, "feederprice.csv"))
colnames(feeder)[1:2] <- c("MonthRaw", "feederPrice")
# 3. Parse Month and clean
feeder <- feeder %>%
  filter(!is.na(MonthRaw) & MonthRaw != "-") %>%
  mutate(
    Month = as.Date(paste0("01-", MonthRaw), format = "%d-%b-%y"),  # e.g., 01-Nov-89
    Month = format(Month, "%Y-%m")
  ) %>%
  select(Month, feederPrice)


# ---- Merge All into hay_data ----

# Add Month for proper merging
hay_data <- hay_data %>%
  mutate(Month = format(YearMonth, "%Y-%m"))


#merge everything
hay_data <- hay_data %>%
  left_join(export_q, by = "YearMonth") %>%
  left_join(stock, by = c("state", "YearMonth")) %>%
  left_join(corn, by = "Month") %>%
  left_join(feeder, by = "Month")


hay_data <- subset(hay_data, select = -c(month.y, year.y, CropAge,Use))  #delete unnecessary column



write.csv(hay_data, file = paste0(dir$clndata, "hay data with outliers.csv"), row.names = FALSE)


#-----------------------------------------------------------------------
# Outlier Detection and Removal
#-----------------------------------------------------------------------

#Check the data for any outlier of alfalfal price
boxplot(hay_data$priceAvgPerTon, main = "Boxplot of Alfalfa Price per Ton",
        ylab = "Price ($/Ton)", col = "lightgreen")


#Compute IQR
iqr_bounds <- quantile(hay_data$priceAvgPerTon, probs = c(0.25, 0.75), na.rm = TRUE)
iqr_range <- diff(iqr_bounds)
lower <- unname(iqr_bounds[1] - 1.5 * iqr_range)
upper <- unname(iqr_bounds[2] + 1.5 * iqr_range)

hay_clean <- hay_data %>%
  filter(dplyr::between(priceAvgPerTon, lower, upper))

#filter data based on outliers
hay_clean <- hay_data %>%
  filter(between(priceAvgPerTon, lower, upper))

#check if outliers are removed 
boxplot(hay_clean$priceAvgPerTon, main = "Boxplot of Alfalfa Price per Ton",
        ylab = "Price ($/Ton)", col = "lightgreen")

#---------------------------Export clean data-------------------------------
write.csv(hay_clean, file = paste0(dir$clndata, "hay data without outliers.csv"), row.names = FALSE)

# ---- Subset and Export by Production Type ----

write.csv(hay_clean %>% filter(Conventional == "Conventional"),
          file.path(dir$clndata, "hay_conventional.csv"), row.names = FALSE)

write.csv(hay_clean %>% filter(Conventional != "Conventional"),
          file.path(dir$clndata, "hay_organic.csv"), row.names = FALSE)

#End code------------------------------------------------------------------


