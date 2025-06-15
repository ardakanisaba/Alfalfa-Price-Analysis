# This repository contains all R scripts and data-processing workflows used in the analysis for the paper:"Quantifying Drought’s Role in Alfalfa Price Formation: A Multi-Factor Empirical Study of the Western U.S."

# -----------------------Repository Srtucture----------------------#

├── Code/
│   ├── data processing.R                  # Data cleaning, merging, and transformation
│   ├── Figures.R                          # Script for generating publication-ready figures
│   └── Summary statistics and Regression.R  # Summary stats + econometric modeling

├── Data/
│   ├── Climate Engine Data/              # (Optional sub-data source)
│   ├── alfalfa historical trends.csv
│   ├── alfalfa price trend (1990–2024).csv
│   ├── alfalfa production historical trends.csv
│   ├── base_dataset.xlsx
│   ├── corn price.csv
│   ├── drought DSCI.csv
│   ├── export china quantity.csv
│   ├── feederprice.csv
│   ├── monthly precip.csv
│   ├── monthly temp.csv
│   ├── production-yield-area for 4 crops.csv
│   └── stock.csv

# ------------------ Scripts Description--------------------------#
## data processing.R
### Loads, cleans, and merges raw datasets:
###USDA hay market reports
###Drought Severity and Coverage Index (DSCI)
###Climate data (precipitation, temperature)
###Corn and feeder cattle prices
###Export volumes(China) and inventory data

#Converts all price values to a standardized $/ton basis
#Removes price outliers using IQR method
#Produces cleaned datasets for modeling and visualization


## figures.R
##Generates all exploratory and presentation-ready figures, including:
###Alfalfa production value comparison
###Western U.S. production share
###State-level historical production trends
###Alfalfa price trend over time 
###Price variation across states 
###Price–export and price–drought relationships
###Packaging/quality effects on price 


## Sumaary statistics and Regression models.R
##Generates summary statistics

##Runs multiple regression models:
###Baseline price model
###Interaction models (drought × export, drought × quality, etc.)



# ------------------ Scripts Description--------------------------#

# Raw Data Files:
##base_dataset.xlsx: USDA hay market prices ( Direct Reports)
##drought DSCI.csv: Drought Severity and Coverage Index (Drought Monitor)
##monthly precip.csv, monthly temp.csv: Climate Engine precipitation and temperature data
##corn price.csv, feederprice.csv: Market prices for feed substitutes(USDA)
##export china quantity.csv: Alfalfa export volumes to China(USDA FAS)
##stock.csv: Inventory data by state(USDA NASS)
##production-yield-area for 4 crops.csv: Comparative production value(USDA NASS)
#historical alfalfa trend ( production and price ): USDA NASS



