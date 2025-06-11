rm(list = ls())

### Packages (load all of them at the beginning)
want <- c("fixest", "lmtest", "sandwich", "plm", "plm", "modelsummary",
          "Officer")
need <- want[!(want %in% installed.packages()[,"Package"])]
if (length(need)) install.packages(need)
sapply(want, function(i) require(i, character.only = TRUE))
rm(want, need)
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


#import data------------------------------------------------------------
hay_data <- read.csv(file.path(dir$clndata, "hay_conventional.csv"))


#-----------------------------------------------------------------------
#summary statistics
#-----------------------------------------------------------------------
# Identify variable types
is_cat <- sapply(hay_data, is.factor) | sapply(hay_data, is.character)
is_num <- sapply(hay_data, is.numeric)

# Separate variables
data_cont <- hay_data[, is_num]
data_cat <- hay_data[, is_cat]

# === STEP 4: Summary table for continuous variables ===
cont_summary <- describe(data_cont)
cont_summary <- cbind(Variable = rownames(cont_summary), cont_summary)
rownames(cont_summary) <- NULL


cat_summary <- lapply(data_cat, function(x) {
  freq_table <- as.data.frame(table(x))
  colnames(freq_table) <- c("Category", "Frequency")
  freq_table$Percent <- round(100 * freq_table$Frequency / sum(freq_table$Frequency), 1)
  freq_table$Variable <- deparse(substitute(x))
  return(freq_table)
})

# Combine all categorical summaries into one table
cat_summary_df <- bind_rows(cat_summary, .id = "Variable")

# Rearrange columns
cat_summary_df <- cat_summary_df[, c("Variable", "Category", "Frequency", "Percent")]

# View the tables
print("Continuous Variables Summary:")
print(cont_summary)

print("Categorical Variables Summary:")
print(cat_summary)

write.csv(cont_summary, "continuous_summary.csv", row.names = FALSE)
write.csv(cat_summary_df, "categorical_summary.csv", row.names = FALSE)

#
#-----------------------------------------------------------------------


#--------------------------------------------------------------------
#Model for Regression------------------------------------------------
#--------------------------------------------------------------------
# Required packages
install.packages(c("lmtest", "sandwich", "plm", "modelsummary"))
library(lmtest)
library(sandwich)
library(plm)
library(modelsummary)

#Assign variables to factors--------------------------------------------------------------
# Convert relevant variables to factors
hay_data$state <- as.factor(hay_data$state)
hay_data$Quality <- as.factor(hay_data$Quality)
hay_data$Package <- as.factor(hay_data$Package)
hay_data$month.x <- as.factor(hay_data$month.x)

# Set the most common categories as reference
hay_data$Quality <- relevel(as.factor(hay_data$Quality), ref = "Premium")
hay_data$Package <- relevel(as.factor(hay_data$Package), ref = "Large Square 3x4")

# clean data for missing values------------------------------------
library(dplyr)
hay_data <- hay_data %>%
  filter(
    !is.na(priceAvgPerTon), !is.na(Quality), !is.na(Package),
    !is.na(AvgDSCI), !is.na(precipitation), !is.na(temperature), !is.na(Export_Quantity),
    !is.na(CornPrice), !is.na(feederPrice)
  )



#---------------------------------------------------------------------
# Drought Market Model : Baseline
#--------------------------------------------------------------------


# Baseline model 
baseline_model <- feols(
  priceAvgPerTon ~ AvgDSCI + precipitation + temperature+
    Quality + Package + Export_Quantity + CornPrice +
    feederPrice | month.x + state,
  cluster = ~state,
  data = hay_data
)


#get summary 
summary(baseline_model)




#---------------------------------------------------------------
# 2.Trade Buffer Model : Drought and Export interaction
#---------------------------------------------------------------

model_buffer <- feols(
  priceAvgPerTon ~ AvgDSCI + precipitation + temperature +
    Quality + Package + Export_Quantity +
    CornPrice + feederPrice + AvgDSCI*Export_Quantity |
    state + month.x,
  cluster = ~state,
  data = hay_data
)


summary(model_buffer)



#---------------------------------------------------------------------
#Market Segmentation Model : drought and Packaging
#-----------------------------------------------------------------------
model_package <- feols(
  priceAvgPerTon ~ AvgDSCI + precipitation + temperature +
    Quality + Package + Export_Quantity +
    CornPrice + feederPrice + AvgDSCI*Package|
    state + month.x,
  cluster = ~state,
  data = hay_data
)

summary(model_package)



#-------------------------------------------------------------------------------
# Quality Premium MOdel : Drought and Quality Interaction
#---------------------------------------------------------------------------

model_quality <- feols(
  priceAvgPerTon ~ AvgDSCI + precipitation + temperature +
    Quality + Package + Export_Quantity +
    CornPrice + feederPrice + AvgDSCI*Quality|
    state + month.x,
  cluster = ~state,
  data = hay_data
)

summary(model_quality)

#-------------------------------------------------------------------------------
#inventory model
#-------------------------------------------------------------------------------
model_inventory <- feols(
  priceAvgPerTon ~  AvgDSCI + stock +
    CornPrice + feederPrice +
    Quality + Package + Export_Quantity |
    state + month.x,
  data = hay_data,
  cluster = ~state
)

summary(model_inventory)



# Export summary (optional)-------------------------------------------------------------------


# Install required packages if not installed
install.packages(c("fixest", "broom", "flextable", "officer", "dplyr"))

library(fixest)
library(flextable)
library(officer)
library(modelsummary)

# GOF extractor with numeric conversion
get_model_gof <- function(model) {
  fs <- fitstat(model, type = c("rmse", "ar2", "wr2"))
  
  # Ensure fs is treated as a list and extract with $
  list(
    RMSE = round(as.numeric(fs$rmse), 1),
    Adj_R2 = round(as.numeric(fs$ar2), 2),
    Within_R2 = round(as.numeric(fs$wr2), 2)
  )
}

# Export function for single model
export_model_detailed <- function(model, title, file_name) {
  s <- summary(model)
  coefs <- as.data.frame(s$coeftable)
  coefs$Variable <- rownames(coefs)
  rownames(coefs) <- NULL
  
  colnames(coefs) <- c("Coefficient", "Std. Error", "t value", "P-value", "Variable")
  
  # Round all numeric values to 2 decimal places
  coefs$Coefficient <- round(coefs$`Coefficient`, 3)
  coefs$`Std. Error` <- round(coefs$`Std. Error`, 3)
  coefs$`P-value` <- round(coefs$`P-value`, 3)
  
  # Add significance stars
  coefs$Significance <- cut(coefs$`P-value`,
                            breaks = c(-Inf, 0.01, 0.05, 0.1, Inf),
                            labels = c("***", "**", "*", ""),
                            right = TRUE)
  
  # Reorder and select desired columns
  coefs <- coefs[, c("Variable", "Coefficient", "Std. Error", "P-value", "Significance")]
  
  # Format as table
  ft <- flextable(coefs)
  ft <- autofit(ft)
  
  # Get model fit stats
  gof <- get_model_gof(model)
  
  # Build Word document
  doc <- read_docx()
  doc <- body_add_par(doc, title, style = "heading 1")
  doc <- body_add_flextable(doc, ft)
  doc <- body_add_par(doc, "")
  doc <- body_add_par(doc, paste0("Within R²: ", gof$Within_R2))
  doc <- body_add_par(doc, paste0("Adjusted R²: ", gof$Adj_R2))
  doc <- body_add_par(doc, paste0("RMSE: ", gof$RMSE))
  doc <- body_add_par(doc, "Fixed Effects: State and Month")
  doc <- body_add_par(doc, "Standard Errors: Clustered at the state level")
  doc <- body_add_par(doc, "Significance codes: *** p < 0.01, ** p < 0.05, * p < 0.1")
  
  # Save the document
  print(doc, target = file_name)
}




# === Apply to each model ===
export_model_detailed(baseline_model, "Drought Market Baseline", "baseline_model_results.docx")
export_model_detailed(model_buffer, "Buffering Export*DSCI", "buffer_model_results.docx")
export_model_detailed(model_package, "Buffering Package*DSCI", "package_drought_model_results.docx")
export_model_detailed(model_quality, "Buffering Quality*DSCI", "quality_drought_model_results.docx")
export_model_detailed(model_inventory, "Buffering stock*DSCI", "stock_drought_model_results.docx")


# Custom coefficient mapping for ordering and labeling
coef_map <- c(
  "AvgDSCI" = "Drought Severity Index(DSCI)",
  # Export interaction
  "Export_Quantity" = "Export Quantity",
  "AvgDSCI:Export_Quantity" = "DSCI × Export Quantity",
  
  # Quality main effects
  "QualityFair" = "Quality: Fair",
  "QualityFair/Good" = "Quality: Fair/Good",
  "QualityGood" = "Quality: Good",
  "QualityGood/Premium" = "Quality: Good/Premium",
  "QualityPremium/Supreme" = "Quality: Premium/Supreme",
  "QualitySupreme" = "Quality: Supreme",
  "QualityUtility" = "Quality: Utility",
  "QualityUtility/Fair" = "Quality: Utility/Fair",
  
  # Interactions: Quality
  "AvgDSCI:QualityFair" = "DSCI × Quality: Fair",
  "AvgDSCI:QualityFair/Good" = "DSCI × Quality: Fair/Good",
  "AvgDSCI:QualityGood" = "DSCI × Quality: Good",
  "AvgDSCI:QualityGood/Premium" = "DSCI × Quality: Good/Premium",
  "AvgDSCI:QualityPremium/Supreme" = "DSCI × Quality: Premium/Supreme",
  "AvgDSCI:QualitySupreme" = "DSCI × Quality: Supreme",
  "AvgDSCI:QualityUtility" = "DSCI × Quality: Utility",
  "AvgDSCI:QualityUtility/Fair" = "DSCI × Quality: Utility/Fair",
  
  # Package main effects
  "PackageLarge Square 4x4" = "Package: Large Square 4x4",
  "PackageSmall Square" = "Package: Small Square",
  "PackageSmall Square 3 Tie" = "Package: Small Square 3 Tie",
  # Interactions: Package
  "AvgDSCI:PackageLarge Square 4x4" = "DSCI × Package: Large Square 4x4",
  "AvgDSCI:PackageSmall Square" = "DSCI × Package: Small Square",
  "AvgDSCI:PackageSmall Square 3 Tie" = "DSCI × Package: Small Square 3 Tie",
  
  
  # Others
  "precipitation" = "Precipitation",
  "temperature" = "Temperature",
  "CornPrice" = "Corn Price",
  "feederPrice" = "Feeder Price",
  "stock" = "Stock"
)


coef_order <- names(coef_map)


# Create the modelsummary table as a flextable

models <- list(
  "Trade Buffer Model" = model_buffer,
  "Market Segment Model" = model_package,
  "Quality Boost Model" = model_quality,
  "Inventory Model" = model_inventory
)

table_ft <- modelsummary(
  models,
  coef_map = coef_map,
  coef_order = coef_order,
  coef_omit = "state|month.x",
  gof_omit = "IC|Log.Lik",
  statistic = "std.error",
  stars = c('*' = .1, '**' = .05, '***' = .01),  # CUSTOM SIGNIFICANCE LEVELS
  fmt = 3,
  output = "flextable",
  na_rows = TRUE
)

# Write the flextable into a Word document
doc <- read_docx()
doc <- body_add_par(doc, "Combined Regression Results", style = "heading 1")
doc <- body_add_flextable(doc, table_ft)
doc <- body_add_par(doc, "Significance codes: *** p < 0.01, ** p < 0.05, * p < 0.1")
print(doc, target = "combined_model_table.docx")

