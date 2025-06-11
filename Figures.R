
rm(list = ls())
library(tidyverse)
install.packages("patchwork")
install.packages("usmap")
library(patchwork)
library(scales)
library(viridis)
library(ggthemes)
library(usmap)
library(tidyverse)
library(maps)
library(scales)
library(readr)
library(dplyr)
library(ggplot2)

#-------------------------------------------------------------
### Working directories and theme
#------------------------------------------------------------
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


#Define an style for figures
journal_theme <- function() {
  theme_minimal(base_size = 14, base_family = "serif") +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.title = element_text(size = 15),
      axis.text.x = element_text(size = 13, face = "bold"),  # ⬅️ Increase x-axis label size
      axis.text.y = element_text(size = 12),  # (optional) y-axis size,
      panel.grid.major.y = element_line(color = "grey90"),
      panel.grid.major.x = element_blank(),
      plot.margin = margin(8, 8, 8, 8),
      axis.line.x = element_line(color = "black"),
      axis.line.y = element_line(color = "black"),
      panel.border = element_blank()
    )
}

#------------------------------------------------------------------------------------------------
#Figure A1: Production of alfalfa compared to corn,soybean and wheat in 2024 (introduction)
#----------------------------------------------------------------------------------------------

crop_data <- read.csv(file.path(dir$rawdata, "production-yield-area for 4 crops.csv"))

# Plot 1: Production Value (in $B)
P1 <- ggplot(crop_data, aes(x = reorder(Commodity, -Production), y = Production / 1000, fill = Commodity)) +
  geom_col(width = 0.6, color = "black") +
  geom_text(aes(label = sprintf("$%.1fB", Production / 1000)),
            vjust = -0.2, color = "black", size = 5.5,
            family = "serif", fontface = "bold") +
  scale_y_continuous(
    labels = label_number(suffix = "B"),
    expand = expansion(mult = c(0, 0.05)),
    limits = c(0, 70)
  ) +
  labs(
    title = "Production Value of Major U.S. Crops (2024)",
    y = "Value (Billion USD)", x = NULL
  ) +
  journal_theme()


ggsave(filename = paste0(dir$fig, "crop_production_2024.png"),
       width = 10, height = 6, units = "in", dpi = 300)




#----------------------------------------------------------------------------------
# Figure A2: alfalfa production share over time for western states(1990-2024)
#-----------------------------------------------------------------------------------

install.packages("rnassqs")
library(rnassqs)


nassqs_auth(key ="Your API Key")  # get API from USDA for data obtain


#use parametrs to download data , assign year and what do you want
params <- list(
  commodity_desc = "HAY",
  class_desc = "ALFALFA",
  statisticcat_desc = "PRODUCTION",
  unit_desc = "TONS",
  agg_level_desc = "STATE",
  year__GE = "1990",
  year__LE = "2024",
  source_desc = "SURVEY"
)

alfalfa_historical_data <- nassqs(params)


# Clean values and assign state groups
western_state <- c("ARIZONA", "CALIFORNIA", "COLORADO", "IDAHO", "MONTANA",
                   "NEVADA", "NEW MEXICO", "OREGON", "UTAH", "WASHINGTON", "WYOMING")


alfalfa_data_clean <- alfalfa_historical_data %>%
  mutate(Value = as.numeric(gsub(",", "", Value)),
         is_west = state_name %in% western_state,
         year = as.integer(year)) %>%
  group_by(year, is_west) %>%
  summarise(total = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = is_west, values_from = total, names_prefix = "west_") %>%
  rename(total_us = west_FALSE, total_west = west_TRUE) %>%
  mutate(western_share = 100 * total_west / (total_west + total_us)) %>%
  drop_na()


share_plot <- ggplot(alfalfa_data_clean, aes(x = year, y = western_share)) +
  
  # 2. Inner red line (drawn on top)
  geom_line(color = "blue", linewidth = 1.2) +
  geom_smooth(method = "lm", se = FALSE, color = "red2", linetype = "dashed", linewidth = 0.7) +
  scale_x_continuous(
    breaks = seq(1990, 2024, by = 2),
    expand = c(0.01, 0.01),
    limits = c(1990,2024)
  ) +
  labs(
    title = "Western U.S. States Alfalfa Production (1990–2024)",
    x = "Year", y = "Western Share (%)"
  ) +
  journal_theme() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.line.x = element_line(),
    axis.line.y = element_line()
    
  )


#Export the data as csv
write.csv(alfalfa_data_clean, file.path(dir$clndata, "western states production share(1990-2024).csv"))

ggsave(filename = paste0(dir$fig, "A2-alfalfa_production share_2024.png"),
       width = 10, height = 6, units = "in", dpi = 500)



#-----------------------------------------------------------------------------------------
# Figure A3:Alfalfa Production Historical Trend by State
#------------------------------------------------------------------------------------------
hay_production <- read.csv(file.path(dir$rawdata, "alfalfa historical trends.csv"))

western_states <- c("ARIZONA", "CALIFORNIA", "COLORADO", "IDAHO", "MONTANA",
                    "NEW MEXICO", "OREGON", "UTAH", "WASHINGTON")

# Prepare data
western_data <- hay_production %>%
  mutate(State = toupper(State)) %>%
  filter(State %in% western_states) %>%
  filter(!is.na(Year), !is.na(production_tons), is.finite(Year), is.finite(production_tons)) %>%
  mutate(production_ktons = production_tons / 1000)



plot_by_state<- ggplot(western_data, aes(x = Year, y = production_ktons)) +
  geom_line(color = "blue", size = 1) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "red", size = 0.8) +
  scale_x_continuous(breaks = seq(2000, 2024, by = 3), limits = c(2000, 2024)) +
  scale_y_continuous(breaks = seq(0, max(western_data$production_ktons, na.rm = TRUE), by = 2000)) +
  labs(
    title = "Alfalfa Production Trends in Western U.S. States (2000–2024)",
    x = "Year",
    y = "Production (1,000 tons)"
  ) +
  facet_wrap(~ State, ncol = 3, scales = "free_y") +
  journal_theme() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 0.7)
  )

ggsave(filename = paste0(dir$fig, "A4_ alfalfa production by state.png"),
       width = 10, height = 6, units = "in", dpi = 300)


install.packages("patchwork")  # if not already installed
library(patchwork)


combined_plot <- share_plot / plot_by_state  # "/" stacks vertically
ggsave(filename = paste0(dir$fig, "alfalfa production by state history.png"),
       width = 10, height = 6, units = "in", dpi = 300)



#---------------------------------------------------------------------------
#Figure A4: Alfalfa price over time (1990-2024)
#----------------------------------------------------------------------------
alfalfa_price <- read.csv(file.path(dir$rawdata, "alfalfa price trend (1990-2024).csv"))

# Calculate yearly average price
annual_avg <- alfalfa_price %>%
  group_by(Year) %>%
  summarise(Avg_Price = mean(Price, na.rm = TRUE))


# Plot the annual trend
ggplot(annual_avg, aes(x = Year, y = Avg_Price, color= as.factor(state))) +
  geom_line(color = "blue", linewidth = 1.2) +
  scale_x_continuous(
    breaks = seq(1990, 2024, by = 2)) +
  geom_point(color = "darkblue", size = 2) +
  geom_smooth(method = "lm", color = "red2") +
  labs(
    title = "Annual Average Alfalfa Hay Price (1990–2024)",
    x = "Year",
    y = "Average Price ($/ton)"
  ) +
  journal_theme() +
  theme(
    axis.line.x = element_line(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.line.y = element_line(color = "black"),
    panel.border = element_blank()
  )
    
ggsave(filename = paste0(dir$fig, "A4-Alfalfa price over time (1990-2024).png"),
       width = 10, height = 6, units = "in", dpi = 500)



#---------------------------------------------------------
# Figure A5:box plot for alfalfa price across west states
#------------------------------------------------------------
ggplot(hay_data, aes(x = state, y = priceAvgPerTon, fill = state)) +
  geom_boxplot(outlier.color = "red4", outlier.shape = 16, outlier.size = 3) +  # Highlights outliers in red
  labs(title = "State-Level Variation in Alfalfa Hay Prices Across the Western U.S. (2021–2024)",
       x = "State",
       y = "Price Per Ton (USD)") +
  theme(
    plot.margin = margin(10, 10, 10, 10),  # 1️⃣ Set a margin (top, right, bottom, left)
    
    axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),  # 2️⃣ Increase Y-axis title distance
    axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold", angle = 45),  # 2️⃣ Increase X-axis title distance
    
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 20)),  # 4️⃣ Increase title distance from axis labels
    
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),  # Bold x-axis labels
    legend.title = element_text(face = "bold", size = 12),  # Bold legend title
    legend.text = element_text(face = "bold", size = 10),  # Bold legend labels
    
    panel.grid.major = element_line(color = "gray85", linewidth = 0.2),  # Add grid lines
  ) +
  scale_fill_manual(values = c("#453882", "#2C3E8A", "#2C4E8A", "#276E8A", "#2F8D66", "#37A65F", "#61BB4F", "#95C93D", "#95D95D")) +
  journal_theme()+
  theme(
    axis.text.x = element_text(size= 16,angle = 45, hjust = 0.9),
    axis.line.x = element_line(color = "grey"),
    axis.line.y = element_line(color = "grey"),
    legend.position = "none"
  )

ggsave(filename = paste0(dir$fig, "alfalafa price boxplot across state.png"),
       width = 10, height = 6, units = "in", dpi = 300)




#---------------------------------------------------
#Figure A6: Export and Price
#------------------------------------------------------

export_bin_V <- hay_data %>%
  mutate(price_bin = cut(priceAvgPerTon, breaks = seq(min(priceAvgPerTon), max(priceAvgPerTon), by = 50))) %>%
  group_by(price_bin) %>%
  summarise(mean_price = mean(priceAvgPerTon, na.rm = TRUE),
            mean_value = mean(Export_Quantity, na.rm = TRUE))

ggplot(hay_data, aes(x = priceAvgPerTon, y = Export_Quantity)) +
  # Scatter plot (original data)
  geom_point(alpha = 0.3, color = "skyblue3", shape = 16, size = 1.5) +  
  # Regression line with shadow
  geom_smooth(method = "lm", color = "blue", se = TRUE, fill = "orange", alpha = 0.3, linetype = "dashed", size = 1.5) +  
  # Binned trend line
  geom_line(data = export_bin_V, aes(x = mean_price, y = mean_value), color = "red2", size = 1, linetype = "solid") +  
  # Highlight bin points
  geom_point(data = export_bin_V, aes(x = mean_price, y = mean_value), color = "red4", size = 2, shape = 19) +  
  # Custom title and labels
  labs(title = "Export Volume of Alfalfa to China and Relationship with Price ",
       x = "Average Price Per Ton (USD)", 
       y = "Export Volume(1000 MT)") +
  # Custom theme
  journal_theme()


ggsave(filename = paste0(dir$fig, "Export quantity and price relationship.png"),
       width = 10, height = 6, units = "in", dpi = 300)


#-------------------------------------------------------------------
#Figure A7:alfalfa price and drought relationship
#----------------------------------------------------------------------

# Compute percentiles for the shaded area
percentile_data <- hay_data %>%
  group_by(AvgDSCI) %>%
  summarise(
    p10 = quantile(priceAvgPerTon, 0.1, na.rm = TRUE),
    p90 = quantile(priceAvgPerTon, 0.9, na.rm = TRUE),
    median_price = median(priceAvgPerTon, na.rm = TRUE)
  ) %>%
  ungroup()  # Ensure proper grouping

# Create scatter plot with the shaded percentile range
ggplot() +
  # Orange shaded area for the 10th–90th percentile range
  geom_ribbon(data = percentile_data, aes(x = AvgDSCI, ymin = p10, ymax = p90), 
              fill = "orange", alpha = 0.4) +
  
  # Scatter points
  geom_point(data = hay_data, aes(x = AvgDSCI, y = priceAvgPerTon), alpha = 0.3, color = "steelblue3") +  
  
  # Regression trend line
  geom_smooth(data = hay_data, aes(x = AvgDSCI, y = priceAvgPerTon), method = "lm", color = "red", se = TRUE) +  
  
  # Labels and theme
  labs(title = "Alfalfa Hay Price and Drought Severity Index Over Time",
       x = "Drought Severity Index",
       y = "Average Price Per Ton (USD)") +
  journal_theme()



#Save the plot
ggsave(filename = paste0(dir$fig, "drought and alfalfa price relationship.png"),
       width = 10, height = 6, units = "in", dpi = 300)

#---------------------------------------------------------------------------
#Figure A8: Quality and Package Relationship with price
#-----------------------------------------------------------------------
# Prepare Quality Data
quality_summary <- hay_data %>%
  filter(Quality %in% c("Supreme", "Premium", "Good", "Fair", "Utility"), !is.na(Quality)) %>%
  group_by(state, Category = Quality) %>%
  summarise(avg_price = mean(priceAvgPerTon, na.rm = TRUE)) %>%
  mutate(
    Type = "Quality Grade",
    Fill_Label = paste("Quality:", Category)
  )

# Prepare Packaging Data
package_summary <- hay_data %>%
  filter(!is.na(Package)) %>%
  group_by(state, Category = Package) %>%
  summarise(avg_price = mean(priceAvgPerTon, na.rm = TRUE)) %>%
  mutate(
    Type = "Packaging Type",
    Fill_Label = paste("Package:", Category)
  )

# Combine
combined_summary <- bind_rows(quality_summary, package_summary)

# Plot with shared x-axis and split legend labels
ggplot(combined_summary, aes(x = state, y = avg_price, fill = Fill_Label)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~Type, ncol = 1, scales = "free_y") +
  labs(
    title = "Average Alfalfa Price by Quality Grade and Packaging Type Across States",
    x = "State",
    y = "Average Price (USD/ton)",
    fill = "Legend"
  ) +
  journal_theme() +
  scale_fill_brewer(palette = "Set1") +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    strip.text = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 13, face = "bold")
  )


#Save the plot
ggsave(filename = paste0(dir$fig, "quality and package and alfalfa price relationship.png"),
       width = 10, height = 6, units = "in", dpi = 300)
