#Library used
library(readxl)
library(tidyverse)
library(psych)
library(mice)

#Load the Data Set
Ames_data <- read_excel("ames.xlsx")

# Sub-setting that data to remove unnecessary variable 
Ames <- Ames_data[, c("ID", "d_type", "zone", "lot_area", "shape", "contour", "neighbourhood", "building", "stories", "house_quality", "house_condition", "year_built", "year_remod", "exter l_qual", "exter l_cond", "foundations", "basement_qual", "bsmt_area", "heat_type", "heat_qual", "aircon", "floor1_sf", "floor2_sf", "low_qual_sf", "full_bath", "half_bath", "bedroom", "kitchen", "kitchen_qual", "rooms_tot", "month_sold", "year_sold", "sale_type", "sale_cond", "sale_price")]

# Factorizing relevant variables
# A variable that stores the number of columns in the data set 
num_cols <- ncol(Ames)

# Iterate through each column
for (i in 1:num_cols){
  # Checks if the column is a character type
  if (is.character(Ames[[i]])){
    # If true converts character columns to factor
    Ames[[i]] <- as.factor(Ames[[i]])
  }
}

#Identify percentage of NA for each variable

Ames_NA <- map_vec(colSums(is.na(Ames)), ~ .x*100/count(Ames))
view(Ames_NA)

#Cleaning the data set

#Sumamary of data using psych
describe(Ames)

#changing column name
colnames(Ames)[which(names(Ames) == "exter l_cond")] <- "external_condition"
colnames(Ames)[which(names(Ames) == "exter l_qual")] <- "external_quality"

#viewing outliers

#Lot area
boxplot(Ames$lot_area,
        ylab = "Lot Area",  
        main = "Figure 1: Boxplot of Lot Area")
lot_area_outliers <- boxplot(Ames$lot_area)$out
#Removing values that are over 100,000 and under 100
Ames$lot_area[Ames$lot_area > 100000] <- NA
Ames$lot_area[Ames$lot_area < 100] <- NA
#Removing any rows that contain a NA in lot_area
Ames <- Ames[complete.cases(Ames$lot_area),]

#bsmt_area
boxplot(Ames$bsmt_area)
bsmt_area_outliers <- boxplot(Ames$bsmt_area)$out
hist(bsmt_area_outliers)
#Removing any rows that are over 5000
Ames$bsmt_area[Ames$bsmt_area >5000] <- NA
Ames <- Ames[complete.cases(Ames$bsmt_area),]

#floor1_sf
boxplot(Ames$floor1_sf)
floor1_sf_outliers <- boxplot(Ames$floor1_sf)$out
hist(floor1_sf_outliers)
#Removing any rows that are over 3500
Ames$floor1_sf[Ames$floor1_sf >3500] <- NA
Ames <- Ames[complete.cases(Ames$floor1_sf),]

#low_qual_sf
boxplot(Ames$low_qual_sf)
low_qual_sf_outliers <- boxplot(Ames$low_qual_sf)$out
hist(low_qual_sf_outliers)
#Removing any rows that are over 600
Ames$low_qual_sf[Ames$low_qual_sf >600] <- NA
Ames <- Ames[complete.cases(Ames$low_qual_sf),]

#sale_price
boxplot(Ames$sale_price,
        ylab = "Sale Price",  
        main = "Figure 2: Boxplot of Sale Price")
sale_price_outliers <- boxplot(Ames$sale_price)$out
hist(sale_price_outliers)
#Removing any rows that are over 750000
Ames$sale_price[Ames$sale_price >750000] <- NA
Ames <- Ames[complete.cases(Ames$sale_price),]

#changing exter l_qual and exter l_cond to appropriate factors
Ames$external_condition[Ames$external_condition == "Good"] <- "Gd"
Ames$external_condition <- droplevels(Ames$external_condition)

Ames$external_quality[Ames$external_quality == "Good"] <- "Gd"
Ames$external_quality <- droplevels(Ames$external_quality)

#removing basement qual po due to limited data 
Ames$basement_qual[Ames$basement_qual == "Po"] <- NA
Ames$basement_qual <- droplevels(Ames$basement_qual)

#Adding Age column. 
# Calculate the current year
current_year <- as.integer(format(Sys.Date(), "%Y"))
# Create the age column
Ames <- Ames %>%
  mutate(age = current_year - Ames$year_built)

# Perform multiple imputation using predictive mean matching
Ames$bedroom[Ames$bedroom == 0] <- NA
imputed_data <- mice(Ames, method = 'pmm', seed = 40265478)

imputed_data <- complete(imputed_data)

# Extract the imputed values for the 'bedroom' column
imputed_bedroom <- imputed_data$bedroom

#Adding imputed bedrooms to the data set
Ames$bedroom <- imputed_bedroom
