#Libraries Used
library(scales)
library(ggplot2)
library(reshape2)
library(stargazer)
library(summarytools)

#Summary Statistics 
hypothesis_variable <- subset(Ames, select = c("sale_price", "age", "bedroom", "neighbourhood", "house_quality", "lot_area"))
hstats <- descr(hypothesis_variable)

stargazer(hstats, type = "html", out = "hypothesis_stats.html")

# price by count
options(scipen = 10000)
ggplot(Ames, aes(sale_price, fill = ..count..))+
  geom_histogram(binwidth = 5000) +
  ggtitle("Figure 3: Histogram of Sale Price") +
  xlab("Sale Price ($)") +
  ylab("Number of Properties") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(labels = scales::comma)

describe(Ames$sale_price)

# Exploration of the distribution of Sale_Price by Lot Area
ggplot(Ames, aes(x = sale_price, y = lot_area)) +
  geom_point() +
  xlab("Sale Price ($)") +
  ylab("Lot Area") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("Figure 4: Scatterplot of Lot Area vs Sales Price") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(labels = scales::label_number())

describe(Ames$age)

# Exploration of the distribution of Sale_Price by Age
ggplot(Ames, aes(x = age, y = sale_price)) +
  geom_point() +
  xlab("Sale Price ($)") +
  ylab("Age") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Age", y = "Sale Price", title = "Figure 5: Scatterplot of Age vs Sale Price") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::label_number())

# Exploration of the distribution of Sale_Price by house quality
ggplot(Ames, aes(x = sale_price, fill = as.factor(house_quality))) +
  geom_histogram(position = "stack", aes(y = ..count..), binwidth = 10000) +
  ggtitle("Figure 6: Histogram of House Quality vs Sale Price") +
  xlab("Sale Price ($)") +
  ylab("Number of Properties") +
  scale_fill_discrete(name = "House Quality") +
  theme(plot.title = element_text(hjust = 0.5))

# Exploration of Age by House Quality
ggplot(Ames, aes(x = as.factor(house_quality), y = age, fill = as.factor(house_quality))) +
  geom_boxplot() +
  ggtitle("Figure 7: Boxplot Age and House Quality") +
  xlab("House Quality") +
  ylab("Age") +
  scale_fill_discrete(name = "House Quality") +
  theme(plot.title = element_text(hjust = 0.5))

# Exploration of the distribution of Sale_Price by Bedrooms
ggplot(Ames, aes(x = as.factor(bedroom), y = sale_price, fill = as.factor(bedroom))) +
  geom_boxplot() +
  ggtitle("Figure 8: Boxplot of Bedrooms and Sale Price") +
  xlab("Number of Bedrooms") +
  ylab("Sale Price ($)") +
  scale_x_discrete(name = "Number of Bedrooms") +
  scale_fill_discrete(name = "Number of Bedrooms") +
  theme(plot.title = element_text(hjust = 0.5))

# Exploration of the distribution of Sale_Price by Neighborhood
# Calculate median sale price for each neighborhood
median_prices <- aggregate(sale_price ~ neighbourhood, data = Ames, FUN = median)

# Order neighborhoods by median sale price
ordered_neighborhoods <- median_prices$neighbourhood[order(median_prices$sale_price)]

# Convert neighbourhood to factor with ordered levels
Ames$neighbourhood <- factor(Ames$neighbourhood, levels = ordered_neighborhoods)

# Create the boxplot with neighborhoods ordered by median sale price
ggplot(Ames, aes(x = neighbourhood, y = sale_price)) +
  geom_boxplot() +
  ggtitle("Figure 9: Boxplot of Neighbourhood and Sale Price") +
  xlab("Neighborhood") +
  ylab("Sale Price ($)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::comma)

# correlation analysis for numerical variable ~ Correlation Matrix
numerical_data <- Ames[, sapply(Ames, is.numeric)]
# Remove specific columns from the numerical_data subset
columns_to_remove <- c("garage_area", "garage_cars", "garage_year", "ID")
numerical_data <- numerical_data[, !names(numerical_data) %in% columns_to_remove]


correlation_matrix <- cor(numerical_data)

correlation_melted <- melt(correlation_matrix)

ggplot(data = correlation_melted, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limit = c(-1, 1), 
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  ggtitle("Correlation Heatmap for Numerical Variables")




