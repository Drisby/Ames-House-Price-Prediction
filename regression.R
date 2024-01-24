# Libraries Used
library(caret)
library(stargazer)
set.seed(402652)

index <- createDataPartition(Ames$sale_price, times = 1, p = 0.8, list = FALSE)

train <- Ames[index, ] #80% of the data
test <- Ames[-index, ] #20% of the data

formula1 <- sale_price ~ lot_area + age + bedroom + house_quality + neighbourhood
Hmodel <- lm(formula = formula1, data = train)

formula2 <- sale_price ~ lot_area + age + bedroom + house_quality
HmodelNoN <- lm(formula = formula2, data = train)
summary(HmodelNoN)

formula3 <- sale_price ~ neighbourhood
HmodelN <- lm(formula = formula3, data = train)

formula4 <-sale_price ~ lot_area + age + bedroom + house_quality + heat_qual + year_remod + basement_qual + aircon
model2 <- lm(formula = formula4, data = train)

formula5 <- sale_price ~ lot_area + age + bedroom + house_quality + heat_qual + year_remod + basement_qual + bsmt_area + aircon + floor1_sf + floor2_sf + full_bath + sale_cond 
model3 <- lm(formula = formula5, data = train)

summary(model3)
# model accuracy
predH <- predict(Hmodel, test)
postResample(predH, test$sale_price)

pred3 <- predict(model3, test)
postResample(pred3, test$sale_price)

# Calculate Cook's distance
cooksd <- cooks.distance(model3)
describe(cooksd)

Hmodel_list <- list(Hmodel, HmodelN, HmodelNoN)
stargazer(Hmodel_list, type = "html", out = "Hmodel.html", title = "Regression Models")

Revised_model_list <- list(HmodelNoN, model2, model3)
stargazer(Revised_model_list, type = "html", out = "Rmodel.html", title = "Regression Models")
