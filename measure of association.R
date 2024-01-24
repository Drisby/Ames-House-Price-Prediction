#Measures of association
#Interval
cor.test(x = Ames$lot_area, Ames$sale_price)
cor.test(x = Ames$age, Ames$sale_price)

#Ordinal
Bedroom_correlation <- cor(x = Ames$bedroom, y = Ames$sale_price, method = "spearman")
print(Bedroom_correlation)

Quality_correlation <- cor(Ames$house_quality, Ames$sale_price, method = "spearman")
print(Quality_correlation)

#Nominal
model_anova <- lm(Ames$sale_price ~ as.factor(Ames$neighbourhood), data = Ames)


# ANOVA test
anova_result <- anova(model_anova)
print(anova_result)

class(Ames$house_quality)
