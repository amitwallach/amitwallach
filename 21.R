# טעינת חבילות
install.packages("moments")
install.packages("dplyr")

library(moments)  # עבור חישוב skewness
library(dplyr)    # עבור פעולות עיבוד נתונים

# טעינת נתונים
dataset <- read.csv(file.choose(), header = TRUE, fileEncoding = "latin1")

# מחיקת עמודות ריקות או בעלות תוכן מיותר
dataset <- dataset[, -c(12, 13, 14)]

# שינוי שם של עמודות
colnames(dataset) <- c("country", "GDP_per_capita", "population_density", "alcohol_consumption", 
                       "suicide_rate", "continent", "smoking_deaths", "drug_legalization", 
                       "military_service", "covid_cases", "covid_deaths")

#טבלה חדשה לאחר הוצאת משתנים
dataset1<-subset(dataset, covid_cases< 1144853)
bp<- boxplot(dataset1$covid_cases, main = "covid_cases", outline = F)
# Get the outlier value(s)
bp$out

# בדיקת חריגים והוצאתם population_dencity
bp_population <- boxplot(dataset$population_density, main = "population_density", outline = TRUE)
bp_population$stats  # סטטיסטיקות
bp_population$out    # ערכים חריגים
outliers_population <- sort(bp_population$out, decreasing = TRUE)[1:4]
dataset1 <- subset(dataset1, !population_density %in% outliers_population)


# בדיקת חריגים והוצאתם smoking_death
bp_smoking <- boxplot(dataset$smoking_deaths, main = "smoking_deaths", outline = TRUE)
bp_smoking$stats  # סטטיסטיקות
bp_smoking$out    # ערכים חריגים
outliers_smoking <- sort(bp_smoking$out, decreasing = TRUE)[1:2]
dataset1 <- subset(dataset1, !smoking_deaths %in% outliers_smoking)
#---------------------------------------------------------------------------------------------

# יצירת עותק חדש לניתוח מתאמים, כולל רק משתנים רציפים (ולא בינאריים או קטגוריאליים)
numeric_dataset <- dataset1 %>%
  select(where(is.numeric)) %>%
  select(-military_service, -drug_legalization)  # מסיר משתנים בינאריים

# הסרת משתנים כמו 'country' ו-'continent' אם יש
dataset$country <- as.numeric(factor(dataset$country))  # המרת 'country' למספרים
dataset$continent <- as.numeric(factor(dataset$continent))  # המרת 'continent' למספרים

# יצירת עותק חדש עם המשתנים המומרות למספריים
numeric_dataset <- dataset %>% select(where(is.numeric))

# חישוב מתאם פירסון בין כל המשתנים הרציפים לבין covid_cases
cor_values <- sapply(numeric_dataset, function(x) cor(x, numeric_dataset$covid_cases, use = "complete.obs"))

# דירוג המשתנים לפי ערכי המתאם בסדר יורד
ranked_cor <- sort(cor_values, decreasing = TRUE)

# הצגת התוצאה שורה אחרי שורה
for (i in 1:length(ranked_cor)) {
  cat(names(ranked_cor)[i], ":", ranked_cor[i], "\n")
}

# הרצת t-test כדי לבדוק את הקשר בין military_service לבין covid_cases
t_test_result <- t.test(covid_cases ~ military_service, data = dataset1)

# הצגת תוצאות ה-t-test, כולל p-value
print(t_test_result)

# יצירת Box Plot להשוואת covid_cases בין קבוצות עם ובלי שירות צבאי
boxplot(covid_cases ~ military_service, data = dataset1,
        main = "Boxplot of Covid Cases by Military Service",
        xlab = "Military Service (0 = No, 1 = Yes)",
        ylab = "Covid Cases",
        col = c("lightblue", "lightgreen"))

# הסרת המשתנים Military, Country ו-Suicide Rate מהדאטה
dataset1 <- dataset1[, !colnames(dataset1) %in% c("military_service", "country", "Suicide_Rate")]




#---------------------------------------------------------------------------------------------

# דיסקרטיזציה של GDP per capita לקטגוריות - נמוך, בינוני, גבוה
dataset1$GDP_per_capita_discretized <- cut(dataset1$GDP_per_capita, 
                                           breaks = c(-Inf, 5000, 20000, Inf), 
                                           labels = c("Low", "Medium", "High"))

# הצגת טבלת השכיחות של כל קטגוריה לאחר הדיסקרטיזציה
table(dataset1$GDP_per_capita_discretized)

# מודל רגרסיה עם משתנים דיסקרטיים
model_discretized <- lm(covid_cases ~ GDP_per_capita_discretized, data = dataset1)

# הצגת התוצאות של המודל הדיסקרטי
summary(model_discretized)

model_original <- lm(covid_cases ~ GDP_per_capita, data = dataset1)

# הצגת התוצאות של המודל המקורי
summary(model_original)

dataset1$GDP_per_capita_discretized <- cut(dataset1$GDP_per_capita, 
                                           breaks = c(-Inf, 5000, 20000, Inf), 
                                           labels = c("Low", "Medium", "High"))

dataset1 <- dataset1[, !colnames(dataset1) %in% c("GDP_per_capita")]


#----------------------------------------------------------------------------------
# שלב 1: מודל רגרסיה לפני יצירת המשתנה החדש (רק עם smoking_deaths ו- alcohol_consumption)
model_before <- lm(covid_cases ~ smoking_deaths + alcohol_consumption, data = dataset1)

# הצגת התוצאות של המודל לפני
summary(model_before)

# שלב 2: יצירת המשתנה החדש - Health Lifestyle Index (הממוצע של smoking_deaths ו- alcohol_consumption)
dataset1$health_lifestyle_index <- (dataset1$smoking_deaths + dataset1$alcohol_consumption) / 2

# הצגת סטטיסטיקות תיאוריות למשתנה החדש
summary(dataset1$health_lifestyle_index)

# שלב 3: מודל רגרסיה אחרי יצירת המשתנה החדש
model_after <- lm(covid_cases ~ health_lifestyle_index, data = dataset1)

# הצגת תוצאות המודל אחרי
summary(model_after)

#--------------------------------------------------------------------------------------------------------


# יצירת משתני דמה עבור היבשות, כשהיבשת "אסיה" היא קבוצת הבסיס
dataset1$U1 <- ifelse(dataset1$continent == "EUROPE", 1, 0)
dataset1$U2 <- ifelse(dataset1$continent == "AFRICA", 1, 0)
dataset1$U3 <- ifelse(dataset1$continent == "LATIN AMER", 1, 0)
dataset1$U4 <- ifelse(dataset1$continent == "OCEANIA", 1, 0)
dataset1$U5 <- ifelse(dataset1$continent == "CENTRAL AMERICA", 1, 0)

# הצגת טבלת שכיחות של משתני הדמה
table(dataset1$U1, dataset1$U2, dataset1$U3, dataset1$U4, dataset1$U5)

#---------------------------------------------------------------------------------------------------------

# יצירת מודל רגרסיה עם אינטראקציה בין צפיפות אוכלוסין ללגליזציה של סמים רפואיים
model_with_interaction <- lm(
  covid_cases ~ population_density * drug_legalization,
  data = dataset1
)

# סיכום תוצאות המודל
summary(model_with_interaction)


# התקנת ggplot2 (אם לא מותקן)
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# יצירת גרף פיזור
ggplot(dataset1, aes(x = population_density, y = covid_cases, color = as.factor(drug_legalization))) +
  geom_point(alpha = 0.6, size = 3) +  # נקודות פיזור
  geom_smooth(method = "lm", se = FALSE) +  # קווי רגרסיה
  scale_color_manual(values = c("blue", "red"), labels = c("No Legalization", "Legalization")) +
  labs(
    title = "Covid Cases vs Population Density by Drug Legalization",
    x = "Population Density",
    y = "Covid Cases",
    color = "Drug Legalization"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12)
  )

#---------------------------------------------------------------------

# יצירת מודל רגרסיה עם אינטראקציה בין תמ"ג לנפש לשיעור תמותה מעישון
model_with_interaction <- lm(
  covid_cases ~ GDP_per_capita_discretized * smoking_deaths,
  data = dataset1
)

# סיכום תוצאות המודל
summary(model_with_interaction)

ggplot(dataset1, aes(x = smoking_deaths, y = covid_cases, color = GDP_per_capita_discretized)) +
  geom_point(alpha = 0.6, size = 3) +  # נקודות פיזור
  geom_smooth(method = "lm", se = FALSE) +  # קווי רגרסיה לכל קטגוריה
  scale_color_manual(values = c("blue", "green", "red"), labels = c("Low GDP", "Medium GDP", "High GDP")) +
  labs(
    title = "Covid Cases vs Smoking Deaths by GDP Levels",
    x = "Smoking Deaths",
    y = "Covid Cases",
    color = "GDP Level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12)
  )


#------------------------------------------------------------------
# יצירת מודל רגרסיה עם אינטראקציה בין תמותה מעישון ללגליזציה של סמים רפואיים
model_with_interaction <- lm(
  covid_cases ~ smoking_deaths * drug_legalization,
  data = dataset1
)

# סיכום תוצאות המודל
summary(model_with_interaction)


# התקנת ggplot2 (אם לא מותקן)
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# יצירת גרף פיזור
ggplot(dataset1, aes(x = smoking_deaths, y = covid_cases, color = as.factor(drug_legalization))) +
  geom_point(alpha = 0.6, size = 3) +  # נקודות פיזור
  geom_smooth(method = "lm", se = FALSE) +  # קווי רגרסיה
  scale_color_manual(values = c("blue", "red"), labels = c("No Legalization", "Legalization")) +
  labs(
    title = "Covid Cases vs Smoking Deaths by Drug Legalization",
    x = "Smoking Deaths",
    y = "Covid Cases",
    color = "Drug Legalization"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12)
  )


#---------------------------------------------------------------

# יצירת המודל המלא (כולל את כל המשתנים והאינטראקציות)
full_model <- lm(covid_cases ~ ., data = dataset1)

# ביצוע Backward Elimination על המודל המלא
backward_model <- step(full_model, direction = 'backward')

# חישוב AIC של המודל לאחר Backward Elimination
aic_backward_model <- AIC(backward_model)
cat("AIC לאחר רגרסיה לאחור הוא:", aic_backward_model, "\n")

# סיכום המודל שנבחר
summary(backward_model)

#--------------------------------------------------------------

# יצירת מודל ריק
null_model <- lm(covid_cases ~ 1, data = dataset1)

# ביצוע Forward Selection באמצעות step()
forward_model <- step(
  null_model,  # מודל ריק
  direction = "forward",  # כיוון קדימה
  scope = formula(full_model)  # טווח משתנים ממודל מלא
)

# חישוב ערך AIC
aic_value <- AIC(forward_model)
cat("AIC של המודל הוא:", aic_value, "\n")

# סיכום המודל שנבחר
summary(forward_model)

AIC(forward_model)
#--------------------------------------------------------

# חישוב AIC עבור המודל המלא
aic_full_model <- AIC(full_model)
cat("AIC של המודל המלא הוא:", aic_full_model, "\n")

#--------------------------------------------------------------

# חישוב ערכים
fitted_values <- fitted(backward_model)  # תחזיות מהמודל
standardized_residuals <- rstandard(backward_model)  # שגיאות מנורמלות

# יצירת תרשים
plot(fitted_values, standardized_residuals,
     xlab = "Fitted Values",
     ylab = "Normalized Residuals",
     main = "Residuals VS Fitted",
     pch = 19, col = "blue")
abline(h = 0, lty = 2, col = "black")  # קו אפס

# תרשים QQplot
qqnorm(standardized_residuals,
       main = "QQ-Plot of Residuals")
qqline(standardized_residuals, col = "blue", lwd = 2)

# יצירת היסטוגרמה
hist(normalized_residuals, 
     main = "Histogram of Normalized Residuals", 
     xlab = "Normalized Residuals", 
     col = "gray", 
     border = "black", 
     breaks = 10,  # מספר המחלקות בהיסטוגרמה
     freq = FALSE)  # כדי להציג את הצפיפות ולא את התדירות

# הוספת קו צפיפות נורמלית
curve(dnorm(x, mean = 0, sd = 1), 
      add = TRUE, 
      col = "blue", 
      lwd = 2)


# בדיקת שוויון שונויות
library(lmtest)
bptest(backward_model)

#-------------------------------------------------------------------------
# יצירת מודל רגרסיה עם אינטראקציות
model_with_interactions <- lm(covid_cases ~ 
                                population_density * drug_legalization + 
                                GDP_per_capita_discretized * smoking_deaths + 
                                smoking_deaths * drug_legalization + 
                                continent + 
                                covid_deaths + 
                                U1 + U2 + U3 + U4, 
                              data = dataset1)


# חישוב השאריות של המודל
residuals_full_model <- residuals(model_with_interactions)

# מבחן SW
cat("Shapiro-Wilk Test:\n")
shapiro_test <- shapiro.test(residuals_full_model)
print(shapiro_test)

# נירמול השאריות
normalized_residuals <- (residuals_full_model - mean(residuals_full_model)) / sd(residuals_full_model)

# מבחן KS
cat("\nKolmogorov-Smirnov Test:\n")
ks_test <- ks.test(normalized_residuals, "pnorm")
print(ks_test)

#----------------------------------------------------------------------------------
#מבחן Chow 
install.packages("strucchange")
library(strucchange)
chow_test <- sctest(model_with_interactions, type = "Chow", point = breakpoint)
chow_test

#----------------------------------------------------------------------------------

# יצירת מודל רגרסיה ליניארי עם dataset1
model_original <- lm(covid_cases ~ ., data = dataset1)
\
# התקנת חבילת MASS אם עדיין לא מותקנת
if (!require(MASS)) install.packages("MASS")
library(MASS)

# הרצת מבחן Box-Cox
boxcox_result <- boxcox(model_original, lambda = seq(-2, 2, 0.1))

# מציאת הערך האופטימלי של λ
lambda_optimal <- boxcox_result$x[which.max(boxcox_result$y)]
cat("הערך האופטימלי של λ הוא:", lambda_optimal, "\n")


# ביצוע טרנספורמציה למשתנה המוסבר
dataset1$covid_cases_transformed <- ifelse(lambda_optimal == 0,
                                           log(dataset1$covid_cases),
                                           (dataset1$covid_cases^lambda_optimal - 1) / lambda_optimal)
# מודל חדש עם המשתנה המוסבר שעבר טרנספורמציה
model_transformed <- lm(covid_cases_transformed ~ ., data = dataset1)
summary(model_transformed)

# השוואת AIC
cat("AIC של המודל המקורי:", AIC(model_original), "\n")
cat("AIC של המודל עם טרנספורמציה:", AIC(model_transformed), "\n")

shapiro_test <- shapiro.test(residuals(model_transformed))
print(shapiro_test)

normalized_residuals <- (residuals(model_transformed) - mean(residuals(model_transformed))) / 
  sd(residuals(model_transformed))
ks_test <- ks.test(normalized_residuals, "pnorm")
print(ks_test)

library(strucchange)
chow_test <- sctest(model_transformed, type = "Chow", point = 0.5 * nrow(dataset1))
print(chow_test)

#------------------------------------------------------------------------------------------

# חישוב ערכי Adjusted R-squared
adj_r2_original <- summary(model_original)$adj.r.squared
adj_r2_transformed <- summary(model_transformed)$adj.r.squared

# יצירת טבלה מסודרת להצגה
results <- data.frame(
  Model = c("Original Model", "Transformed Model"),
  Adjusted_R2 = c(adj_r2_original, adj_r2_transformed)
)

# הצגת התוצאות בצורה מסודרת
print(results)

