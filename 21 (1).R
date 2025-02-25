

# R Lab
install.packages("moments")
install.packages("dplyr")


# טעינת הספריות הנדרשות
library(moments)  # עבור חישוב skewness
library(dplyr)    # עבור פעולות עיבוד נתונים

# טעינת נתונים
dataset <- read.csv(file.choose(), header = T, fileEncoding = "latin1")

# מחיקת עמודות ריקות או בעלות תוכן מיותר
dataset <- dataset[, -c(12, 13, 14)]

# שינוי שם של עמודות
colnames(dataset) <- c("country", "GDP_per_capita", "population_density", "alcohol_consumption", "suicide_rate" , "continent", 
                       "smoking_deaths" , "drug_legalization" , "military_service" , "covid_cases" , "covid_deaths" )

# בחירת משתנים רציפים מתוך מאגר הנתונים
continuous_vars <- dataset[, c("GDP_per_capita", "population_density", "alcohol_consumption", 
                               "suicide_rate", "smoking_deaths", "covid_deaths", "covid_cases")]

# יצירת עותק מותאם למשתנים הרציפים ללא covid_cases
continuous_vars_no_cases <- dataset[, c("GDP_per_capita", "population_density", "alcohol_consumption", 
                                        "suicide_rate", "smoking_deaths", "covid_deaths")]

# יצירת plot עבור המשתנים הרציפים ללא covid_cases
plot(continuous_vars_no_cases)

# plot על קשרים בין משתנים מסבירים
plot(dataset$alcohol_consumption, dataset$suicide_rate)
plot(dataset$GDP_per_capita, dataset$suicide_rate)
plot(dataset$GDP_per_capita, dataset$alcohol_consumption)
plot(dataset$population_density, dataset$covid_deaths)
plot(dataset$GDP_per_capita, dataset$covid_deaths)


# פונקציה מותאמת להצגת תוצאות סטטיסטיקה תיאורי
get_summary <- function(var) {
  summary <- list(
    Mean = mean(var, na.rm = TRUE),  # ממוצע
    Median = median(var, na.rm = TRUE),  # חציון
    `Standard Deviation` = sd(var, na.rm = TRUE),  # סטיית תקן
    `1st Qu.` = quantile(var, 0.25, na.rm = TRUE),  # רבעון 1
    `3rd Qu.` = quantile(var, 0.75, na.rm = TRUE),  # רבעון 3
    `Interquartile Range` = IQR(var, na.rm = TRUE),  # טווח בין רבעוני (IQR)
    Skewness = skewness(var, na.rm = TRUE)  # אסימטריה (Skewness)
  )
  return(summary)
}


# הדפסת התוצאות עבור כל משתנה רציף
for (var_name in colnames(continuous_vars)) {
  cat("Statistics for", var_name, ":\n")
  stats <- get_summary(continuous_vars[[var_name]])
  cat("Mean: ", stats$Mean, "\n")
  cat("Median: ", stats$Median, "\n")
  cat("Standard deviation: ", stats$`Standard Deviation`, "\n")
  cat("1st Qu.: ", stats$`1st Qu.`, "\n")
  cat("3rd Qu.: ", stats$`3rd Qu.`, "\n")
  cat("Interquartile range: ", stats$`Interquartile Range`, "\n")
  cat("Skewness: ", stats$Skewness, "\n\n")
}

# ניתוח משתנה קטגוריאלי לפי קטגוריות של military_service
descriptive_stats <- dataset %>%
  group_by(military_service) %>%
  summarise(
    Mean = mean(covid_cases, na.rm = TRUE),  # ממוצע
    Median = median(covid_cases, na.rm = TRUE),  # חציון
    Std_Dev = sd(covid_cases, na.rm = TRUE),  # סטיית תקן
    Q1 = quantile(covid_cases, 0.25, na.rm = TRUE),  # רבעון 1
    Q3 = quantile(covid_cases, 0.75, na.rm = TRUE),  # רבעון 3
    IQR = IQR(covid_cases, na.rm = TRUE),  # טווח בין רבעוני (IQR)
    Skewness = skewness(covid_cases, na.rm = TRUE)  # אסימטריה (Skewness)
  )

# הצגת התוצאות עבור המשתנה הקטגוריאלי
print(descriptive_stats)


#הוצאת חריגים covid_cases
bp<- boxplot(dataset$covid_cases, main = "covid_cases", outline = T)
# Get that statistics values
bp$stats
# Get the outlier value(s)
bp$out
# Remove the outlier


#טבלה חדשה לאחר הוצאת משתנים
dataset1<-subset(dataset, covid_cases< 1144853)
bp<- boxplot(dataset1$covid_cases, main = "covid_cases", outline = F)
bp$stats
# Get the outlier value(s)
bp$out


# בדיקת חריגים GPD
bp_gdp <- boxplot(dataset$GDP_per_capita, main = "GDP_per_capita", outline = TRUE)
bp_gdp$stats  # סטטיסטיקות
bp_gdp$out    # ערכים חריגים


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


# alcohol_consumption הצגת חריגים
bp_alcohol <- boxplot(dataset$alcohol_consumption, main = "alcohol_consumption", outline = TRUE)
bp_alcohol$stats  # סטטיסטיקות
bp_alcohol$out    # ערכים חריגים

# הצגת חריגים suicide_rate
bp_suicide <- boxplot(dataset$suicide_rate, main = "suicide_rate", outline = TRUE)
bp_suicide$stats  # סטטיסטיקות
bp_suicide$out    # ערכים חריגים


# היסטוגרמה GPD
hist(dataset1$GDP_per_capita, prob = TRUE,
     main = 'Histogram of GDP per Capita',
     xlab = 'GDP per Capita', col = "lightblue", breaks = 11,
     cex.axis = 0.8, cex.lab = 0.9)
box()
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")

# הוספת קו צפיפות
lines(density(dataset1$GDP_per_capita, na.rm = TRUE), col = "blue", lwd = 2)


# פונקציית צפיפות GPD
plot(density(dataset1$GDP_per_capita, na.rm = TRUE),
     main = "Density Function of GDP per Capita",
     xlab = "GDP per Capita (USD)",
     ylab = "Density",
     col = "blue", lwd = 2)
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")


# הכנת הנתונים לפונקציית ההתפלגות המצטברת (CDF)
sorted_data <- sort(dataset1$GDP_per_capita[!is.na(dataset1$GDP_per_capita)])

# יצירת CDF
cdf <- ecdf(sorted_data)

# תרשים CDF
plot(cdf, 
     main = "Cumulative GDP per Capita",  
     xlab = "GDP per Capita (USD)", 
     ylab = "Cumulative Probability", 
     col = "blue", lwd = 2.2)
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")

# הוספת קו של התפלגות נורמלית
mean_val <- mean(sorted_data)
sd_val <- sd(sorted_data)
x_vals <- seq(min(sorted_data), max(sorted_data), length = 100)
normal_cdf <- pnorm(x_vals, mean = mean_val, sd = sd_val)

# הוספת קו ההתפלגות הנורמלית
lines(x_vals, normal_cdf, col = "lightblue", lwd = 2)

# density function ו alcohol consumption היסטוגרמה
hist(dataset1$alcohol_consumption, prob = TRUE,
     main = "Histogram Alcohol Consumption",
     xlab = "Alcohol Consumption (liters per capita)",
     col = "lightblue",
     breaks = 11,
     cex.axis = 0.8, cex.lab = 0.9)
box()
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")


# הוספת קו צפיפות
lines(density(dataset1$alcohol_consumption, na.rm = TRUE), col = "blue", lwd = 2)


# פונקציית התפלגות מצטברת alcohol consumption
sorted_data <- sort(dataset1$alcohol_consumption[!is.na(dataset1$alcohol_consumption)])
cdf <- ecdf(sorted_data)

plot(cdf, 
     main = "Cumulative Alcohol Consumption",  
     xlab = "Alcohol Consumption (liters per capita)", 
     ylab = "Cumulative Probability", 
     col = "blue", lwd = 2.2)
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")


# הוספת קו התפלגות נורמלית
mean_val <- mean(sorted_data)
sd_val <- sd(sorted_data)
x_vals <- seq(min(sorted_data), max(sorted_data), length = 100)
normal_cdf <- pnorm(x_vals, mean = mean_val, sd = sd_val)


# הוספת קו התפלגות מצטברת
lines(x_vals, normal_cdf, col = "lightblue", lwd = 2)


# היסטוגרמה עם קו צפיפות עבור Population Density
hist(dataset1$population_density, 
     prob = TRUE, 
     main = "Histogram Population Density", 
     xlab = "Population Density (people per sq. km)", 
     col = "lightblue", 
     breaks = 15, 
     cex.axis = 0.8, 
     cex.lab = 0.9)

lines(density(dataset1$population_density, na.rm = TRUE), col = "blue", lwd = 2)
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")

# פונקציית התפלגות מצטברת Population Density
sorted_data <- sort(dataset1$population_density[!is.na(dataset1$population_density)])
cdf <- ecdf(sorted_data)

# תרשים CDF
plot(cdf, 
     main = "Cumulative Population Density",  
     xlab = "Population Density (people per sq. km)", 
     ylab = "Cumulative Probability", 
     col = "blue", lwd = 2.2)
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")

# הוספת קו להשוואה מול התפלגות נורמלית
mean_val <- mean(sorted_data)
sd_val <- sd(sorted_data)
x_vals <- seq(min(sorted_data), max(sorted_data), length = 100)
normal_cdf <- pnorm(x_vals, mean = mean_val, sd = sd_val)

# הוספת קו ההתפלגות הנורמלית
lines(x_vals, normal_cdf, col = "lightblue", lwd = 2)


# התקנת ggplot2 וטעינת החבילה
install.packages("ggplot2")  
library(ggplot2)


# יצירת תרשים פיזור עם משתנים רציפים וקטגוריאליים
ggplot(dataset1, aes(x = population_density, y = covid_deaths, color = continent)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Scatterplot of Population Density vs. Covid Deaths by Continent",
       x = "Population Density (people per sq. km)",
       y = "Covid Deaths",
       color = "Continent") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer(palette = "Set1")


# יצירת Violin Plot עבור GDP לנפש
ggplot(dataset1, aes(x = continent, y = GDP_per_capita, fill = continent)) +
    geom_violin(trim = FALSE, alpha = 0.7) +  # יצירת גרף violin
    labs(
      title = "GDP per Capita by Continent",
      x = "Continent",
      y = "GDP per Capita (USD)",
      fill = "Continent"
    ) +
    scale_fill_brewer(palette = "Set2") +  # צבעים עבור כל יבשת
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "none"
    )
  

# התקנת ggridges וטעינת החבילה
install.packages("ggridges")
library(ggridges)


# Ridgeline Plot עבור צריכת אלכוהול לנפש
ggplot(dataset1, aes(x = alcohol_consumption, y = continent, fill = continent)) +
    geom_density_ridges(scale = 1.5, alpha = 0.8) +  # יצירת קווי צפיפות לכל יבשת
    scale_fill_brewer(palette = "Set2") +  # צבעים לכל יבשת
    labs(
      title = "Ridgeline Plot of Alcohol Consumption by Continent",
      x = "Alcohol Consumption (liters per capita)",
      y = "Continent",
      fill = "Continent"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "none"
    )
  
# יצירת טבלת שכיחות עבור continent
  table_continent <- table(dataset1$continent)
  
# הצגת הטבלה עם אחוזים מצטברים
  prop_continent <- prop.table(table_continent) * 100  # חישוב אחוזים
  cumulative_continent <- cumsum(prop_continent)  # חישוב אחוזים מצטברים
  
# הדפסה
  result_continent <- cbind(Frequency = table_continent, Percent = round(prop_continent, 2), Cumulative_Percent = round(cumulative_continent, 2))
  print(result_continent)
  
# יצירת טבלה דו-ממדית בין continent ו-drug_legalization
  table_drug_legalization_continent <- table(dataset1$continent, dataset1$drug_legalization)
  
# חישוב אחוזים לכל קומבינציה
  percent_table <- prop.table(table_drug_legalization_continent) * 100
  
# חישוב אחוזים מצטברים
  cumulative_percent_table <- apply(percent_table, 1, cumsum)  # אחוזים מצטברים לפי שורות
  
# יצירת טבלה עם כל הערכים
  result <- as.data.frame.table(table_drug_legalization_continent)
  colnames(result) <- c("Continent", "Drug_Legalization", "Frequency")
  
# הוספת עמודת Percent
  result$Percent <- round(percent_table[cbind(as.character(result$Continent), as.character(result$Drug_Legalization))], 2)
  
# חישוב אחוזים מצטברים עבור כל יבשת בנפרד
  result$Cumulative_Percent <- NA  # הגדרת עמודת Cumulative Percent
  for (i in unique(result$Continent)) {
    # עבור כל יבשת, מחשבים את אחוזים המצטברים
    continent_data <- result[result$Continent == i, ]
    continent_data$Cumulative_Percent <- cumsum(continent_data$Percent)
    result[result$Continent == i, "Cumulative_Percent"] <- continent_data$Cumulative_Percent
  }
  
# הצגת התוצאה
  print(result)
  