# Loading necessary libraries
#install packages
#install.packages("readxl")
#install.packages("dplyr")
library(dplyr)
library(readxl)
library(corrplot)
library(viridis)

# Setting working directory and loading data
setwd('C:/Nauka/Dataanalytiker/Moje projekty/konkursar/Konk')
# Check if the file exists before loading
if (!file.exists("K_sysselsettnings.xlsx")) {
  stop("The file K_sysselsettnings.xlsx was not found.")
}
if (!file.exists("K_organisasjon.xlsx")) {
  stop("The file K_organisasjon.xlsx was not found.")
}

# Loading datasets
sysselsetning_data <- read_excel("K_sysselsettnings.xlsx")
organisasjon_data <- read_excel("K_organisasjon.xlsx")
konkurser_data <- read_excel("K_konk1.xlsx", sheet = "konkursar,næring")
as_asa_data <- read_excel("K_as_asa.xlsx")


#Merging datasets (sysselsetning with organisasjon data)
syssel_organisasjon <- merge(
  x = sysselsetning_data, 
  y= organisasjon_data, 
  by = 'Kvartal', 
  all.x = TRUE)

#Remove empty columns
syssel_organisasjon <- syssel_organisasjon %>% select(-`Sysselsatte i alt`, -Uoppgitt.y)
konkurser_data <- konkurser_data %>% select(-id)

#Check a structure and summary
lapply(list(syssel_organisasjon, as_asa_data, konkurser_data), function(x){
  list(
    structure = str(x),
    summary = summary(x),
    head = head(x)
  )
})

class(konkurser_data)
class(as_asa_data)
class(syssel_organisasjon)

# Check missing data NA
sapply(list(
  syssel_organisasjon = syssel_organisasjon,
  as_asa_data = as_asa_data,
  konkurser_data = konkurser_data
), function(df) sum(is.na(df)))


#Convert 'Kvartal' to Year and Quarter
convert_quarter_to_date <- function(df, kvartal_col, new_date_col) {
  df$År <- as.numeric(substr(df[[kvartal_col]], 1, 4))
  df$Kvartalsnummer <- as.numeric(substr(df[[kvartal_col]], 6, 6))
  df[[new_date_col]] <- as.Date(paste(df$År, c('01', '04', '07', '10')[df$Kvartalsnummer], '01', sep = '-'))
  df[[kvartal_col]] <- NULL
  return(df)
}

syssel_organisasjon <- convert_quarter_to_date(syssel_organisasjon, "Kvartal", "Data")
as_asa_data <- convert_quarter_to_date(as_asa_data, "Kvartal", "Data")
konkurser_data <- convert_quarter_to_date(konkurser_data, "Dato", "Data")


# Handle issues with character (Konkursar $ Omsetning (1 000 kr), $ Sysselsette)
for (col in c('Omsetning (1 000 kr)', 'Sysselsette')){
    konkurser_data[[col]] <- as.numeric(konkurser_data[[col]])
}

#Debugs
for (col in c('Omsetning (1 000 kr)', 'Sysselsette')) {
  print(unique(konkurser_data[[col]]))
}

#Handle missing numeric data (replace .. with NA and clean up)
for (col in c('Omsetning (1 000 kr)', 'Sysselsette')) {
  konkurser_data[[col]] <- gsub("\\.\\.", "", konkurser_data[[col]])
  konkurser_data[[col]] <- as.numeric(konkurser_data[[col]])
}

summary(konkurser_data)

# Replace levels in as_asa_data$Aksjekapital
as_asa_data$Aksjekapital_num <- sapply(as_asa_data$Aksjekapital, function(x){
  if (x == "Mindre enn 51 000"){
    return(1)
  } else if (x == "51 000-100 000") {
    return(2)  
  } else if (x == "101 000-250 000") {
    return(3)  
  } else if (x == "251 000-500 000") {
    return(4)  
  } else if (x == "Mer enn 500 000") {
    return(5)  
  } else {
    return(NA) 
  }
})

unique(as_asa_data$Aksjekapital)

#Mapping long names to short names
short_names <-  c(
  'Nordland - Nordlánnda' = 'Nordland',
  "Troms - Romsa - Tromssa" = 'Troms',
  'Trøndelag - Trööndelage' = 'Trøndelag',
  'Finnmark - Finnmárku - Finmarkku' = 'Finnmark'
)
as_asa_data$Region <- ifelse(
  as_asa_data$Region %in% names(short_names),
  short_names[as_asa_data$Region],
  as_asa_data$Region
)


################################################################################
# Data analysis
lapply(list(syssel_organisasjon, as_asa_data, konkurser_data), function(x) {
  list(
    structure = str(x),
    summary = summary(x)
  )
})

#Correlation calculation between Konkurser and Syselsette
correlation_result <- cor(konkurser_data$Konkursar, konkurser_data$Sysselsette, use = "complete.obs")
print(correlation_result)
cor.test(konkurser_data$Konkursar, konkurser_data$Sysselsette)
#Det er en sterk positiv lineær sammenheng mellom antall konkurser (Konkursar) og antall sysselsatte (Sysselsette).

#Correlation plot
cor_matrixx <- cor(konkurser_data[, c(3,4, 5)], use= 'complete.obs')
corrplot(cor_matrixx,
         type = 'upper', order='hclust',
           method = 'number',
         addCoef.col = "black", 
         tl.col = "black")

#Hvis et selskap har høy omsetning og et høyt antall ansatte, er det stor sannsynlighet for at dette selskapet også vil ha et høyere antall konkurser.
#Disse sammenhengene kan antyde dypere forbindelser i dataene dine, for eksempel at større selskaper med høyere omsetning og flere ansatte 
#er mer utsatt for økonomiske problemer som kan føre til konkurs.

plot(konkurser_data$Sysselsette, konkurser_data$Konkursar,
     main = "Korelasjon mellom konkurs og sysselsetting",
     xlab = "Antall sysselsatte",
     ylab = "Antall konkurser",
     pch = 16)

model <- lm(Konkursar ~ Sysselsette, data = konkurser_data)
abline(model, col = 'red')
summary(model)

#Den lineære regresjonsanalysen viser en sterk positiv sammenheng mellom antall konkurser og antall sysselsatte, med en R-squared på 0.91, noe som indikerer at 91% av variasjonen i konkurser kan forklares av sysselsettingen.


#More correlation analysis
#Correlation with 'Omsetning'
correlation_result_1 <- cor(konkurser_data$Sysselsette, konkurser_data$`Omsetning (1 000 kr)`, use ='complete.obs')
print(correlation_result_1)
cor.test(konkurser_data$Sysselsette, konkurser_data$`Omsetning (1 000 kr)`)


correlation_result_2 <- cor(konkurser_data$Konkursar, konkurser_data$`Omsetning (1 000 kr)`, use = "complete.obs")
print(correlation_result_2)
cor.test(konkurser_data$Konkursar, konkurser_data$`Omsetning (1 000 kr)`)

correlation_result_3 <-  cor(syssel_organisasjon$`I alt`, syssel_organisasjon$`Aksjeselskap (AS)`, use = 'complete.obs')
cor.test(syssel_organisasjon$`I alt`, syssel_organisasjon$`Aksjeselskap (AS)`)


# Linear regression model
lm_model_sysselsette <- lm(Konkursar ~ Sysselsette, data = konkurser_data)
summary(lm_model_sysselsette)

# Plot with regression line
plot(konkurser_data$Sysselsette, konkurser_data$Konkursar,
     main = "Korelasjon mellom konkurs og sysselsetting",
     xlab = "Antall sysselsatte",
     ylab = "Antall konkurser",
     pch = 16)
abline(lm_model_sysselsette, col = 'red')
predictions <- predict(lm_model_sysselsette, newdata = konkurser_data, interval = "confidence")

sorted_sysselsette <- sort(konkurser_data$Sysselsette)
lower_bound <- predictions[, 2]
upper_bound <- predictions[, 3]
fitted_values <- predictions[, 1]
lines(konkurser_data$Sysselsette, predictions[, 2], col = rgb(0/255, 0/255, 255/255, 0.2), lty = 2)
lines(konkurser_data$Sysselsette, predictions[, 3], col = rgb(0/255, 0/255, 255/255, 0.2), lty = 2)


# Scatter plot with trend line (linear)
plot(konkurser_data$`Omsetning (1 000 kr)`, konkurser_data$Konkursar,
     main = "Konkursar vs Omsetning (1 000 kr)",
     xlab = "Omsetning (1 000 kr)",
     ylab = "Konkursar",
     pch = 16, cex = 0.7)
abline(lm(Konkursar ~ `Omsetning (1 000 kr)`, data = konkurser_data), col = "blue", lwd = 2)


# Histogramy dla zmiennych
par(mfrow = c(1, 2)) 
hist(konkurser_data$`Omsetning (1 000 kr)`,
     main = "Histogram: Omsetning (1 000 kr)",
     xlab = "Omsetning (1 000 kr)", 
     col = "lightgreen")

hist(konkurser_data$Konkursar,
     main = "Histogram: Konkursar",
     xlab = "Konkursar",
     col = "lightcoral")

par(mfrow = c(1, 1))


# Multiple regression model
lm_model_multi <- lm(Konkursar ~ Sysselsette + `Omsetning (1 000 kr)`, data = konkurser_data)
summary(lm_model_multi)

#Regresjonsmodellen viser at både antall sysselsatte og omsetning har en signifikant innvirkning på antall konkurser. 
#Modellen er godt tilpasset dataene og gir verdifull innsikt i forholdet mellom økonomiske faktorer og risikoen for konkurs.


#Create a boxplot to visualize the relationship between Region and Konkurser
as_asa_data <- na.omit(as_asa_data)
par(mar = c(6, 4, 2, 2))
boxplot(Konkurser ~ as.factor(Region), data = as_asa_data,
        main = "Sammenligning av konkurser i ulike regioner",
        xlab = "Region",
        ylab = "Antall konkurser",
        col = viridis(length(unique(as_asa_data$Region))),
        las = 2,
        cex.axis = 0.7)


# Exclude the category "Alle næringer" and calculate total Konkursar per industry (Næring)
konkurser_per_naering <- konkurser_data %>%
  filter(Næring != "A-Z Alle næringar") %>%
  group_by(Næring) %>%
  summarise(Sum_Konkursar = sum(Konkursar, na.rm = TRUE)) %>%
  arrange(desc(Sum_Konkursar)) %>%
  slice_head(n=10)


# Create a bar plot showing the total Konkursar for each Næring (excluding "Alle næringer")
naringer_top10 <- konkurser_per_naering$Næring
sum_konkursar_top10 <- konkurser_per_naering$Sum_Konkursar
naringer_top10_rev <- rev(naringer_top10)
sum_konkursar_top10_rev <- rev(sum_konkursar_top10)

colors <- rev(viridis(10, option = "cividis"))

par(mar = c(5, 12, 4, 2))

barplot_height <- barplot(
  height = sum_konkursar_top10_rev,
  names.arg = naringer_top10_rev,
  main = "Top 10 antall konkurser per næring",
  xlab = "Sum av konkurser",
  col = colors,
  las = 1,
  cex.names = 0.7,
  cex.axis = 0.8,
  horiz = TRUE,
  xlim = c(0, 10000)
)


# Filter data for the industry "Byggje- og anleggsverksemd" (Construction and Civil Engineering)
bygge_data <- konkurser_data %>%
  filter(Næring == "Byggje- og anleggsverksemd") %>%
  group_by(År) %>%
  summarise(Totalt_Konkursar = sum(Konkursar, na.rm = TRUE))

# Convert to vectors for plot
ar <- bygge_data$År
totalt_konkursar <- bygge_data$Totalt_Konkursar

# Filter data for the industry "Varehandel" (Retail trade)
varehandel_data <- konkurser_data %>%
  filter(Næring == "Varehandel, reparasjon av motorvogner") %>%
  group_by(År) %>%
  summarise(Totalt_Konkursar = sum(Konkursar, na.rm = TRUE))

# Convert to vectors for plot
ar_varehandel <- varehandel_data$År
totalt_konkursar_varehandel <- varehandel_data$Totalt_Konkursar


# Set up the plot layout to show plots one after the other
par(mfrow = c(2, 1), mar = c(5, 6, 4, 2))

# Plot for 'Byggje- og anleggsverksemd'
plot(
  x = ar,
  y = totalt_konkursar,
  type = "o", 
  col = "blue",
  main = "Antall konkurser i bygge- og anleggsbransjen fra 2018",
  xlab = "År",
  ylab = "Antall konkurser",
  pch = 16,
  cex.main = 0.9,
  ylim = c(min(totalt_konkursar) * 0.8, max(totalt_konkursar) * 1.1)
)

# Plot for 'Varehandel'
plot(
  x = ar_varehandel,
  y = totalt_konkursar_varehandel,
  type = "o", 
  col = 'steelblue',
  main = "Trender i konkurser i Varehandel, reparasjon av motorvogner fra 2018",
  xlab = "År",
  ylab = "Antall konkurser",
  pch = 16,
  cex.main = 0.9,
  ylim = c(min(totalt_konkursar_varehandel) * 0.8, max(totalt_konkursar_varehandel) * 1.1)
)

par(mfrow = c(1, 1))



# Filter data for the years 2023 and 2024, and quarters 1, 2, and 3
konkurser_23_24 <- konkurser_data[konkurser_data$År %in% c(2023, 2024) & 
                                    konkurser_data$Kvartalsnummer %in% c(1, 2, 3), ]

konkurser_23_24_summary <- aggregate(Konkursar ~ År + Kvartalsnummer, 
                                     data = konkurser_23_24, 
                                     FUN = sum, na.rm = TRUE)

quarters <- konkurser_23_24_summary$Kvartalsnummer
years <- konkurser_23_24_summary$År
values <- konkurser_23_24_summary$Konkursar

bar_data <- matrix(values, 
                   ncol = length(unique(years)), 
                   byrow = TRUE, 
                   dimnames = list(unique(quarters), unique(years)))

colors <- viridis(5, option = "cividis")
quarter_colors <- rep(colors, length.out = nrow(bar_data))


barplot(bar_data, beside = TRUE, 
        col = quarter_colors,
        legend = rownames(bar_data), 
        args.legend = list(title = "Kvartal", x = "topright"),
        main = "Antall konkurser i kvartalene 1, 2 og 3 for 2023 og 2024",
        ylab = "Antall konkurser",
        ) 

# Boxplot for the variable 'Aksjekapital_num' in relation to the number of bankruptcies.
# Create a factor variable for 'Aksjekapital_num' with defined categories.
as_asa_data$Aksjekapital_num <- factor(as_asa_data$Aksjekapital_num, 
                                       levels = 1:5, 
                                       labels = c("Mindre enn 51 000", 
                                                  "51 000-100 000", 
                                                  "101 000-250 000", 
                                                  "251 000-500 000", 
                                                  "Mer enn 500 000"))

# Filter data to include only rows where the number of bankruptcies is greater than 0
filtered_data <- as_asa_data[as_asa_data$Konkurser > 0, ]

# Create a boxplot to visualize how share capital (Aksjekapital) affects the number of bankruptcies.
boxplot(Konkurser ~ Aksjekapital_num, data = filtered_data,
        main = "Hvordan aksjekapital påvirker antall konkurser",
        xlab = "Aksjekapital",
        ylab = "Antall konkurser",
        col = viridis(length(unique(filtered_data$Aksjekapital_num))),
        cex.axis = 0.9)



# English:
#   The boxplot shows regional variations in Konkursar across Norway.
# The bar chart highlights which industries have the most Konkursar.
# The line chart demonstrates a trend in Konkursar in the construction industry over the years.
# The bar chart compares Konkursar in the first three quarters of 2023 and 2024.

#Norwegian:
#   Boksplottet viser regionale variasjoner i konkurser i Norge.
# Stolpediagrammet fremhever hvilke næringer som har flest konkurser.
# Linjediagrammet viser trenden for konkurser i byggebransjen over tid.
# Stolpediagrammet sammenligner konkurser i de første tre kvartalene av 2023 og 2024.
