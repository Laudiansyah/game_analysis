#Below is my code for the Business Analytics course, which investigates a game 
#dataset, specifically focusing on Data Loading and Libraries, Trend Analysis, 
#Charts by Region, Pie Charts, Correlation Analysis, Linear Regression, and 
#Additional Visualizations.

install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("ggcorrplot")
install.packages("tidyverse")
install.packages("plotly")
install.packages("reshape2")
install.packages("forcats")
install.packages("Hmisc")

library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)
library(ggcorrplot)
library(tidyverse)
library(plotly)
library(reshape2)
library(forcats)
library(Hmisc)

data <- read_csv("E:/project github/(R) Game/vgsales.csv")
head(data)

#TREN
#Grafik Tren Pubslishers
top_publishers <- data %>% count(Publisher, sort = TRUE) %>% head(5)
publisher_trend <- data %>% count(Year, Publisher) %>% 
  filter(Publisher %in% top_publishers$Publisher)
ggplot(publisher_trend, aes(x = Year, y = n, color = Publisher, group = Publisher)) +
  geom_line() + geom_point() + labs(title = "Tren Jumlah Game dari Top 5 Publisher",
  x = "Tahun", y = "Jumlah Game", color = "Publisher") + theme_minimal()

#Grafik Tren Genre
top_genres <- data %>% count(Genre, sort = TRUE) %>% head(5)
platforms_genres <- data %>% count(Year, Genre) %>% filter(Genre %in% top_genres$Genre)
ggplot(platforms_genres, aes(x = Year, y = n, color = Genre, group = Genre)) +
  geom_line() + geom_point() + labs(title = "Tren Jumlah Game dari Top 5 Genres",
  x = "Tahun", y = "Jumlah Game", color = "Genre") + theme_minimal()

#Grafik Tren Platforms
top_platforms <- data %>% count(Platform, sort = TRUE) %>% head(5)
platforms_trend <- data %>% count(Year, Platform) %>% 
  filter(Platform %in% top_platforms$Platform)
ggplot(platforms_trend, aes(x = Year, y = n, color = Platform, group = Platform)) +
  geom_line() + geom_point() + labs(title = "Tren Jumlah Game dari Top 5 Platforms",
  x = "Tahun", y = "Jumlah Game", color = "Platforms") + theme_minimal()

################################################################################

#TOP 10 GAME 2009
#Grafik bar game penjualan Global tertinggi 2009
data_2009 <- data %>% filter(Year == "2009")
sales_by_name <- data_2009 %>% group_by(Name) %>%
  summarise(Total_Global_Sales = sum(Global_Sales))
top_name <- sales_by_name %>% top_n(10, Total_Global_Sales)
top_name <- top_name %>% arrange(desc(Total_Global_Sales))
bar_chart <- ggplot(top_name, aes(x = reorder(Name, Total_Global_Sales), 
  y = Total_Global_Sales)) + geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = Total_Global_Sales), vjust = -0.5, color = "black") + 
  labs(title = "Top 10 Game berdasarkan penjualan Global di 2009",
  x = "Game", y = "Total Global Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(bar_chart)

#Grafik bar game penjualan Amerika Utara tertinggi 2009
data_2009 <- data %>% filter(Year == "2009")
sales_by_name <- data_2009 %>%  group_by(Name) %>%
  summarise(Total_NA_Sales = sum(NA_Sales))
top_name <- sales_by_name %>% top_n(10, Total_NA_Sales)
top_name <- top_name %>% arrange(desc(Total_NA_Sales))
bar_chart <- ggplot(top_name, aes(x = reorder(Name, Total_NA_Sales), 
  y = Total_NA_Sales)) + geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = Total_NA_Sales), vjust = -0.5, color = "black") + 
  labs(title = "Top 10 Game berdasarkan penjualan Amerika Utara di 2009",
  x = "Game", y = "Total Amerika Utara Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(bar_chart)

#Grafik bar game penjualan Eropa tertinggi 2009
data_2009 <- data %>% filter(Year == "2009")
sales_by_name <- data_2009 %>% group_by(Name) %>%
  summarise(Total_EU_Sales = sum(EU_Sales))
top_name <- sales_by_name %>% top_n(10, Total_EU_Sales)
top_name <- top_name %>% arrange(desc(Total_EU_Sales))
bar_chart <- ggplot(top_name, aes(x = reorder(Name, Total_EU_Sales), 
  y = Total_EU_Sales)) + geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = Total_EU_Sales), vjust = -0.5, color = "black") + 
  labs(title = "Top 10 Game berdasarkan penjualan Eropa di 2009",
  x = "Game", y = "Total Eropa Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(bar_chart)

#Grafik bar game penjualan Jepang tertinggi 2009
data_2009 <- data %>% filter(Year == "2009")
sales_by_name <- data_2009 %>% group_by(Name) %>%
  summarise(Total_JP_Sales = sum(JP_Sales))
top_name <- sales_by_name %>% top_n(10, Total_JP_Sales)
top_name <- top_name %>% arrange(desc(Total_JP_Sales))
bar_chart <- ggplot(top_name, aes(x = reorder(Name, Total_JP_Sales), 
  y = Total_JP_Sales)) + geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = Total_JP_Sales), vjust = -0.5, color = "black") + 
  labs(title = "Top 10 Game berdasarkan penjualan Jepang di 2009",
  x = "Game", y = "Total Jepang Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(bar_chart)

#Grafik bar game penjualan Others tertinggi 2009
data_2009 <- data %>% filter(Year == "2009")
sales_by_name <- data_2009 %>%  group_by(Name) %>%
  summarise(Total_Other_Sales = sum(Other_Sales))
top_name <- sales_by_name %>% top_n(10, Total_Other_Sales)
top_name <- top_name %>% arrange(desc(Total_Other_Sales))
bar_chart <- ggplot(top_name, aes(x = reorder(Name, Total_Other_Sales), 
  y = Total_Other_Sales)) + geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = Total_Other_Sales), vjust = -0.5, color = "black") + 
  labs(title = "Top 10 Game berdasarkan penjualan Others di 2009",
  x = "Game", y = "Total Others Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(bar_chart)

################################################################################

#PIE GLOBAL
#Grafik PIE Global persentase genre
genre_sales <- data %>% group_by(Genre) %>% 
  summarise(Total_Global_Sales = sum(Global_Sales)) %>%
  arrange(desc(Total_Global_Sales)) %>% head(5)
pie_chart <- ggplot(genre_sales, aes(x = "", y = Total_Global_Sales, fill = Genre)) +
  geom_bar(stat = "identity") + coord_polar("y", start = 0) + theme_void() +
  theme(legend.position = "right") + labs(title = "Top 5 Genres berdasarkan penjualan global") +
  geom_text(aes(label = paste(scales::comma(Total_Global_Sales), "\n", 
  scales::percent(Total_Global_Sales / sum(Total_Global_Sales))), y = Total_Global_Sales), 
  position = position_stack(vjust = 0.5), angle = 0, hjust = 0.5)+
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd"))
print(pie_chart)

#Grafik PIE global persentase platform
platform_sales <- data %>% group_by(Platform) %>% summarise(Total_Global_Sales = sum(Global_Sales)) %>%
  arrange(desc(Total_Global_Sales)) %>% head(5)
pie_chart <- ggplot(platform_sales, aes(x = "", y = Total_Global_Sales, fill = Platform)) +
  geom_bar(stat = "identity") + coord_polar("y", start = 0) + coord_polar("y", start = 0) + 
  theme_void() + theme(legend.position = "right") +
  labs(title = "Top 5 Platform berdasarkan penjualan global") +
  geom_text(aes(label = paste(scales::comma(Total_Global_Sales), "\n", 
  scales::percent(Total_Global_Sales / sum(Total_Global_Sales))), y = Total_Global_Sales), 
  position = position_stack(vjust = 0.5), angle = 0, hjust = 0.5) + 
  scale_fill_manual(values = c("#1f77b4", "#7f7f7f", "#8c564b", "#d62728", "#9467bd"))
print(pie_chart)

#Grafik PIE global persentase publisher
publisher_sales <- data %>% group_by(Publisher) %>% summarise(Total_Global_Sales = sum(Global_Sales)) %>%
  arrange(desc(Total_Global_Sales)) %>% head(5)
pie_chart <- ggplot(publisher_sales, aes(x = "", y = Total_Global_Sales, fill = Publisher)) +
  geom_bar(stat = "identity") + coord_polar("y", start = 0) + coord_polar("y", start = 0) + 
  theme_void() + theme(legend.position = "right") +
  labs(title = "Top 5 Publisher berdasarkan penjualan global") +
  geom_text(aes(label = paste(scales::comma(Total_Global_Sales), "\n", 
  scales::percent(Total_Global_Sales / sum(Total_Global_Sales))), y = Total_Global_Sales), 
  position = position_stack(vjust = 0.5), angle = 0, hjust = 0.5)+ 
  scale_fill_manual(values = c("#1f77b4", "#2ca02c", "#ff7f0e", "#7f7f7f", "#bcbd22"))
print(pie_chart)

################################################################################
#Grafik Garis Nintendo tahun 2006-2016
nintendo_data <- data %>% filter(Publisher == "Nintendo" & Year >= 2006 & Year <= 2016)
sales_by_year <- nintendo_data %>% group_by(Year) %>% summarise(Total_Global_Sales = sum(Global_Sales))
line_chart <- ggplot(sales_by_year, aes(x = Year, y = Total_Global_Sales)) +
  geom_line(aes(group = 1), color = "black") +
  geom_point(color = "blue", size = 3) + geom_text(aes(label = Total_Global_Sales), 
  vjust = -0.5, color = "black", size = 3) + 
  labs(title = "Total Penjualan Global Game Publisher Nintendo (2006-2016)",
  x = "Year", y = "Total Global Sales") + theme_minimal()
print(line_chart)

#Grafik Garis Electronic Arts tahun 2006-2016
EA_data <- data %>% filter(Publisher == "Electronic Arts" & Year >= 2006 & Year <= 2016)
sales_by_year <- EA_data %>% group_by(Year) %>% summarise(Total_Global_Sales = sum(Global_Sales))
line_chart <- ggplot(sales_by_year, aes(x = Year, y = Total_Global_Sales)) + geom_line(aes(group = 1), color = "black") +
  geom_point(color = "blue", size = 3) + geom_text(aes(label = Total_Global_Sales), 
  vjust = -0.5, color = "black", size = 3) +  # Menambahkan label di atas setiap titik
  labs(title = "Total Penjualan Global Game Publisher Electronic Arts (2006-2016)",
  x = "Year", y = "Total Global Sales") + theme_minimal()
print(line_chart)

#Grafik Garis Activision tahun 2006-2016
ACT_data <- data %>%  filter(Publisher == "Activision" & Year >= 2006 & Year <= 2016)
sales_by_year <- ACT_data %>% group_by(Year) %>% summarise(Total_Global_Sales = sum(Global_Sales))
line_chart <- ggplot(sales_by_year, aes(x = Year, y = Total_Global_Sales)) +
  geom_line(aes(group = 1), color = "black") + geom_point(color = "blue", size = 3) +
  geom_text(aes(label = Total_Global_Sales), vjust = -0.5, color = "black", size = 3) +
  labs(title = "Total Penjualan Global Game Publisher Activision (2006-2016)",
  x = "Year", y = "Total Global Sales") + theme_minimal()
print(line_chart)

################################################################################

#Cleaning Data
head(data)
clean_data <- data %>% mutate_all(~ifelse(. == "N/A", NA, .))
clean_data <- na.omit(clean_data)
clean_data <- distinct(clean_data)
str(clean_data)

#Korelasi heatmap
cor_matrix <- cor(clean_data[, c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales")])
ggcorrplot(cor_matrix, type = "lower", lab = TRUE, outline.color = "white", 
   ggtheme = ggplot2::theme_minimal() )
ggcorrplot(cor_matrix, type = "lower", lab = TRUE)
print(ggcorrplot)

#regresi
model <- lm(JP_Sales ~ NA_Sales + EU_Sales + Other_Sales, data = genre_sales)
summary(model)

#JP_Sales = 0.031359 + 0.114754 * NA_Sales + 0.171217 * EU_Sales - 0.171478 * Other_Sales

Modeling <- 0.031359 + 0.114754 * data$NA_Sales + 0.171217 * data$EU_Sales - 0.171478 * data$Other_Sales
genre_sales$Modeling <- Modeling
sorted_data <- arrange(genre_sales, desc(Modeling))
print(sorted_data)

################################################################################

#years games release by genre
ggplot(data, aes(x = as.factor(Year))) + geom_bar(aes(fill = Genre), position = "dodge") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Years of Game Releases by Genre", x = "Year", y = "Number of Games Released") +
  scale_fill_brewer(palette = "Set3") 

#Top 5 years games release by genre
top_years <- names(sort(table(data$Year), decreasing = TRUE)[1:5])
filtered_data <- subset(data, Year %in% top_years)
ggplot(filtered_data, aes(x = as.factor(Year))) + 
  geom_bar(aes(fill = Genre), position = "dodge") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Top 5 Years of Game Releases by Genre", x = "Year",
  y = "Number of Games Released") + scale_fill_brewer(palette = "Set3")

#Tahun per genre
year_max_genre <- data %>% filter(Year >= 1980 & Year <= 2016) %>% group_by(Year, Genre) %>%
  summarise(count = n()) %>% group_by(Year) %>% filter(count == max(count)) %>% slice(n())
ggplot(year_max_genre, aes(x = as.factor(Year), y = count, fill = Genre)) +
  geom_bar(stat = "identity") + geom_text(aes(label = paste(Genre, "-", count)), vjust = -0.5, 
  size = 1.5, angle = 0, hjust = 0.5, position = position_dodge(width = 0.1)) +
  labs(title = "Genre with Most Releases in a Single Year (1980-2016)", x = "Year",
  y = "Number of Releases") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_brewer(palette = "Set3")  

#####################################################################################

#genre per global
genre_sales <- data %>% group_by(Genre) %>% summarise(Total_Sales = sum(Global_Sales)) %>%
  arrange(desc(Total_Sales))
top_genre <- genre_sales$Genre[1]
genre_sales <- genre_sales %>% mutate(Percentage = Total_Sales / sum(Total_Sales) * 100)
ggplot(genre_sales, aes(x = reorder(Genre, Total_Sales), y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "skyblue") + geom_text(aes(label = paste0(round(Total_Sales, 2), 
  " (", round(Percentage, 1), "%)")), vjust = -0.5, size = 4, color = "black") +
  labs(title = "Global Sales by Genre", x = "Genre", y = "Global Sales (in millions)") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#genre per count
genre_counts <- data %>% group_by(Genre) %>% filter(Year >= 1980 & Year <= 2016) %>%
  summarise(Count = n()) %>% arrange(Count) 
ggplot(genre_counts, aes(x = reorder(Genre, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = Count), vjust = -0.5, size = 3) + 
  labs(title = "Jumlah Game Berdasarkan Genre (1980-2016)", x = "Genre", y = "Jumlah Game") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

################################################################################

#comparison genre
comp_genre <- data %>%  select(Genre, NA_Sales, EU_Sales, JP_Sales, Other_Sales)
comp_map <- comp_genre %>% group_by(Genre) %>% summarise_all(sum)
ggplot(melt(comp_map), aes(x = Genre, y = variable, fill = value)) + geom_tile(color = "white") +
  geom_text(aes(label = round(value, 1)), vjust = 1) + scale_fill_gradient(low = "lightyellow", high = "darkred") +
  labs(title = "Comparison of Genre Sales", x = "Genre", y = "Region Sales", fill = "Sales") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
  axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
  legend.title = element_text(size = 12), legend.text = element_text(size = 10))

#Grafik bar Global persentase genre (top)
top_5_global_genre <- data %>% select(Genre, Global_Sales) %>% group_by(Genre) %>%
  summarise(Total_Sales = sum(Global_Sales)) %>% top_n(5, Total_Sales) %>%
  arrange(desc(Total_Sales))
custom_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd")
ggplot(top_5_global_genre, aes(x = reorder(Genre, Total_Sales), y = Total_Sales, fill = Genre)) +
  geom_bar(stat = "identity") + geom_text(aes(label = Total_Sales), vjust = -0.5, size = 4, color = "black") + 
  labs(title = "Top 5 Genre Sales Global", x = "Genre", y = "Global Sales (in millions)") +
  scale_fill_manual(values = custom_colors) + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), axis.text.y = element_text(size = 12))

#Grafik bar Global persentase genre (bottom)
bottom_5_global_genre <- data %>% select(Genre, Global_Sales) %>% group_by(Genre) %>%
  summarise(Total_Sales = sum(Global_Sales)) %>% top_n(-5, Total_Sales) %>% arrange(Total_Sales)
custom_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd")
ggplot(bottom_5_global_genre, aes(x = reorder(Genre, Total_Sales), y = Total_Sales, fill = Genre)) +
  geom_bar(stat = "identity") + geom_text(aes(label = Total_Sales), vjust = -0.5, size = 4, color = "black") +
  labs(title = "Bottom 5 Genre Sales Global", x = "Genre", y = "Global Sales (in millions)") +
  scale_fill_manual(values = custom_colors) + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
  axis.text.y = element_text(size = 12))

################################################################################

#genre dan publisher ACT
activision_data <- data[data$Publisher == "Activision", ]
top_5_activision_genre <- activision_data %>% select(Genre, Global_Sales) %>%
  group_by(Genre) %>% summarise(Total_Sales = sum(Global_Sales)) %>%
  top_n(5, Total_Sales) %>% arrange(desc(Total_Sales))
custom_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd")
ggplot(top_5_activision_genre, aes(x = reorder(Genre, Total_Sales), y = Total_Sales, fill = Genre)) +
  geom_bar(stat = "identity") + geom_text(aes(label = Total_Sales), vjust = -0.5, size = 4, color = "black") + 
  labs(title = "Top 5 Genre Sales for Activision", x = "Genre", y = "Global Sales (in millions)") +
  scale_fill_manual(values = custom_colors) + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
  axis.text.y = element_text(size = 12))

#genre dan publisher EA
EA_data <- data[data$Publisher == "Electronic Arts", ]
top_5_EA_genre <- EA_data %>% select(Genre, Global_Sales) %>% group_by(Genre) %>%
  summarise(Total_Sales = sum(Global_Sales)) %>% top_n(5, Total_Sales) %>%
  arrange(desc(Total_Sales))
custom_colors <- c("#1f77b4", "#7f7f7f", "#d62728", "#8c564b", "#9467bd")
ggplot(top_5_EA_genre, aes(x = reorder(Genre, Total_Sales), y = Total_Sales, fill = Genre)) +
  geom_bar(stat = "identity") + geom_text(aes(label = Total_Sales), vjust = -0.5, size = 4, color = "black") + 
  labs(title = "Top 5 Genre Sales for Electronic Arts", x = "Genre", y = "Global Sales (in millions)") +
  scale_fill_manual(values = custom_colors) + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
  axis.text.y = element_text(size = 12))

#genre dan publisher Nintendo
nintendo_data <- data[data$Publisher == "Nintendo", ]
top_5_nintendo_genre <- nintendo_data %>% select(Genre, Global_Sales) %>% group_by(Genre) %>%
  summarise(Total_Sales = sum(Global_Sales)) %>% top_n(5, Total_Sales) %>%
  arrange(desc(Total_Sales))
custom_colors <- c("#ff7f0e", "yellow", "#7f7f7f", "#2ca02c", "#9467bd")
ggplot(top_5_nintendo_genre, aes(x = reorder(Genre, Total_Sales), y = Total_Sales, fill = Genre)) +
  geom_bar(stat = "identity") + geom_text(aes(label = Total_Sales), vjust = -0.5, size = 4, color = "black") + 
  labs(title = "Top 5 Genre Sales for Nintendo", x = "Genre", y = "Global Sales (in millions)") +
  scale_fill_manual(values = custom_colors) + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
  axis.text.y = element_text(size = 12))