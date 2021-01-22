#=================================================================================#
# FILE   : appstore.R
# AUTHOR : Jiin Jeong
# DATE   : Sunday, September 30, 2018
# DESC   : Analysis of Apple App Store and Google Play Store applications
#=================================================================================#

### Loads necessary libraries and datasets; modifies as necessary.
library(ggplot2)
library(mosaic)  # Can also use dplyr.
library(tidyverse)
library(stringr)
options(scipen=999)  # Removes scientific notation.

apple = read.csv("AppleStore.csv")
google = read.csv("GooglePlayStore.csv")

# Apple data: add a new column to the dataset that determines if the app is free or not.
apple = within(apple, {free = ifelse(price == 0, FALSE, TRUE)})
View(apple)  # Can check the changed dataset.

# Google data: Delete one row with weirdly formatted data.
google = google[-c(10473),]

# Google data: Using regex, replace punctuations and convert into integers.
# Regex: "[$]" also works!
google$new_price = as.numeric(gsub(pattern = "\\$", "", google$Price))
google$new_install = as.numeric(gsub(pattern = "[[:punct:]]", "", google$Installs))
View(google)

summary(apple)
summary(google)

# =============== 01. Genre =============== #
summary(apple$prime_genre)
summary(google$Category)    # 1.9 is the mislabled data which I deleted.

# ----- Apple App Store. ----- #
# To improve this, we could reorder the bars from highest to lowest (how???)!
ggplot(data = apple) +
  aes(x = factor(prime_genre)) +
  geom_bar(fill="steelblue") +
  # So that we can see the x-axis tick labels!
  #theme(axis.text.x = element_text(angle = -90)) +
  coord_flip() +
  labs(x = 'App Genres',
       y = 'Count',
       title = 'Apple App Store: Count by Genre')

# Game is such an outlier! What if we try, without games genre.
apple %>%
  filter(prime_genre != "Games") %>%
  ggplot() +
  aes(x = factor(prime_genre)) +
  coord_flip() +
  # theme(axis.text.x = element_text(angle = -90)) +
  geom_bar(fill="steelblue") +
  labs(x = 'App Genres',
       y = 'Count',
       title = 'Apple App Store: Genre Distribution (without Games)')

# ----- Google Play Store. ----- #
ggplot(data = google) +
  aes(x = factor(Category)) +
  geom_bar(fill="steelblue") +
  #theme(axis.text.x = element_text(angle = -90)) +
  coord_flip() +
  labs(x = 'App Genres',
       y = 'Count',
       title = 'Google Play Store: Count by Genre')

# Family, game, and tools are the three biggest genres. What if we try, without these three.
google %>%
  filter(Category != "FAMILY" & Category != "GAME" & Category != "TOOLS") %>%
  ggplot() +
  aes(x = factor(Category)) +
  geom_bar(fill="steelblue") +
  #theme(axis.text.x = element_text(angle = -90)) +
  coord_flip() +
  labs(x = 'App Genres',
       y = 'Count',
       title = 'Google Play Store: Genre Distribution (without three largest genres)')

# =============== 02. Price =============== #
summary(apple$price)
summary(google$new_price)

prop.table(table(apple$free))  # Proportion table of free/paid apps.
prop.table(table(google$Type))

t.test(apple$price, google$new_price, alternative = "two.sided")

# ----- Apple App Store. ----- #
# For both Apple and Google datasets, I removed outliers over $50.
apple %>%
  filter(price < 50) %>%
  ggplot() +
  aes(x = price, fill = prime_genre, color = prime_genre) +
  geom_histogram(alpha = 0.5) +
  labs(x = 'Price (USD)',
       y = 'Count',
       title = 'Apple App Store: Price Distribution of Genres')

# ----- Google Play Store. ----- #
google %>%  # Google Play Store apps by genre without outliers (over $50...).
  filter(new_price < 50) %>%
  ggplot() +
  aes(x = new_price, fill = Category, color = Category) +
  geom_histogram(alpha = 0.5) +
  labs(x = 'Price (USD)',
       y = 'Count',
       title = 'Google Play Store: Price Distribution by Genre')

# Distribution of Google free apps. (Try layering on top of category...!)
google %>%
  filter(new_price == 0) %>%
  ggplot() +
  aes(x = factor(Category)) +
  geom_bar(fill = "steelblue") +
  coord_flip() +
  labs(x = 'Category',
       y = 'Count',
       title = 'Google Play Store: Genre Distribution of free apps')

# =============== 03. Rating =============== #
summary(apple$user_rating)  # User ratings are intervals of 0.5
summary(google$Rating)      # User ratings are intervals of 0.1

t.test(apple$user_rating, google$Rating, alternative = "two.sided")

# ----- Apple App Store. ----- #
# CITE : http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
# DESC : Found additional functions for histograms in ggplot2
# Histogram of user ratings (left-skewed, with exception of 0-ratings.)
ggplot(data = apple) +
  geom_histogram(col = 'tomato1', fill = 'lightpink2', binwidth = 0.5) +
  aes(x = user_rating) +
  labs(x = 'User Rating',
       y = 'Count',
       title = 'Apple Store User Rating') +
  # Shows the count above the bars.
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  # Line through the mean.
  geom_vline(aes(xintercept=mean(apple$user_rating)),
             color="red", linetype="dashed", size=1) +
  # Relabel tick marks.
  scale_x_continuous(breaks=seq(min(apple$user_rating), max(apple$user_rating), by = 0.5),
                     labels=c("0", "0.5", "1", "1.5", "2", "2.5", "3", "3.5", "4", "4.5", "5"))

# ----- Google Play Store. ----- #
ggplot(data = google) +
  geom_histogram(col = 'tomato1', fill = 'lightpink2', binwidth = 0.5) +
  aes(x = Rating) +
  labs(x = 'User Rating',
       y = 'Count',
       title = 'Google Play Store User Rating') +
  # Line through the mean.
  geom_vline(aes(xintercept=mean(google$Rating, na.rm=TRUE)),  # Removes NA values.
             color="red", linetype="dashed", size=1)

# =============== 04. Application Size (Apple) =============== #
# CITE : http://sape.inf.usi.ch/quick-reference/ggplot2/colour
# DESC : Additional colors for ggplot2
summary(apple$size_bytes)

# Aggregate & Order: Shows app size by genre in descending order (Highest to lowest.)
size_by_genre = aggregate(size_bytes ~ prime_genre,
                          data = apple,
                          FUN = mean)  # Can substitute FUN to summary to get overall picture.
size_by_genre[order(size_by_genre$size_bytes, decreasing = TRUE),]

# Histogram: Genre and app size?
ggplot(data = apple) +
  aes(x = size_bytes, fill = prime_genre, color = prime_genre) +
  geom_histogram(alpha = 0.5) +  # Changes opacity of the fill.
  labs(x = 'App Size (bytes)',
       y = 'Count',
       title = 'Apple App Store: App Size by Genre')

# Scatterplot & Regression line: Languages and app size? 
ggplot(data = apple) +
  aes(x = lang.num, y = size_bytes, color = prime_genre) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(x = 'Number of Supported Languages',
       y = 'App Size (bytes)',
       title = 'Apple App Store: Languages and App Size')

# Medical genre seems to be the only category where there seems to be a relation?
# So I tried to observe this.
apple %>%
  filter(prime_genre == "Medical") %>%
    ggplot() +
      aes(x = lang.num, y = size_bytes) +
      geom_point() +
      geom_smooth(method = lm, se = FALSE) +
      labs(x = 'Medical: Number of Supported Languages',
           y = 'Medical: App Size (bytes)',
           title = 'Apple App Store: Languages and App Size')

# Scatterplot & Regression line: Supported devices and app size?
ggplot(data = apple) +
  aes(x = sup_devices.num, y = size_bytes, color = prime_genre) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(x = 'Number of Supported Devices',
       y = 'App Size (bytes)',
       title = 'Apple App Store: Supported Devices and App Size')

# Scatterplot & Regression line: User ratings and app size? 
ggplot(data = apple) +
  aes(x = user_rating, y = size_bytes, color = prime_genre) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(x = 'User Ratings',
       y = 'App Size (bytes)',
       title = 'Apple App Store: User Ratings and App Size')

# =============== 05. Installs (Google) =============== #
# Histogram: Installs by genre.
summary(google$new_install)
ggplot(data = google) +
  aes(x = new_install, fill = Category, color = Category) +
  geom_histogram(alpha = 0.5) +  # Changes opacity of the fill.
  labs(x = 'Installs',
       y = 'Count',
       title = 'Google Play Store: Installs by Genre')

# Barchart: Highest installed apps by genre.
google %>%
  filter(new_install == 100000000) %>%
  ggplot() +
  aes(x = factor(Category)) +
  geom_bar(fill = "steelblue") +
  coord_flip() +
  labs(x = 'Category',
       y = 'Count',
       title = 'Google Play Store: Genre Distribution of the highest installed apps')

# Scatterplot & Regression line: Rating and installs?
ggplot(data = google) +
  aes(x = Rating, y = new_install, color = Category) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(x = 'User Ratings',
       y = 'Installs',
       title = 'Google Play Store: User Ratings and Installs')

# =============== 06. Ratings In-Depth (Apple) =============== #
# Aggregate & Order: Shows app rating by genre in descending order (Highest to lowest.)
rating_by_genre = aggregate(user_rating ~ prime_genre,
                            data = apple,
                            FUN = mean)
rating_by_genre[order(rating_by_genre$user_rating, decreasing = TRUE),]

# Histogram: User Rating by genre? 
ggplot(data = apple) +
  geom_histogram(binwidth = 0.5, alpha = 0.5) +  # Semi-transparent fill.
  aes(x = user_rating, fill = prime_genre, color = prime_genre) +
  labs(x = 'User Rating',
       y = 'Genre',
       title = 'Apple Store User Rating') +
  scale_x_continuous(breaks=seq(min(apple$user_rating), max(apple$user_rating), by = 0.5),
                     labels=c("0", "0.5", "1", "1.5", "2", "2.5", "3", "3.5", "4", "4.5", "5"))

# Scatterplot & Regression line: User Rating and price (without outliers)? 
apple %>%
  filter(price < 50) %>%
  ggplot() +
  aes(x = user_rating, y = price) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(x = 'User Rating',
       y = 'Price',
       title = 'Apple App Store: User Rating and Price') +
  geom_smooth(method = 'lm')

# Scatterplot & Regression line: User Rating and rating count? 
ggplot(data = apple) +
  aes(x = user_rating, y=rating_count_tot) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(x = 'User Rating',
       y = 'Rating Count',
       title = 'Apple Store User Rating')
