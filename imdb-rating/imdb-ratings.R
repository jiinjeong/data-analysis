# ================================================================================== #
# FILE    : imdb-ratings.R
# AUTHORS : Duc Pham and Jiin Jeong
# DATE    : Sunday, November 17, 2018
# ================================================================================== #

# ============================= PART 0 : DATA ================================ #
# Loads necessary libraries and datasets; modifies as necessary.
library(ggplot2)
library(mosaic)
library(car)
library(naniar)

# Two way of importing dataset:
# 1) Manually -- Import dataset: From Text (readr)
# 2) Run the following code after changing file directory.
Movies <- read.csv("Movies.csv", header=TRUE, na.strings=c("","NA"))

# ============================= PART 1 : SOCIAL MEDIA/PROMOTION ================================ #
### a) Director FB Likes
summary(Movies$director_facebook_likes)

Movies_director1 = Movies %>%
  filter(dir_like > 0)

ggplot(data = Movies_director1) +
  aes(x = dir_like, y = imdb_score) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs (title = 'IMDB Score by Director FB Likes',
        x = 'Director FB Likes (1/1000)', y = 'IMDB score') +
  scale_x_continuous(breaks=seq(0, 30, by = 2.5))

mymodeldir1 = lm(imdb_score ~ dir_like, data = Movies_director1)
summary(mymodeldir1)

Movies_director = Movies %>%
  filter(dir_like > 10)

ggplot(data = Movies_director) +
  aes(x = dir_like, y = imdb_score) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs (title = 'IMDB Score by Director FB Likes (with famous directors only)',
        x = 'Director FB Likes (1/1000)', y = 'IMDB score') +
  scale_x_continuous(breaks=seq(10, 30, by = 2.5))

mymodeldir2 = lm(imdb_score ~ dir_like, data = Movies_director)
summary(mymodeldir2)

### b) Cast Total FB Likes
summary(Movies$CT_like)

ggplot(data = Movies) +
  aes(x = CT_like, y = imdb_score) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs (title = 'IMDB Score by Cast Total FB Likes',
        x = 'Cast total FB Likes (1/1000)', y = 'IMDB score') +
  scale_x_continuous(breaks=seq(0, 600, by = 100))

Movies_casttotal = Movies %>%
  filter(CT_like < 350) %>%
  filter(CT_like > 0)

ggplot(data = Movies_casttotal) +
  aes(x = CT_like, y = imdb_score) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs (title = 'IMDB Score by Cast Total FB Likes',
        x = 'Cast total FB Likes (1/1000)', y = 'IMDB score') +
  scale_x_continuous(breaks=seq(0, 350, by = 20))

mymodelcast = lm(imdb_score ~ CT_like, data = Movies_casttotal)
summary(mymodelcast)

### c) Movie FB Likes
summary(Movies$movie_facebook_likes)

ggplot(data = Movies) +
  aes(x = movie_like, y = imdb_score) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs (title = 'IMDB Score by Movie FB Likes',
        x = 'Movie FB Likes (1/1000)', y = 'IMDB score')

Movies_movie = Movies %>%
  filter(movie_like < 200) %>%
  filter(movie_like > 0)

ggplot(data = Movies_movie) +
  aes(x = movie_like, y = imdb_score) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs (title = 'IMDB Score by Movie FB Likes',
        x = 'Movie FB Likes (1/1000)', y = 'IMDB score') +
  scale_x_continuous(breaks=seq(0, 200, by = 10))

mymodelmovie = lm(imdb_score ~ movie_like, data = Movies_movie)
summary(mymodelmovie)

# Multiple regression with LIKES
mymodel = lm(imdb_score ~ dir_like + CT_like + movie_like, 
             data = Movies)
summary(mymodel)

Movies_filterlike = Movies %>%
  filter(movie_like < 200) %>%
  filter(movie_like > 0) %>%
  filter(CT_like < 350) %>%
  filter(CT_like > 0) %>%
  filter(dir_like > 0)

mymodel = lm(imdb_score ~ dir_like + CT_like + movie_like, 
             data = Movies_filterlike)
summary(mymodel)
avPlots(mymodel)

### d) Face in Poster
# Get a feel
summary(Movies$facenumber_in_poster)
sd(Movies$facenumber_in_poster, na.rm=T)

# Compare w/ and w/o faces on posters
ggplot(data = Movies) +
  aes(x = imdb_score) +
  geom_histogram() +
  facet_grid(face_or_not ~ .) +
  labs (title = 'IMDB score by Face_or_not ',
        x = 'IMDB Ratings', y = 'Count')

t.test(imdb_score ~ face_or_not, data = Movies)

# Scatterplot of IMDB by the number of faces on poster
ggplot(data = Movies) +
  aes(x = facenumber_in_poster, y = imdb_score) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs (title = 'IMDB score by the number of poster faces',
        x = 'Number of faces on poster', y = 'IMDB score')

# Only focus on movies with faces (excluding 0 face)
Movies_face = Movies %>%
  filter(facenumber_in_poster > 0) %>%
  filter(facenumber_in_poster < 10)

ggplot(data = Movies_face) +
  aes(x = facenumber_in_poster, y = imdb_score) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs (title = 'IMDB Score by the number of faces in the poster',
        x = 'Number of faces', y = 'IMDB score') +
  scale_x_continuous(breaks=seq(1, 20, by = 1))

# Simple linear regression with # of faces
modelface = lm(imdb_score ~ facenumber_in_poster, data = Movies_face)
summary(modelface)

# Adjust for number of likes
modelface2 = lm(imdb_score ~ facenumber_in_poster + dir_like + CT_like + movie_like, 
                data = Movies_face)
summary(modelface2)

avPlot(model = modelface2, 
       variable = 'facenumber_in_poster',
       col.lines = 0)

# ============================= PART 2 : FINANCE ================================ #
summary(Movies$grs)
summary(Movies$bgt)

### a) Gross Earnings
ggplot(data = Movies) +
  aes(x = grs, y = imdb_score) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs (title = 'IMDB Score by Gross',
        x = 'Gross (1/1,000,000)', y = 'IMDB Score')

Movies_gross = Movies %>%
  filter(grs > 0)

ggplot(data = Movies_gross) +
  aes(x = grs, y = imdb_score) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs (title = 'IMDB Score by Gross',
        x = 'Gross (1/1,000,000)', y = 'IMDB Score') +
  scale_x_continuous(breaks=seq(0, 800, by = 100))

modelsg = lm(imdb_score ~ grs, data = Movies_gross)
summary(modelsg)

### b) Budget
ggplot(data = Movies) +
  aes(x = bgt, y = imdb_score) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs (title = 'IMDB Score by Budget',
        x = 'Budget (1/1,000,000)', y = 'IMDB Score')

Movies_budget = Movies %>%
  filter(bgt > 0) %>%
  filter(bgt < 1000)

ggplot(data = Movies_budget) +
  aes(x = bgt, y = imdb_score) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs (title = 'IMDB Score by Budget (< 1,000,000,000 USD)',
        x = 'Budget (1/1,000,000)', y = 'IMDB Score') +
  scale_x_continuous(breaks=seq(0, 1000, by = 100))


modelbs = lm(imdb_score ~ bgt, data = Movies_budget)
summary(modelbs)

Movies_filterfinance = Movies %>%
  filter(bgt > 0) %>%
  filter(bgt < 1000) %>%
  filter(grs > 0)

modelbsg = lm(imdb_score ~ bgt + grs, data = Movies_filterfinance)
summary(modelbsg)

avPlots(modelbsg)

### c) Gross & Budget
ggplot(data = Movies) +
  aes(x = bgt, y = grs) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs (title = 'Gross by Budget (1/1,000,000)',
        x = 'Budget', y = 'Gross')

Movies_bggs = Movies %>%
  filter(bgt > 0) %>%
  filter(bgt < 1000) %>%
  filter(grs > 0)

ggplot(data = Movies_bggs) +
  aes(x = bgt, y = grs) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs (title = 'Gross by Budget (Scale: 1/1,000,000)',
        x = 'Budget', y = 'Gross') +
  scale_x_continuous(breaks=seq(0, 1000, by = 100))

modelbg = lm(grs ~ bgt, data = Movies_bggs)
summary(modelbg)

# ============================= PART 3 : CONTENT/AESTHETICS ================================ #
options(scipen=999)  # Removes scientific notation.
options(scipen=0)    # Adds scientific notation.

### a) Duration
# Overall picture.
summary(Movies$duration)
Movies %>%               # 511 is such a big outlier, so we exclude it.
  filter(duration < 350) %>%
  ggplot() +
  geom_histogram(col = 'tomato1', fill = 'lightpink2') +
  aes(x = duration) +
  labs(x = 'Duration',
       y = 'Count',
       title = 'Movies by Duration (< 350 min)') +
  geom_vline(aes(xintercept=mean(Movies$duration, na.rm=TRUE)),  # Removes NA values.
             color="red", linetype="dashed", size=1) +
  scale_x_continuous(breaks=seq(0, 250, by = 25))

# Scatterplot
Movies %>%
  filter(duration < 350) %>%
  ggplot() +
  aes(x = duration, y = imdb_score) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(x = 'Duration',
       y = 'IMDB Score',
       title = 'Duration and IMDB Score')

Movies %>%
  filter(duration < 150) %>%
  filter(duration > 90) %>%
  ggplot() +
  aes(x = duration, y = imdb_score) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(x = 'Duration',
       y = 'IMDB Score',
       title = 'Duration and IMDB Score (Average-length)')

# Linear Model Analysis
is.na(Movies$duration) <- Movies$duration > 350  # Removes outliers by converting into NA.
durationModel = lm( imdb_score ~ duration, data = Movies, na.action=na.omit)  # na.exclude works as well.
summary(durationModel)

# Now, you need to reload the data (since we removed one data point above with outlier).
Movies <- read.csv("~/Desktop/18FALL/MATH254/Monthly_Project/10_October/Movies.csv", header=TRUE, na.strings=c("","NA"))

### b) Genres
# Overall picture
summary(Movies$genres)
ggplot(data = Movies) +
  aes(x = num_of_genres) +
  geom_bar(fill="salmon") +
  labs(x = 'Num of Genres',
       y = 'Count',
       title = 'Movies by Num of Genres')

# Scatterplot
ggplot(data = Movies) +
  aes(x = num_of_genres, y = imdb_score) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(x = 'Number of Genres',
       y = 'IMDB Score',
       title = 'Duration and IMDB Score')

# Linear Model Analysis
genreModel = lm( imdb_score ~ num_of_genres, data = Movies, na.action=na.omit)  # na.exclude works as well.
summary(genreModel)

# Comedy or Not: Histograms and t-test
ggplot(data = Movies) +
  aes(x = imdb_score) +
  geom_histogram() +
  facet_grid (comedy_or_not ~ .) +
  labs(x = 'IMDB Score',
       y = 'Count',
       title = 'Non-comedy/Comedy IMDB Score')  # Separates by carrier (Row).
t.test(imdb_score ~ comedy_or_not, paired = FALSE, data = Movies, na.action = na.omit)

# Drama or Not: Histograms and t-test
ggplot(data = Movies) +
  aes(x = imdb_score) +
  geom_histogram() +
  facet_grid (drama_or_not ~ .) +
  labs(x = 'IMDB Score',
       y = 'Count',
       title = 'Non-drama/Drama IMDB Score')  # Separates by carrier (Row).
t.test(imdb_score ~ drama_or_not, paired = FALSE, data = Movies, na.action = na.omit)

# Romance or Not: Histograms and t-test
ggplot(data = Movies) +
  aes(x = imdb_score) +
  geom_histogram() +
  facet_grid (romance_or_not ~ .) +
  labs(x = 'IMDB Score',
       y = 'Count',
       title = 'Non-romance/Romance IMDB Score')  # Separates by carrier (Row).
t.test(imdb_score ~ romance_or_not, paired = FALSE, data = Movies, na.action = na.omit)

### c) Content Rating
# Reorder movie ratings (highest -> lowest, with the exception of NA.
Movies <- within(Movies,content_rating <- 
                   factor(content_rating,
                          levels = names(sort(table(content_rating),decreasing=FALSE))))

# Overall picture (Without NA values)
Movies %>%
  filter(content_rating != "NA") %>%
  ggplot() +
  aes(x = content_rating) +
  geom_bar(fill="salmon") +
  coord_flip() +
  labs(x = 'Content Rating',
       y = 'Count',
       title = 'Movies by Content Rating (Reordered)')

# MPAA-Only
Movies <- within(Movies,content_rating <- 
                   factor(content_rating,
                          levels = c("G", "PG", "PG-13", "R", "NC-17")))

# Reorder movies by desired MPAA Rating
Movies %>%
  filter(content_rating == "G" | content_rating == "PG" | content_rating == "PG-13" | 
           content_rating == "R" | content_rating == "NC-17") %>%
  ggplot() +
  aes(x = content_rating) +
  geom_bar(fill="salmon") +
  coord_flip() +
  labs(x = 'MPAA Rating',
       y = 'Count',
       title = 'Movies by MPAA Rating') +
  geom_text(stat='count', aes(label=..count..), hjust = -0.25)

# Boxplot
Movies %>%
  filter(is.na(mpaaRating) == FALSE) %>%
  ggplot() +
  aes( x = mpaaRating , y = imdb_score) +
  geom_boxplot(na.rm = TRUE) +
  stat_summary( fun.y = "mean", geom = "point", col = "red") +  # Puts red dot on the mean.
  geom_hline(yintercept = common_mean, col = 'blue') +
  labs(x = 'MPAA Content Rating',
       y = 'IMDB Score',
       title = 'IMDB Score by MPAA Content Rating')

aggregate(imdb_score ~ mpaaRating, data = Movies, FUN = summary, na.action = na.omit)
contentModel = aov(imdb_score ~ mpaaRating, data=Movies, na.action = na.omit)
summary(contentModel)

### d) BW/Color
# Overall picture
Movies %>%
  filter(color != "NA") %>%
  ggplot() +
  aes(x = factor(color)) +
  geom_bar(fill="salmon") +
  labs(x = 'BW/Color',
       y = 'Count',
       title = 'Movies by BW/Color') +
  geom_text(stat='count', aes(label=..count..), vjust= -0.1)

# Side-by-side histogram
Movies %>%
  filter(color != "NA") %>%
  ggplot() +
  aes(x = imdb_score) +
  geom_histogram() +  # Looks ugly because we have outliers. Check by doing summary.
  facet_grid (color ~ .) +
  labs(x = 'IMDB Score',
       y = 'Count',
       title = 'BW/Color IMDB Score')  # Separates by carrier (Row).

# t-test
aggregate(imdb_score ~ color, data = Movies, FUN = summary, na.rm=TRUE, na.action=NULL)
t.test(imdb_score ~ color, paired = FALSE, data = Movies, na.action = na.omit)

# ============================= PART 4 : MOVIE RELEASE =================================== #
### a) Release Year
# Overall Picture
summary(Movies$title_year)

# Timeline
commonmean = mean(Movies$imdb_score)
ggplot(data=Movies, aes(x=title_year, y=imdb_score, group = 1)) +
  stat_summary(aes(y = imdb_score,group=1), fun.y=mean, colour="red", geom="line",group=1) +
  geom_hline(yintercept = common_mean, col = 'blue') +
  labs(x = 'Title Year',
       y = 'IMDB Score',
       title = 'Mean IMDB Score by Title Year')

# Before/after 21C: Side-by-side histograms
Movies %>%
  filter(c21_or_not != "NA") %>%
  ggplot() +
  aes(x = imdb_score) +
  geom_histogram() +
  facet_grid (c21_or_not ~ .) +
  labs(x = 'IMDB Score',
       y = 'Count',
       title = 'Before/After 21C IMDB Score')  # Separates by carrier (Row).

t.test(imdb_score ~ c21_or_not, paired = FALSE, data = Movies, na.action = na.omit)

### b) Language
# Overall picture
prop.table(table(Movies$English_or_not))

Movies <- within(Movies,language <- 
                   factor(language,
                          levels = names(sort(table(language),decreasing=FALSE))))
ggplot(data = Movies) +
  aes(x = factor(language)) +
  geom_bar(fill="steelblue") +
  coord_flip() +
  labs(x = 'Language',
       y = 'Count',
       title = 'Movies by Language')

# Pie chart
ggplot(data = Movies) +
  aes(x = factor(1), fill = factor(English_or_not)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Movies by Languages (English/Non-English)") +
  scale_fill_discrete(name = "English/Non-English",
                      breaks=c("0", "1"),
                      labels=c("Non-English (6.79%)", "English (93.21%)")) + 
  theme(plot.title = element_text(size=16))

aggregate(imdb_score ~ English_or_not, data = Movies, FUN = summary)
t.test(imdb_score ~ English_or_not, paired = FALSE, data = Movies)

### c) Country
# Overall picture
prop.table(table(Movies$US_or_not))

Movies <- within(Movies,country <- 
                   factor(country,
                          levels = names(sort(table(country),decreasing=FALSE))))
ggplot(data = Movies) +
  aes(x = factor(country)) +
  geom_bar(fill="steelblue") +
  coord_flip() +
  labs(x = 'Country',
       y = 'Count',
       title = 'Movies by Country')

# Pie chart
ggplot(data = Movies) +
  aes(x = factor(1), fill = factor(US_or_not)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Movies by Countries (USA/Non-USA)") +
  scale_fill_discrete(name = "USA/Non-USA",
                      breaks=c("0", "1"),
                      labels=c("Non-US (24.53%)", "US (75.47%)")) +
  theme(plot.title = element_text(size=16))

Movies %>%
  filter(US_or_not != "NA") %>%
  ggplot() +
  aes(x = imdb_score) +
  geom_histogram() +
  facet_grid (US_or_not ~ .) +
  labs(x = 'IMDB Score',
       y = 'Count',
       title = 'Non-USA/USA Movies IMDB Score')  # Separates by carrier (Row).

aggregate(imdb_score ~ US_or_not, data = Movies, FUN = summary)
t.test(imdb_score ~ US_or_not, paired = FALSE, data = Movies)
