# ================================================================================== #
# FILE    : abc-fb.R
# AUTHORS : Duc Pham and Jiin Jeong
# DATE    : December 2018
# DESC    : Analysis of ABC News Posts on Facebook
# ================================================================================== #

library(readxl)
library(ggplot2)
library(mosaic)
library(car)
options(scipen=999)  # Removes scientific notation.
options(scipen=0)    # Adds scientific notation.

ABCNews <- read_excel("ABCNews.xlsx")

# Another method: Use recode to change labels...!
ABCNews$newSuccess[ABCNews$success == 0] <- "No"
ABCNews$newSuccess[ABCNews$success == 1] <- "Yes"

# Success / Fail
ggplot(data = ABCNews) +
  aes(x = factor(newSuccess)) +
  geom_bar(fill="navy") +
  labs(x = 'Success',
       y = 'Count',
       title = 'ABC News by Success') +
  geom_text(stat='count', aes(label=..count..), vjust= -0.2)  +
  theme(plot.title = element_text(size=16))

# =============================== PART 1 =============================== #
# ----------------------- 1) Post Types -------------------- #
# FILTER
# filter(is.na(post_type) == FALSE) %>%
#   filter(post_type != "event") %>%

# General (Barchart)
ABCNews %>%
  filter(is.na(post_type) == FALSE) %>%
  filter(post_type != "event") %>%
    ggplot() +
    aes(x = post_type, fill = factor(newSuccess)) +
    geom_bar() +
    labs(x = 'Post Type',
         y = 'Count',
         title = 'Success by Post Type')  +
    geom_text(stat='count', aes(label=..count..), vjust = 1.1)  +
    theme(plot.title = element_text(size=16))

# Chi-squared Test of Association
postData=matrix(0, nrow = 2,ncol=4)
rownames(postData)= c("Success? Yes", "Success? No")
colnames(postData)= c('Link', 'Photo', 'Status', 'Video')
postData[1,]=c(2987,1037,299, 811)
postData[2,]=c(25509, 5793, 2023, 4816)
postData

postProp = prop.table(postData, margin = 2)  # Column.
postProp

barplot(postProp, col=c('navy','salmon'))
title(main = list("Success Rate by Post Type", cex = 1.5,
                  col = "black", font = 3))
legend("top", inset=0.1, title="Success?",
       c("Yes","No"), fill=c('navy', 'salmon'), horiz=TRUE, cex=0.8)

chisq.test(postData)
chisq.test(postData)$observed
chisq.test(postData)$expected
chisq.test(postData)$residual

# ----------------------- 2) Status Type -------------------- #
# FILTER
# filter(is.na(status_type) == FALSE) %>%
#   filter(status_type != "created_event" & status_type != "NULL") %>%

# General (Barchart)
ABCNews %>%
  filter(is.na(status_type) == FALSE) %>%
  filter(status_type != "created_event" & status_type != "NULL" &
           status_type != "published_story") %>%
  ggplot() +
  aes(x = status_type, fill = factor(newSuccess)) +
  geom_bar() +
  labs(x = 'Status Type',
       y = 'Count',
       title = 'Success by Status Type')  +
  geom_text(stat='count', aes(label=..count..), vjust = 1.1)  +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        plot.title = element_text(size=16))

# Chi-squared Test of Association
statusData=matrix(0, nrow = 2,ncol=4)
rownames(statusData)= c("Success? Yes", "Success? No")
colnames(statusData)= c('Photo', 'Video', 'Mobile', 'Share')
statusData[1,]=c(1034, 742, 320, 3037)
statusData[2,]=c(5586, 4208, 2210, 25881)
statusData

statusProp = prop.table(statusData, margin = 2)  # Column.
statusProp
barplot(statusProp, col=c('navy','salmon'))
title(main = list("Success Rate by Status Type", cex = 1.5,
                  col = "black", font = 3))
legend("top", inset=0.1, title="Success?",
       c("Yes","No"), fill=c('navy', 'salmon'), horiz=TRUE, cex=0.8)
mtext(side = 4, cex = 0.75, "* Photo/Video = Added Photo/Video,\nMobile = Mobile Status Update, Share = Shared Story")

chisq.test(statusData)
chisq.test(statusData)$observed
chisq.test(statusData)$expected
chisq.test(statusData)$residual

# ----------------------- 3) Description Length -------------------- #
# Logistic Regression model: Description length
ABCNews %>%
  filter(description_length <= 1000) %>%
  ggplot() +
  aes(x=description_length, y=success) +
  geom_point() +
  geom_smooth(method='glm', method.args=list(family = "binomial"),se=FALSE , col='red', fullrange=TRUE) +
  labs(x = 'Description Length',
       y = 'Success',
       title = 'Description Length and Success')  +
  theme(plot.title = element_text(size=16))

is.na(ABCNews$description_length) <- ABCNews$description_length > 1000
m=glm(success ~ description_length , family='binomial', data=ABCNews)
summary(m)

# Scatterplot (Regression line)
ABCNews %>%
  filter(description_length <= 1000) %>%
  ggplot() +
  aes(x = description_length, y = likesnreactions, col = factor(newSuccess)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs (title = 'Description Length & ABCNews Interactions',
        x = 'Description Length', y = 'ABCNews Like + Reactions')  +
  theme(plot.title = element_text(size=16))

# ----------------------- 4) Question or Not -------------------- #
# General (Barchart)
ABCNews %>%
  ggplot() +
  aes(x = question, fill = factor(newSuccess)) +
  geom_bar() +
  labs(x = 'Question/Not',
       y = 'Count',
       title = 'Success by Question/Not')  +
  geom_text(stat='count', aes(label=..count..), vjust = 1)  +
  theme(plot.title = element_text(size=16))

ABCNews %>%
  filter(is.na(status_type) == FALSE) %>%
  filter(status_type != "created_event" & status_type != "NULL") %>%
  ggplot() +
  aes(x = status_type) +
  geom_bar(fill="navy", na.rm = TRUE) +
  labs(x = 'Status Types',
       y = 'Count',
       title = 'ABCNews Post by Status Types') +
  geom_text(stat='count', aes(label=..count..), vjust = -0.2)  +
  theme(plot.title = element_text(size=16))

# ----------------------- 5) Time Posted -------------------- #
# Reorders by time of day.
ABCNews <- within(ABCNews,timeoftheday <- 
                   factor(timeoftheday,
                          levels = c("8am-4pm", "4pm-8pm", "8pm-midnight", "Midnight-8am")))
# General (Barchart)
ABCNews %>%
  filter(is.na(timeoftheday) == FALSE) %>%
  ggplot() +
  aes(x = timeoftheday, fill = factor(newSuccess)) +
  geom_bar() +
  labs(x = 'Time of Day',
       y = 'Count',
       title = 'Success by Time of Day')  +
  geom_text(stat='count', aes(label=..count..), vjust = 1.1)  +
  theme(plot.title = element_text(size=16))

# Chi-squared Test of Association
timeData=matrix(0, nrow = 2,ncol=4)
rownames(timeData)= c("Success? Yes", "Success? No")
colnames(timeData)= c('8am-4pm', '4pm-8pm', '8pm-mid', 'Midnight-8am')
timeData[1,]=c(817,1239,1274, 1804)
timeData[2,]=c(9073, 8505, 7735, 12829)
timeData

timeProp = prop.table(timeData, margin = 2)  # Column.
timeProp
barplot(timeProp, col=c('navy','salmon'))
title(main = list("Success Rate by Time of Day", cex = 1.5,
                  col = "black", font = 3))
legend("top", inset=0.1, title="Success?",
       c("Yes","No"), fill=c('navy', 'salmon'), horiz=TRUE, cex=0.8)

chisq.test(timeData)
chisq.test(timeData)$observed
chisq.test(timeData)$expected
chisq.test(timeData)$residual

# =============================== PART 2 =============================== #
# ----------------------- Quarterly -------------------- #
# Timeline
likesMean = mean(ABCNews$likes_count, na.rm = T)
ABCNews %>%
  filter(yrq != "Q1") %>%
    ggplot(aes(x=yrq, y=likes_count, group = 1)) +
      stat_summary(aes(y = likes_count,group=1), fun.y=mean, colour="red", geom="line",group=1) +
      geom_hline(yintercept = likesMean, col = 'blue') +
      labs(x = 'Quarter/Year',
           y = 'Likes',
           title = 'Quarterly Mean Likes')   +
      theme(axis.text.x = element_text(angle=45, hjust=1),
            plot.title = element_text(size=16))

reactMean = mean(ABCNews$likesnreactions - ABCNews$likes_count, na.rm = T)
ABCNews %>%
  filter(yrq != "Q1") %>%
    ggplot(aes(x=yrq, y=likesnreactions - likes_count, group = 1)) +
      stat_summary(aes(y = likesnreactions - likes_count,group=1), fun.y=mean, colour="red", geom="line",group=1) +
      geom_hline(yintercept = reactMean, col = 'blue') +
      labs(x = 'Quarter/Year',
           y = 'Reactions',
           title = 'Quarterly Mean Reactions')  +
      theme(axis.text.x = element_text(angle=45, hjust=1),
            plot.title = element_text(size=16))

interMean = mean(ABCNews$likesnreactions, na.rm = T)
ABCNews %>%
  filter(yrq != "Q1") %>%
    ggplot(aes(x=yrq, y=likesnreactions, group = 1)) +
      stat_summary(aes(y = likesnreactions,group=1), fun.y=mean, colour="red", geom="line",group=1) +
      geom_hline(yintercept = interMean, col = 'blue') +
      labs(x = 'Quarter/Year',
           y = 'Total Interactions',
           title = 'Quarterly Total Interactions') +
      theme(axis.text.x = element_text(angle=45, hjust=1),
            plot.title = element_text(size=16))

# =============================== ETC =============================== #
##### BOXPLOT & ANOVA
# Boxplot (Unsuccessful)
is.na(ABCNews$likesnreactions) <- ABCNews$likesnreactions > 10000
commonMean = mean(ABCNews$likesnreactions, na.rm=T)
ABCNews %>%
#  filter(post_type == "link" | post_type == "photo" | post_type == "video") %>%
  filter(likesnreactions > 10000 & likesnreactions <= 100000) %>%
  ggplot() +
  aes( x = timeoftheday , y = likesnreactions) +
  geom_boxplot(na.rm = TRUE)  +
  stat_summary( fun.y = "mean", geom = "point", col = "red") +  # Puts red dot on the mean.
  geom_hline(yintercept = commonMean, col = 'blue') +
  labs(x = 'Post Type',
       y = 'Likes + Reactions',
       title = 'Post Type & ABCNews Interactions (Normal)')

# ANOVA
is.na(ABCNews$post_type) <- ABCNews$post_type == "event"  # Removes outliers by converting into NA.
aggregate(likesnreactions ~ post_type, data = ABCNews, FUN = summary, na.action = na.omit)
postModel = aov(likesnreactions ~ post_type, data=ABCNews, na.action = na.omit)
summary(postModel)
hist(postModel$residuals)

ggplot() +
  aes( sample = postModel$residuals) +
  stat_qq() +
  stat_qq_line()

##### HISTOGRAM (GENERAL)
commonMean = mean(ABCNews$likesnreactions, na.rm=T)
ABCNews %>%
  filter( likesnreactions < 100000) %>%
  ggplot() +
  aes(x = likesnreactions) +
  geom_histogram(fill="navy") +
  geom_vline(xintercept = commonMean, col = 'grey') +
  labs(x = 'ABCNews Likes + Reactions',
       y = 'Count',
       title = 'ABC News Post ABCNews Likes + Reactions') +
  theme(plot.title = element_text(size=16))

##### HISTOGRAM (DETAILED)
ABCNews %>%
  filter( likesnreactions < 10000) %>%
  filter(is.na(post_type) == FALSE) %>%
  filter(post_type != "event") %>%
  ggplot() +
  aes(x = likesnreactions, fill = factor(post_type)) +
  geom_histogram(col = "black") +
  labs(x = 'ABCNews Likes + Reactions',
       y = 'Count',
       title = 'ABC News Post ABCNews Likes + Reactions') +
  theme(plot.title = element_text(size=16))


##### BARCHART + HISTOGRAM (FACET)
# Barchart
ABCNews %>%
  filter(is.na(status_type) == FALSE) %>%
  filter(status_type != "created_event" & status_type != "NULL") %>%
  ggplot() +
  aes(x = status_type) +
  geom_bar(fill="navy", na.rm = TRUE) +
  labs(x = 'Status Types',
       y = 'Count',
       title = 'ABCNews Post by Status Types') +
  geom_text(stat='count', aes(label=..count..), vjust = -0.2)  +
  theme(plot.title = element_text(size=16))

# Histogram: Normal
ABCNews %>%
  filter(is.na(post_type) == FALSE) %>%
  filter(status_type != "created_event" & status_type != "NULL") %>%
  filter(likesnreactions <= 10000) %>%
  ggplot() +
  aes(x = likesnreactions) +
  geom_histogram() +  # Looks ugly because we have outliers. Check by doing summary.
  facet_grid (status_type ~ .) +
  labs(x = 'ABCNews Likes + Reactions',
       y = 'Count',
       title = 'ABCNews Likes + Reactions (Normal) by Status Type')

# Histogram: Successful
options(scipen=999)  # Removes scientific notation.
ABCNews %>%
  filter(is.na(status_type) == FALSE) %>%
  filter(status_type != "created_event" & status_type != "NULL") %>%
  filter(likesnreactions > 10000 & likesnreactions < 100000) %>%
  ggplot() +
  aes(x = likesnreactions) +
  geom_histogram() +  # Looks ugly because we have outliers. Check by doing summary.
  facet_grid (status_type ~ .) +
  labs(x = 'ABCNews Likes + Reactions',
       y = 'Count',
       title = 'ABCNews Likes + Reactions (Successful) by Status Type')  # Separates by carrier (Row).

ABCNews %>%
  ggplot() +
  aes(x = Year, y = likesnreactions, col = factor(success)) +
  geom_point() +
  geom_jitter() +
  labs (title = 'Status Type over Years',
        x = 'Year', y = 'ABCNews Like + Reactions')  +
  theme(plot.title = element_text(size=16))

# General (Barchart)
ABCNews %>%
  filter(ABCNews$yrq != "Q1") %>%
  ggplot() +
  aes(x = yrq, fill = factor(newSuccess)) +
  geom_bar() +
  labs(x = 'Year/Quarter',
       y = 'Count',
       title = 'Success by Year/Quarter')  +
  geom_text(stat='count', aes(label=..count..), vjust = 1.1)  +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        plot.title = element_text(size=16))

# =============================== PART 1 =============================== #
# ------ 1) Likes + reaction, comments & shares (pairwise) ----------- #
# Likes & comments
ggplot(data = ABCNews) +
  aes(x = likesnreactions, y = comments_count) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs (title = 'Facebook post likes and reactions by the number of comments',
        x = 'Number of likes & reactions', y = 'Number of comments') +
  scale_x_continuous(breaks=seq(0, 1250000, by = 200000))

# Remove outliers
likes_cmt = ABCNews %>%
  filter(comments_count < 100000) %>%
  filter(likesnreactions < 300000)

ggplot(data = likes_cmt) +
  aes(x = likesnreactions, y = comments_count) +
  geom_point() +
  geom_jitter() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs (title = 'Facebook likes and reactions by the number of comments',
        x = 'Number of likes & reactions', y = 'Number of comments') +
  scale_x_continuous(breaks=seq(0, 300000, by = 50000))

likes_cmt2 = ABCNews %>%
  filter(comments_count < 20000) %>%
  filter(likesnreactions < 100000)

ggplot(data = likes_cmt2) +
  aes(x = likesnreactions, y = comments_count) +
  geom_point() +
  geom_jitter() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs (title = 'Facebook likes and reactions by the number of comments ("normal posts")',
        x = 'Number of likes & reactions', y = 'Number of comments') +
  scale_x_continuous(breaks=seq(0, 100000, by = 20000))

# Comments & share
ggplot(data = ABCNews) +
  aes(x = shares_count, y = comments_count) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs (title = 'Facebook comments by the number of shares',
        x = 'Number of shares', y = 'Number of comments')

share_cmt = ABCNews %>%
  filter(shares_count < 200000) %>%
  filter(comments_count < 100000)

ggplot(data = share_cmt) +
  aes(x = shares_count, y = comments_count) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs (title = 'Facebook comments by the number of shares (shares < 200,000, comments < 100,000)',
        x = 'Number of shares', y = 'Number of comments') +
  scale_x_continuous(breaks=seq(0, 200000, by = 50000)) +
  scale_y_continuous(breaks=seq(0, 100000, by = 20000))

model_sharecmt = lm(shares_count ~ comments_count, data = share_cmt)
summary(model_sharecmt)

# Likes & shares
ggplot(data = ABCNews) +
  aes(x = likesnreactions, y = shares_count) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs (title = 'Facebook likes by number of shares',
        x = 'Number of likes and reactions', y = 'Number of shares') +
  scale_x_continuous(breaks=seq(0, 1250000, by = 200000)) +
  scale_y_continuous(breaks=seq(0, 1000000, by = 200000))

share_like = ABCNews %>%
  filter(shares_count < 200000) %>%
  filter(likesnreactions < 200000)

ggplot(data = share_like) +
  aes(x = likesnreactions, y = shares_count) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs (title = 'Facebook likes by number of shares (shares < 200,000, likes < 200,000)',
        x = 'Number of likes and reactions', y = 'Number of shares') +
  scale_x_continuous(breaks=seq(0, 200000, by = 40000)) +
  scale_y_continuous(breaks=seq(0, 200000, by = 50000))

# ------------ 2) Relationship b/w Angry & Love -------------- #
# Simple regression
ggplot(data = ABCNews) +
  aes(x = love_count, y = angry_count) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs (title = 'Facebook angry reactions by love reactions',
        x = 'Number of love reactions', y = 'Number of angry reactions') +
  scale_x_continuous(breaks=seq(0, 200000, by = 40000)) +
  scale_y_continuous(breaks=seq(0, 60000, by = 10000))

# Remove outliers
love_angry = ABCNews %>%
  filter(love_count < 20000) %>%
  filter(angry_count < 5000)

ggplot(data = love_angry) +
  aes(x = love_count, y = angry_count) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs (title = 'Facebook angry reactions by love reactions (outliers removed)',
        x = 'Number of love reactions', y = 'Number of angry reactions')

model_loveangry = lm(love_count ~ angry_count, data = love_angry)
summary(model_loveangry)

# more extreme for political posts?
political_post = ABCNews %>%
  filter(politics_or_not == 'Politics')

ggplot(data = political_post) +
  aes(x = love_count, y = angry_count) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs (title = 'Facebook angry reactions by love reactions (political posts)',
        x = 'Number of love reactions', y = 'Number of angry reactions')

model_loveangry2 = lm(love_count ~ angry_count, data = political_post)
summary(model_loveangry2)

# --------- # 3) Do questions lead to more comments and/or shares? ------------ #
# Descriptive Stats & histogram
aggregate(comments_count ~ question, data = ABCNews, FUN = summary)
ggplot(data = ABCNews) +
  aes(x = comments_count) +
  geom_histogram() +
  facet_grid(. ~ question) +
  labs (title = 'Distribution of ABCNews comment counts for two groups (with and without question)',
        x = 'Number of comments', y = 'Frequency')

aggregate(shares_count ~ question, data = ABCNews, FUN = summary)
ggplot(data = ABCNews) +
  aes(x = shares_count) +
  geom_histogram() +
  facet_grid(. ~ question) +
  labs (title = 'Distribution of ABCNews share counts for two groups (with and without question)',
        x = 'Number of shares', y = 'Frequency')

# =============================== FOOTNOTE =============================== #
# CITE : https://ryouready.wordpress.com/2009/02/17/r-good-practice-adding-footnotes-to-graphics/
# Basic information at the beginning of each script
scriptName <- "finalcode.R"
author <- "Jiin Jeong & Duc Pham"
footnote <- paste(scriptName, format(Sys.time(), "%d %b %Y"),
                  author, sep=" / ")

# Default footnote
makeFootnote <- function(footnoteText=
                           format(Sys.time(), "%d %b %Y"),
                         size= .7, color= grey(.5))
{
  require(grid)
  pushViewport(viewport())
  grid.text(label= footnoteText ,
            x = unit(1,"npc") - unit(2, "mm"),
            y= unit(2, "mm"),
            just=c("right", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}

makeFootnote(footnote)  # To make footnote
