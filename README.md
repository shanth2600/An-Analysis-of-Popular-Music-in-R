---
title: "An Analysis of Popular Music"
author: "Bosco Ndemeye and Shant Hairapetian"
date: "6/7/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
library(xts)
library(magrittr) 
library(dplyr)
library(tidyverse)
library(sjPlot)
library(data.table)
influences <- read.csv('~/Desktop/670/data/influence_data.csv')
full_music_data <- read.csv('~/Desktop/670/data/full_music_data.csv')
```

## The Data

The data we used is comprised of two separate datasets:

<ol>
<li>**full_music_data**: Data which provides 16 variable entries about each of the top 100 songs from 1929 to 2020. This data was scraped from Spotify's API by some third party.</li>
<li>**influence_data**: Data outlining the self-reported direct influences of artists. This data was scraped by a Harvard graduate student (Wenzhe Xue) from allmusic.com for the purposes of writing a thesis analyzing musical influence among popular artists.</li>
</ol>


## Our Questions

### What are the characteristics of popular music over the years

We plotted the values of specific variables over time to see if we could find distinct trends in popular music over time. By applying some geometric smoothing, we found that a subset of the variables exhibited distinct shifts over time which corresponded to popularity of certain types of music in specific time periods.

```{r Tempo, echo=TRUE, message=FALSE}
ggplot(full_music_data, aes(x = year, y=tempo)) + geom_smooth()
```

<br>
**Tempo**: The tempo of popular music hit's a distinct hump at around 1935 which is when swing was at it's most popular. The following dip is swing going out of style. After the post-swing dip however, the tempo of popular music as steadily grown (Though there is a dip after 2010).

```{r Liveness, echo=TRUE, message=FALSE}
ggplot(full_music_data, aes(x = year, y=liveness)) + geom_smooth()
```

**Liveness**: Detects the presence of an audience in a track. Higher liveness values represent an increased probability that the track was performed live. There are two distinct peaks present around 1935 and around 1975.

```{r Energy, echo=TRUE, message=FALSE}
ggplot(full_music_data, aes(x = year, y=energy)) + geom_smooth()
```

**Energy**: A measure representing a perception of intensity and activity. As with tempo, there is a dip at around 1935 then a steady climb upward to present day. Like tempo also, there is also a dip after 2010.

```{r Instrumentalness, echo=TRUE, message=FALSE}
ggplot(full_music_data, aes(x = year, y=instrumentalness)) + geom_smooth()
```

**Instrumentalness**: Predicts whether a track contains no vocals. The popularity of instrumental music has been on a steady decline since the 1920's except for a hump in the 1940's.

```{r Duration, echo=TRUE, message=FALSE}
full_music_data$duration_s = as.difftime(full_music_data$duration_ms/1000, units = 'secs')
ggplot(full_music_data, aes(x = year, y=duration_s)) + geom_smooth() + scale_y_time(labels = function(l) strftime(l, '%M:%S'))
```

**Duration**: The length of a song in minutes. Has also seen a decline since the 1920's with a small hump in the late 40's and a resurgence in the 1970's when many popular artists such as Pink Floyd, Led Zepplin, and Bob Dylan had songs topping out at 10 minutes.

```{r Excplicit, echo=TRUE, message=FALSE}
full_music_data$date=as.Date(full_music_data$release_date, format =  "%m/%d/%Y")
fm_f <- full_music_data %>% drop_na()
music_ts <- xts(x = fm_f$explicit, order.by = fm_f$date)
music_monthly <- to.period(music_ts, period = "months")
music_monthly <- music_monthly["19300101/20200101"]
plot_frq(music_monthly$music_ts.High)
plot(music_monthly$music_ts.High)
```

**Explicitness**: Although the split between explicit and non-explicit is more or less even, the distribution is skewed towards explicit songs appearing more often later on in the time series.

#### Top 10 Predictor

After establishing distinct trends in popular music over time, we aimed to find out whether we could build a model to predict reliably, based on a subset of variables whether a song would be in the top 10. Our aim was discover how formulaic the creation of popular music might be. Our assertion is that if we are able to successfully build a predictive model of song popularity, that may indicate that creativity, musicianship and originality have little do success in popular music.

We normalized the popularity value to 1 for top ten tracks and 0 for anything outside of the top ten. Due to the binary nature of this classification, we trained a logistic regression classifier using the variables in the dataset which were most highly correlated to popularity. These variables were:

<ul>
  <li>liveness</li>
  <li>loudness</li>
  <li>duration_ms</li>
  <li>(date * duration_ms)</li>
</ul>
(The last variable confounds the date at which a song was released and its length)


For validation, we did a 80/20 training/test split.

```{r Predictor, echo=TRUE, message=FALSE}

full_music_data$date=as.Date(full_music_data$release_date, format ="%m/%d/%Y")
fm_f <- full_music_data %>% drop_na()

fm_normed <- fm_f
set.seed(123)

# normalizing popularity data
fm_normed$popularity[fm_f$popularity > 10] <- 0
fm_normed$popularity[fm_f$popularity < 11] <- 1


# training/test split
split <- round(nrow(fm_normed) * 0.80)
training = fm_normed[1:split, ]
test = fm_normed[(split + 1):nrow(fm_normed), ]

# fitting data
lr.fit = glm(popularity ~ liveness + loudness + duration_ms + date + (date * duration_ms), family = binomial, data=training)

# making predictions on test data
probs <- predict(lr.fit,test,type="response")
pred.fit <- rep(0,length(probs))
pred.fit[probs > 0.5] <- 1

# analyzing error
table(pred.fit,test$popularity)


```

**Results**: (13242 + 171) / (13242 + 171 + 41 + 251) = ~98% Correct or ~2% Error on test data. We found that this was the optimal subset of variables and that 0.5 was the best threshold for top 10 or not (0 or 1). The fact that we were able to predict to this high of an accuracy seems to suggests that popularity in music may be even more formulaic than we previously thought.


### Does Originality have any effect on popularity?

In a further exploration of role of originality and creativity (or lackthereof) in popular music, we sought to find a measure for the originality of an artist within our dataset and correlate that to the number of top 100 tracks that artist has had. As a measure of originality we counted the number of influences an artist self-reported. The lower the number of influences, the more original their music (at least that's our hypothesis). 

For this model we found that a linear fit yielded the lowest p-value, and as such it seems that the actual relationship between "originality" and popularity is close to linear.

```{r Originality, echo=TRUE, message=FALSE}
full_music_data <- read.csv('~/Desktop/670/data/full_music_data.csv')
fm_single_id <- full_music_data

# Doing some nasty things because artists_id field has multiple artists
fm_single_id$artist_id <- apply(fm_single_id, 1,  function(x) str_split(x[2],",")[[1]][1])

fm_single_id$artist_id <- apply(fm_single_id, 1,  function(x) str_replace(x[20], "[\\[]", ""))
fm_single_id$artist_id <- apply(fm_single_id, 1,  function(x) str_replace(x[20], "[\\]]", ""))

fm_single_id$artist_id=as.integer(fm_single_id$artist_id)

# Finding the number of influences any give artist has
influence_count <- as.data.frame(table(influences$follower_id))
colnames(influence_count) <- c("artist_id", "influence_count")

# Finding the number of top 100 tracks any given artist has had
artist_top_100_count <- as.data.frame(table(fm_single_id$artist_id))
colnames(artist_top_100_count) <- c("artist_id", "top_100s")

# Joining the two dataframes
joined <- data.table(artist_top_100_count, key="artist_id")[
  data.table(influence_count, key="artist_id"),
  allow.cartesian=TRUE
]

joined[is.na(joined)] <- 0

# Fitting a linear model with number hits as the response
# and influence count as the predictor
lm.fit <- lm(joined$top_100s ~ joined$influence_count)

summary(lm.fit)

```

**Results**: As we can see due the low p-value (< 2e-16), chances are high that the number of influences of an artist has a significant positive correlation with the number of top 100 hits. This suggests that, if our interpretation of orginality is correct in this data, it is not rewarded when it comes to popular music. Quite the opposite.


### How many observable "genres" are present in the data?

Using attributes such as: 
<ul>
  <li>danceability</li>
  <li>energy</li>
  <li>valence</li>
  <li>tempo</li>
  <li>loudness</li>
  <li>mode</li>
  <li>key</li>
  <li>acousticness</li>
  <li>instrumentalness</li>
  <li>liveness</li>
  <li>speechiness</li>
  <li>danceability</li>
  <li>energy</li>
</ul>
  

We extract a small sample of data (this is done to avoid trying to run the clustering algorithm on millions of data points) and use the [HCPC method](http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/117-hcpc-hierarchical-clustering-on-principal-components-essentials/) provided by the *FactoMineR* package to compute principle components, perform hierarchical clustering and further adjust the resulting partitioning with *K-Means*: 


```{r Generes, echo=TRUE, message=FALSE}

library(FactoMineR)
library(factoextra)

fullMusicDNew <- subset(full_music_data, select=-c(artist_names, artists_id, year, release_date, popularity, song_title..censored.))

ind <- sample(2, nrow(fullMusicDNew), replace = T, prob = c(0.9,0.1))
fmusic1 <- fullMusicDNew[ind==1, ]
fmusic2 <- fullMusicDNew[ind==2, ]

res.pca <- PCA(fmusic2, ncp = 3, graph = FALSE)
res.hcpc <- HCPC(res.pca, graph = FALSE)

plot(res.hcpc)

```

***Results***
As can be seen in the following graph, we identify at least 3 different kinds (or genres) of songs in our dataset (the sample that we chose to work with). 


## Analysis


## Impact
Though the social impact of our inquiries may not have the same weight as an analysis of Covid-19 statistics or global debt, they may have a cultural or artistic impact; oftentimes, popular music is seen as the soundtrack of a generation or time period. Millions of music fans all over the world spend countless billions of dollars on recordings and concert tickets for a shared experience of something “transcendent”. The issue is, especially with popular music, the art is more often manufactured than created; record label executives have formulas, both musical and aesthetic, which when utilized, will reliably generate hit music. Our analysis seems to point to the existence of these formulas (whether or not they are created deliberately). We believe that in the act of attempting to reverse engineer these formulas, we may begin to better understand not only the mechanics behind what makes music popular, but also those characteristics of music which evoke certain feelings within ourselves; thus potentially inspiring a more mindful consumption of art. 

There is however the unfortunate other side of the coin in this research which can’t be helped. Though the analysis of such statistics can be used in an introspective sense to help fans see through the “music by numbers” approach employed by lazy artists and greedy executives, it can also be used by those very same bad actors to create even more optimized formulas for mass consumption.



