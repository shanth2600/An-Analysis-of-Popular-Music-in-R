---
title: "An Analysis of Popular Music"
author: "Bosco Ndemeye and Shant Hairapetian"
date: "6/7/2021"
output: html_document
---

```{r setup, include=TRUE}
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
library(xts)
library(magrittr) 
library(dplyr)
library(tidyverse)
library(sjPlot)
library(data.table)
influences <- read.csv('music_data_set/influence_data.csv')
full_music_data <- read.csv('music_data_set/full_music_data.csv')
```

## The Data

The data we used is comprised of two separate datasets:

-   **full_music_data** : Data which provides 16 variable entries about each of the top 100 songs from 1929 to 2020. This data was scraped from Spotify's API by some third party.
-   **influence_data**: Data outlining the self-reported direct influences of artists. This data was scraped by a Harvard graduate student (Wenzhe Xue) from allmusic.com for the purposes of writing a thesis analyzing musical influence among popular artists.

## Our Questions

### What are the characteristics of popular music over the years

```{r, fig.cap="Tempo", fig.width=10, echo=TRUE, message=FALSE, eval=do_it}
ggplot(full_music_data, aes(x = year, y=tempo)) + geom_smooth()
```

<br> **Tempo**: The tempo of popular music hit's a distinct hump at around 1935 which is when swing was at it's most popular. The following dip is swing going out of style.

```{r Liveness, echo=TRUE, message=FALSE}
ggplot(full_music_data, aes(x = year, y=liveness)) + geom_smooth()
```

**Liveness**: Detects the presence of an audience in a track. Higher liveness values represent an increased probability that the track was performed live.

```{r Energy, echo=TRUE, message=FALSE}
ggplot(full_music_data, aes(x = year, y=energy)) + geom_smooth()
```

**Energy**: A measure representing a perception of intensity and activity.

```{r Instrumentalness, echo=TRUE, message=FALSE}
ggplot(full_music_data, aes(x = year, y=instrumentalness)) + geom_smooth()
```

**Instrumentalness**: Predicts whether a track contains no vocals.

```{r Duration, echo=TRUE, message=FALSE}
full_music_data$duration_s = as.difftime(full_music_data$duration_ms/1000, units = 'secs')
ggplot(full_music_data, aes(x = year, y=duration_s)) + geom_smooth() + scale_y_time(labels = function(l) strftime(l, '%M:%S'))
```

**Duration**: The length of a song.

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

We normalized the popularity value to 1 for top ten tracks and 0 for anything outside of the top ten. Then we trained a logistic regression classifier using the most relevant subset of variables to predict whether a song would be a top 10 track or not. For validation, we did a 80/20 training/test split.

```{r Predictor, echo=TRUE, message=FALSE}
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
lr.fit = glm(popularity ~ liveness + loudness + duration_ms, family = binomial, data=training)

# making predictions on test data
probs <- predict(lr.fit,test,type="response")
pred.fit <- rep(0,length(probs))
pred.fit[probs > 0.5] <- 1

# analyzing error
table(pred.fit,test$popularity)


```

**Results**: (13272 + 1) / (13272 + 1 + 11 + 412) = \~97% Correct or \~3% Error on test data. We found that this was the optimal subset of variables and that 0.5 was the best threshold for top 10 or not. The fact that we're able to predict to this high of an accuracy suggests that popularity in music may be even more formulaic than we think.

### Does Originality have any effect on popularity?

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

**Results**: As we can see due the low p-value, chances are high that the number of influences of an artist has a significant positive correlation with the number of top 100 hits. This suggests that originality is not rewarded when it comes to popular music.

### Which artists have had the most musical influence?

We answer this question by counting the frequency of each unique singer in the influence data, and present the results using a worldcloud of the influencer names:

```{r}
influence_edges <- select(influences, influencer_name, follower_name)
influencers     <- select(influence_edges, influencer_name) 
followers       <- select(influence_edges, follower_name) 
influencers_df  <- as.data.frame(table(influencers))
followers_df    <- as.data.frame(table(followers))

library(wordcloud2)
new_influencers_df <- influencers_df[order(-influencers_df$Freq),]
wordcloud2(new_influencers_df, size=0.5, shape='pentagon', shuffle=FALSE)
```

**Results** The generated wordcloud allows us to communicate that, according to our data, singers such as *The Beatles*, *Bob Dylan*, *The Rolling Stones* and *Hank Williams* have had great influence throughout music. We can get more specific about who the topmost influencers are by viewing their names in the *new_influencers_df* with the help of the *head* function. For example, the top 10 influencers can be viewed such:

```{r}
head(new_influencers_df, 10)
```

In the same manner that we can measure a given singer's *influence* by counting how many artists have listed them as such, we can measure a singers *inspiration* by counting how many influencers the artist appears as a follower of. Similarly, we list the top 10 most "inspired" artists below:

```{r}
new_followers_df <- followers_df[order(-followers_df$Freq),] 
head(new_followers_df, 10)
```

### How many observable "genres" are present in the data?

Using attributes such as:

-   danceability
-   energy
-   valence
-   tempo
-   loudness
-   mode
-   key
-   acousticness
-   instrumentalness
-   liveness
-   speechiness  
-   explicit
-   duration

We extract a small sample of data (this is done to avoid trying to run the clustering algorithm on millions of data points) and use the [HCPC method](http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/117-hcpc-hierarchical-clustering-on-principal-components-essentials/) provided by the *FactoMineR* package to compute principle components, perform hierarchical clustering and further adjust the resulting partitioning with *K-Means*:

    install.packages(c("FactoMineR", "factoextra"))
    library(factoMineR)
    library(factoextra)

    ind <- sample(2, nrow(fullMusicDNew), replace = T, prob = c(0.8,0.2))
    fmusic1 <- fullMusicDNew[ind==1, ]
    fmusic2 <- fullMusicDNew[ind==2, ]

    res.pca <- PCA(fmusic2, ncp = 3, graph = FALSE)
    res.hcpc <- HCPC(res.pca, graph = FALSE)

    plot(res.hcpc)

***Results*** The above cluster analysis reveals that the songs in this dataset can be grouped into three categories. This is despite the fact that our data lists artists belonging to a total of 20 different genres as the following lines of R code demonstrate:

```{r}
library(sets)
inf_set <- as.set(influences$influencer_main_genre) 
foll_set <- as.set(influences$follower_main_genre)
genres  <- set_union(inf_set, foll_set)
length(genres)
```

This disparity might be because, in the hopes to cut down on runtime, we only used 20% of the data for this clustering task, but it might also be because, in terms of the metrics listed above, many songs are more closely related than self-reported genres of their singers might have you believe.

One a related note, similar to how we settled the debate about who the most influential singers might be, we show a pie chart of genres as reported by the artists themselves. Perhaps unsurprisingly, most artists identify as Pop/rock singers.

```{r}
pop_genres <- select(influences, influencer_main_genre) 
pop_genres_df <- as.data.frame(table(pop_genres)) 
pop_genres_df <- pop_genres_df[order(-pop_genres_df$Freq), ] 

install.packages('viridis') 
library(viridis) 

pie(pop_genres_df$Freq, labels = NA, radius = 1, col = inferno(20)) 
legend(.9, .1, pop_genres_df$pop_genres, cex = 0.7, fill = inferno(20))
```

### Conclusions

Music forms such a big part of the human experience that its influence on many people's lives cannot be overstated. Because the listening experience is such a subjective experience, we believe it would be fair to say that no objective study of songs (including our own) can every truly capture its effects on humanity. However, subjective though it might be, perhaps because of the fact, it is still interesting to study the numbers related to the subject and uncover any patterns. The same song might illicit two completely different feelings in two individuals, but if many more people are listening to the same song (sometimes over and over) there has to be something special about the song. The fact that these "popularity" numbers and other physical properties of the songs were measured and collected into a dataset with 98340 unique items, allowed us to investigate the last 90 years of music on this planet and identify patterns that reveal information about this time period. For example, by looking at tempo, we were able to identify the time period when swing rose to popularity, and by looking at explicitness, we pinpointed when vulgarity in songs stopped being something the general public took issue with.

Moreover, using a model that used three parameters (i.e liveness, loudness, and duration) we developed a highly accurate model (97%) for predicting the popularity of an unseen song. This information could be used by talented but avaricious artists for example who want to succeed quickly. In addition to this, we settled a few dinner debates. For example, we found that chances are high that music originality and popularity are negatively correlated. Therefore, it is indeed true that to find original music one has to probably dig deeper than following the billboard top 100. In addition, we confirmed that The Beatles have had the most musical influence in all of the past 90 years, and that most artists identify as Pop/rock singers. Lastly, a cluster analysis of 20% of the total songs we had at our disposal, revealed that if everything about music could be captured by the parameters recorded in this dataset, we could group them in only 3 clusters. Either this points to the above claim that music experience, and therefore similarity, is a highly subjective matter and thus no dataset that relies on physical properties of songs is well equiped to investigate "song similarity" or the number of songs that were used for this clustering task was insufficient and perhaps more groups could have been uncovered given more running time and more data (after all if each 20% chunk produces 3 different groupings, we could have 15 clusters in total; which sounds more reasonable).

### Impact

Though the social impact of our inquiries may not have the same weight as an analysis of Covid-19 statistics or global debt, they may have a cultural or artistic impact; oftentimes, popular music is seen as the soundtrack of a generation or time period. Millions of music fans all over the world spend countless billions of dollars on recordings and concert tickets for a shared experience of something "transcendent". The issue is, especially with popular music, the art is more manufactured than created; record label executives have formulas, both musical and aesthetic, which when utilized, will reliably generate hit music. Our analysis seems to point to the existence of these formulas (whether or not they are created deliberately). We believe that in the act of attempting to reverse engineer these formulas, we may begin to better understand not only the mechanics behind what makes music popular, but also those characteristics of music which evoke certain feelings within ourselves; thus potentially inspiring a more mindful consumption of art. There is however the unfortunate other side of the coin in this research which can't be helped. Though the analysis of such statistics can be used in an introspective sense to help fans see through the "music by numbers" approach employed by lazy artists and greedy executives, it can also be used by those very same bad actors to create even more optimized formulas for mass consumption.
