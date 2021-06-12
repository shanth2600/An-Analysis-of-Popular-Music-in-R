---
title: "An Analysis of Popular Music"
author: "Bosco Ndemeye and Shant Hairapetian"
date: "6/7/2021"
output:
  html_document: default
  pdf_document: default
---

```{r setup, echo = TRUE,  message=FALSE}
library(ggplot2)
library(xts)
library(magrittr) 
library(dplyr)
library(tidyverse)
library(sjPlot)
library(data.table)
library(viridis) 
library(FactoMineR)
library(factoextra)
influences <- read.csv('~/Desktop/670/data/influence_data.csv')
full_music_data <- read.csv('~/Desktop/670/data/full_music_data.csv')
```

## The Data

The data we used is comprised of two separate datasets:

-   **full_music_data** : Data which provides 16 variable entries about each of the top 100 songs from 1929 to 2020. This data was scraped from Spotify's API by some third party.

    ```{r}
    head(full_music_data)
    ```

-   **influence_data**: Data outlining the self-reported direct influences of artists. This data was scraped by a Harvard graduate student (Wenzhe Xue) from allmusic.com for the purposes of writing a thesis analyzing musical influence among popular artists.

## Our Questions

### What are the characteristics of popular music over the years

We plotted the values of specific variables over time to see if we could find distinct trends in popular music over time. By applying some geometric smoothing, we found that a subset of the variables exhibited distinct shifts over time which corresponded to popularity of certain types of music in specific time periods.

```{r Tempo, echo=TRUE, message=FALSE}
ggplot(full_music_data, aes(x = year, y=tempo)) + geom_smooth()
```

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

-   liveness
-   loudness
-   duration_ms
-   (date \* duration_ms)

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

### Can we predict whether a song will be a hit?

**Results**: (13242 + 171) / (13242 + 171 + 41 + 251) = \~98% Correct or \~2% Error on test data. We found that this was the optimal subset of variables and that 0.5 was the best threshold for top 10 or not (0 or 1). The fact that we were able to predict to this high of an accuracy seems to suggests that popularity in music may be even more formulaic than we previously thought.

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

**Results**: As we can see due the low p-value (\< 2e-16), chances are high that the number of influences of an artist has a significant positive correlation with the number of top 100 hits. This suggests that, if our interpretation of orginality is correct in this data, it is not rewarded when it comes to popular music. Quite the opposite.

### Which artists have had the most musical influence?

We answer this question by counting the frequency of each unique singer in the influence data, and present the results using a worldcloud of the influencer names:

```{r , echo = TRUE, message = FALSE}
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

To answer this question, we posit that "song similarity" can be quantified using the euclidean distance between two vectors, each corresponding to one song, with an entry in the vector corresponding to the song's recorded value of the attribute in the dataset when these attributes are sorted in the order presented above. We extract a small sample of the data (this is done to avoid trying to run the clustering algorithm on millions of data points) and use the [HCPC method](http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/117-hcpc-hierarchical-clustering-on-principal-components-essentials/) provided by the *FactoMineR* package to compute principle components, perform hierarchical clustering and further adjust the resulting partitioning with *K-Means*:

``` {r K-Means, echo = TRUE, message = FALSE}

    fullMusicDNew <- subset(full_music_data, select=-c(artist_names, artists_id, year, release_date, popularity, song_title..censored.))
    ind <- sample(2, nrow(fullMusicDNew), replace = T, prob = c(0.9,0.1))
    fmusic1 <- fullMusicDNew[ind==1, ]
    fmusic2 <- fullMusicDNew[ind==2, ]

    res.pca <- PCA(fmusic2, ncp = 3, graph = FALSE)
    res.hcpc <- HCPC(res.pca, graph = FALSE)

    plot(res.hcpc)
  ```

***Results*** The above cluster analysis reveals that, based on this similarity criterion, the songs in this dataset can be grouped into three categories. This is despite the fact that our data lists artists belonging to a total of 20 different genres as the following lines of R code demonstrate:

```{r, echo = TRUE, message = FALSE}
library(sets)
inf_set <- as.set(influences$influencer_main_genre) 
foll_set <- as.set(influences$follower_main_genre)
genres  <- set_union(inf_set, foll_set)
length(genres)
```

This disparity might be because, in the hopes to cut down on runtime, we only used 10% of the data for this clustering task, but it might also be because our notion of similarity does not precisely correspond to a genre and many songs across genres are more closely related.

On a related note, similar to how we settled the debate about who the most influential singers might be, we show a pie chart of genres as reported by the artists themselves. Perhaps unsurprisingly, we find that most artists identify as Pop/Rock singers.

```{r}
pop_genres <- select(influences, influencer_main_genre) 
pop_genres_df <- as.data.frame(table(pop_genres)) 
pop_genres_df <- pop_genres_df[order(-pop_genres_df$Freq), ] 



pie(pop_genres_df$Freq, labels = NA, radius = 1, col = inferno(20)) 
legend(.9, .1, pop_genres_df$pop_genres, cex = 0.7, fill = inferno(20))
```

### Conclusions

Music forms such a big part of the human experience that its influence on many people's lives cannot be overstated. Because the listening experience is so subjective, we believe it would be fair to say that no objective study of songs (including our own) can ever truly capture its grip on the zeitgeist across time. However, subjective though it might be--- and perhaps because of the fact---it is still interesting to study the numbers related to the subject and relate the findings to physical world. The same song might elicit two completely different feelings in two individuals, but if many more people are listening to the same song (sometimes over and over) there has to be something special about the song, and we would like to quantify the characteristics of these special songs. The fact that these quantifiable properties were measured and collected into a dataset with 98340 unique items, allowed us to investigate the last 90 years of music on this planet. For example, by looking at tempo, we were able to identify the time period when swing rose to popularity, and by looking at explicitness, we pinpointed when vulgarity in songs stopped being something the general public took issue with.

Moreover, using a model that used three parameters (i.e liveness, loudness, and duration) we developed a highly accurate model (98%) for predicting the popularity of an unseen song. For example, such information could be used (and is probably being used) by artists who want to succeed quickly without much care about the originality of their material. In addition to this, we settled a few dinner debates. For example, we found that chances are high that music originality and popularity are negatively correlated. Therefore, it is indeed true that to find original music one has to probably dig deeper than following the billboard top 100. In addition, we confirmed that The Beatles have had the most musical influence in all of the past 90 years, and that most artists identify as Pop/rock singers. Lastly, a cluster analysis of 10% of the total songs we had at our disposal, revealed that if everything about music could be captured with the parameters recorded in this dataset, we could group them in only 3 clusters. Either this points to the above claim that music experience, and therefore similarity, is a highly subjective matter and thus no statistical analysis is well equiped to investigate "song similarity" or the number of songs that were used for this clustering task was insufficient and perhaps more groups could have been uncovered given more running time and more data.

## Analysis

### Trends

It comes as a big surprise to no one that popular music follows distinct trends over time. We saw this reflected in our plots of tempo, liveness, energy, instrumentalness, and song duration over time. This suggests that popular artists, whether by parallel thinking or deliberate choice, largely follow the lead of other popular artists in the type of the music which they release.

### Music by Numbers

We were able to create a model which given those variables (and combination of variables) most highly correlated to popularity was able to predict with almost \~98% accuracy whether a song would be a top 10 hit or not. We posit that if songwriting were a purely creative endeavor, that this would be impossible. Our remarkable ability to predict popularity to such a high level of accuracy suggests popular music can be created through a "music by numbers" approach. Further evidence against the creativity of pop music was our model which correlated "originality" with popularity and found a statistically significant positive relationship between the number of influences of an artist and their popularity (we do concede however, that our notion of originality is not tautological and it is further made dubious by that fact that the influence data is self-reported).

The counterpoint to this argument that pop music is by and large not creative would be that the trends in music come from some external source (correlation, not causation) i.e massive parallel thinking, a collective unconscious, zeitgeist etc. We would however argue that due to the lucrative nature of popular music, and the vast number of record companies and individuals which stand to profit from hits, occam's razor would seem to suggest that the formulas behind popular music do not emerge from a Jungian construct.

## Impact

Though the social impact of our inquiries may not have the same weight as an analysis of Covid-19 statistics or global debt, they may have a cultural or artistic impact; oftentimes, popular music is seen as the soundtrack of a generation or time period. Millions of music fans all over the world spend countless billions of dollars on recordings and concert tickets for a shared experience of something "transcendent". The issue is, especially with popular music, the art is more often manufactured than created; record label executives have formulas, both musical and aesthetic, which when utilized, will reliably generate hit music. Our analysis seems to point to the existence of these formulas (whether or not they are created deliberately). We believe that in the act of attempting to reverse engineer these formulas, we may begin to better understand not only the mechanics behind what makes music popular, but also those characteristics of music which evoke certain feelings within ourselves; thus potentially inspiring a more mindful consumption of art.

There is however the unfortunate other side of the coin in this research which can't be helped. Though the analysis of such statistics can be used in an introspective sense to help fans see through the "music by numbers" approach employed by lazy artists and greedy executives, it can also be used by those very same bad actors to create even more optimized formulas for mass consumption.
