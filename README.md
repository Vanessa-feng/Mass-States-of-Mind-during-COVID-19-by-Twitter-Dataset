# Mass-States-of-Mind-during-COVID-19-by-Twitter-Dataset

In this project, we are mainly interested in analysing the social media posts (Tweets) re- lated to the COVID19 topics from Jan 2020 to April 2021. Leveraging the public datsets [2], we are aimed at completing three major tasks in this context. First, data mining and extracting summary statistics from big data. Second, we would visualize the key trajectories representing public status using ggplot and R-shiny App. Lastly, the modeling part concentrated on unsupervised learning. We will use time series based clustering methods to group topics with similar trends of popularity.

## Data Summary
The dataset we have selected is ongoing collection of tweets IDs associated with the novel coron- avirus COVID-19 (SARS-CoV-2)(https://github.com/echen102/COVID-19-TweetIDs)[2], which com- menced on January 28, 2020 and still collects in real-time tweets that mention specific keywords. By means of the hydrating tools Hydrator[7] or Twarc[8], the hydrated Tweets will be stored and is saved as a compressed .jsonl file.
Due to the limitation of Twitter developer student account authority, we could only achieve 500,000 tweets each month. The alternative dataset is Augmented Multilingual Twitter Dataset for Studying the COVID-19 Infodemic (https://github.com/lopezbec/COVID19_Tweets_Dataset) [1], which has hydrated the Twitter dataset from SARS-CoV-2 during January 2020 to April 2021. It provides the information about the tweet id, language, geolocation, country, hashtag, original tweets, retweets, likes, mentions, sentiment label and some other features.
As of April/30/2021, there are a total of 1,651,202,565 tweets collected and the average daily number of tweets was 153,463, among which there are a total of 4,032,760 tweets with geo-location information. The dataset with such magnitude is enough for us to carry out our project.

## Data Pre-processing
The challenge for data processing is that the original data size is in hundred GB level, and thus it’s difficult to download and process locally. The procedures are shown as follows:
• Use SVN tool to transport original data from github to HPCC
• Merge the original data (originally separate .csv files by different types of features and by
each hour) to a long and wide form with keys as tweets ID in Python
• Data re-aggregation: calculate daily or monthly statistics for different features, re-organize in terms of date. For example, we have to aggregate the tweets statistics in terms sentiment scores, hashtag counts, language counts, etc by each time window. The number of metrics corresponding to the number of summarized tables. The preliminary statistics is calculated by Python basics and pandas. Feature merge and further calculation is done by dplyr and tidyr R package.

## Data Visualization by R Shiny
To make a more comprehensive and interactive visualization, we decided to use shiny, an open source R package that provides an elegant and powerful web framework for building web ap- plications using R (https://vanessa-xin.shinyapps.io/207_Project_Shiny/). ggplot2 and leaflet R packages are the main tools we will use with Shiny.

## Data Modeling
we mainly focus on clustering of most frequent hashtags, group the hashtags according to their popularity trends. First, the basic algorithms and terms related to DTW are introduced. Second, we will introduce the procedures of selecting optimal number of clusters, experiment with different center/distance measurements, and model evaluation.
