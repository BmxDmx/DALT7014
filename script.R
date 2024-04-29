source("require_packages.R")
require_packages(c(
  "xml2",
  "httr",
  "dplyr",
  "readr",
  "tibble",
  "tidyr",
  "tidytext",
  "stringr",
  "ggplot2",
  "reshape2"
))
library(reshape2)

afinn <- read_csv("afinn.csv")

# HTTP GET Request
a <- GET("http://rss.cnn.com/rss/cnn_latest.rss")

html_doc <- read_xml(rawToChar(a$content))

all_items <- xml_find_all(html_doc, ".//item")

titles <- xml_text(xml_find_all(all_items,".//title"))
description <- xml_text(xml_find_all(all_items,".//description"))
pub_date <- xml_text(xml_find_all(all_items,".//pubDate"))

df <- tibble(Title= titles, Description = description,Publication_Date = pub_date)



(title_tokenised <- df %>% unnest(Title) %>% unnest_tokens(token, Title))
(title_word_counts <- title_tokenised %>% 
    count(token))
title_afinn_sentiment_counts <- afinn %>%
  inner_join(title_word_counts, by = c("word" = "token")) %>% 
  group_by(value) %>%
  summarise(n = sum(n))


title_afinn_graph <- title_afinn_sentiment_counts %>% 
  ggplot(aes(x = value, y = n, fill = value)) +
  geom_col(show.legend = FALSE) + 
  labs(title = "Frequency of afinn sentiments on the title of the news")
title_afinn_graph


(details_tokenised <- df %>% unnest(Description) %>% unnest_tokens(token, Description))
(details_word_counts <- details_tokenised %>% 
    count(token))

details_afinn_sentiment_counts <- afinn %>%
  inner_join(details_word_counts, by = c("word" = "token")) %>% 
  group_by(value) %>%
  summarise(n = sum(n))

details_afinn_graph <- details_afinn_sentiment_counts %>% 
  ggplot(aes(x = value, y = n, fill = value)) +
  geom_col(show.legend = FALSE) + 
  labs(title = "Frequency of afinn sentiments of the description of the news")
details_afinn_graph


dataDf<- merge(title_afinn_sentiment_counts,details_afinn_sentiment_counts,by = 'value', all = TRUE)
dataDf <- dataDf %>%
  rename(
    sentiment_value = value,
    occurrences_in_Title = n.x,
    occurrences_in_Description = n.y
  )

dataDf<- dataDf %>%
  replace(is.na(.),0)

temp<- melt(dataDf,id.vars='sentiment_value')

plot<-ggplot(temp,aes(x=sentiment_value,y=value,fill=variable))+
  geom_bar(stat='identity',position='dodge')
ggsave(format(Sys.Date(), "SentAnalysis-%Y-Week-%V.png"),plot = plot)



scoreDf<-setNames(data.frame(matrix(ncol=4,nrow=1)),c("title_score","description_score","mean_title","mean_description"))

scoreDf$title_score<- sum(dataDf$sentiment_value* dataDf$occurrences_in_Title)
scoreDf$description_score<- sum(dataDf$sentiment_value* dataDf$occurrences_in_Description)
scoreDf$mean_title<- sum(dataDf$sentiment_value* dataDf$occurrences_in_Title)/sum(dataDf$occurrences_in_Title)
scoreDf$mean_description<- sum(dataDf$sentiment_value* dataDf$occurrences_in_Description)/sum(dataDf$occurrences_in_Description)



dataDf|>write_csv(format(Sys.Date(), "DataDf-%Y-Week-%V.csv"))
scoreDf|>write_csv(format(Sys.Date(), "ScoreDf-%Y-Week-%V.csv"))
writeLines(format(Sys.time()), "timestamp.txt") # Added to make sure there is always a change to commit
