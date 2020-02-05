#create a sentiment bar plot by matching words with nrc lexicon
Nrc_Plot<- function(text,
                    title) {
  nrc<-text %>% inner_join(get_sentiments("nrc"),
                           by = c("term"="word"))
  plot<- nrc %>%
    group_by(sentiment) %>%
    summarise(word_count = sum(`sum(count)`)) %>%
    ungroup %>%
    mutate(sentiment = reorder(sentiment,word_count))
  ggplot(plot, aes(sentiment, word_count, fill = -word_count)) +
    geom_col() +
    guides(fill = FALSE) +
    labs(x = NULL, y = "Word Count") +
    scale_y_continuous(limits = c(0, 9000)) + 
    ggtitle(title) +
    coord_flip()
}