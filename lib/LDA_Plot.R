#The function that tidy the data and plot top ten used words for each topic 
LDA_Plot <- function(data, 
                     plot = T, 
                     topics = 2) 
{  
  Corpus <- Corpus(VectorSource(data))
  DTM <- DocumentTermMatrix(Corpus) 
  lda <- LDA(DTM, k = topics, control = list(seed = 164))
  topics <- tidy(lda, matrix = "beta") 
  top_terms <- topics  %>% 
    group_by(topic) %>% 
    top_n(10, beta) %>%
    ungroup() %>% 
    arrange(topic, -beta) 
  if(plot == T){
    top_terms %>% 
      mutate(term = reorder(term, beta)) %>% 
      ggplot(aes(term, beta, fill = factor(topic))) + 
      geom_col(show.legend = FALSE) + 
      facet_wrap(~ topic, scales = "free") + 
      labs(x = NULL, y = "Beta") + 
      coord_flip() 
  }else{ 
    return(top_terms)
  }
}