# Code to follow along to Chapter 6: Topic Modelling in the tidy text book

library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(tidyr)

data("AssociatedPress")
AssociatedPress

# We can use the LDA() function from the topicmodels package to create 2 topic LDA model

# set a seed so that the output of the model is predictable
ap_lda <- LDA(AssociatedPress, k=2, control = list(seed = 1234))
ap_lda

# 6.1 Word-topic probabilities ------------------------------------------------

# Using the tidy() for tidying model objects, in this case, extracting the per-topic-per-word
# probabilities (beta)

# Each of these probabilities indicate the likelihood of that term being
# generated from that topic

ap_topics <- tidytext::tidy(ap_lda, matrix = "beta")

# Viewing top words in each topic
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# Notice that some words are in both topics, this is an advantage of topic
# modelling compared to "hard-clustering" methods

# Finding the greatest difference

beta_wide <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_wide

# Document-topic probabilities --------------------------------------------

# LDA also models each document as a mixture of topics. 
# We can examin the per-document-per-topic probabilities, called gamma

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

# Each value is an estimated proportion of words from that document that are
# generated from that topic

# Document 6 is almost entirely from topic 2, let's review 

tidy(AssociatedPress) %>% 
  filter(document == 6) %>% 
  arrange(desc(count))


# 6.2: great library heist ------------------------------------------------

# When examining a statistical method, it can be useful to try on a 
# very simple case where you know the right answer. 

