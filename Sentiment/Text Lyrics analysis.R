setwd("C:/Users/giuli/Dropbox/R imh")

#Berenice - Allan Poe
#Harry Potter e la pietra filosofale

library(dplyr)
library(ggplot2)
library(tidytext)
library(wordcloud)
library(tidyr)

library(tidyverse)      # data manipulation & plotting,it has several built in lexicons for sentiment analysis
library(stringr)   # text cleaning and regular expressions

library(wordcloud)
library(devtools)
library(reshape2)
library(igraph)
library(ggraph)

#HP
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}

devtools::install_github("bradleyboehmke/harrypotter")
library(harrypotter)    # provides the first seven novels of the Harry Potter series


# Read lyrics into R:

#text <- readLines("Berenice - Allan Poe.txt")
#text <- readLines("Harry Potter and the Sorcerer's Stone.txt")
text = tolower(text) #make it lower case
text = gsub('[[:punct:]]', '', text) #remove punctuation


# Preview all the texts:

text_df <- data_frame(Text = text) # tibble aka neater data frame

head(text_df)

#converts the data frame in such a way such that each word has its own row
text_words <- text_df %>% 
  unnest_tokens(output = word, input = Text)

head(text_words)


# Remove English stop words: the, and, me , myself, of and so on.
text_words <- text_words %>%
anti_join(stop_words , by = "word") 

# Word Counts:
text_wordcounts <- text_words %>% count(word, sort = TRUE)
head(text_wordcounts)



#make a plot of the word counts.
# ggplot2 Plot (Counts greater than 8)
# Bottom axis removed with element_blank()
# Counts in the bar with geom_text.

text_wordcounts %>% 
  mutate(word = reorder(word, n)) %>% 
  filter(n >= 50) %>%
  ggplot(aes(word, n)) + 
  geom_col(fill = "blue") +
  coord_flip() +
  labs(x = "Word \n", y = "\n Count ", title = "Word Counts \n") +
  geom_text(aes(label = n), hjust = 1.2, colour = "yellow", fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12))

#Wordcloud
wordcloud(text_wordcounts$word, text_wordcounts$n, random.color = T, max.words = 100)


#Sentiment Analysis
#analysed with three lexicons: bing, AFINN and nrc.

#Bing Lexicon
#it categorizes words in a binary fashion into positive and negative categories
text_words_bing <- text_wordcounts %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  ungroup()

head(text_words_bing)

#words not plotted are neutral
text_words_bing %>%
  filter(n >= 20) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = sentiment)) + 
  geom_bar(stat = "identity", position = "identity") +
  geom_text(aes(label = n), colour = "black", hjust = 1, fontface = "bold") +
  coord_flip() +
  labs(x = "\n Word \n", y = "Word Count \n", title = "Sentiment Scores Of Words \n Under bing Lexicon") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="darkblue", size = 12),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12)) +
  scale_fill_manual(values=c("#FF0000", "#01DF3A"), guide=FALSE)


# Separate Sentiment plots for positive and negative words

text_words_bing %>%
  filter(n >= 20) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), colour = "black", hjust = 1, fontface = "bold") +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "\n Word \n", y = "\n Word Count ", title = "Sentiment Scores Of Words \n Under bing Lexicon") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="darkblue", size = 12),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12)) +
  scale_fill_manual(values=c("#FF0000", "#01DF3A"), guide=FALSE) +
  coord_flip()


#AFINN Lexicon
#AFINN lexicon measures sentiment with a numeric score between -5 and 5
text_words_afinn <- text_wordcounts %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  ungroup()

head(text_words_afinn)




#Bigrams
text_bigrams <- text_df %>% 
  unnest_tokens(bigram, input = Text, token = "ngrams", n = 2)

text_bigrams

# Remove stop words from bigrams with tidyr's separate function
# along with the filter() function
bigrams_separated <- text_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#Counts can be produced with the count() function from R's dplyr package.
# Filtered bigram counts:
text_bigrams_wordcounts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

# Unite the words with the unite() function:
text_bigrams_wordcounts <- text_bigrams_wordcounts %>%
  unite(bigram, word1, word2, sep = " ")

text_bigrams_wordcounts

#plot bigrams
text_bigrams_wordcounts %>% 
  filter(n >= 20) %>%
  ggplot(aes(reorder(bigram, n), n)) + 
  geom_col(fill = "blue") +
  coord_flip() +
  labs(x = "Bigram \n", y = "\n Count ", title = "Bigrams \n") +
  geom_text(aes(label = n), hjust = 1.2, colour = "yellow", fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12))


################################################################
################################################################
# #philosophers_stone: Harry Potter and the Philosophers Stone (1997)
# #chamber_of_secrets: Harry Potter and the Chamber of Secrets (1998)
# #prisoner_of_azkaban: Harry Potter and the Prisoner of Azkaban (1999)
# goblet_of_fire: Harry Potter and the Goblet of Fire (2000)
# order_of_the_phoenix: Harry Potter and the Order of the Phoenix (2003)
# half_blood_prince: Harry Potter and the Half-Blood Prince (2005)
# deathly_hallows: Harry Potter and the Deathly Hallows (2007)

titles <- c("Philosopher's Stone", "Chamber of Secrets", "Prisoner of Azkaban",
            "Goblet of Fire", "Order of the Phoenix", "Half-Blood Prince",
            "Deathly Hallows")

books <- list(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban,
              goblet_of_fire, order_of_the_phoenix, half_blood_prince,
              deathly_hallows)

series <- tibble()

for(i in seq_along(titles)) {
  
  clean <- tibble(chapter = seq_along(books[[i]]),
                  text = books[[i]]) %>%
    unnest_tokens(word, text) %>%
    mutate(book = titles[i]) %>%
    select(book, everything())
  
  series <- rbind(series, clean)
}

# set factor to keep books in order of publication
series$book <- factor(series$book, levels = rev(titles))
series


#Text Tidying

# Remove English stop words 
series <- series %>%
  anti_join(stop_words)

#Order for frequency by group
series %>%
  group_by(book) %>%
  count(word, sort = TRUE) %>%
  top_n(10)

#Plot frequency per group
series %>%
  group_by(book) %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(book = factor(book, levels = titles),
         text_order = nrow(.):1) %>%
  ggplot(aes(reorder(word, text_order), n, fill = book)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ book, scales = "free_y") +
  labs(x = "NULL", y = "Frequency") +
  coord_flip() +
  theme(legend.position="none")


#Wordcloud
series %>% 
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

#Now, let's calculate the frequency for each word across the entire Harry Potter series versus within each book. 
#This will allow us to compare strong deviations of word frequency within each book
#as compared to across the entire series.
# calculate percent of word use across all novels
words_pct <- series %>%
  count(word) %>%
  transmute(word, all_words = n / sum(n))

# calculate percent of word use within each novel
frequency <- series %>%
  count(book, word) %>%
  mutate(book_words = n / sum(n)) %>%
  left_join(words_pct) %>%
  arrange(desc(book_words)) %>%
  ungroup() 
frequency

#Plot frequency
ggplot(frequency, aes(x = book_words, y = all_words, color = abs(all_words - book_words))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5, color="black") +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "blue", high = "black") +
  facet_wrap(~ book, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Harry Potter Series", x = NULL)

#Words that are close to the line in the plots have similar frequencies across all the novels.
#For example, words such as "harry", "ron", "dumbledore" are fairly common and used with similar frequencies across most of the books.
#Words that are far from the line are words that are found more in one set of texts than another.
#Furthermore, words standing out above the line are common across the series but not within that book;
#whereas words below the line are common in that particular book but not across the series.
#For example, "cedric" stands out above the line in the Half-Blood Prince.
#This means that "cedric" is fairly common across the entire Harry Potter series but is not used as much in Half-Blood Prince.
#In contrast, a word below the line such as "quirrell" in the Philosopher's Stone suggests this word is common in this novel but far less common across the series.


#Correlate the word frequencies between the entire series and each book
frequency %>%
  group_by(book) %>%
  summarize(correlation = cor(book_words, all_words),
            p_value = cor.test(book_words, all_words)$p.value)
#The high correlations, significant (p-values < 0.0001), suggests that the relationship
#between the word frequencies is highly similar across the entire Harry Potter series.


#Sentiment Analysis
#for this example we will stick with 'nrc' and 'bing'
#The 'nrc' is a more advanced lexicon that categorizes words into several sentiment categories - sadness, anger, positive, negative, trust, etc. 
#A single word in this lexicon may fall into multiple categories.

series_bing <- series %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  ungroup()
series_bing

series_nrc <-  series %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentiment))
series_nrc

#Wordcloud comparison
series_bing %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8768D", "#00BFC8"),
                   max.words = 50)

#As a next step, one might look at the maximum sentiment score and the minimum sentiment score for each book
#to see what text groups produced the extreme scores.
series %>%
  group_by(book) %>% 
  mutate(word_count = 1:n(),
         index = word_count %/% 500 + 1) %>% 
  inner_join(get_sentiments("bing")) %>%
  count(book, index = index , sentiment) %>%
  ungroup() %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative,
         book = factor(book, levels = titles)) %>%
  ggplot(aes(index, sentiment, fill = book)) +
  geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) +
  facet_wrap(~ book, ncol = 2, scales = "free_x")

#Bigrams
series_bi <- tibble()
for(i in seq_along(titles)) {
  
  temp <- tibble(chapter = seq_along(books[[i]]),
                 text = books[[i]]) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    ##Here we tokenize each chapter into bigrams
    mutate(book = titles[i]) %>%
    select(book, everything())
  
  series_bi <- rbind(series_bi, temp)
}
# set factor to keep books in order of publication
series_bi$book <- factor(series_bi$book, levels = rev(titles))
series_bi

#Frequency of bigrams
series_bi %>%
  count(bigram, sort = TRUE)

series_bi %>%
  group_by(book) %>%
  count(bigram, sort = TRUE) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(book = factor(book, levels = titles),
         text_order = nrow(.):1) %>%
  ggplot(aes(reorder(bigram, text_order), n, fill = book)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ book, scales = "free_y") +
  labs(x = "NULL", y = "Frequency") +
  coord_flip() +
  theme(legend.position="none")

