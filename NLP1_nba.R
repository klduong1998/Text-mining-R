#Text mining: Word frequency, word length, lexical diversity, lexical density

library(tidytext)
library(dplyr)
library(ggplot2)
library(knitr) #For kable_tyling()
library(kableExtra) #For kable_tyling()
library(wordcloud2)
library(formattable) #for the color_title function
NBA_Data_Sample_Clean1

##More cleaning for the dataset
#Remove '.fastscript' that was attached to many words in the data file
removefastscripts <- function(x) gsub("[[:punct:]]*fastscripts[[:punct:]]*", "", x)
NBA_Data_Sample_Clean1$Interview <- sapply(NBA_Data_Sample_Clean1$Interview, removefastscripts)
# function to remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
NBA_Data_Sample_Clean1$Interview <- sapply(NBA_Data_Sample_Clean1$Interview, removeSpecialChars)

#Saving the file
save(NBA_Data_Sample_Clean1, file = "NBA_Data_Sample_Clean1.RData")

#Create customized fucntions for the plots
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00")

theme_lyrics <- function() 
{
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
}

#Unnest 'Interview'
nba1 <- NBA_Data_Sample_Clean1 %>%
  unnest_tokens(word, Interview) %>%
  distinct()

#Determind the class and dimension of the unnested dataset
class(nba1)
dim(nba1) #58528 rows and 3 columns compared to 167 rows in the old dataset

##Check which player used "confident" and "confidence" is in the interviews: 21
#21 players used "confident"
nba1 %>% 
  filter(word == "confident") %>%
  arrange() %>%
  distinct() %>%
  mutate(No. = seq(21)) %>%
  mutate(word = color_tile("lightgreen","lightgreen")(word)) %>%
  kable("html", escape = FALSE, align = "c", caption = "Tokenized Format Example") %>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"), 
                full_width = FALSE)

#47 players used "confidence"
nba1 %>% 
  filter(word == "confidence") %>%
  arrange() %>%
  mutate(seq(46)) %>%
  mutate(word = color_tile("lightgreen","lightgreen")(word)) %>%
  kable("html", escape = FALSE, align = "c", caption = "Tokenized Format Example") %>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"), 
                full_width = FALSE)

#68 players used either "confident" or "confidence" ==> Need to remove duplicate records of players
nba1 %>% 
  filter(word == "confidence"| word == "confident") %>%
  arrange() %>%
  mutate(word = color_tile("lightgreen","lightgreen")(word)) %>%
  kable("html", escape = FALSE, align = "c", caption = "Tokenized Format Example") %>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"), 
                full_width = FALSE)

##Word frequency
nba2_wordcount <- NBA_Data_Sample_Clean1 %>%
  unnest_tokens(word, Interview) %>%
  group_by(Person) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words))

#Word count distribution: Range: 0-6000 words. 
#Most players spoke about 1000 words, not much of a big deal. It may just depend on the length of the interview
nba2_wordcount %>%
  ggplot() +
  geom_histogram(aes(x = num_words)) +
  ylab("Number of players") + 
  xlab("Word Count per Player") +
  ggtitle("Word Count Distribution") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor.y = element_blank())

##Most used words:
#The most used are like "to, the, of, it, etc." LOL ==> got to filter stuff out
nba1 %>%
  count(word, sort = TRUE) %>% #count(word) count the frequency of the words
  top_n(10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n), fill = my_colors[4]) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Player count") +
  ggtitle("Most Frequently Used Words in by the players") +
  coord_flip()

#nba3_nostopw: Remove stop words, 1-3 character words and words that don't carry much meaning
#Different forms of "play" were the most popular words, so they were removed as well
words_to_remove = c("yeah", "play","playing","played","fastscripts","transcript")
nba3_nostopw <- NBA_Data_Sample_Clean1 %>%
  unnest_tokens(word, Interview) %>%
  anti_join(stop_words) %>%
  filter(nchar(word) > 3) %>%
  filter(!word %in% words_to_remove) %>%
  distinct()
#Should we add "play" back in?

#Top 10: time, game, pretty, feel, sports, asap, couple, week, hard, tough
nba3_nostopw %>%
  count(word, sort = TRUE) %>% #count(word) count the frequency of the words
  top_n(20) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n), fill = my_colors[4]) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Player count") +
  ggtitle("Most Frequently Used Words in by the players") +
  coord_flip()
#Word cloud
wcwordcount <- nba3_nostopw %>%
  count(word, sort = TRUE) 

wordcloud2(wcwordcount[1:300, ], size = .5)

##Word length: Well the longest words are just a bunch of words stuck together
nba_word_lengths <- NBA_Data_Sample_Clean1 %>%
  unnest_tokens(word, Interview) %>%
  group_by(Person) %>%
  distinct() %>%
  mutate(word_length = nchar(word)) 

nba_word_lengths %>%
  count(word_length, sort = TRUE) %>%
  ggplot(aes(word_length), 
         binwidth = 10) + 
  geom_histogram(aes(fill = ..count..),
                 breaks = seq(1,25, by = 2), 
                 show.legend = FALSE) + 
  xlab("Word Length") + 
  ylab("Word Count") +
  ggtitle("Word Length Distribution") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank())
#Word cloud for word length
wc <- nba_word_lengths %>%
  ungroup() %>%
  select(word, word_length) %>%
  distinct() %>%
  arrange(desc(word_length))
wordcloud2(wc[1:300, ], 
           size = .15,
           minSize = .0005,
           ellipticity = .3, 
           rotateRatio = 1, 
           fontWeight = "bold")


###TF - IDF: So confused about these codes
nba4_popwords <- NBA_Data_Sample_Clean1 %>%
  unnest_tokens(word, Interview) %>%
  distinct() %>%
  anti_join(stop_words) %>%
  filter(nchar(word) > 3) %>%
  filter(!word %in% words_to_remove) %>%
  count(Person, word, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, Person, n)

nba_toppopwords <- nba4_popwords %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(Person) %>% 
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(Person, tf_idf) %>%
  mutate(row = row_number())

nba_toppopwords %>%
  ggplot(aes(x = row, tf_idf, fill = Person)) +
  geom_col(show.legend = NULL) +
  labs(x = NULL, y = "TF-IDF") + 
  ggtitle("Important Words using TF-IDF by Person") +
  theme_lyrics() +  
  facet_wrap(~Person, scales = "free") +
  scale_x_continuous(  # This handles replacement of row 
    breaks = nba_toppopwords$row, # notice need to reuse data frame
    labels = nba_toppopwords$word) +
  coord_flip()

