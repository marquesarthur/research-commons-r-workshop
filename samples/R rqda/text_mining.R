
# skip if already installed
# install.packages('pdftools')
# install.packages('tidyverse')
# install.packages('tidytext')
# install.packages("tm")
# install.packages("ggplot2")
# install.packages("stringi")


library('tm')
library('tidyverse')
library('tidytext')
library('ggplot2')
library('dplyr')


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

txt_data <- read.delim("FullTextReview_953.txt", header=FALSE)

data = data.frame(title=as.character(txt_data$V1), abstract=as.character(txt_data$V2))


# Clean text with the following steps
# 1. convert from whatever type to character, needed by the TM library
# 2. remove numbers
# 3. remove punctuation
# 4. convert to lower case
# 5. remove words of length smaller than 3
# 6. remove consecutive white spaces 
# 7. trim white spaces at the end or start


cleanMyText <- function(text) {
  text <- as.character(text)
  text <- removeNumbers(text)
  text <- removePunctuation(text)
  text <- tolower(text)
  text <- removeWords(text, stopwords("english"))
  text <- gsub('\\b\\w{1,2}\\b', '', text) # remove words of length smaller than 3
  text <- stripWhitespace(text)
  text <- trimws(text)
  text
}

# Custom list of words to be removed 

title <- c()
abstract <- c()
errors <- c()

for (row in 1:nrow(data)){
  tryCatch(
    expr = {
      a <- cleanMyText(data$title[row])
      b <- cleanMyText(data$abstract[row])
      
      title <- c(title, a)
      abstract <- c(abstract, b)
      
    },
    error = function() {
      errors <- c(errors, row)
    }
  )
}

sprintf("Error reading %d lines", length(errors))
sprintf("lines with errors :: ", errors)

data = data.frame(title=title, abstract=abstract)



# start getting bigrams, and trigrams.
# You need to change the column name of the dataframe to whatever you want, e.g. "text" 
# to control if it is unigram, bigram, trigram, etc, change n = 1, 2, 3...

# text clean block




# --------------------------------------------------------------
# example
mybigrams <- data %>% # " mybigrams_pdf" has to be changed whenever it is mentioned up until save figure
  unnest_tokens(bigram, abstract, token="ngrams", n = 2) %>%
  na.omit()  %>%
  count(bigram, sort = TRUE) 

head(mybigrams, 100)


plt <- mybigrams %>%
  arrange(desc(n)) %>%
  mutate(bigram=factor(bigram, levels=bigram)) %>%
  top_n(15) %>%
  ggplot(aes(x=bigram, y=n)) +
  geom_bar(stat = "identity") +
  coord_flip() 

plt
ggsave("bigram.png") # remember to have .png

# --------------------------------------------------------------