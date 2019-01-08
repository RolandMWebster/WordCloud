
# Packages ----------------------------------------------------------------

# As always, we'll start by loading our packages and giving a quick overview of what we're using them for.
library("tidyr")
library("dplyr")
library("wordcloud")
library("tm")

# * We'll use tidyr and dplyr for some  simple data manipulation
# * The tm package allows us to work with our text by creating a corpus and from there a Term Document Matrix.
# * The wordcloud package givues us what we need to plot our wordcloud!

# Wine Data ---------------------------------------------------------------

# We're going to make a word cloud on the below wine data set that I grabbed after a quick browse on Kaggle. 
# I don't have a large interest in wine so I'm not sure what to expect but there' a specific column that attracted me to this
# dataset for my wordcloud project.

# Let's read the data and take a look. It's in a .csv format so we use the simple read.csv() function:
wine.data <- read.csv("winemag-data-130k-v2.csv",
                      stringsAsFactors = FALSE)

# And we'll take a look
glimpse(wine.data)
# It's the description column that we're interested in. We'll use this column to build a word cloud that will give us some insight into some
# of the more popular words used when describing wines. We'll then use a colour scale to see if we can determine which words correlate
# to a good wine score and which words are closer associated with a bad wine score.

# But first, we need to cut down the number of observations in this dataset (my laptop can't handle all these rows once we start
# working with the tm package functions). 

# So how can we cut down our number of observations? Well, the data set contains A LOT of different wines:
length(unique(wine.data$variety))
# 708!

# Well, I would imagine we would be look for very different characteristics when comparing red and white wines.
# So why don't we only look at red wine? (Since I don't much like white wine anyway...)

# I've found a list of the "top 10 most popular red grapes", we'll build a word cloud from these 10 varieties
top.red.wines <- c("Cabernet Sauvignon", "Merlot", "Pinot Noir", "Zinfandel", "Syrah",
                   "Shiraz", "Malbec", "Tempranillo", "Sangiovese", "Barbera", "Carménère")

# This is 11 but I'm told Syrah and Shiraz are the same grape so we'll bunch those toghether.
# We'll make sure our data has these varieties in it:

unique(wine.data$variety[wine.data$variety %in% top.red.wines])

# We're missing "Carménère", it could be an accent thing:
varieties <- unique(wine.data$variety)

varieties[grepl("^Carm", varieties)]

# That's why, we'll skip the blends and rename that first one:

# Replace our spelling with the spelling in the data
top.red.wines[11] <- varieties[grepl("^Carm", varieties)][1]


# Filter our wine data for our chosen grape varieties
wine.data.red <- wine.data %>%
  filter(variety %in% top.red.wines)

# 42,003 observations, much better, we can also remove most of these columns:

wine.data.red <- wine.data.red %>%
  select(description, points)


# Unfortunately 42,000 is still too many observations so we'll sample some points:

set.seed(1000)
sample.rows <- sample(nrow(wine.data.red))[1:10000]
wine.data.sample <- wine.data.red[sample.rows,]





# We'll start by using the corpus function to turn our description column into a corpus of text.
wine.corpus <- Corpus(VectorSource(wine.data.sample$description))


# Tidying the data --------------------------------------------------------

# There is some tidying to do with the text to get the most out of our wordloud. In this document we'll use three very basic 
# tidying technqiues. We'll:
# * Transform all our text to lower case;
# * Remove any punctuation from our descriptions;
# * Remove any extra white space from our descritptions.

# Transform all characters to lowercase
wine.corpus <- tm_map(wine.corpus, content_transformer(tolower))

# Remove punctuations
wine.corpus <- tm_map(wine.corpus, removePunctuation)

# Eliminate extra white spaces
wine.corpus <- tm_map(wine.corpus, stripWhitespace)              


# The last thing we'll do is remove stopwords, these are common words in the english language that will just be noise
# in our wordcloud. Words like "and" or "the", that give us no insight into what words are used often that are specific
# to describing wine.

# It might be that we spot some more words that are appearing frequently in our word cloud which we aren't intesreted in.
# We can analyze the wordcloud once we create and see if we want to remove anything.

# Remove stopwords
wine.corpus <- tm_map(wine.corpus, removeWords, stopwords("english"))
       
                              
# Create Term Document Matrix ---------------------------------------------

# Convert our Corpus into a Term Document Matrix. This will allow us to count the frequency of each word in our Corpus.
wine.tdm <- TermDocumentMatrix(wine.corpus)          

# Convert to a matrix for manipulation
wine.matrix <- as.matrix(wine.tdm)

# Sum the rows to get a vector of frequencies. Order this vector:
wine.vector <- sort(rowSums(wine.matrix),decreasing=TRUE)

# Store the words and their corresponding frequencies in a data.frame.
wine.wordcloud.data <- data.frame(word = names(wine.vector),
                                  freq = wine.vector)

# Take a look at what our data.frame looks like:
head(wine.wordcloud.data,
     n = 10)

# Filter for minimum frequency (We'll go for 100)
wine.wordcloud.data <- wine.wordcloud.data %>% 
  filter(freq >= 100)

# We now want to add on the mean score for each word.
# We'll start by initializing a mean_points column in our wordcloud data:
wine.wordcloud.data <- wine.wordcloud.data %>%
  mutate(mean_points = 0)

# Join on average wine score per word

# We'll use a for loop to join on the mean score for each word (we could probably use ddply here so this is something to improve):

# For each word in our wordcloud data we take the average wine score for wine that contains that word in the description.
# We then update our mean_points column with that value:
for(i in wine.wordcloud.data$word){
  
  y <- wine.data.sample %>%
    filter(grepl(i, description)) %>%
    summarise(mean_points = mean(points))
  
  wine.wordcloud.data$mean_points[wine.wordcloud.data$word == i] <- y[[1]]
  
}


# Order by mean points
wine.wordcloud.data <- wine.wordcloud.data %>%
  arrange(mean_points)

# Look at the shape of the distribution of points.

# We're hoping for a linear distribution. This is close enough though. 
plot(wine.wordcloud.data$mean_points)

# Well do quite a basic (and rough) application of a colour value
# to each point using the colorRampPalette function:
# Get unique values and assign a colour
colfunc <- colorRampPalette(c("red", "blue"))


# And let's build the wordcloud
wordcloud(words = wine.wordcloud.data$word, freq = wine.wordcloud.data$freq, 
          min.freq = 100,
          max.words=200, random.order=FALSE, rot.per=0.35,
          ordered.colors = TRUE,
          colors = colfunc(nrow(wine.wordcloud.data)))


# This is retroactively added once we find what words are appearing that we aren't interested in.
custom.stopwords <- c("wine", "grape", "flavors", "flavor", "drink", "red", "like", "bit")



# Remove custom stopwords
wine.corpus <- tm_map(wine.corpus, removeWords, custom.stopwords)
