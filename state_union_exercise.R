# --------------------------------------------------------------------------
# Step 1: Install and load needed packages
# --------------------------------------------------------------------------

# a package that is not normally part of the R repository,
# so we need to be explicit where it should be installed from
install.packages("austin", repos = "http://R-Forge.R-project.org", 
                 dependencies = "Depends", type = "source")
packages_to_install <- c("tm", "NLP", "austin", "ggplot2", "SnowballC", "rio")
install.packages(packages_to_install, type = "binary",
                 repos = "https://cran.rstudio.com",
                 dependencies = TRUE)
rm(packages_to_install)
library(tm)
library(NLP)
library(austin)
library(ggplot2)
library(SnowballC)
library(rio)
library(haven)

# --------------------------------------------------------------------------
# Step 2: Import texts and create corpus
# --------------------------------------------------------------------------
# Download and extract ZIP file
# https://archive.org/details/State-of-the-Union-Addresses-1945-2006

# Set directory to the folder where the .txt files are located
setwd("state_union")
ds <- DirSource(
  directory = ".",
  pattern = ".*txt",
  mode = "text"
)

# Create a corpus
corpus <- VCorpus(ds)

# Load additional precleaned data with speeches after 2006
load(url("http://www.joselkink.net/files/data/sou_corpus_precleaning.Rdata"))

# Load additional metadata to corpus
# Original source was http://www.joselkink.net/files/data/sou_meta_data.dta
# However, the file doesn't contain cruz.txt and sanders.txt (rightfully, they were not presidents)
# But their 2016 speeches are included in the corpus and I couldn't remove documents from corpus
# Otherwise, the ggplot script complains that corpus has 78 docs and data has only 76 fields
# Thus, I downloaded DTA file and added metadata about these speeches
data <- read_dta("../sou_meta_data.dta")

# --------------------------------------------------------------------------
# Step 3: Pre-processing of data
# --------------------------------------------------------------------------

# put all text in lower case
corpus <- tm_map(corpus, content_transformer(tolower))

# remove English stop words
# think if some other stop words are needed to be removed
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeWords, c("must", "will", "can", "make"))

# stem the text
corpus <- tm_map(corpus, stemDocument)

# remove punctuation
corpus <- tm_map(corpus, removePunctuation)

# remove numbers
corpus <- tm_map(corpus, removeNumbers)

# preview some document in the corpus
inspect(corpus[[1]])

# preview first few lines inside some document in the corpus
lines_of_text <- head(content(corpus[[1]]), 7)
print(lines_of_text)
rm(lines_of_text)

# --------------------------------------------------------------------------
# Step 4: Create document-term matrix
# --------------------------------------------------------------------------

# create document-term matrix
dtm <- DocumentTermMatrix(corpus)

# remove rare words
dtm <- removeSparseTerms(dtm, sparse = 0.4)

# preview to get an idea what was created
inspect(dtm)

# find often used words, for example all words that occur more than 400 times
findFreqTerms(dtm, 400)

# --------------------------------------------------------------------------
# Step 5: WordFish
# --------------------------------------------------------------------------

# purpose of WordFish: get underlying ideological dimension
# variation of Latent Semantic Analysis (LSA)
# uses Singular Value Decomposition (SVD) to identify latent topics in text corpus

# --------------------------------------------------------------------------
# Step 5.1: Mark reference texts for WordFish
# --------------------------------------------------------------------------

# need to give two examples of different positions to the model
# risk that word usage is more correlated with time than with ideology
# mitigate this problem by using two reference texts from the same time period
# the text do not have to be extreme points
# this exercise's extreme points are cruz.txt and sanders.txt, used for Wordscores
reference.texts <- c(
  which(names(corpus) == "1981-Reagan.txt"),
  which(names(corpus) == "1978-Carter.txt")
)

# --------------------------------------------------------------------------
# Step 5.2: Run WordFish
# --------------------------------------------------------------------------

# tolerance can be reduced to 0.003 if a quicker estimation needed
# lower tolerance - slower estimation (knitting)
wf <- wordfish(as.wfm(dtm), reference.texts,
               control = list(tol = .00003))
plot(wf)

# --------------------------------------------------------------------------
# Step 5.3: Convert into normal plot, show per party
# --------------------------------------------------------------------------

# assign z.critical a value of 97.5th percentile of the standard normal distribution
z.critical <- qnorm(.975)

lower <- wf$theta - z.critical * wf$se.theta
upper <- wf$theta + z.critical * wf$se.theta

ggplot(mapping = aes(y = wf$theta, x = data$year, color = data$party)) + geom_point() + 
  geom_smooth() +
  geom_segment(aes(x = data$year, y = lower, xend = data$year, yend = upper)) +
  labs(x = "Year", y = "Wordfish theta") + guides(col = guide_legend(title = "Party"))