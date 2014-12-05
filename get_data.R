# Code for importing releant data sets into R:
#   "tweets" the 1.6m emoticon-stripped tweets from Sentiment140
#   "nrclex" the subjectiity lexicons based on the s140tweets created by the
#     national research council canada
#===============================================================================


# SETTINGS
# ============================================================================

data.folder = "data/"
tweets.file = "tweets.Rdat"
nrclex.file = "nrclex.Rdat"
s140lex.file = "s140lex.Rdat"


# FUNCTIONS
# =============================================================================

AcquireTweets = function() {
  # Get the tweets, unzip, load into a data frame and save the data
  # frame down for future reference.

  # Settings
  zip.file = "sent140.zip"
  csv.file = "training.1600000.processed.noemoticon.csv"
  download.url = "http://cs.stanford.edu/people/alecmgo/trainingandtestdata.zip"

  # Download the data file.
#  download.file(url = download.url, destfile = paste0(data.folder, zip.file))

  # Unzip the data.file.
#  unzip(zipfile = zip.file, exdir="data")

  # Read the extracted .csv file.
  tweets = read.csv(
    file = paste0(data.folder, csv.file),
    stringsAsFactors = FALSE,
    header = FALSE,
  )[, c(1,6)]

  colnames(tweets) = c("sent", "txt")

  # Save the data frame with the Sentiment140 data down as variable "tweets".
  save(tweets, file=paste0(data.folder, tweets.file))

  # Return the tweets.
  tweets
}


AcquireS140Lex = function() {
  # Read the Sentiment140 subjectiviy lexicon for unigrams found at
  # saifmohammad.com (copyright National Research Council Canada).
  # Get the data, save it down, unzip, load into a data frame and save the data
  # frame down for future reference.

  # Settings
  zip.file = "Sentiment140-Lexicon-v0.1.zip"
  csv.file = "unigrams-pmilexicon.txt"
  download.url =
    "http://www.saifmohammad.com/WebDocs/Sentiment140-Lexicon-v0.1.zip"

  # Download the data file.
  download.file(url = download.url, destfile = paste0(data.folder, zip.file))

  # Unzip the data.file.
  unzip(zipfile = zip.file, exdir="data")

  # Read the tab-separated data file.
  # (We use readLines rather than read.csv because there is an unresolved
  # parsing issue with read.csv and read.table).
  s140lex = readLines(paste0(data.folder, "Sentiment140-Lexicon-v0.1/", csv.file))
  s140lex = unname(sapply(s140lex, strsplit, split="\t"))

  # Convert to a data frame.
  s140lex = unname(t(data.frame(s140lex, stringsAsFactors=FALSE)))

  s140lex = data.frame(
    "sent" = as.numeric(s140lex[, 2]),
    "pos" = as.integer(s140lex[, 3]),
    "neg" = as.integer(s140lex[, 4]),
    row.names = s140lex[, 1],
    stringsAsFactors=FALSE
  )

  # Save the data frame with the variable name "s140lex".
  save(s140lex, file=paste0(data.folder, s140lex.file))

  # Return the lexicon.
  s140lex
}


AcquireNRCLex = function() {
  # Read the Hashtag Sentiment Lexicon for unigrams found at
  # saifmohammad.com (copyright National Research Council Canada).
  # Get the data, save it down, unzip, load into a data frame and save the data
  # frame down for future reference.

  # Settings
  zip.file = "NRC-Hashtag-Sentiment-Lexicon-v0.1.zip"
  csv.file = "unigrams-pmilexicon.txt.gz"
  download.url =
    "http://www.saifmohammad.com/WebDocs/NRC-Hashtag-Sentiment-Lexicon-v0.1.zip"

  # Download the data file.
  download.file(url = download.url, destfile = paste0(data.folder, zip.file))
  
  # Unzip the data.file.
  unzip(zipfile = paste0("data/", zip.file), exdir = data.folder)

  # Read the tab-separated data file.
  # (We use readLines rather than read.csv because there is an unresolved
  # parsing issue with read.csv and read.table).
  nrclex = readLines(paste0(data.folder, "NRC-Hashtag-Sentiment-Lexicon-v0.1/", csv.file))
  nrclex = unname(sapply(nrclex, strsplit, split="\t"))

  # Convert to a data frame.
  nrclex = unname(t(data.frame(nrclex, stringsAsFactors=FALSE)))

  nrclex = data.frame(
    "sent" = as.numeric(nrclex[, 2]),
    "pos" = as.integer(nrclex[, 3]),
    "neg" = as.integer(nrclex[, 4]),
    row.names = nrclex[, 1],
    stringsAsFactors=FALSE
  )

  # Save the data frame with the variable name "nrclex".
  save(nrclex, file=paste0(data.folder, nrclex.file))

  # Return the lexicon.
  nrclex
}


AcquireBingLiu = function() {
  # Processes the BingLiu positive and negative word lists.

  # See:
  #  Minqing Hu and Bing Liu. "Mining and Summarizing Customer Reviews." 
  #       Proceedings of the ACM SIGKDD International Conference on Knowledge 
  #       Discovery and Data Mining (KDD-2004), Aug 22-25, 2004, Seattle, 
  #       Washington, USA, 
  #   Bing Liu, Minqing Hu and Junsheng Cheng. "Opinion Observer: Analyzing 
  #       and Comparing Opinions on the Web." Proceedings of the 14th 
  #       International World Wide Web conference (WWW-2005), May 10-14, 
  #       2005, Chiba, Japan.

  pos = readLines("lexicons/bing_liu/positive-words.txt")
  neg = readLines("lexicons/bing_liu/negative-words.txt")

  # Strip the first 35 lines which contain meta-information.
  pos = pos[36:length(pos)]
  neg = neg[36:length(neg)]

  # Assign (+1) sentiment score to positive words, and (-1) sentiment score
  # to negative words.
  pos = data.frame(stringsAsFactors=FALSE, txt = pos, sent = 1)
  neg = data.frame(stringsAsFactors=FALSE, txt = neg, sent = -1)
  # Remove a few duplicate entries (three).
  pos = pos[!pos$txt %in% neg$txt, ]

  rbind(pos, neg)
}


# PROGRAM
# ============================================================================

# Load the Sentiment140 R data file if it already exists, otherwise get and
# process the data from their website.

if(tweets.file %in% list.files(data.folder)) {
  load(paste0(data.folder, tweets.file), envir=globalenv())
} else {
  tweets = AcquireTweets()
}


# Load the S140 lexicon file if it already exists, otherwise get and
# process the data from their website.

if(nrclex.file %in% list.files(data.folder)) {
  load(paste0(data.folder, s140lex.file), envir=globalenv())
} else {
  s140lex = AcquireS140Lex()
}

# Load the NRC lexicon file if it already exists, otherwise get and
# process the data from their website.

if(nrclex.file %in% list.files(data.folder)) {
  load(paste0(data.folder, nrclex.file), envir=globalenv())
} else {
  nrclex = AcquireNRCLex()
}
