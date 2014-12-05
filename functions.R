# Functions for use in compiling a unigram sentiment lexicon.
# =============================================================================

# Set 3 gigabyte memory allocation for weka (increase if possible).
options(java.parameters = "-Xmx3g")
require("RWeka")   # R front-end for Weka machine learning library.


ExtractUnigrams = function(dat, chunk=100000) {
  # Extracts unigrams from a data frame containing tweets with emoticons
  # stripped and sorted by whether the stripped emoticon was positive (encoded
  # "sent"=4) or negative (encoded "sent"=0).

  # Arguments:
  # "dat" contains two columns:
  #   $sent, which is the inferred sentiment of a tweet from the emoticon(s)
  #   stripped from it (integer in c(0,4)).
  #   $txt, the text content of the tweet with the emoticon scrubbed (char).
  # "chunk" the number of tweets to process at a time (if not set there may
  #   be performance issues with running the RWeka tokeniser).

  # Returns: one table for "neg" and one for "pos" containing the number of
  # occurences of a scrubbed entry (integer) with the entry being the row name
  # (table).

  # Run the unigram extraction process for "pos" and "neg" tweets separately.
  lapply(c("pos" = 4, "neg" = 0), function(sent) {

    # Filter tweets by sentiment.
    d = dat[dat$sent == sent, "txt"]

    # Apply the Unigram tokenizer by chunks. It is based on RWeka's ngram-
    # tokenizer setting min and max gram-length to 1. At first we run it just
    # with space as a delimiter so we can trim the list further before
    # tokenizing over other symbols.

    u = lapply(
      lapply(seq(1, length(d), chunk), function(s) s:min(length(d), s + chunk - 1)),
      function(j) {
        NGramTokenizer(
          d[j],
          Weka_control(min=1, max=1, delimiters = " ")
        )
      }
    )

    # Unlist the results so we are looking at entries rather than whole tweets.
    u = unlist(u)
    # Remove weblinks or mentions of people (twitter handles)
    u = u[!grepl("(http|@)", u)]

    # Convert HTML code for lesser and greater than symbols.
    u = gsub("&gt", ">", u)
    u = gsub("&lt", "<", u)

    # Run further tokenization on a range of punctuation symbols. 
    u = NGramTokenizer(
      u,
      Weka_control(min=1, max=1, delimiters = ".,;?!:|()/*+\\[\\]{}<>")
    )

    # Remove any entries that contain non-"grapical" symbols (neiher
    # alphanumeric or punctuation).
    u = u[!grepl("[^[:graph:]]", u, perl=TRUE)]

    # Clean up HTML code and remove hash signs.
    u = gsub("&quot", "", u)
    u = gsub("#", "", u)
    u = gsub("&amp", "&", u)

    # Collapse all letters: any alphanumeric symbol which is found more than
    # two times consecutively is collapsed to two times (e.g "uuu" -> "uu").
    u = gsub("([[:alnum:]])\\1+", "\\1\\1", u)
 
    # Make all entries lower-case 
    u = tolower(u)

    # Return a table with entries as rownames and the number of times they
    # have occurred as a value (integer).
    table(u)
  })
}


RemoveSparseUnigrams = function(ugrams, cutoff=5) {
  # Removes unigrams from the "pos" and "neg" lists which do not occur at least
  # "cutoff" times in eiher list.

  # Arguments:
  # "ugrams" $pos and $neg each a table wth entry occurrence counts
  #   and the entry as rowname (list).
  # "cutoff" the minimum number of times an entry must occur in either $pos or
  #   $neg to be included in the unigrams lists (integer).

  # Returns: a list with too sparse unigrams removed from $pos and $neg (list).

  # Get the entries that exceed the sparsity cutoff.
  entries = union(
    names(ugrams$neg)[ugrams$neg >= cutoff],
    names(ugrams$pos)[ugrams$pos >= cutoff]
  )

  # Subset the unigram tables on the entries meeting the cutoff.
  list(
    "neg" = ugrams$neg[intersect(names(ugrams$neg), entries)],
    "pos" = ugrams$pos[intersect(names(ugrams$pos), entries)]
  )
}


ScoreLexicon = function(lex) {
  # Calculates sentiment scores given a lexicon with count for $neg and $pos
  # (negatie and positive). Scores are in the range (-5, 5) and are based on
  # subtracting the pairwise mutual information of negative from positive
  # entries ( score = pmi(entry, pos) - pmi(entry, neg).

  # Arguments: "lex" tables for positive and negative with entry occurence
  # (int) and the entry as rowname (list).

  # Returns: sentiment score per term (numeric vector).

  # For each of the two sentiment outcome (positive negative) and for each
  # lexicon entry we divide the probability of the entry given sentiment by the
  # probability of the entry irrespective of sentiment classification.
  x = lapply(c("pos"="pos", "neg"="neg"), function(sent) {
    (lex[, sent] / sum(lex[ , sent])) /
      ((lex$pos + lex$neg) / sum(lex$pos, lex$neg))
  })

  # We return the difference betwen the log of the probability ratios from
  # above subject to a min-max range of (-5, 5) (otherwise we would get some
  # Inf and -Inf results.
  pmax(pmin(log(x$pos) - log(x$neg), 5), -5)
}


CompileUnigramLex = function(ugrams) {
  # Compiles a unigram data frame with positive and negative counts by entry
  # given two tables (one for positive and one for negative)

  # Purge any NA values.
  ugrams$pos = ugrams$pos[complete.cases(ugrams$pos)]
  ugrams$neg = ugrams$neg[complete.cases(ugrams$neg)]

  # The unigrams that appear in both tables.
  i = intersect(names(ugrams$pos), names(ugrams$neg))
  # The unigrams that appear in only the positive table.
  p = setdiff(names(ugrams$pos), names(ugrams$neg))
  # The unigrams that appear in only the negative table.
  n = setdiff(names(ugrams$neg), names(ugrams$pos))

  # Bind the three sets of entries together; assigning zero count to
  # NA entries.
  i = data.frame(cbind("pos"=ugrams$pos[i], "neg"=ugrams$neg[i]))
  p = data.frame(cbind("pos"=ugrams$pos[p], "neg"=0))
  n = data.frame(cbind("pos"=0, "neg"=ugrams$neg[n]))

  rbind(i, p, n)
}
