# Functions for use in compiling a unigram sentiment lexicon.
# =============================================================================

# Load the tweets data (Sentiment140) and also the NRC-Sent140 unigrams lexicon
# source("get_data.R")

# Load the functions needed to create the unigram subjectivity lexicon.
# source("functions.R")

# CREATE THE UNIGRAM SUBJECTIVITY LEXICON
# -----------------------------------------------------------------------------

# First create a data frame with the entries and the counts by positive and
# negative tweet.
iplex = CompileUnigramLex(
  RemoveSparseUnigrams(
    ExtractUnigrams(tweets, chunk=100000)
  )
)

# Score our lexicon using the difference between pointwise mutual information
# of term with positie and negative tweet classification.
iplex = cbind(
  sent = ScoreLexicon(iplex),
  iplex
)


# BENCHMARK THE INPIRICAL LEXICON AGAINST THE OTHER LEXICONS
# -----------------------------------------------------------------------------

Coverage = function(lex.A, lex.B) {c(
  "both" = length(intersect(rownames(lex.A), rownames(lex.B))),
  "A.only" = length(setdiff(rownames(lex.A), rownames(lex.B))),
  "B.only" = length(setdiff(rownames(lex.B), rownames(lex.A)))
)}

LexCorrelation = function(lex.A, lex.B) {
  inter = intersect(rownames(lex.A), rownames(lex.B))
  cor(lex.A[inter, "sent"], lex.B[inter, "sent"])
}

Disagreement = function(lex.A, lex.B) {
  inter = intersect(rownames(lex.A), rownames(lex.B))
  sum(sign(lex.A[inter, "sent"]) != sign(lex.B[inter, "sent"])) / length(inter)
}
