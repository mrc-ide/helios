library(hunspell)

spell_check <- function(filename) {
  content <- tolower(readLines(filename, warn = FALSE))

  words <- unlist(strsplit(content, "\\W+"))
  words <- words[words != ""]

  misspelled_words <- hunspell(words, dict = 'en_GB')
  misspelled_words <- unlist(misspelled_words)
  correct_words <- c("uvc", "helios", "ggplot")
  misspelled_words <- setdiff(misspelled_words, correct_words)

  return(sort(misspelled_words))
}

spell_check("vignettes/blueprint.Rmd")
