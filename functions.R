## Normalize song names

normalizeSongNameF <- function(songname) {
  songname <- tolower(songname)                
  songname <- gsub("[[:punct:]]", "", songname) 
  songname <- trimws(songname)                
  return(songname)
}


## Tokenize lyrics

createTokenizedLyricsF <- function(songData) {
  # First create a unique identifier for each song
  songsWithIdF <- songData %>%
    mutate(songId = row_number()) %>%
    select(songId, everything())
  
  # Now tokenize the lyrics
  tokenizedLyricsF <- songsWithIdF %>%
    # Split lyrics into individual words
    unnest_tokens(
      output = word,      # Name of the new column containing individual words
      input = lyrics,     # Column containing the lyrics
      token = "words",    # Split by words (could also use other options like characters, sentences)
      drop = FALSE        # Keep the original lyrics column
    ) %>%
    # Remove empty strings and NA values
    filter(!is.na(word), word != "") %>%
    # Remove numbers
    filter(!str_detect(word, "^[0-9]+$"))
  
  return(tokenizedLyricsF)
}



## Create the sentiment analysis with correct join syntax
calculateSentimentsF <- function(tokenizedData) {
  sentimentResults <- tokenizedData %>%
    inner_join(get_sentiments("bing"), by = c("word" = "word"), 
               relationship = "many-to-many")
  
  return(sentimentResults)
}
