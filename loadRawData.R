require(readxl)
require(taylor)
require(tidyverse)
require(here)


## Renaming the datasets from the taylor package because I don't like underscores haha!
taylorAlbums <- taylor_albums #includes album names, release dates, critics scores and user scores
taylorAlbumSongs <- taylor_album_songs #includes album names, album releases, track numbers, track names, featuring, all spotify metrics and lyrics
taylorAllSongs <- taylor_all_songs ##same as taylorAlbumSongs, except it also includes songs she wrote/sang and are not in her albums

# Load the Excel files
# include the edited taylorAlbumSongs (removed all the Taylor's version, from the vault, etc)
taylorAlbumSongs <- "taylorAlbumSongs.xlsx"
surpriseSongsDressColours <- "surpriseSongsDressColoursMashupsLong.xlsx" #Edited, all the mashups are underneath the main songs, and the names of the songs are changed
allSongsMetadata <- "albumInfoMetadataNeutralMK.xlsx"
relationshipsTimeline <- "surpriseSongsDressColoursMashupsLong.xlsx"

surpriseSongsDressColours <- read_excel(surpriseSongsDressColours, sheet = "List")
allSongsMetadata <- read_excel(allSongsMetadata, sheet = "metadata")
relationshipsTimeline <- read_excel(relationshipsTimeline, sheet = "Relationships")

#Now, I need to select only one row per concert, which I can do by choosing only
# the first song played in each concert
oneRowPerConcert <- surpriseSongsDressColours %>%
  group_by(Date) %>%
  arrange(Date, Order) %>%  # Assuming 'Order' indicates song order in the concert
  slice(1) %>%
  ungroup()

oneRowPerConcert$Date <- as.Date(oneRowPerConcert$Date)


## Dresses images

pathToDressColors <- here("Dresses")

oneRowPerConcertWithImages <- oneRowPerConcert %>%
  count(DressName) %>%
  mutate(
    percentage = n / sum(n) * 100,
    # Automatically build the path based on the dress name
    imagePath = file.path(pathToDressColors, paste0(DressName, ".jpg"))
  )


