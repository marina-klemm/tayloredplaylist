require(shiny)
require(shinythemes)
require(dplyr)
require(DT)
require(stringr)
require(shinydashboard)
require(readr)
require(tidyr)
require(httr)
require(jsonlite)
require(shinyjs)  # Added for runjs function
require(shinyWidgets)  # For slider emoji functionality



# First, update the sentiment mapping - we'll keep this structure but use text labels instead of emojis
mood_category_map <- list(
  "Mad" = c("angry", "resentful", "vengeful"),
  "Glad" = c("happy", "playful", "joyful", "grateful", "admired", "excited", "peaceful", "warm"),
  "Down" = c("regretful", "hopeless", "vulnerable", "concerned", "anxious", "melancholic", "frustrated", "sad", "desperate"),
  "Uplifting" = c("encouraging", "empowering", "free", "optimistic", "empathetic", "hopeful", "compassion", "brave", "resolute", "passionate", "defiant"),
  #"Sentimental" = c("tempted", "reflective", "conflicted", "pleading", "accepting"),
  "Tender" = c("heartbroken", "lovesick", "grieving", "longing", "lonely"),
  "Longing" = c("reminiscent", "nostalgic", "tempted", "reflective", "conflicted", "pleading", "accepting")
)

# Create reverse mapping (sentiment to category)
sentiment_to_category <- list()
for (category in names(mood_category_map)) {
  for (sentiment in mood_category_map[[category]]) {
    sentiment_to_category[[sentiment]] <- category
  }
}


# Function to preprocess colors for the dropdown menu
preprocess_colors <- function(data) {
  # Extract the colour_MK column
  colors <- data$colour_MK
  
  # Remove NA values
  colors <- colors[!is.na(colors)]
  
  # Split by semicolons and trim whitespace
  split_colors <- unlist(strsplit(colors, ";"))
  clean_colors <- trimws(split_colors)
  
  # Get unique colors and sort them
  unique_colors <- sort(unique(clean_colors))
  
  return(unique_colors)
}


filter_by_muses <- function(data, selected_muses) {
  if (length(selected_muses) == 0 || is.null(selected_muses)) {
    return(data)
  }
  
  # Clean selected muses
  selected_muses_clean <- tolower(trimws(selected_muses))
  
  # Filter rows where any of the selected muses appear in muse_MK
  data %>%
    filter(!is.na(muse_MK)) %>%
    filter(sapply(muse_MK, function(muse_string) {
      # Split by all delimiters and clean
      muses_in_row <- strsplit(muse_string, ";|/|,| and ")[[1]] %>%
        trimws() %>%
        tolower()
      
      # Check if any selected muse matches any muse in the row
      any(selected_muses_clean %in% muses_in_row)
    }))
}

preprocess_muses <- function(data) {
  # Extract the muse_MK column
  muses <- data$muse_MK
  
  # Remove NA values
  muses <- muses[!is.na(muses)]
  
  # Split by all possible delimiters
  split_muses <- muses %>% 
    str_split(";|/|,| and ") %>% 
    unlist() %>% 
    trimws()
  
  # Further clean any remaining punctuation or whitespace
  clean_muses <- split_muses %>% 
    str_replace_all("[[:punct:]]$", "") %>%  # Remove trailing punctuation
    trimws() %>% 
    str_replace("^\\s+|\\s+$", "") %>%  # Remove any remaining whitespace
    .[. != ""]  # Remove empty strings
  
  # Get unique values and sort
  unique_muses <- clean_muses %>% 
    unique() %>% 
    sort()
  
  return(unique_muses)
}

# Process data for UI elements
all_unique_colors <- preprocess_colors(allSongsMetadata)
all_unique_muses <- preprocess_muses(allSongsMetadata)




# Modified UI - removing sidebar, Spotify integration, correcting colors and collapsing to single page
ui <- dashboardPage(
  skin = "black", 
  dashboardHeader(title = "Taylor Swift Playlist Generator"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    useShinyjs(),
    tags$style(HTML("
      /* Override default colors */
      .small-box.bg-teal { 
        background-color: #88C4D7 !important; 
      }
      .small-box.bg-black { 
        background-color: #1A1A1A !important; 
      }
      .small-box.bg-fuchsia { 
        background-color: #F1B3D1 !important; 
      }
      .small-box.bg-orange { 
        background-color: #9E9E9E !important; 
      }
      .small-box.bg-blue { 
        background-color: #cc621b !important; 
      }
      .small-box.bg-yellow { 
        background-color: #2C2E52 !important; 
      }
      
      /* Ensure text is white */
      .small-box h3, .small-box p { 
        color: white !important; 
      }
      
      /* Custom box colors */
      .box.box-solid.box-primary>.box-header {
        background-color: #b8cfb3 !important;
        color: #000000;
      }
      
      /* Your ~Tailored~ Playlist~ box */
      #playlist_table ~ .box.box-solid.box-primary>.box-header {
        background-color: #1A1A1A !important;
        color: #FFFFFF !important;
      }
      
      /* Playlist Stats box */
      .box.box-solid.box-primary[title='Playlist Stats']>.box-header {
        background-color: #b8cfb3 !important;
        color: #000000 !important;
      }
      
      /* Box body background */
      .box.box-solid.box-primary {
        background-color: #FFFFFF !important;
      }
    ")),
    
    tabsetPanel(
      id = "main_tabs",
      
      # Generate Playlist Tab
      tabPanel(
        "Generate Playlist",
        fluidRow(
          box(
            title = "Filter Options", status = "primary", solidHeader = TRUE, width = 12,
            column(width = 3,
                   selectizeInput("sentiment", "Sentiments", 
                                  choices = sort(unique(allSongsMetadata$sentiment_MK)),
                                  multiple = TRUE,
                                  options = list(placeholder = "Select sentiment(s)")),
                   selectizeInput("message", "Message", 
                                  choices = sort(unique(allSongsMetadata$message_MK)),
                                  multiple = TRUE,
                                  options = list(placeholder = "Select message(s)"))
            ),
            column(width = 3,
                   selectizeInput("muse", "Muse", 
                                  choices = all_unique_muses,
                                  multiple = TRUE,
                                  options = list(placeholder = "Select muse(s)")),
                   selectizeInput("color", "Color References", 
                                  choices = all_unique_colors,
                                  multiple = TRUE,
                                  options = list(placeholder = "Select color(s)")),
                   radioButtons("color_match", "Color Match Type", 
                                choices = c("Exact match" = "exact", 
                                            "Contains text" = "contains"),
                                selected = "exact")
            ),
            column(width = 3,
                   textInput("keywords", "Keywords (comma separated)", ""),
                   selectizeInput("dress_filter", "Eras Tour Dress Color", 
                                  choices = c("No filter" = "", 
                                              sort(unique(surpriseSongsDressColours$DressName))),
                                  multiple = FALSE,
                                  options = list(placeholder = "Filter by dress color")),
                   radioButtons("match_type", "Matching Criteria", 
                                choices = c("Match any selected filters" = "any", 
                                            "Match all selected filters" = "all"),
                                selected = "any")
            ),
            column(width = 3,
                   selectInput("preset", "Preset Playlists (Available on Spotify!)", 
                               choices = c(
                                 "Choose a preset (optional)" = "",
                                 "Only Taylor Swift Understands My Heartbreak" = "heartbreak",
                                 "I AM FUMING (Taylor's Version)" = "mood_mad",
                                 "Doing a happy dance with Taylor Swift" = "mood_glad",
                                 "Taylor is Down BAD and So Am I" = "mood_down",
                                 "That Longing Feeling Only TayTay Gets" = "mood_longing",
                                 "Taylor Swift Made Me Feel Empowered Today" = "mood_uplifting",
                                 "Taylor Swift Sings About Golden" = "golden",
                                 "POV: Taylor sings a surprise song in her pink dress" = "pinkdress"
                               )),
                   div(
                     style = "display: flex; flex-direction: column; gap: 10px;",
                     actionButton("generate", "Generate Playlist", 
                                  icon = icon("play"), 
                                  style = "color: #000000; background-color: #ffea9f; width: 100%;"),
                     actionButton("clear", "Clear Filters", 
                                  icon = icon("eraser"), 
                                  style = "color: #fff; background-color: #8000ff; width: 100%;"),
                     downloadButton("downloadData", "Download CSV", 
                                    style = "color: #fff; background-color: #D34C58; width: 100%;")
                   )
            )
          )
        ),
        fluidRow(
          box(
            title = "Your ~Tailored~ Playlist", status = "primary", solidHeader = TRUE, width = 12, 
            DTOutput("playlist_table")
          )
        ),
        fluidRow(
          box(
            title = "Playlist Stats", status = "primary", solidHeader = TRUE, width = 12,
            fluidRow(
              valueBoxOutput("songCount", width = 4),
              valueBoxOutput("mainSentiment", width = 4),
              valueBoxOutput("mainColor", width = 4)
            ),
            fluidRow(
              valueBoxOutput("mainMuse", width = 4),
              valueBoxOutput("mainAlbum", width = 4),
              valueBoxOutput("mainDress", width = 4)
            )
          )
        )
      ),
      
      # About Tab
      tabPanel(
        "About",
        box(
          title = "About This App <3", 
          status = "primary", 
          solidHeader = TRUE, 
          width = 12, 
          HTML("
      <h3>Taylor Swift Playlist Generator</h3>
      <p>This app was built by a long-time Brazilian Swiftie (COME TO BRAZIL! Ooop, she did!) to other Swifties with the intent of creating custom playlists based on all the lore behind her songs.</p>
      <p>This database was collated throughout the Eras Tour and includes information about each song's:</p>
      <ul>
        <li>Sentiment: 43 unique sentiments to choose from (e.g., playful, vulnerable, hopeless);</li>
        <li>Message: the main message of the song (e.g., forbidden love, long distance relationship, second chances);</li>
        <li>Keywords: a braindump of all the lore involved (e.g., hindsight, missed chances, LBGT+ rights);</li>
        <li>Muses: people (confirmed and alleged) that inspired her songs (including enemies);</li>
        <li>Color references in her lyrics (as it turns out, she does mentions colors A LOT);</li>
        <li>And, of course: Eras Tour dresses she wore while singing them!</li>
      </ul>
      <p>Use the filters to create your perfect Taylor Swift playlist! All the preset playlists already exist on Spotify.</p>
      <p>Created and conceptualized by Marina Klemm and Charlotte Jones-Todd</p>
      
      <div style='margin-top: 20px;'>
        <p>
          <i class='fas fa-book' style='color: #3884FF;'></i> 
          <a href='https://cmjt.github.io/studyinswift/outfit_transitions.html' target='_blank' style='color: #3884FF;'>
            Read more about all the methods and statistical analysis documentation
          </a>
        </p>
        <p>
          <i class='fab fa-github' style='color: #333;'></i> 
          <a href='https://github.com/marina-klemm/ScreamingColor' target='_blank' style='color: #3884FF;'>
            View datasets and source code on GitHub
          </a>
        </p>
      </div>
    ")
        )
      )
    )
  )
)




# Server logic
server <- function(input, output, session) {
  # Create a lookup table for colors when the app starts
  color_lookup <- reactive({
    allSongsMetadata %>%
      mutate(song_id = row_number()) %>%
      separate_rows(colour_MK, sep = ";") %>%
      mutate(colour = trimws(colour_MK)) %>%
      filter(colour != "")
  })
  
  # Improved muse filtering function
  filter_by_muses <- function(data, selected_muses) {
    if (length(selected_muses) == 0) {
      return(data)
    }
    
    # Clean selected muses
    selected_muses_clean <- tolower(trimws(selected_muses))
    
    data %>%
      filter(!is.na(muse_MK)) %>%
      filter(sapply(muse_MK, function(muse_string) {
        # Split by all delimiters and clean
        muses_in_row <- strsplit(muse_string, ";|/|,| and ")[[1]] %>%
          trimws() %>%
          tolower()
        
        # Check if any selected muse matches any muse in the row
        any(selected_muses_clean %in% muses_in_row)
      }))
  }
  
 
  # Function to apply preset filters
  observeEvent(input$preset, {
    # Handle mood-based presets first
    if(input$preset == "mood_mad") {
      updateSelectizeInput(session, "sentiment", selected = mood_category_map[["Mad"]])
      updateSelectizeInput(session, "color", selected = NULL)
      updateSelectizeInput(session, "muse", selected = NULL)
      updateSelectizeInput(session, "message", selected = NULL)
    } else if(input$preset == "mood_glad") {
      updateSelectizeInput(session, "sentiment", selected = mood_category_map[["Glad"]])
      updateSelectizeInput(session, "color", selected = NULL)
      updateSelectizeInput(session, "muse", selected = NULL)
      updateSelectizeInput(session, "message", selected = NULL)
    } else if(input$preset == "mood_down") {
      updateSelectizeInput(session, "sentiment", selected = mood_category_map[["Down"]])
      updateSelectizeInput(session, "color", selected = NULL)
      updateSelectizeInput(session, "muse", selected = NULL)
      updateSelectizeInput(session, "message", selected = NULL)
    } else if(input$preset == "mood_uplifting") {
      updateSelectizeInput(session, "sentiment", selected = mood_category_map[["Uplifting"]])
      updateSelectizeInput(session, "color", selected = NULL)
      updateSelectizeInput(session, "muse", selected = NULL)
      updateSelectizeInput(session, "message", selected = NULL)
   # } else if(input$preset == "mood_sentimental") {
  #    updateSelectizeInput(session, "sentiment", selected = mood_category_map[["Sentimental"]])
   #   updateSelectizeInput(session, "color", selected = NULL)
  #    updateSelectizeInput(session, "muse", selected = NULL)
   #   updateSelectizeInput(session, "message", selected = NULL)
    #} else if(input$preset == "mood_tender") {
    #  updateSelectizeInput(session, "sentiment", selected = mood_category_map[["Tender"]])
    #  updateSelectizeInput(session, "color", selected = NULL)
    #  updateSelectizeInput(session, "muse", selected = NULL)
    #  updateSelectizeInput(session, "message", selected = NULL)
    } else if(input$preset == "mood_longing") {
      updateSelectizeInput(session, "sentiment", selected = mood_category_map[["Longing"]])
      updateSelectizeInput(session, "color", selected = NULL)
      updateSelectizeInput(session, "muse", selected = NULL)
      updateSelectizeInput(session, "message", selected = NULL)
    } 
    # Original presets remain unchanged
    else if(input$preset == "golden") {
      updateSelectizeInput(session, "color", selected = c("golden", "gold", "rose golden"))
      updateSelectizeInput(session, "sentiment", selected = NULL)
      updateSelectizeInput(session, "muse", selected = NULL)
    #} else if(input$preset == "blues") {
    #  updateSelectizeInput(session, "color", selected = c("blue", "blues", "ocean blue", "deep blue"))
    #  updateSelectizeInput(session, "sentiment", selected = NULL)
    #  updateSelectizeInput(session, "muse", selected = NULL)
    #} else if(input$preset == "blackwhite") {
    #  updateSelectizeInput(session, "color", selected = c("black", "white", "black and white"))
    #  updateSelectizeInput(session, "sentiment", selected = NULL)
    #  updateSelectizeInput(session, "muse", selected = NULL)
    #} else if(input$preset == "playful") {
    #  updateSelectizeInput(session, "sentiment", selected = "playful")
    #  updateSelectizeInput(session, "color", selected = NULL)
    #  updateSelectizeInput(session, "muse", selected = NULL)
    #} else if(input$preset == "vulnerable") {
    #  updateSelectizeInput(session, "sentiment", selected = "vulnerable")
    #  updateSelectizeInput(session, "color", selected = NULL)
    #  updateSelectizeInput(session, "muse", selected = NULL)
    #} else if(input$preset == "defiance") {
    #  updateSelectizeInput(session, "sentiment", selected = "defiant")
    #  updateSelectizeInput(session, "color", selected = NULL)
    #  updateSelectizeInput(session, "muse", selected = NULL)
    #} else if(input$preset == "healing") {
    #  updateSelectizeInput(session, "message", selected = c("moving on", "healing"))
    #  updateSelectizeInput(session, "sentiment", selected = NULL)
    #  updateSelectizeInput(session, "color", selected = NULL)
    #  updateSelectizeInput(session, "muse", selected = NULL)
    #} else if(input$preset == "revenge") {
    #  updateSelectizeInput(session, "message", selected = c("revenge", "vengeful"))
    #  updateSelectizeInput(session, "sentiment", selected = "vengeful")
    #  updateSelectizeInput(session, "color", selected = NULL)
    #  updateSelectizeInput(session, "muse", selected = NULL)
    #} else if(input$preset == "exes") {
    #  updateSelectizeInput(session, "muse", 
     #                      selected = c("Jake Gyllenhaal", "Joe Jonas", "Harry Styles", 
    #                                    "John Mayer", "Tom Hiddlestone", "Calvin Harris",
    #                                    "Matty Healey", "Joe Alwyin"))
    #  updateSelectizeInput(session, "sentiment", selected = NULL)
    #  updateSelectizeInput(session, "color", selected = NULL)
    } else if(input$preset == "heartbreak") {
      updateSelectizeInput(session, "sentiment", selected = "heartbroken")
      updateSelectizeInput(session, "message", selected = "breakup")
      updateSelectizeInput(session, "color", selected = NULL)
      updateSelectizeInput(session, "muse", selected = NULL)
    } else if(input$preset == "pinkdress") {
      updateSelectizeInput(session, "sentiment", selected = NULL)
      updateSelectizeInput(session, "message", selected = NULL)
      updateSelectizeInput(session, "color", selected = NULL)
      updateSelectizeInput(session, "muse", selected = NULL)
      updateSelectInput(session, "dress_filter", selected = "Pink")
    }
  })
  
  # Clear button to reset all filters - alternative approach
  observeEvent(input$clear, {
    # Reset all inputs
    session$sendInputMessage("sentiment", list(value = NULL))
    session$sendInputMessage("message", list(value = NULL))
    session$sendInputMessage("muse", list(value = NULL))
    session$sendInputMessage("color", list(value = NULL))
    session$sendInputMessage("keywords", list(value = ""))
    session$sendInputMessage("dress_filter", list(value = ""))
    session$sendInputMessage("color_match", list(value = "exact"))
    session$sendInputMessage("match_type", list(value = "any"))
    session$sendInputMessage("preset", list(value = ""))
    
    # Reset emoji slider to middle position
    # updateSliderInput(session, "emoji_slider", value = 3)
    
    # Force UI refresh using shinyjs
    runjs("
      // Reset selectize inputs
      if ($('#sentiment')[0] && $('#sentiment')[0].selectize) { $('#sentiment')[0].selectize.clear(); }
      if ($('#message')[0] && $('#message')[0].selectize) { $('#message')[0].selectize.clear(); }
      if ($('#muse')[0] && $('#muse')[0].selectize) { $('#muse')[0].selectize.clear(); }
      if ($('#color')[0] && $('#color')[0].selectize) { $('#color')[0].selectize.clear(); }
    ")
    
    # Show notification
    showNotification("All filters have been cleared!", type = "message")
  })
  
 
  
  # Reactive expression for filtered data
  filtered_data <- eventReactive(input$generate, {
    # Start with all data
    result <- allSongsMetadata
    
    # Create a list to track which filters have been applied
    applied_filters <- list()
    
    # If emoji sentiment filter is selected, apply it first
    if(!is.null(input$selected_emoji) && input$selected_emoji != "") {
      # Get all sentiments associated with this emoji
      emoji_sentiments_list <- sentiment_emoji_map[[input$selected_emoji]]
      
      if(input$match_type == "all") {
        result <- result %>% 
          filter(sentiment_MK %in% emoji_sentiments_list)
        applied_filters$emoji <- TRUE
      } else {
        applied_filters$emoji_sentiments <- emoji_sentiments_list
      }
    }
    
    # If dress filter is active, filter by songs performed in that dress
    if(!is.null(input$dress_filter) && input$dress_filter != "") {
      dress_songs <- surpriseSongsDressColours %>%
        filter(DressName == input$dress_filter) %>%
        pull(`Song title`)
      
      result <- result %>%
        filter(track_name %in% dress_songs)
      applied_filters$dress <- TRUE
    }
    
    # Parse keyword input
    keywords <- trimws(unlist(strsplit(input$keywords, ",")))
    if(length(keywords) > 0 && keywords[1] != "") {
      applied_filters$keywords <- keywords
    }
    
    # Apply the rest of the filters
    if(!is.null(input$sentiment) && length(input$sentiment) > 0) {
      applied_filters$sentiment <- input$sentiment
    }
    
    if(!is.null(input$message) && length(input$message) > 0) {
      applied_filters$message <- input$message
    }
    
    if(!is.null(input$muse) && length(input$muse) > 0) {
      applied_filters$muse <- input$muse
    }
    
    if(!is.null(input$color) && length(input$color) > 0) {
      applied_filters$color <- input$color
      applied_filters$color_match <- input$color_match
    }
    
    # Apply filters based on match type
    if(input$match_type == "all") {
      # Standard filters for sentiment, message, and muse
      if("sentiment" %in% names(applied_filters)) {
        result <- result %>% filter(sentiment_MK %in% applied_filters$sentiment)
      }
      
      if("message" %in% names(applied_filters)) {
        result <- result %>% filter(message_MK %in% applied_filters$message)
      }
      
      if("muse" %in% names(applied_filters)) {
        result <- filter_by_muses(result, applied_filters$muse)
      }
      
      # Modified color filtering for "match all" with partial matching option
      if("color" %in% names(applied_filters)) {
        # Get the lookup table
        lookup <- color_lookup()
        
        # For each selected color, find matching songs
        for(color_value in applied_filters$color) {
          # Choose the matching method based on the radio button
          if(applied_filters$color_match == "contains") {
            # Partial matching - find songs where color contains the search term
            matching_songs <- lookup %>%
              filter(grepl(color_value, colour, ignore.case = TRUE)) %>%
              distinct(song_id) %>%
              pull(song_id)
          } else {
            # Exact matching - original behavior
            matching_songs <- lookup %>%
              filter(colour == color_value) %>%
              distinct(song_id) %>%
              pull(song_id)
          }
          
          # Filter the result to only include songs with this color
          result <- result %>%
            filter(row_number() %in% matching_songs)
        }
      }
      
      if("keywords" %in% names(applied_filters)) {
        for(kw in applied_filters$keywords) {
          result <- result %>% filter(grepl(kw, keywords_MK, ignore.case = TRUE))
        }
      }
    } else {
      # Match ANY criteria (OR logic)
      if(length(names(applied_filters)) > 0) {
        filtered <- FALSE
        temp_result <- NULL
        
        # Apply standard filters
        if("sentiment" %in% names(applied_filters)) {
          temp <- allSongsMetadata %>% filter(sentiment_MK %in% applied_filters$sentiment)
          if(filtered) {
            temp_result <- rbind(temp_result, temp) %>% distinct()
          } else {
            temp_result <- temp
            filtered <- TRUE
          }
        }
        
        # Apply emoji-based sentiment filter for "match any" mode
        if("emoji_sentiments" %in% names(applied_filters)) {
          temp <- allSongsMetadata %>% filter(sentiment_MK %in% applied_filters$emoji_sentiments)
          if(filtered) {
            temp_result <- rbind(temp_result, temp) %>% distinct()
          } else {
            temp_result <- temp
            filtered <- TRUE
          }
        }
        
        if("message" %in% names(applied_filters)) {
          temp <- allSongsMetadata %>% filter(message_MK %in% applied_filters$message)
          if(filtered) {
            temp_result <- rbind(temp_result, temp) %>% distinct()
          } else {
            temp_result <- temp
            filtered <- TRUE
          }
        }
        
  if("muse" %in% names(applied_filters)) {
          temp <- filter_by_muses(allSongsMetadata, applied_filters$muse)
          if(filtered) {
            temp_result <- rbind(temp_result, temp) %>% distinct()
          } else {
            temp_result <- temp
            filtered <- TRUE
          }
        }
        
        # Modified color filtering for "match any" with partial matching option
        if("color" %in% names(applied_filters)) {
          # Get the lookup table
          lookup <- color_lookup()
          
          # Choose the matching method based on the radio button
          if(applied_filters$color_match == "contains") {
            # Create a pattern to match any of the selected colors (partial match)
            pattern <- paste(applied_filters$color, collapse="|")
            
            # Find all songs that contain any of the selected color terms
            matching_songs <- lookup %>%
              filter(grepl(pattern, colour, ignore.case = TRUE)) %>%
              distinct(song_id) %>%
              pull(song_id)
          } else {
            # Exact matching - original behavior
            matching_songs <- lookup %>%
              filter(colour %in% applied_filters$color) %>%
              distinct(song_id) %>%
              pull(song_id)
          }
          
          # Get songs that match the color criteria
          temp <- allSongsMetadata %>%
            filter(row_number() %in% matching_songs)
          
          if(filtered) {
            temp_result <- rbind(temp_result, temp) %>% distinct()
          } else {
            temp_result <- temp
            filtered <- TRUE
          }
        }
        
        if("keywords" %in% names(applied_filters)) {
          keyword_results <- NULL
          keyword_filtered <- FALSE
          
          for(kw in applied_filters$keywords) {
            temp <- allSongsMetadata %>% filter(grepl(kw, keywords_MK, ignore.case = TRUE))
            if(keyword_filtered) {
              keyword_results <- rbind(keyword_results, temp) %>% distinct()
            } else {
              keyword_results <- temp
              keyword_filtered <- TRUE
            }
          }
          
          if(filtered) {
            temp_result <- rbind(temp_result, keyword_results) %>% distinct()
          } else {
            temp_result <- keyword_results
            filtered <- TRUE
          }
        }
        
        # If dress filter is active, add it to the OR conditions
        if("dress" %in% names(applied_filters)) {
          dress_songs <- surpriseSongsDressColours %>%
            filter(DressName == input$dress_filter) %>%
            pull(`Song title`)
          
          temp <- allSongsMetadata %>% filter(track_name %in% dress_songs)
          
          if(filtered) {
            temp_result <- rbind(temp_result, temp) %>% distinct()
          } else {
            temp_result <- temp
            filtered <- TRUE
          }
        }
        
        if(filtered) {
          result <- temp_result
        }
      }
    }
    
    return(result)
  })
  
  # Render the playlist table
  output$playlist_table <- renderDT({
    req(filtered_data())
    
    # Select relevant columns for display
    playlist <- filtered_data() %>%
      select(
        `Song Title` = track_name,
        Sentiment = sentiment_MK,
        Message = message_MK,
        Keywords = keywords_MK,
        Muse = muse_MK,
        `Color Reference` = colour_MK
      )
    
    datatable(
      playlist,
      options = list(
        pageLength = 15,
        order = list(list(0, 'asc')),
        dom = 'ltipr'
      ),
      rownames = FALSE,
      filter = 'top',
      selection = 'multiple',
      class = 'cell-border stripe'
    )
  })
  
  # Download handler for the playlist
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("taylor-swift-playlist-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Song count value box
  output$songCount <- renderValueBox({
    req(filtered_data())
    count <- nrow(filtered_data())
    
    valueBox(
      count, 
      "Songs in Playlist", 
      icon = icon("headphones-simple", style = "color: rgba(255, 255, 255, 0.7);"), #cool code to make it 70% opaque :-)
      color = "teal" #fixed, 1989
    )
  })
  
  
  # Main sentiment value box
  output$mainSentiment <- renderValueBox({
    req(filtered_data())
    if(nrow(filtered_data()) > 0) {
      main_sentiment <- filtered_data() %>%
        count(sentiment_MK, sort = TRUE) %>%
        slice(1) %>%
        pull(sentiment_MK)
      
      main_sentiment <- ifelse(is.null(main_sentiment) || is.na(main_sentiment), 
                               "Various", main_sentiment)
      
      # Add emoji to the sentiment
      if (!is.null(main_sentiment) && main_sentiment %in% names(sentiment_to_emoji)) {
        emoji <- sentiment_to_emoji[[main_sentiment]]
        main_sentiment <- paste(emoji, main_sentiment)
      }
    } else {
      main_sentiment <- "N/A"
    }
    
    valueBox(
      main_sentiment, 
      "Most Common Sentiment", 
      icon = icon("heart-pulse", style = "color: rgba(255, 255, 255, 0.7);"),
      color = "black" # Fixed, Reputation
    )
  })
  
  # Main color value box
  output$mainColor <- renderValueBox({
    req(filtered_data())
    if(nrow(filtered_data()) > 0) {
      main_color <- filtered_data() %>%
        count(colour_MK, sort = TRUE) %>%
        slice(1) %>%
        pull(colour_MK)
      
      main_color <- ifelse(is.null(main_color) || is.na(main_color), 
                           "Various", main_color)
    } else {
      main_color <- "N/A"
    }
    
    valueBox(
      main_color, 
      "Most Mentioned Color", 
      icon = icon("brush", style = "color: rgba(255, 255, 255, 0.7);"),
      color = "fuchsia" ## Fixed, Lover
    )
  })
  
  # Main muse value box
  output$mainMuse <- renderValueBox({
    req(filtered_data())
    if(nrow(filtered_data()) > 0) {
      main_muse <- filtered_data() %>%
        filter(!is.na(muse_MK)) %>%
        count(muse_MK, sort = TRUE) %>%
        slice(1) %>%
        pull(muse_MK)
      
      main_muse <- ifelse(is.null(main_muse) || is.na(main_muse) || main_muse == "", 
                          "Various", main_muse)
    } else {
      main_muse <- "N/A"
    }
    
    valueBox(
      main_muse, 
      "Main Muse", 
      icon = icon("person-half-dress", style = "color: rgba(255, 255, 255, 0.7);"),
      color = "orange" ## Fixed, Evermore - there is no gray unfortunately for evermore
    )
  })
  
  # Main album value box
  output$mainAlbum <- renderValueBox({
    req(filtered_data())
    if(nrow(filtered_data()) > 0) {
      main_album <- filtered_data() %>%
        filter(!is.na(album_name)) %>%
        count(album_name, sort = TRUE) %>%
        slice(1) %>%
        pull(album_name)
      
      main_album <- ifelse(is.null(main_album) || is.na(main_album) || main_album == "", 
                          "Various", main_album)
    } else {
      main_album <- "N/A"
    }
    
    valueBox(
      main_album, 
      "Most Songs From This Album", 
      icon = icon("record-vinyl", style = "color: rgba(255, 255, 255, 0.7);"),
      color = "blue" ## Fixed, Evermore - there is no gray unfortunately for evermore
    )
  })
  
  # Main dress value box
  output$mainDress <- renderValueBox({
    req(filtered_data())
    
    if(nrow(filtered_data()) > 0) {
      # Get the song titles from the filtered data
      filtered_songs <- filtered_data()$track_name
      
      # Find matching dresses
      dress_counts <- surpriseSongsDressColours %>%
        filter(`Song title` %in% filtered_songs) %>%
        count(DressName, sort = TRUE)
      
      if(nrow(dress_counts) > 0) {
        main_dress <- dress_counts %>%
          slice(1) %>%
          pull(DressName)
      } else {
        main_dress <- "Various"
      }
    } else {
      main_dress <- "N/A"
    }
    
    valueBox(
      main_dress, 
      "Most Worn Surprise Songs Dress", 
      icon = icon("tshirt", style = "color: rgba(255, 255, 255, 0.7);"),  # Changed to more appropriate icon
      color = "yellow"
    )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
