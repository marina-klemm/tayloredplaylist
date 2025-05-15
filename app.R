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


## To link this code to your Spotify account, you need to go to the spotify
## developer website (developer.spotify.com), login, then go to dashboard 
## and add the ClientID and Client secret to the fields below.


# Spotify API Configuration
spotify_config <- list(
  client_id = "",  # Add your Spotify Client ID here
  client_secret = "",  # Add your Spotify Client Secret here
  redirect_uri = "http://localhost:3838/",  # Our Shiny app URL
  auth_url = "https://accounts.spotify.com/authorize",
  token_url = "https://accounts.spotify.com/api/token",
  scope = "playlist-modify-private playlist-modify-public user-read-private"
)

# First, update the sentiment mapping - we'll keep this structure but use text labels instead of emojis
mood_category_map <- list(
  "Mad" = c("angry", "resentful", "vengeful", "defiant"),
  "Glad" = c("happy", "playful", "joyful", "grateful", "admired", "excited", "peaceful", "warm"),
  "Down" = c("regretful", "hopeless", "vulnerable", "concerned", "anxious", "nostalgic", "melancholic", "frustrated", "sad", "desperate"),
  "Uplifting" = c("encouraging", "empowering", "free", "optimistic", "empathetic", "hopeful", "compassion", "brave", "resolute", "passionate"),
  "Sentimental" = c("tempted", "reflective", "conflicted", "pleading", "accepting", "reminiscent"),
  "Tender" = c("heartbroken", "lovesick", "grieving", "longing", "lonely")
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

# Function to preprocess muses for the dropdown menu
preprocess_muses <- function(data) {
  # Extract the muse_MK column
  muses <- data$muse_MK
  
  # Remove NA values
  muses <- muses[!is.na(muses)]
  
  # First split by semicolons and trim whitespace
  split_by_semicolon <- unlist(strsplit(muses, ";"))
  
  # Then split each element by forward slash
  split_by_slash <- unlist(strsplit(split_by_semicolon, "/"))
  
  # Also split by comma:
  split_by_comma <- unlist(strsplit(split_by_slash, ","))
  
  # Clean up whitespace
  clean_muses <- trimws(split_by_comma)
  
  # Get unique values and sort them
  unique_muses <- sort(unique(clean_muses))
  
  return(unique_muses)
}

# Process data for UI elements
all_unique_colors <- preprocess_colors(allSongsMetadata)
all_unique_muses <- preprocess_muses(allSongsMetadata)

# Function to create a Spotify authorization URL
create_spotify_auth_url <- function(config) {
  params <- list(
    client_id = config$client_id,
    response_type = "code",
    redirect_uri = config$redirect_uri,
    scope = config$scope,
    show_dialog = "true"
  )
  
  query <- paste0(names(params), "=", URLencode(unlist(params)), collapse = "&")
  paste0(config$auth_url, "?", query)
}

# Function to exchange auth code for access token
get_spotify_token <- function(code, config) {
  response <- POST(
    url = config$token_url,
    authenticate(config$client_id, config$client_secret),
    body = list(
      grant_type = "authorization_code",
      code = code,
      redirect_uri = config$redirect_uri
    ),
    encode = "form"
  )
  
  if (http_status(response)$category == "Success") {
    content(response, "parsed")
  } else {
    NULL
  }
}

# Function to create a Spotify playlist
create_spotify_playlist <- function(token, user_id, playlist_name, tracks, description = NULL) {
  # Create empty playlist
  playlist_response <- POST(
    url = paste0("https://api.spotify.com/v1/users/", user_id, "/playlists"),
    add_headers(
      Authorization = paste("Bearer", token),
      "Content-Type" = "application/json"
    ),
    body = toJSON(list(
      name = playlist_name,
      public = FALSE,
      description = description
    ), auto_unbox = TRUE)
  )
  
  if (http_status(playlist_response)$category != "Success") {
    return(NULL)
  }
  
  playlist_data <- content(playlist_response, "parsed")
  playlist_id <- playlist_data$id
  
  # Add tracks to playlist
  if (length(tracks) > 0) {
    # Spotify allows max 100 tracks per request
    chunk_size <- 100
    track_uris <- tracks
    
    for (i in seq(1, length(track_uris), chunk_size)) {
      end_idx <- min(i + chunk_size - 1, length(track_uris))
      chunk <- track_uris[i:end_idx]
      
      tracks_response <- POST(
        url = paste0("https://api.spotify.com/v1/playlists/", playlist_id, "/tracks"),
        add_headers(
          Authorization = paste("Bearer", token),
          "Content-Type" = "application/json"
        ),
        body = toJSON(list(
          uris = chunk
        ), auto_unbox = FALSE)
      )
      
      if (http_status(tracks_response)$category != "Success") {
        return(NULL)
      }
    }
  }
  
  return(playlist_id)
}

# Function to search for tracks on Spotify
search_spotify_tracks <- function(token, tracks) {
  track_uris <- list()
  
  for (track in tracks) {
    # Search for the track by name and artist
    search_query <- paste0("track:", track, " artist:Taylor Swift")
    
    search_response <- GET(
      url = "https://api.spotify.com/v1/search",
      add_headers(Authorization = paste("Bearer", token)),
      query = list(
        q = search_query,
        type = "track",
        limit = 1  # Just get the top result
      )
    )
    
    if (http_status(search_response)$category == "Success") {
      search_data <- content(search_response, "parsed")
      
      if (length(search_data$tracks$items) > 0) {
        track_uri <- search_data$tracks$items[[1]]$uri
        track_uris <- c(track_uris, track_uri)
      }
    }
    
    # Avoid hitting rate limits
    Sys.sleep(0.1)
  }
  
  return(unlist(track_uris))
}

# Function to get user profile info
get_spotify_user <- function(token) {
  response <- GET(
    url = "https://api.spotify.com/v1/me",
    add_headers(Authorization = paste("Bearer", token))
  )
  
  if (http_status(response)$category == "Success") {
    content(response, "parsed")
  } else {
    NULL
  }
}

# Modified UI - removing sidebar and collapsing to single page
ui <- dashboardPage(
  skin = "black", 
  dashboardHeader(title = "Taylor Swift Playlist Generator"),
  # Remove the sidebar entirely
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    useShinyjs(),  # Initialize shinyjs
    tags$head(
      # Add necessary JavaScript for Spotify callback handling
      tags$script("
        // Function to get URL parameters
        function getUrlParameter(name) {
          name = name.replace(/[\\[]/, '\\\\[').replace(/[\\]]/, '\\\\]');
          var regex = new RegExp('[\\\\?&]' + name + '=([^&#]*)');
          var results = regex.exec(location.search);
          return results === null ? '' : decodeURIComponent(results[1].replace(/\\+/g, ' '));
        };
        
        // Check if we have a code parameter (Spotify callback)
        $(document).ready(function() {
          var code = getUrlParameter('code');
          if (code) {
            // If we have a code, tell Shiny about it
            Shiny.setInputValue('spotify_code', code);
          }
        });
        
        // Function to remove URL parameters
        Shiny.addCustomMessageHandler('removeUrlParams', function(message) {
          if (window.history && window.history.replaceState) {
            var newUrl = window.location.href.split('?')[0];
            window.history.replaceState({}, document.title, newUrl);
          }
        });
      ")
    ),
    
    # Add tabs directly in the dashboard body
    tabsetPanel(
      id = "main_tabs",
      tabPanel(
        "Generate Playlist",
        fluidRow(
          box(
            title = "Filter Options", status = "primary", solidHeader = TRUE, width = 12, #primary: blue
            column(width = 3,
                   selectizeInput("sentiment", "Sentiments", 
                                  choices = sort(unique(allSongsMetadata$sentiment_MK)),
                                  multiple = TRUE,
                                  options = list(placeholder = "Select sentiment(s)")
                   ),
                   selectizeInput("message", "Message", 
                                  choices = sort(unique(allSongsMetadata$message_MK)),
                                  multiple = TRUE,
                                  options = list(placeholder = "Select message(s)")
                   )
            ),
            column(width = 3,
                   selectizeInput("muse", "Muse", 
                                  choices = sort(unique(allSongsMetadata$muse_MK)),
                                  multiple = TRUE,
                                  options = list(placeholder = "Select muse(s)")
                   ),
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
                   # Add dress filter dropdown back
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
                   # Preset playlists dropdown
                   # Add the new mood-based presets to the choices in the selectInput for presets:
                   selectInput("preset", "Preset Playlists", 
                               choices = c(
                                 "Choose a preset (optional)" = "",
                                 # Mood-based presets
                                 "Taylor is Mad" = "mood_mad",
                                 "Feeling Glad with Taytay" = "mood_glad",
                                 "Taylor is Down Bad and So Am I" = "mood_down",
                                 "Only Taylor Could Uplift Me Today" = "mood_uplifting",
                                 "Feeling Sentimental with Taylor Swift" = "mood_sentimental",
                                 "Taylor Understands My Heartbreak" = "mood_tender",
                                 # Original presets
                                 "Golden Hour" = "golden",
                                 "Taylor's Blues" = "blues",
                                 "Black & White Story" = "blackwhite",
                                 "Playfully Taylor" = "playful",
                                 "Vulnerability Hour" = "vulnerable",
                                 "Taylor's Defiance" = "defiance",
                                 "Healing with Taylor" = "healing",
                                 "Taylor Sings About Revenge" = "revenge",
                                 "Famous Ex Files" = "exes",
                                 "Taylor Understands Heartbreak" = "heartbreak"
                               )
                   ),
                   div(
                     style = "display: flex; justify-content: space-between;",
                     actionButton("generate", "Generate Playlist", 
                                  icon = icon("play"), 
                                  style = "color: #fff; background-color: #b8cfb3;"), ## Debut color
                     actionButton("clear", "Clear Filters", 
                                  icon = icon("eraser"), 
                                  style = "color: #000000; background-color: #ffea9f;") ## Fearless color
                   ),
                   br(),
                   div(
                     style = "display: flex; justify-content: space-between;",
                     downloadButton("downloadData", "Download CSV", style = "color: #fff; background-color: #8000ff;"), ## Speak now color 
                     actionButton("createSpotify", "Create Spotify Playlist", 
                                  icon = icon("spotify"), 
                                  style = "color: #fff; background-color: #D34C58;") ## Red color
                   ),
                   # Display Spotify connection status
                   conditionalPanel(
                     condition = "output.spotify_connected",
                     tags$div(
                       style = "margin-top: 10px; text-align: center;",
                       tags$span(
                         icon("check-circle"), 
                         "Connected to Spotify", 
                         style = "color: #D34C58; font-weight: bold;" ## Red color
                       )
                     )
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
            valueBoxOutput("songCount", width = 3),
            valueBoxOutput("mainSentiment", width = 3),
            valueBoxOutput("mainColor", width = 3),
            valueBoxOutput("mainMuse", width = 3)
          )
        )
      ),
      tabPanel(
        "About",
        box(
          title = "About This App <3", status = "primary", solidHeader = TRUE, width = 12, 
          HTML("<h3>Taylor Swift Playlist Generator</h3>
                <p>This app was built by a long-time Swiftie to other Swifties with the intent of creating custom playlists based on all the lore behind her songs.</p>
                <p>The dataset includes information about each song's:</p>
                <ul>
                  <li>Sentiment: 43 unique sentiments to choose from (e.g., playful, vulnerable, hopeless)</li>
                  <li>Message: the main message of the song (e.g., forbidden love, long distance relationship, second chances)</li>
                  <li>Keywords: a braindump of all the lore involved in that song (e.g., hindsight, missed chances, LBGT+ rights)</li>
                  <li>Muses: a list of all confirmed and alleged people that inspired her songs (including enemies)</li>
                  <li>Color references in her lyrics</li>
                  <li>Dresses she was wearing while performing these songs in the Accoustic Set of the Eras Tour</li>
                </ul>
                
                <p>Use the filters to create your perfect Taylor Swift playlist for any mood or occasion!</p>
                <p>Data compiled from Taylor Swift's lyrics, fan analyses, and performances.</p>
                <h4>Spotify Integration</h4>
                <p>To use the Spotify integration feature:</p>
                <ol>
                  <li>Click 'Create Spotify Playlist' after generating a playlist</li>
                  <li>Connect to your Spotify account when prompted</li>
                  <li>Name your playlist and add a description</li>
                  <li>The app will search for each song on Spotify and add them to your playlist</li>
                </ol>")
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
      # Add a song ID
      mutate(song_id = row_number()) %>%
      # Split the colors into separate rows
      separate_rows(colour_MK, sep = ";") %>%
      # Clean up any whitespace
      mutate(colour = trimws(colour_MK)) %>%
      # Remove any empty strings
      filter(colour != "")
  })
  
  # Store Spotify access token and user info
  spotify_token <- reactiveVal(NULL)
  spotify_user <- reactiveVal(NULL)
  
  
  # Output to indicate if Spotify is connected
  output$spotify_connected <- reactive({
    !is.null(spotify_token())
  })
  outputOptions(output, "spotify_connected", suspendWhenHidden = FALSE)
  
  # Handle Spotify authentication flow using JavaScript callback
  observeEvent(input$spotify_code, {
    # Exchange authorization code for access token
    token_data <- get_spotify_token(input$spotify_code, spotify_config)
    
    if (!is.null(token_data)) {
      # Store the access token
      spotify_token(token_data$access_token)
      
      # Get user profile info
      user_data <- get_spotify_user(token_data$access_token)
      spotify_user(user_data)
      
      # Show success notification
      showNotification("Successfully connected to Spotify!", type = "message")
      
      # Remove code from URL to avoid reprocessing
      session$sendCustomMessage("removeUrlParams", list())
    } else {
      showNotification("Failed to connect to Spotify. Please try again.", type = "error")
    }
  })
  
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
    } else if(input$preset == "mood_sentimental") {
      updateSelectizeInput(session, "sentiment", selected = mood_category_map[["Sentimental"]])
      updateSelectizeInput(session, "color", selected = NULL)
      updateSelectizeInput(session, "muse", selected = NULL)
      updateSelectizeInput(session, "message", selected = NULL)
    } else if(input$preset == "mood_tender") {
      updateSelectizeInput(session, "sentiment", selected = mood_category_map[["Tender"]])
      updateSelectizeInput(session, "color", selected = NULL)
      updateSelectizeInput(session, "muse", selected = NULL)
      updateSelectizeInput(session, "message", selected = NULL)
    } 
    # Original presets remain unchanged
    else if(input$preset == "golden") {
      updateSelectizeInput(session, "color", selected = "golden")
      updateSelectizeInput(session, "sentiment", selected = NULL)
      updateSelectizeInput(session, "muse", selected = NULL)
    } else if(input$preset == "blues") {
      updateSelectizeInput(session, "color", selected = c("blue", "blues", "ocean blue", "deep blue"))
      updateSelectizeInput(session, "sentiment", selected = NULL)
      updateSelectizeInput(session, "muse", selected = NULL)
    } else if(input$preset == "blackwhite") {
      updateSelectizeInput(session, "color", selected = c("black", "white", "black and white"))
      updateSelectizeInput(session, "sentiment", selected = NULL)
      updateSelectizeInput(session, "muse", selected = NULL)
    } else if(input$preset == "playful") {
      updateSelectizeInput(session, "sentiment", selected = "playful")
      updateSelectizeInput(session, "color", selected = NULL)
      updateSelectizeInput(session, "muse", selected = NULL)
    } else if(input$preset == "vulnerable") {
      updateSelectizeInput(session, "sentiment", selected = "vulnerable")
      updateSelectizeInput(session, "color", selected = NULL)
      updateSelectizeInput(session, "muse", selected = NULL)
    } else if(input$preset == "defiance") {
      updateSelectizeInput(session, "sentiment", selected = "defiant")
      updateSelectizeInput(session, "color", selected = NULL)
      updateSelectizeInput(session, "muse", selected = NULL)
    } else if(input$preset == "healing") {
      updateSelectizeInput(session, "message", selected = c("moving on", "healing"))
      updateSelectizeInput(session, "sentiment", selected = NULL)
      updateSelectizeInput(session, "color", selected = NULL)
      updateSelectizeInput(session, "muse", selected = NULL)
    } else if(input$preset == "revenge") {
      updateSelectizeInput(session, "message", selected = c("revenge", "vengeful"))
      updateSelectizeInput(session, "sentiment", selected = "vengeful")
      updateSelectizeInput(session, "color", selected = NULL)
      updateSelectizeInput(session, "muse", selected = NULL)
    } else if(input$preset == "exes") {
      updateSelectizeInput(session, "muse", 
                           selected = c("Jake Gyllenhaal", "Joe Jonas", "Harry Styles", 
                                        "John Mayer", "Tom Hiddlestone", "Calvin Harris",
                                        "Matty Healey"))
      updateSelectizeInput(session, "sentiment", selected = NULL)
      updateSelectizeInput(session, "color", selected = NULL)
    } else if(input$preset == "heartbreak") {
      updateSelectizeInput(session, "sentiment", selected = "heartbroken")
      updateSelectizeInput(session, "message", selected = "breakup")
      updateSelectizeInput(session, "color", selected = NULL)
      updateSelectizeInput(session, "muse", selected = NULL)
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
  
  # Create Spotify playlist button handler
  observeEvent(input$createSpotify, {
    req(filtered_data())
    
    if (is.null(spotify_token())) {
      # If not authenticated, redirect to Spotify authorization
      auth_url <- create_spotify_auth_url(spotify_config)
      
      showModal(modalDialog(
        title = "Connect to Spotify",
        "You need to connect to your Spotify account to create a playlist.",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("auth_spotify", "Connect to Spotify", 
                       icon = icon("spotify"),
                       style = "color: #fff; background-color: #ffea9f;") # Fearless color
        )
      ))
      
      observeEvent(input$auth_spotify, {
        removeModal()
        # Open Spotify authorization URL
        showNotification("Opening Spotify login...", type = "message")
        # Use the browser's JavaScript to open the Spotify auth URL
        runjs(sprintf("window.location.href = '%s';", auth_url))
      }, once = TRUE)
    } else {
      # Already authenticated, create the playlist
      showModal(modalDialog(
        title = "Create Spotify Playlist",
        textInput("playlist_name", "Playlist Name", 
                  value = paste0("Taylor Swift - ", 
                                 format(Sys.time(), "%b %d, %Y"))),
        textAreaInput("playlist_desc", "Description", 
                      value = "Custom Taylor Swift playlist created with the Playlist Generator app"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_create", "Create", 
                       icon = icon("check"),
                       style = "color: #fff; background-color: #2C2E52;") ## Midnights color
        )
      ))
    }
  })
  
  # Handle playlist creation confirmation
  observeEvent(input$confirm_create, {
    req(spotify_token(), spotify_user(), filtered_data())
    
    withProgress(message = 'Creating Spotify playlist...', value = 0, {
      # Get track names to search for
      tracks <- filtered_data()$track_name
      
      # Update progress
      incProgress(0.2, detail = "Finding tracks on Spotify...")
      
      # Search for tracks on Spotify
      track_uris <- search_spotify_tracks(spotify_token(), tracks)
      
      if (length(track_uris) > 0) {
        # Update progress
        incProgress(0.5, detail = "Creating playlist...")
        
        # Create the playlist
        playlist_id <- create_spotify_playlist(
          token = spotify_token(),
          user_id = spotify_user()$id,
          playlist_name = input$playlist_name,
          description = input$playlist_desc,
          tracks = track_uris
        )
        
        if (!is.null(playlist_id)) {
          # Update progress
          incProgress(0.3, detail = "Success!")
          
          # Show success notification with link to playlist
          playlist_url <- paste0("https://open.spotify.com/playlist/", playlist_id)
          
          showModal(modalDialog(
            title = "Playlist Created!",
            HTML(paste0(
              "<p>Your playlist '<strong>", input$playlist_name, "</strong>' has been created successfully with ", 
              length(track_uris), " tracks out of ", length(tracks), " in your selection.</p>",
              "<p>Click the button below to open it in Spotify:</p>"
            )),
            footer = tagList(
              modalButton("Close"),
              actionButton("open_spotify", "Open in Spotify", 
                           icon = icon("spotify"),
                           style = "color: #fff; background-color: #cc621b;", ## Evermore color
                           onclick = sprintf("window.open('%s', '_blank')", playlist_url))
            )
          ))
        } else {
          showNotification("Failed to create playlist. Please try again.", type = "error")
        }
      } else {
        showNotification("No matching tracks found on Spotify.", type = "warning")
      }
    })
    
    removeModal()
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
        # For "match all" logic, this is the base filter
        result <- result %>% 
          filter(sentiment_MK %in% emoji_sentiments_list)
        applied_filters$emoji <- TRUE
      } else {
        # For "match any" logic, save this for later
        applied_filters$emoji_sentiments <- emoji_sentiments_list
      }
    }
    
    # If dress filter is active, filter by songs performed in that dress
    if(!is.null(input$dress_filter) && input$dress_filter != "") {
      # Get song titles from the surpriseSongsDressColours dataset
      dress_songs <- surpriseSongsDressColours %>%
        filter(DressName == input$dress_filter) %>%
        pull(`Song title`)
      
      # Filter the main dataset to only include these songs
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
        result <- result %>% filter(muse_MK %in% applied_filters$muse)
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
          temp <- allSongsMetadata %>% filter(muse_MK %in% applied_filters$muse)
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
      icon = icon("music"),
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
      "Primary Sentiment", 
      icon = icon("heart"),
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
      "Primary Color", 
      icon = icon("palette"),
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
      "Primary Muse", 
      icon = icon("user"),
      color = "orange" ## Fixed, Evermore - there is no gray unfortunately for evermore
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
