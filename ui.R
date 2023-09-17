library(janitor)
library(shinycssloaders)
library(shinymaterial)
library(rbokeh)

ui = shiny::htmlTemplate(
  # Index Page
  "www/index.html",
  
  # # City Selector
  # city_selector = selectInput(
  #   "city", 
  #   label = "Select City", 
  #   choices = d_clean$city %>% 
  #     unique(),
  #   selected = "Auckland"
  # ),
  
  
  # Selector for Time
  # time_selector = material_card(
  #   title = "",
  #   sliderInput(
  #     "time", 
  #     "Date",
  #     min(d_routes$request_time) %>% as.Date(), 
  #     max(d_routes$request_time) %>% as.Date(),
  #     value = max(d_routes$request_time) %>% as.Date(),
  #     step = 30,
  #     animate = animationOptions(
  #       playButton = HTML("<img src='images/icons/play-button.png' height='20' width='20'>"), 
  #       pauseButton = HTML("<img src='images/icons/pause-button.png' height='20' width='20'>")
  #     )
  #   )
  # ),
  
  # Leaflet map
  walk_score_map = leafletOutput(outputId = "walk_score_map") %>% 
    withSpinner(color="#0dc5c1"),
  
  population_proj_map = leafletOutput(outputId = "population_proj_map") %>% 
    withSpinner(color="#0dc5c1"),
  
  collision_map = leafletOutput(outputId = "collision_map") %>% 
    withSpinner(color="#0dc5c1"),

  
  crime_map = leafletOutput(outputId = "crime_map") %>% 
    withSpinner(color="#0dc5c1"),
  
  collision_plot = plotlyOutput(outputId = "collision_plot") %>% 
    withSpinner(color="#0dc5c1"),
  
  
  #DTOutput("walkReg")
  
  walk_plot = plotOutput(outputId = "walk_plot") %>% 
    withSpinner(color="#0dc5c1"),
  
  safe_transit_plot = plotOutput(outputId = "safe_transit_plot") %>% 
    withSpinner(color="#0dc5c1"),
  
  survey_question = selectInput("y_axis", "Survey Question", 
                                choices = c("How Safe Do You Feel Taking Public Transportation on a Scale of 1-10" = "safetransiting", 
                                            "Over the past year, has public safety in your city or town gotten better or gotten worse?”?" = "safety.city", 
                                            "How Safe Do You Feel In your Community on a Scale of 1-10" = "safecommunity")),
  
  age_range = sliderInput("age_range", "Age Range", 
                          min = min(gender_survey$Age, na.rm = TRUE),
                          max = max(gender_survey$Age, na.rm = TRUE),
                          value = c(min(gender_survey$Age, na.rm = TRUE), max(gender_survey$Age, na.rm = TRUE)),
                          width = "80%",  # Adjust the width as needed
                          step = 5),
  
  
  
  city_plot = plotOutput("city_plot") %>% 
    withSpinner(color="#0dc5c1"),
  
  map_1_button = actionButton("map_1_button", "Uniform"),
  
  map_2_button = actionButton("map_2_button", "Normal")
)