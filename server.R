

server <- function(input, output) {
  
  output$walk_score_map <- renderLeaflet({
    pal = colorNumeric("magma",domain = census_data$walk_score)
    census_data %>%
      leaflet() %>%
      addProviderTiles("CartoDB") %>%
      addPolygons(weight = 1,
                  color = ~pal(census_data$walk_score),
                  stroke = FALSE,
                  fillOpacity = .75,
                  label = ~census_data$walk_score) %>%
      addLegend_decreasing("bottomright", pal = pal, values = ~walk_score,
                           title = "Walk Score®",
                           opacity = 0.8,
                           decreasing = TRUE)
  })
  
  v <- reactiveValues(data = NULL)
  
  observeEvent(input$map_1_button, {
    v$data <- NULL
  }) 
  
  observeEvent(input$map_2_button, {
    v$data <- runif(100)
  })
  
  output$collision_map <- renderLeaflet({
      if (is.null(v$data)) {
        
        census_data$relative_crash_count = census_data$relative_crash_count*5000
        pal = colorNumeric("magma",domain = census_data$relative_crash_count)
        census_data %>%
          leaflet() %>%
          addProviderTiles("CartoDB") %>%
          addPolygons(weight = 1,
                      color = ~pal(census_data$relative_crash_count),
                      stroke = FALSE,
                      fillOpacity = .75,
                      label = ~census_data$relative_crash_count) %>%
          addLegend_decreasing("bottomright", pal = pal, values = ~relative_crash_count,
                               title = "Collisions",
                               opacity = 0.8,
                               decreasing = TRUE)
        
      } else {
        
        crash %>%
          dplyr::filter(YEAR %in% c(2022)) %>%
          filter(y != 0.00000)%>%
          leaflet() %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          addCircleMarkers(lng = ~as.numeric(x), lat = ~as.numeric(y), radius = 2,
                           clusterOptions = markerClusterOptions())
      }
    })
  
  output$population_proj_map <- renderLeaflet({
    pal = colorNumeric("magma",domain = census_data$pred_change_pop40)
    census_data %>%
      leaflet() %>%
      addProviderTiles("CartoDB") %>%
      addPolygons(weight = 1,
                  color = ~pal(census_data$pred_change_pop40),
                  stroke = FALSE,
                  fillOpacity = .75,
                  label = ~census_data$walk_score) %>%
      addLegend_decreasing("bottomright", pal = pal, values = ~pred_change_pop40,
                           title = "% Growth",
                           opacity = 0.8,
                           decreasing = TRUE)
  })
  
  output$crime_map <- renderLeaflet({
    crime %>% 
      dplyr::filter(y != 0.00000)%>%
      dplyr::filter(YEAR %in% c(2022)) %>%
      leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addHeatmap(lng=~as.numeric(x),
                 lat=~as.numeric(y),
                 radius = 8)
  })
  
  output$collision_plot <- renderPlotly({
    CollisionCount %>%
      plot_ly(x = ~ YEAR, y = ~per_capita, color = ~TYPE,
              hoverinfo="text",
              text = ~paste("<b>", "Per Capita:", "</b>", per_capita, "<br>",
                            "<b>", "Raw Count:", "</b>", n)) %>%
      add_bars() %>%
      layout(barmode = "stack")
  })

  output$safe_transit_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Gender, y = safetransiting, fill = Gender)) +
      geom_boxplot() +
      scale_fill_manual(values=c("#b3cde0", "#fbb4ae")) +
      facet_wrap(~ city, scales = "free_x") +
      labs(x = "", y = "How Safe Do You Feel In Your City? 1-10") +
      theme(axis.text.x = element_blank(),  # Removed x-axis text
            axis.title.y = element_text(size = 16),
            legend.position = "none",
            panel.background = element_rect(fill = "white"),
            strip.text = element_text(size = 16),
            panel.grid = element_blank())  # Removed gridlines
  })
  
  output$walk_plot <- renderPlot({
    ggplot(data = clean_cen, mapping = aes(x = walk_score, y = walk_prop)) +
      geom_point(aes(color = walk_score > 75)) +
      scale_color_manual(values = c("#2E343F","#3876F2"), labels = c("Below 75", "Above 75")) +
      geom_smooth(se = FALSE, color = "#949494") +
      theme_classic() +
      theme(legend.position = "none") +
      xlab("Walk Score") +
      ylab("Proportion of Walking Population (%)") 
  })
  
  filtered_data <- reactive({
    gender_survey %>%
      filter(Age >= input$age_range[1], Age <= input$age_range[2], 
             !is.na(!!sym(input$y_axis)), 
             !!sym(input$y_axis) != "Don't know",
             !!sym(input$y_axis) != "Prefer not to say",
             !is.na(city))
  })
  
  output$city_plot <- renderPlot({
    if (input$y_axis == "safety.city") {
      ggplot(filtered_data(), aes(x = !!sym(input$y_axis), fill = city)) +
        geom_bar(position = "dodge") +
        scale_fill_brewer(palette = "Pastel1") +
        labs(x = "", y = "Count") +
        theme(plot.background = element_rect(fill = "white"),
              legend.position = "none",
              axis.title = element_text(size = 14),
              axis.text.x = element_blank(),  # Removed x-axis text
              panel.grid = element_blank() +  # Removed gridlines
                theme_classic())
    } else {
      ggplot(filtered_data(), aes(x = city, y = !!sym(input$y_axis), fill = city)) +
        geom_boxplot() +
        scale_fill_brewer(palette = "Pastel1") +
        labs(x = "", y = "How Safe Do You Feel? 1-10") +
        theme_minimal() +
        theme(plot.background = element_rect(fill = "white"),
              axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
              axis.title = element_text(size = 14),
              legend.text = element_text(size = 12),
              legend.position = "none",
              panel.grid = element_blank())  # Removed gridlines
    }
  })
  
}
