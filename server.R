
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    sever(html = disconnected, bg_color = "#3A4A9F", opacity = .92)
    ###--- REACTIVES -------------------------------
    selected_wda_sf <- reactive({
        sf <- wda_sf %>% 
            filter(wda == input$select_wda)
        return(sf)
    })
    
    selected_wda_centroid <- reactive({
        centroid <- centroids %>% 
            filter(wda == input$select_wda) %>% 
            select(lat, lon)
        return(centroid)
    })
    
    selected_wdacounties_sf <- reactive({
        sf <- counties %>% 
            filter(wda == input$select_wda)
        return(sf)
    })
    
    ###--- LANDING PAGE ----------------------------
    ## * Content -----
    ## map --------
    output$home_map <- renderLeaflet({
        pal <- colorFactor(palette = c("#2a366c", "#f26852", "#5f6fc1", "#3ead92"), wda_sf$color_category)
        leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 6, maxZoom = 6)) %>%
            setView(-99.9018, 30.9686, zoom = 6) %>% 
            addPolygons(stroke = F,
                        fill = T,
                        fillOpacity = 01,
                        fillColor = ~pal(color_category),
                        group = "wdas",
                        data = wda_sf) %>%
            addPolygons(color = "black",
                        stroke = T,
                        weight = 1,
                        fill = F,
                        group = "counties",
                        data = counties) %>%
            addPolygons(stroke = T, 
                        weight = 2,
                        color = "black",
                        opacity = 1,
                        fill = T,
                        fillOpacity = 0,
                        label = ~wda,
                        group = "highlight",
                        layerId = ~wda,
                        highlightOptions = highlightOptions(color="white",
                                                            opacity=1, weight=3, bringToFront=T),
                        data = wda_sf) %>% 
            setMapWidgetStyle(list(background= "white")) %>% 
            htmlwidgets::onRender("function(el, x) { 
               map = this
               map.dragging.disable();
               }")
    })
    
    ## * Observes -----
    ## respond to map click ----
    # update select input and change page
    observeEvent(input$home_map_shape_click$id, {
        updateSelectizeInput(session, 
                             inputId = "select_wda", 
                             label = "Choose a different WDA: ",
                             choices = unique(crosswalk$wda),
                             selected = input$home_map_shape_click$id)
        updateNavbarPage(session = session, inputId = "tab_being_displayed", selected = "WDA")
    })
    
    ###--- WDA PAGE ----------------------------
    ## * Well panel --------
    
    # print wda name
    output$wda_name <- renderUI({
        text <- HTML(paste0(input$select_wda), " WDA")
        return(text)
        })
    
    # mini map
    output$wda_map <- renderLeaflet({
        leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 6, maxZoom = 6)) %>%
            setView(as.numeric(selected_wda_centroid()$lat), as.numeric(selected_wda_centroid()$lon), zoom = 6) %>% 
            addPolygons(stroke = F,
                        fill = T,
                        fillOpacity = 01,
                        fillColor = "#f26852",
                        group = "wdas",
                        data = selected_wda_sf()) %>%
            addPolygons(color = "black",
                        stroke = T,
                        weight = 1,
                        fill = T,
                        fillOpacity = 0,
                        group = "counties",
                        label = paste0(selected_wdacounties_sf()$county, " County"),
                        data = selected_wdacounties_sf()) %>% 
            setMapWidgetStyle(list(background= "transparent")) %>% 
            htmlwidgets::onRender("function(el, x) { 
               map = this
               map.dragging.disable();
               }")
    })
    
    # list of counties
    output$wda_counties <- renderUI({
        counties <- selected_wdacounties_sf() %>% 
            pull(county)
        text <- HTML(paste0(strong("Counties in ", input$select_wda, ":")),
                     paste(counties))
    })
    
    # section headers
    output$header_lwh <- renderUI({
        text <- HTML(paste0("Living wage households"))
        return(text)
    })
    output$header_waa <- renderUI({
        text <- HTML(paste0("Trends in working age adults"))
        return(text)
    })
    output$header_idj <- renderUI({
        text <- HTML(paste0("Trends in in-demand jobs"))
        return(text)
    })
    output$header_aj <- renderUI({
        text <- HTML(paste0("Attractive jobs"))
        return(text)
    })
    output$header_lwj <- renderUI({
        text <- HTML(paste0("Living wage jobs"))
        return(text)
    })
    output$header_edu <- renderUI({
        text <- HTML(paste0("Employment by education"))
        return(text)
    })


    ## * Content -----
    ## 1. living wage households --------
    ## 2. trends in working age adults --------
    filter_waa <- reactive({
        df <- waa %>% 
            filter(wda == input$select_wda) %>% 
            pivot_longer(total:nh_other_female)
        if (input$waa_plot_gender_select == F & input$waa_plot_race_select == F) {
            df <- filter(df, name == "total")
        }
        else if (input$waa_plot_gender_select == T & input$waa_plot_race_select == F) {
            df <- filter(df, name %in% c("total_female", "total_male"))
        }
        else if (input$waa_plot_gender_select == F & input$waa_plot_race_select == T) {
            df <- filter(df, name %in% c("nh_white_total", "nh_black_total", "hispanic_total", "nh_asian_total", "nh_other_total"))
        }
        else if (input$waa_plot_gender_select == T & input$waa_plot_race_select == T) {
            df <- filter(df, !name %in% c("nh_white_total", "nh_black_total", "hispanic_total", "nh_asian_total", "nh_other_total", "total", "total_female", "total_male"))
        }
        return(df)
        })
    
    output$waa_plot <- renderHighchart({
        filter_waa() %>% 
            hchart(type = "line",hcaes(x = year, y = value, group = name)) %>% 
            hc_yAxis(title = list(text = "Number of working age adults")) %>% 
            hc_title(text = "Projected Number of Working Age Adults Through 2036") %>% 
            hc_add_theme(ja_hc_theme())
    })
    ## 3. trends in in-demand jobs --------
    ## 4. attractive jobs --------
    ## 5. living wage jobs --------
    ## 6. employment by education --------
})
