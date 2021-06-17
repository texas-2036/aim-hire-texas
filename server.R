
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    sever(html = disconnected, bg_color = "#3A4A9F", opacity = .92)
    
    ###--- LANDING PAGE ----------------------------
    ## * Content -----
    ## map --------
    output$home_map <- renderLeaflet({
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
                             label = "select a wda",
                             choices = unique(wda_sf$wda),
                             selected = input$home_map_shape_click$id)
        updateNavbarPage(session = session, inputId = "tab_being_displayed", selected = "WDA")
    })
    
    ###--- WDA PAGE ----------------------------
    ## * Content -----
    ## 1. living wage households --------
    ## 2. trends in working age adults --------
    ## 3. trends in in-demand jobs --------
    ## 4. attractive jobs --------
    ## 5. living wage jobs --------
    ## 6. employment by education --------
    ## * Content -----
})
