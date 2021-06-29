
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    sever(html = disconnected, bg_color = "#3A4A9F", opacity = .92)
    ###--- REACTIVES -------------------------------
    selected_wda_sf <- reactive({
        sf <- wda_sf %>% 
            filter(wda == input$select_wda)
        return(sf)
    })
    
    # selected_wda_centroid <- reactive({
    #     centroid <- centroids %>% 
    #         filter(wda == input$select_wda) %>% 
    #         select(lat, lon)
    #     return(centroid)
    # })
    
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
                        fillOpacity = 1,
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
            setMapWidgetStyle(list(background= "transparent")) %>% 
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
        updateNavbarPage(session = session, inputId = "tab_being_displayed", selected = "Workforce Development Areas")
    })
    
    ###--- WDA PAGE ----------------------------
    ## * Well panel --------
    
    # print wda name
    output$wda_name <- renderUI({
        text <- HTML(paste0(input$select_wda), "WDA")
        return(text)
        })
    
    # mini map
    output$mini_map <- renderLeaflet({
        pal <- colorFactor(palette = c("#2a366c", "#f26852", "#5f6fc1", "#3ead92"), wda_sf$color_category)
        leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 5, maxZoom = 5)) %>%
            setView(-99.9018, 30.9686, zoom = 6) %>% 
            addPolygons(stroke = F,
                        fill = T,
                        fillOpacity = 0.5,
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
                        weight = 1,
                        color = "black",
                        opacity = 1,
                        fill = T,
                        fillOpacity = 0,
                        data = wda_sf) %>%
            addPolygons(stroke = T,
                        weight = 1,
                        color = "black",
                        opacity = 1,
                        fill = T,
                        fillColor = ~pal(color_category),
                        fillOpacity = 1,
                        data = selected_wdacounties_sf()) %>% 
            addPolygons(stroke = T,
                        weight = 3,
                        color = "black",
                        opacity = 1,
                        fill = F,
                        data = selected_wda_sf()) %>% 
            setMapWidgetStyle(list(background= "transparent")) %>% 
            htmlwidgets::onRender("function(el, x) { 
               map = this
               map.dragging.disable();
               }")

    })
    
    # list of counties
    output$wda_counties <- renderUI({
        counties <- crosswalk %>% 
            filter(wda == input$select_wda) %>% 
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
    # reactives
    filter_waa <- reactive({
        df <- waa %>% 
            filter(wda == input$select_wda) %>% 
            select(wda, wda_number, year,
                   total,
                   "Male" = total_male, "Female" = total_female,
                   "White" = nh_white_total, "Black" = nh_black_total, "Hispanic" = hispanic_total, "Asian" = nh_asian_total, "Other" = nh_asian_total,
                   "White women" = nh_white_female, "Black women" = nh_black_female, "Hispanic women" = hispanic_female, "Asian women" = nh_asian_female, "Other women" = nh_asian_female,
                   "White men" = nh_white_male, "Black men" = nh_black_male, "Hispanic men" = hispanic_male, "Asian men" = nh_asian_male, "Other men" = nh_asian_male) %>% 
            pivot_longer(total:"Other men") %>% 
            mutate(name = factor(name, levels = c("total", "Male", "Female", 
                                                  "White", "Black", "Hispanic", "Asian", "Other",
                                                  "White women", "Black women", "Hispanic women", "Asian women", "Other women",
                                                  "White men", "Black men", "Hispanic men", "Asian men", "Other men"),
                                 ordered = T))
        if (input$waa_plot_gender_select == F & input$waa_plot_race_select == F) {
            df <- filter(df, name == "total")
        }
        else if (input$waa_plot_gender_select == T & input$waa_plot_race_select == F) {
            df <- filter(df, name %in% c("Female", "Male"))
        }
        else if (input$waa_plot_gender_select == F & input$waa_plot_race_select == T) {
            df <- filter(df, name %in% c("White", "Black", "Hispanic", "Asian", "Other"))
        }
        else if (input$waa_plot_gender_select == T & input$waa_plot_race_select == T) {
            df <- filter(df, name %in% c("White women", "Black women", "Hispanic women", "Asian women", "Other women",
                                          "White men", "Black men", "Hispanic men", "Asian men", "Other men"))
        }
        return(df)
        })
    
    filter_waa_vb <- reactive({
        df <- waa %>% 
            filter(wda == input$select_wda & year == 2036) 
        return(df)
    })
    
    observe(print(filter_waa()))
    
    ## Plots
    # line chart
    output$waa_plot <- renderHighchart({
        filter_waa() %>% 
            hchart(type = "line", hcaes(x = year, y = value, group = name)) %>% 
            hc_yAxis(title = list(text = "Number of working age adults")) %>% 
            hc_title(text = "Projected Number of Working Age Adults Through 2036") %>% 
            hc_add_theme(tx2036_hc_light())
    })
    
    # demographics pie
    output$waa_plot_pie <- renderHighchart({
        filter_waa() %>% 
            filter(year == 2036) %>% 
            #select(wda, "white" = nh_white_total, "black" = nh_black_total, "hispanic" = hispanic_total, "asian" = nh_asian_total, "other" = nh_other_total) %>% 
            #pivot_longer(white:other) %>%
            hchart("pie", hcaes(name, value)) %>% 
            hc_plotOptions(
                series = list(showInLegend = F,
                              dataLabels = F)
                
            ) %>% 
            hc_add_theme(tx2036_hc_light())
    })
    
    
    
    ## Value boxes
    output$waa_vb <- renderUI({
        text <- HTML(paste0(strong(formatC(signif(filter_waa_vb()$total, 3), format = "d", big.mark = ",")),
                     br()))
        # valueBox(value = formatC(signif(filter_waa_vb()$total, 3), format = "d", big.mark = ","),
        #          subtitle = "Working age adults (2036)")
    })
    
    
    
    ## 3. trends in in-demand jobs --------
    ## 4. attractive jobs --------
    ## 5. living wage jobs --------
    ## 6. employment by education --------
})
