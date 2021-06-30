
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    sever(html = disconnected, bg_color = "#3A4A9F", opacity = .92)
    ###--- REACTIVES -------------------------------
    selected_wda_sf <- reactive({
        sf <- wda_sf %>% 
            filter(wda == input$select_wda)
        return(sf)
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
    
    observeEvent(input$mini_map_shape_click$id, {
        updateSelectizeInput(session, 
                             inputId = "select_wda", 
                             label = "Choose a different WDA: ",
                             choices = unique(crosswalk$wda),
                             selected = input$mini_map_shape_click$id)
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
                        fillColor = ~pal(color_category),
                        fillOpacity = 1,
                        data = selected_wdacounties_sf()) %>% 
            addPolygons(stroke = T, 
                        weight = 1,
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
            addPolygons(stroke = T,
                        weight = 3,
                        color = "black",
                        opacity = 1,
                        fill = F,
                        data = selected_wda_sf(),
                        layerId = ~wda) %>% 

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
    
    observe(print(input$waa_plot_race_select))
    
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
            hchart("pie", hcaes(name, value)) %>% 
            hc_plotOptions(series = list(showInLegend = F, dataLabels = F)) %>% 
            hc_add_theme(
                hc_theme_merge(
                    tx2036_hc_light(),
                    hc_theme_null(chart = list(backgroundColor = "transparent"))
                )
            )
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
    ## Reactives 
    filter_aj <- reactive({
            df <- aj %>% 
                filter(wfb == input$select_wda) %>% 
                filter(!is.na(demand_index))
    })
    
    ## Plots
    # line chart
    output$aj_plot <- renderHighchart({
        filter_aj() %>% 
            hchart(type = "scatter", hcaes(y = quality_index, x = demand_index, 
                                           group = quality_and_demand_quadrant,
                                           size = share_of_local_jobs_percent)) %>% 
            hc_yAxis(title = list(text = "Quality Index"),
                     plotLines = list(list(
                         value = 0,
                         color = 'black',
                         width = 3,
                         zIndex = 4,
                         label = list(text = "quality threshold",
                                      style = list( color = 'black', fontWeight = 'bold'   )
                         )))) %>% 
            hc_xAxis(title = list(text = "Demand Index"),
                     plotLines = list(list(
                         value = 0,
                         color = 'black',
                         width = 3,
                         zIndex = 4,
                         label = list(text = "demand threshold",
                                      style = list( color = 'black', fontWeight = 'bold'   )
                         )))) %>% 
            hc_colors(c("#3ead92", "#5f6fc1", "#2a366c", "#f26852")) %>% 
            hc_title(text = "Quality and Demand Indices") %>% 
            hc_subtitle(text = "Quality and demand are determined by x and y sources. The size of the bubble indicates the share of local jobs") %>% 
            hc_tooltip(formatter = JS("function(){
                                return (this.point.occupation + 
                                      ' <br> Quality Index: ' + this.y + 
                                      ' <br> Demand Index: ' + this.x +
                                      ' <br> Share of local jobs: ' + this.point.share_of_local_jobs_percent + '%')}")) %>%
            
            hc_add_theme(tx2036_hc_light())
    })
    
    aj_table_data <- reactive ({
        aj %>% 
            filter(wfb == input$select_wda) %>% 
            mutate(quality_index = round(quality_index, 1),
                   demand_index = round(demand_index, 1),
                   share_of_local_jobs_percent = round(share_of_local_jobs_percent, 1)) %>% 
            select("Occupation" = occupation, 
                   Quality = quality_index, 
                   Demand = demand_index,
                   `% of Local Jobs` = share_of_local_jobs_percent) %>%
            arrange(desc(Quality + Demand))
    })

    output$aj_table <- function() {
        
        table <- aj_table_data() %>%
            mutate(`Quality` = ifelse(`Quality` < 0.0,
                                      color_tile("#f26852", "transparent")(`Quality`*c(`Quality`<0)),
                                      color_tile("transparent", "#3ead92")(`Quality`*c(`Quality`>0))),
                   `Demand` = ifelse(`Demand` < 0.0,
                                     color_tile("#f26852", "transparent")(`Demand`*c(`Demand`<0)),
                                     color_tile("transparent", "#3ead92")(`Demand`*c(`Demand`>0))),
                   `% of Local Jobs` = color_tile("transparent", "#3ead92")(`% of Local Jobs`*c(`% of Local Jobs`>0))) %>%
            kable("html", escape = F, table.attr = "style='width:100%;'") %>%
            kable_styling(full_width = T, bootstrap_options = c('striped', "hover", 'condensed', "responsive")) %>%
            row_spec(0, color = "white", background = "#5f6fc1") %>% 
            row_spec(1:nrow(aj_table_data()), color = "black", background = "white") %>% 
            scroll_box(width = "100%", height = "500px")
        
        return(table)
    }
    
    
    ## 5. living wage jobs --------
    ## 6. employment by education --------
})
