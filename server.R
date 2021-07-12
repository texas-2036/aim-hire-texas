
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
    ## slickR images -----
    output$home_slides <- renderSlickR({
        imgs <- list.files(here::here("www", "slickr"), pattern=".png", full.names = TRUE)
        slickR(imgs, height = 300, width = "95%") +
            settings(dots = T, autoplay = T, autoplaySpeed = 3000)
        
    })
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
                        fill = T,
                        fillColor = ~pal(color_category),
                        fillOpacity = 1,
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
        text <- HTML(paste0(" "))
        return(text)
    })
    output$header_idj <- renderUI({
        text <- HTML(paste0(""))
        return(text)
    })
    output$header_lwj <- renderUI({
        text <- HTML(paste0(""))
        return(text)
    })
    output$header_aj <- renderUI({
        text <- HTML(paste0(""))
        return(text)
    })
    output$header_edu <- renderUI({
        text <- HTML(paste0(""))
        return(text)
    })


    ## * Content -----
    ## 1. living wage households --------
    # reactives
    filter_lwh <- reactive({
        df <- alice_hh_counts %>% 
            filter(wda == input$select_wda) 
    })
    
    filter_lwh_details <- reactive({
        df <- alice_demographics %>% 
            filter(wda == input$select_wda)
    })
    
    ## Value boxes
    output$lwh_vb_year <- renderUI({
        df <- filter_lwh() %>% 
            filter(year == 2018)
        text <- HTML(paste0(strong(formatC(signif(df$alice_household, 3), format = "d", big.mark = ",")),
                            br()))
    })
    
    output$lwh_vb_demo <- renderUI({
        df <- filter_lwh_details() %>%
            filter(category == "fam_kids")
        text <- HTML(paste0(strong(formatC(signif(df$above_alice, 3), format = "d", big.mark = ",")),
                            br()))
    })
    
    # plots
    output$lwh_plot_year <- renderHighchart({
        filter_lwh() %>%
            mutate(above_poverty_below_alice_hh_share = below_alice_hh_share - below_poverty_hh_share) %>% 
            rename("Below poverty" = below_poverty_hh_share,
                   "Above poverty, below ALICE" = above_poverty_below_alice_hh_share,
                   "Above ALICE" = above_alice_hh_share) %>% 
            pivot_longer(cols = c(`Below poverty`, `Above poverty, below ALICE`, `Above ALICE`)) %>% 
            mutate(value = round(value, 1)) %>% 
            hchart(type = "column", hcaes(x = year, y = value, group = name)) %>% 
            hc_yAxis(min = 0, max = 100, title = list(text = "")) %>%
            hc_xAxis(title = list(text = "")) %>%
            hc_plotOptions(column = list(stacking = "normal"),
                           series = list(showInLegend = F)) %>%
            hc_add_theme(tx2036_hc) %>% 
            hc_title(text = "Share of households in income tiers over time") %>% 
            # manually reorder colors bc red = bad
            hc_colors(c("#3ead92", "#2a366c", "#f26852")) %>% 
            hc_tooltip(formatter = JS("function(){
                                return (this.point.name + ' ' + this.y + '%')}"))
    })
    
    output$lwh_plot_demo <- renderHighchart({
        filter_lwh_details() %>% 
            mutate(hh = poverty + alice + above_alice,
                   "Below poverty" = 100 * poverty / hh,
                   "Above poverty, below ALICE" = 100 * alice / hh,
                   "Above ALICE" = 100 * above_alice / hh) %>% 
            mutate(category = case_when(category == "fam_kids" ~ "Families with Children",
                                        category == "over_65" ~ "65 and Over",
                                        category == "single_cohab" ~ "Single or Cohabiting")) %>% 
            pivot_longer("Below poverty":"Above ALICE") %>% 
            mutate(value = round(value, 1)) %>% 
            hchart(type = "column", hcaes(x = category, y = value, group = name)) %>% 
            hc_yAxis(min = 0, max = 100, title = list(text = "")) %>%
            hc_xAxis(title = list(text = "")) %>%
            hc_plotOptions(column = list(stacking = "normal"),
                           series = list(showInLegend = F)) %>% 
            
            hc_add_theme(tx2036_hc) %>% 
            hc_title(text = "Share of households in income tiers by family type") %>% 
            # hc_legend(align = "left", 
            #           verticalAlign = "bottom",
            #           layout = "horizontal") %>% 
            hc_colors(c("#3ead92", "#2a366c", "#f26852")) %>% 
            hc_tooltip(formatter = JS("function(){
                                return (this.point.name + ' ' + this.y + '%')}"))
    })
    
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
        if (input$waa_plot_selects == "total") {
            df <- filter(df, name == "total")
        }
        else if (input$waa_plot_selects == "gender") {
            df <- filter(df, name %in% c("Female", "Male"))
        }
        else if (input$waa_plot_selects == "race-ethnicity") {
            df <- filter(df, name %in% c("White", "Black", "Hispanic", "Asian", "Other"))
        }
        else if (input$waa_plot_selects == "race-ethnicity and gender") {
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
            hc_add_theme(tx2036_hc) %>% 
            hc_colors(c("#f26852", "#2a366c", "#3ead92", "#5f6fc1", "#f9cd21", 
                        "#F9BCB3", "#8997D1", "#87D4C1", "#C4CAE8", "#FCE99C")) %>% 
            hc_tooltip(formatter = JS("function(){
                                return (this.point.name + 
                                      ': ' + this.y )}"))
    })
    
    # demographics pie
    output$waa_plot_pie <- renderHighchart({
        filter_waa() %>% 
            filter(year == 2036) %>% 
            hchart("pie", hcaes(name, value)) %>% 
            hc_plotOptions(series = list(showInLegend = F, dataLabels = F)) %>% 
            hc_add_theme(tx2036_hc) %>% 
            hc_colors(c("#f26852", "#2a366c", "#3ead92", "#5f6fc1", "#f9cd21", 
                        "#F9BCB3", "#8997D1", "#87D4C1", "#C4CAE8", "#FCE99C")) %>% 
            hc_tooltip(formatter = JS("function(){
                                return (this.point.name + 
                                      ': ' + this.y )}"))
            #     hc_theme_merge(
            #         tx2036_hc,
            #         hc_theme_null(chart = list(backgroundColor = "transparent"))
            #     )
            # )
        })
    
    ## Value boxes
    output$waa_vb <- renderUI({
        text <- HTML(paste0(strong(formatC(signif(filter_waa_vb()$total, 3), format = "d", big.mark = ",")),
                     br()))
    })
    
    
    
    ## 3. trends in in-demand jobs --------
    ## Reactives 
    idj_table_data <- reactive({
        top <- idj %>% 
            filter(wda == input$select_wda) %>% 
            select(-c(wda, rank))
    })
    
    ## Tables
    output$idj_top_table <- renderReactable({
        df <- idj_table_data() %>% 
            filter(type == "top") %>% 
            select(-type)
       
        options(reactable.theme = reactableTheme(
            color = "black",
            backgroundColor = "#FFFFFF", 
            borderColor = "#5f6fc1",
            stripedColor = "#f8f8ff"
        ))
    
        table <- reactable(df, 
                           defaultColDef = colDef(align = "center"),
                           showPageSizeOptions = F,
                           striped = T,
                           highlight = T)

        return(table)
    })
    
    output$idj_bot_table <- renderReactable({
        df <- idj_table_data() %>% 
            filter(type == "bottom") %>% 
            select(-type)
        
        options(reactable.theme = reactableTheme(
            color = "black",
            backgroundColor = "#FFFFFF", 
            borderColor = "#5f6fc1",
            stripedColor = "#f8f8ff"
        ))
        
        table <- reactable(df, 
                           defaultColDef = colDef(align = "center"),
                           showPageSizeOptions = F,
                           striped = T,
                           highlight = T)
        
        return(table)
    })
    
    output$idj_growth_table <- renderReactable({
        df <- idj_table_data() %>% 
            filter(type == "growth") %>% 
            select(-type)
        
        options(reactable.theme = reactableTheme(
            color = "black",
            backgroundColor = "#FFFFFF", 
            borderColor = "#5f6fc1",
            stripedColor = "#f8f8ff"
        ))
        
        table <- reactable(df, 
                           defaultColDef = colDef(align = "center"),
                           showPageSizeOptions = F,
                           striped = T,
                           highlight = T)
        
        return(table)
    })
    
    ## 5. living wage jobs --------
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
                         color = 'white',
                         width = 3,
                         zIndex = 4,
                         label = list(text = "quality threshold",
                                      style = list(color = "rgba(255,255,255, 0.5)", fontWeight = '400',
                                                   fontSize='12px')
                         )))) %>% 
            hc_xAxis(title = list(text = "Demand Index"),
                     plotLines = list(list(
                         value = 0,
                         color = 'white',
                         width = 3,
                         zIndex = 4,
                         label = list(text = "demand threshold",
                                      style = list(color = "rgba(255,255,255, 0.5)", fontWeight = '400',
                                                   fontSize='12px')
                         )))) %>% 
            hc_colors(c("#3ead92", "#5f6fc1", "#2a366c", "#f26852")) %>% 
            hc_title(text = "Quality and Demand Indices") %>% 
            hc_subtitle(text = "Point size is proportional to the number of workers in the occupation in the selected workforce development area. Thresholds show the Quality Index and Demand Index of the average occupation.") %>% 
            hc_tooltip(formatter = JS("function(){
                                return (this.point.occupation + 
                                      ' <br> Quality Index: ' + this.y + 
                                      ' <br> Demand Index: ' + this.x +
                                      ' <br> Share of local jobs: ' + this.point.share_of_local_jobs_percent + '%')}")) %>%
            
        hc_add_theme(tx2036_hc)
    })
    
    aj_table_data <- reactive ({
        aj %>% 
            filter(wfb == input$select_wda) %>% 
            mutate(quality_index = round(quality_index, 1),
                   demand_index = round(demand_index, 1),
                   share_of_local_jobs_percent = round(share_of_local_jobs_percent, 1)) %>% 
            filter(!is.na(quality_index)) %>% 
            filter(!is.na(demand_index)) %>% 
            filter(!is.na(share_of_local_jobs_percent)) %>% 
            select("Occupation" = occupation, 
                   Quality = quality_index, 
                   Demand = demand_index,
                   `Share of Local Jobs` = share_of_local_jobs_percent) %>%
            arrange(desc(Quality + Demand))
    })

    output$aj_table <- renderReactable({
        options(reactable.theme = reactableTheme(
            color = "black",
            backgroundColor = "#FFFFFF", 
            borderColor = "#5f6fc1",
            stripedColor = "#f8f8ff"
        ))
        
        redgreen_pal <- function(x) rgb(colorRamp(c("#f26852", "white", "#3ead92"))(x), maxColorValue = 255)
        green_pal <- function(x) rgb(colorRamp(c("white", "#3ead92"))(x), maxColorValue = 255)
        
        table <- reactable(aj_table_data(), 
                           defaultColDef = colDef(align = "center"),
                           filterable = T,
                           showPageSizeOptions = F,
                           striped = F,
                           highlight = T,
                           
                           columns = list(
                               `Quality` = colDef(style = function(value) {
                                   # to normalize, do (value - min) / (max - min) : reduces to this
                                   normalized <- ((value + 5) / 10)
                                   color <- redgreen_pal(normalized)
                                   list(background = color)
                                   }),
                               `Demand` = colDef(style = function(value) {
                                   normalized <- ((value + 5) / 10)
                                   color <- redgreen_pal(normalized)
                                   list(background = color)
                               }),
                               `Share of Local Jobs` = colDef(style = function(value) {
                                   normalized <- (value - min(aj_table_data()$`Share of Local Jobs`)) / (max(aj_table_data()$`Share of Local Jobs`) - min(aj_table_data()$`Share of Local Jobs`))
                                   color <- green_pal(normalized)
                                   list(background = color)
                               })
                               ))
        
        
        # table <- aj_table_data() %>%
        #     mutate(`Quality` = ifelse(`Quality` < 0.0,
        #                               color_tile("#f26852", "transparent")(`Quality`*c(`Quality`<0)),
        #                               color_tile("transparent", "#3ead92")(`Quality`*c(`Quality`>0))),
        #            `Demand` = ifelse(`Demand` < 0.0,
        #                              color_tile("#f26852", "transparent")(`Demand`*c(`Demand`<0)),
        #                              color_tile("transparent", "#3ead92")(`Demand`*c(`Demand`>0))),
        #            `Share of Local Jobs` = color_tile("transparent", "#3ead92")(`Share of Local Jobs`*c(`Share of Local Jobs`>0))) %>%
        #     kable("html", escape = F, table.attr = "style='width:100%;'") %>%
        #     kable_styling(full_width = T, bootstrap_options = c('striped', "hover", 'condensed', "responsive")) %>%
        #     row_spec(0, color = "white", background = "#5f6fc1") %>% 
        #     row_spec(1:nrow(aj_table_data()), color = "black", background = "white") %>% 
        #     scroll_box(width = "100%", height = "500px")
        
        return(table)
    })
    
    

    ## 6. employment by education --------
    ## Reactives 
    filter_edu <- reactive({
        df <- edu %>% 
            filter(wda == input$select_wda) %>% 
            mutate(education = case_when(education == "nohs" ~ "No high school diploma",
                                         education == "hs" ~ "High school diploma",
                                         education == "somecollege" ~ "Some college or associate's degree",
                                         education == "college" ~ "Bachelor's degree"))
    })
    
    ## Plots
    # income
    output$edu_plot_income <- renderHighchart({
        filter_edu() %>% 
            mutate(median_income = round(median_income)) %>% 
            hchart(type = "column", hcaes(x = education, y = median_income)) %>% 
            #hc_colors("#f26852") %>% 
            hc_xAxis(title = list(text = "")) %>%
            hc_yAxis(title = list(text = "Median Income")) %>%
            hc_title(text = "Median income by education") %>%
            hc_add_theme(tx2036_hc) %>% 
            hc_tooltip(formatter = JS("function(){
                            return ('$' + this.y)}"))
    })
    
    # employment rate
    output$edu_plot_rate <- renderHighchart({
        filter_edu() %>%
            mutate(pct_employed = round(pct_employed)) %>% 
            hchart(type = "column", hcaes(x = education, y = pct_employed)) %>%
            #hc_colors("#f26852") %>%
            hc_xAxis(title = list(text = "")) %>%
            hc_yAxis(title = list(text = "Employment Rate"),
                     max = 100) %>%
            hc_title(text = "Employment rate by education") %>%
            hc_add_theme(tx2036_hc) %>% 
            hc_tooltip(formatter = JS("function(){
                            return (this.y + '%')}"))
    })
    
    ## Value boxes
    output$edu_vb_income <- renderUI({
        value <- filter_edu() %>% 
            filter(education == "High school diploma")
        text <- HTML(paste0( "$", strong(formatC(signif(value$median_income, 3), format = "d", big.mark = ",")),
                            br()))
    })
    
    output$edu_vb_rate <- renderUI({
        
        value <- filter_edu() %>% 
            filter(education == "High school diploma")
        text <- HTML(paste0(round(value$pct_employed), "%",
                            br()))
    })
    
    ### post high school
    filter_pseo <- reactive({
        df <- pseo_wda_df %>% 
            filter(wda_name == input$select_wda)
    })
    
    output$edu_plot_pseo <- renderHighchart({
        df <- filter_pseo() %>% 
        mutate(degree_level = case_when(degree_level == "01" ~ "Certificate < 1 year",
                                        degree_level == "02" ~ "Certificate 1-2 years",
                                        degree_level == "03" ~ "Associates",
                                        degree_level == "04" ~ "Certificate 2-4 years",
                                        degree_level == "05" ~ "Baccalaureate")) %>% 
            mutate(degree_level = factor(degree_level, levels = c("Certificate < 1 year", "Certificate 1-2 years", "Associates",
                                                                  "Certificate 2-4 years", "Baccalaureate"),
                                         ordered = T))
        highchart() %>% 
            hc_add_series(data = df, "scatter", hcaes(y = y10_p50_earnings, x = degree_level, size = 100, opacity = 1)) %>%
            hc_add_series(data = df, "errorbar", hcaes(x = degree_level, low = y10_p25_earnings, high = y10_p75_earnings, width = 20),
                          color = "#f26852", whiskerWidth = 1,  lineWidth = 5) %>% 
            hc_add_theme(tx2036_hc) %>% 
            hc_xAxis(title = list(text = "")) %>% 
            hc_yAxis(title = list(text = "")) %>% 
            hc_plotOptions(series = list(showInLegend = F)) %>%
            hc_tooltip(formatter = JS("function(){
                            return ('Median annual salary: $' + Highcharts.numberFormat(this.y, 0))}")) %>%
            hc_title(text = "Median and quartile earnings for college graduates")
    })
    
})
