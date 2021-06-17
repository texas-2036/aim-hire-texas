
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    sever(html = disconnected, bg_color = "#3A4A9F", opacity = .92)

    output$home_map <- renderLeaflet({
        leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 6, maxZoom = 6)) %>%
            setView(-99.9018, 30.9686, zoom = 6) %>% 
            addPolygons(stroke = F,
                        fill = T,
                        fillOpacity = 1,
                        fillColor = ~pal(color_category),
                        data = wda_sf) %>%
            addPolygons(color = "black",
                        stroke = T,
                        weight = 1,
                        fill = F,
                        data = counties) %>%
            addPolygons(stroke = T, 
                        weight = 2,
                        color = "black",
                        opacity = 1,
                        fill = T,
                        fillOpacity = 0,
                        label = wda_sf$wda,
                        data = wda_sf) %>% 
            setMapWidgetStyle(list(background= "white")) %>% 
            htmlwidgets::onRender("function(el, x) { 
               map = this
               map.dragging.disable();
               }")
    })
})
