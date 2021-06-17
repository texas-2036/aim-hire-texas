source("read-data.R")

# Define UI for application that draws a histogram
shinyUI(
    tagList(
        useShinyjs(),
        tags$head(
            tags$script(src="https://kit.fontawesome.com/8abb217f2e.js", crossorigin="anonymous"),
            tags$link(rel="shortcut icon", href="favicon.png"),
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
            tags$script(HTML("$('body').addClass('fixed');")),
            tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Montserrat:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,400&display=swap"),
            tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Work+Sans:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,400&display=swap")),
        
        navbarPage(
            id = "tab_being_displayed",
            selected = "Home",
            collapsible = T,
            title = "Aim Hire Texas",
            
            ###--- LANDING PAGE ----------------------------
            tabPanel(title = "Home",
                     fluidRow(
                         leafletOutput("home_map", height = 700)
                         )
                     ), # closes home page
            
            ###--- WDA INFO SHEET ---------------------------
            tabPanel(title = "WDA",
                     fluidRow(
                         p("content")
                     )
                     ) # closes wda page
            ) # close navbarPage
        ) # close tagList
    ) # close ui
