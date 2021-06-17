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
        tags$style(HTML("
                                .well {
        background-color: #07254C !important; 
        margin-bottom: 10px !important;
        }
                        ")),
        navbarPage(
            id = "tab_being_displayed",
            selected = "Home",
            collapsible = T,
            title = "Aim Hire Texas",
            
            ###--- LANDING PAGE ----------------------------
            tabPanel(title = "Home",
                     fluidRow(
                         h1("Aim Hire Texas", align = "center"),
                         h4("Click a Workforce Development Area to learn more", align = "center"),
                         leafletOutput("home_map", height = 700)
                         )
                     ), # closes home page
            
            ###--- WDA PAGE ---------------------------
            tabPanel(title = "WDA",
                     fixedPanel(wellPanel(
                         h1("Selected WDA", align = "center", height = 4), 
                                top = 30, bottom = 1, left = 1, right = 1)),
                     ## 1. living wage households --------
                     br(),
                     h2("Living wage households", align = "center"),
                     ## 2. trends in working age adults --------
                     br(),
                     h2("Trends in working age adults", align = "center"),
                     ## 3. trends in in-demand jobs --------
                     br(),
                     h2("Trends in in-demand jobs", align = "center"),
                     ## 4. attractive jobs --------
                     br(),
                     h2("Attractive jobs", align = "center"),
                     ## 5. living wage jobs --------
                     br(),
                     h2("Living wage jobs", align = "center"),
                     ## 6. employment by education --------
                     br(),
                     h2("Employment by education", align = "center"),
                     br(),
                     h2("Attractive jobs", align = "center"),
                     ## 5. living wage jobs --------
                     br(),
                     h2("Living wage jobs", align = "center"),
                     ## 6. employment by education --------
                     br(),
                     h2("Employment by education", align = "center"),
                     br(),
                     h2("Attractive jobs", align = "center"),
                     ## 5. living wage jobs --------
                     br(),
                     h2("Living wage jobs", align = "center"),
                     ## 6. employment by education --------
                     br(),
                     h2("Employment by education", align = "center"),

                     fluidRow(
                         selectizeInput(inputId = "select_wda",
                                        label = "select a wda",
                                        choices = unique(wda_sf$wda),
                                        selected = "Alamo")
                     ),
                     ) # closes wda page
            ) # close navbarPage
        ) # close tagList
    ) # close ui
