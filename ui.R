source("read-data.R")

# Define UI for application that draws a histogram
shinyUI(
    tagList(
        useShinyjs(),
        scroller::use_scroller(),
        tags$head(
            tags$script(src="https://kit.fontawesome.com/8abb217f2e.js", crossorigin="anonymous"),
            tags$link(rel="shortcut icon", href="favicon.png"),
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
            tags$script(HTML("$('body').addClass('fixed');")),
            tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Montserrat:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,400&display=swap"),
            tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Work+Sans:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,400&display=swap")),
            tags$style(HTML("

                            ")), # close tags$style
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
                     fluidRow(
                         column(4,
                                
                                wellPanel(
                                    h1(htmlOutput("wda_name"), align = "center", height = 4), 
                                    leafletOutput("wda_map", height = 150),
                                    p(htmlOutput("wda_counties", align = "center")),
                                    p(strong("Jump to section:")), 
                                    a("Living wage households", type = "link", href = "##header1"), 
                                    a("Trends in working age adults", type = "link", href = "##header2"), 
                                    a("Trends in in-demand jobs", type = "link", href = "##header3"), 
                                    a("Attractive jobs", type = "link", href = "##header4"), 
                                    a("Living wage jobs", type = "link", href = "##header5"),
                                    a("Employment by education", type = "link", href = "##header6"),
                                    selectizeInput(inputId = "select_wda",
                                                   label = "Choose a different WDA: ",
                                                   choices = unique(crosswalk$wda),
                                                   selected = "Alamo"),
                                    width = "100%")), 
            
                         column(8,
                         
                     ## 1. living wage households --------
                     br(),
                     h2(htmlOutput("header1"), align = "center", height = 4),
                     ## 2. trends in working age adults --------
                     br(),
                     h2(htmlOutput("header2"), align = "center", height = 4),
                     ## 3. trends in in-demand jobs --------
                     br(),
                     h2(htmlOutput("header3"), align = "center", height = 4),
                     ## 4. attractive jobs --------
                     br(),
                     h2(htmlOutput("header4"), align = "center", height = 4),
                     ## 5. living wage jobs --------
                     br(),
                     h2(htmlOutput("header5"), align = "center", height = 4),
                     ## 6. employment by education --------
                     br(),
                     h2(htmlOutput("header6"), align = "center", height = 4),
                     )),
                     ) # closes wda page
            ) # close navbarPage
        ) # close tagList
    ) # close ui
