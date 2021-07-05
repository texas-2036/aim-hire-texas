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
            tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Cabin:wght@400;500;600;700&display=swap"),
            tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Montserrat:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,400&display=swap"),
            tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Work+Sans:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,400&display=swap")
            ),
            tags$style(HTML("

                            ")), # close tags$style
        navbarPage(
            id = "tab_being_displayed",
            selected = "Home",
            collapsible = T,
            position = c("fixed-top"),
            title = a(img(src="AHT-FINAL-LOGO.png", class="aht-logo", height = 70, width = 78), type="link", href="/"),
            
            ###--- LANDING PAGE ----------------------------
            tabPanel(title = "Home",
                     fluidRow(
                         h4("Click a Workforce Development Area to learn more", align = "center", style="padding-top: 120px;"),
                         leafletOutput("home_map", height = 700)
                         )
                     ), # closes home page
            
            ###--- WDA PAGE ---------------------------
            tabPanel(title = "Workforce Development Areas",
                     div(class="wda-wrapper",
                         ## * Well panel -----------
                         div(class="well-placeholder"),
                         div(
                                wellPanel(
                                    class='well-panel',
                                    h2(htmlOutput("wda_name"), align = "center", height = 4), 
                                    leafletOutput("mini_map", height = 310),
                                    p(htmlOutput("wda_counties", align = "center")),
                                    tags$hr(),
                                    p(strong("Jump to section:")), 
                                    
                                    column(11, offset = 1,
                                    strong(a("Living wage households", type = "link", href = "#header_lwh")), 
                                    br(),
                                    strong(a("Trends in working age adults", type = "link", href = "#header_waa")),
                                    br(),
                                    strong(a("Trends in in-demand jobs", type = "link", href = "#header_idj")), 
                                    br(),
                                    strong(a("Attractive jobs", type = "link", href = "#header_aj")), 
                                    br(),
                                    strong(a("Living wage jobs", type = "link", href = "#header_lwj")),
                                    br(),
                                    strong(a("Employment by education", type = "link", href = "#header_edu")),
                                    tags$hr()
                                    ),
                                    
                                    selectizeInput(inputId = "select_wda",
                                                   label = "Choose a different WDA: ",
                                                   choices = unique(crosswalk$wda),
                                                   selected = "Alamo"),
                                    width = "100%")), 
            
                         div(
                                class='main-panel',
                                
                                ## * Main panel -----------
                                ## 1. living wage households --------
                                fluidRow(
                                br(),
                                h2(htmlOutput("header_lwh"), align = "center", height = 4, style="padding-top: 100px;"),
                                includeMarkdown(here::here("text", "1_living_wage_households.md"))
                                ),
                                ## 2. trends in working age adults --------
                                fluidRow(
                                    class="dark-bg",
                                    br(),
                                    h2(htmlOutput("header_waa"), align = "center", height = 4),
                                    includeMarkdown(here::here("text", "2_working_age_adults.md")
                                ),
                                fluidRow(
                                    class="dark-bg",
                                    column(width = 4, align = "center",
                                    checkboxInput(inputId = "waa_plot_race_select",
                                                label = "Show data by race-ethnicity?",
                                                value = F)),
                                    column(width = 4, align = "left",
                                    checkboxInput(inputId = "waa_plot_gender_select",
                                                label = "Show data by gender?",
                                              value = F)
                                    )
                                ),
                                column(8,
                                highchartOutput("waa_plot")
                                ),
                                column(4,
                                       h3(htmlOutput("waa_vb", aligh = "center")),
                                       h5("Working age adults (2036)"),
                                       # only show the pie chart if one of the demographic breakdowns is selected
                                       conditionalPanel(condition = "input.waa_plot_race_select == 'TRUE'",
                                                        highchartOutput("waa_plot_pie")
                                                        )
                                       )
                                ),
                                ## 3. trends in in-demand jobs --------
                                fluidRow(
                                    br(),
                                    h2(htmlOutput("header_idj"), align = "center", height = 4),
                                    includeMarkdown(here::here("text", "3_indemand_jobs.md"))
                                ),
                                ## 4. attractive jobs --------
                                fluidRow(
                                    class="dark-bg",
                                    br(),
                                    h2(htmlOutput("header_aj"), align = "center", height = 4),
                                    includeMarkdown(here::here("text", "4_attractive_jobs.md")),
                                    fluidRow(
                                    column(11,
                                    highchartOutput("aj_plot", height = 500),
                                    )), 
                                    
                                    br(),
                                    br(),

                                    fluidRow(
                                    column(11,
                                    reactableOutput("aj_table")
                                    )
                                    )
                                ),
                                ## 5. living wage jobs --------
                                fluidRow(
                                    br(),
                                    h2(htmlOutput("header_lwj"), align = "center", height = 4),
                                    includeMarkdown(here::here("text", "5_living_wage_jobs.md"))
                                ),
                                ## 6. employment by education --------
                                fluidRow(
                                    class="dark-bg",
                                    br(),
                                    h2(htmlOutput("header_edu"), align = "center", height = 4),
                                    includeMarkdown(here::here("text", "6_employment_by_education.md")),
                                    fluidRow(
                                        column(6,
                                               h3(htmlOutput("edu_vb_income", aligh = "center")),
                                               h5("Median income of high school graduates"),
                                               highchartOutput("edu_plot_income", height = 500)),
                                        column(6,
                                               h3(htmlOutput("edu_vb_rate", aligh = "center")),
                                               h5("of high school graduates employed"),
                                               highchartOutput("edu_plot_rate", height = 500))
                                    )
                                    )
                                )
                            )
                     ) # closes wda page
            ) # close navbarPage
        ) # close tagList
    ) # close ui