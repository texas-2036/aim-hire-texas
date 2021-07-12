source("read-data.R")

# Define UI for application that draws a histogram
shinyUI(
    tagList(
        useShinyjs(),
        # scroller::use_scroller(),
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
                        class="slides-wrapper",
                        tags$div(
                            class="home-slides",
                            slickROutput("home_slides", width = "100%", height = "300px"),
                        )
                    ),
                     tags$div(
                        class="homepage-wrapper",
                        fluidRow(
                            includeMarkdown(here::here("text", "home.md")),
                            includeMarkdown(here::here("text", "home2.md"))
                        ),
                        fluidRow(
                            class="map-row",
                            h4("Click a Workforce Development Area to learn more", align = "center", class="map-header"),
                            leafletOutput("home_map", height = 700)
                            ),
                        includeMarkdown(here::here("text", "home3.md")),
                        tags$hr(),
                        img(src = "AHT-FINAL-LOGO.png", height = 150)
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
                                    strong(a("Future workforce", type = "link", href = "#header_waa")),
                                    br(),
                                    strong(a("Trends in in-demand jobs", type = "link", href = "#header_idj")), 
                                    br(),
                                    strong(a("Living wage jobs", type = "link", href = "#header_lwj")),
                                    br(),
                                    strong(a("Attractive jobs", type = "link", href = "#header_aj")), 
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
                                    class="dark-bg wda-intro",
                                    includeMarkdown(here::here("text", "wda_intro.md"))
                                    ),
                                fluidRow(
                                    div(htmlOutput("header_lwh"), style="margin-top: -100px;"),
                                    h2("Living wage households", align = "center", height = 4, style="padding-top: 130px;"),
                                includeMarkdown(here::here("text", "1_living_wage_households.md")),
                                fluidRow(
                                    column(6,
                                           h3(htmlOutput("lwh_vb_year", aligh = "center")),
                                           h5("Households above ALICE Threshold (2018)"),
                                           highchartOutput("lwh_plot_year", height = 500)),
                                    column(6,
                                           h3(htmlOutput("lwh_vb_demo", aligh = "center")),
                                           h5("Families with kids above alice threshold"),
                                           highchartOutput("lwh_plot_demo", height = 500))
                                )
                                ),
                                ## 2. trends in working age adults --------
                                fluidRow(
                                    class="dark-bg",
                                    div(htmlOutput("header_waa"), style="margin-top: -100px;"),
                                    h2("Future Workforce", align = "center", height = 4, style="padding-top: 130px;"),
                                    includeMarkdown(here::here("text", "2_working_age_adults.md")
                                ),
                                fluidRow(
                                    class="dark-bg",
                                ),
                                column(8,
                                highchartOutput("waa_plot"),
                                radioButtons(inputId = "waa_plot_selects",
                                             label = "Show demographic breakdowns?",
                                             choices = c("total", "race-ethnicity", "gender", "race-ethnicity and gender"),
                                             selected = "total",
                                             inline = T)
                                ),
                                column(4,
                                       h3(htmlOutput("waa_vb", aligh = "center")),
                                       h5("Working age adults (2036)"),
                                       # only show the pie chart if one of the demographic breakdowns is selected
                                       conditionalPanel(condition = "input.waa_plot_selects != 'total'",
                                                        highchartOutput("waa_plot_pie")
                                                        )
                                       )
                                ),
                                ## 3. trends in in-demand jobs --------
                                fluidRow(
                                    br(),
                                    div(htmlOutput("header_idj"), style="margin-top: -100px;"),
                                    h2("Trends in in-demand jobs", align = "center", height = 4, style="padding-top: 130px;"),
                                    includeMarkdown(here::here("text", "3_indemand_jobs.md")),
                                    br(),
                                    h4("Top 10 most in-demand jobs"),
                                    reactableOutput("idj_top_table"),
                                    tags$hr(),
                                    h4("Bottom 10 least in-demand jobs"),
                                    reactableOutput("idj_bot_table"),
                                    tags$hr(),
                                    h4("Top 10 highest growth jobs"),
                                    reactableOutput("idj_growth_table"),
                                ),
                                ## 5. living wage jobs --------
                                fluidRow(
                                    class="dark-bg",
                                    br(),
                                    div(htmlOutput("header_lwj"), style="margin-top: -100px;"),
                                    h2("Living wage jobs", align = "center", height = 4, style="padding-top: 130px;"),
                                    includeMarkdown(here::here("text", "5_living_wage_jobs.md"))
                                ),
                                ## 4. attractive jobs --------
                                fluidRow(
                                    br(),
                                    div(htmlOutput("header_aj"), style="margin-top: -100px;"),
                                    h2("Attractive jobs", align = "center", height = 4, style="padding-top: 130px;"),
                                    includeMarkdown(here::here("text", "4_attractive_jobs.md")),
                                    fluidRow(
                                    column(11,
                                    highchartOutput("aj_plot", height = 500),
                                    p(style = "text-align:center;font-size:.8em;font-weight:400;color:black",
                                      tags$a(href = "https://brookingswof.shinyapps.io/TX_workforce_dev_app/", "Source: Brookings analysis of TWC data on wages and employment projections by workforce development area; EMSI occupational employment data; Burning Glass data on job postings and online resumes."))
                                      
                                    )), 
                                    
                                    br(),
                                    br(),

                                    fluidRow(
                                    column(11,
                                    reactableOutput("aj_table")
                                    )
                                    )
                                ),

                                ## 6. employment by education --------
                                fluidRow(
                                    class="dark-bg",
                                    br(),
                                    div(htmlOutput("header_edu"), style="margin-top: -100px;"),
                                    h2("Employment by education", align = "center", height = 4, style="padding-top: 130px;"),
                                    includeMarkdown(here::here("text", "6_employment_by_education.md")),
                                    fluidRow(highchartOutput("edu_plot_pseo")),
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
                                    ),
                                
                                )
                            )
                     ) # closes wda page
            ) # close navbarPage
        ) # close tagList
    ) # close ui