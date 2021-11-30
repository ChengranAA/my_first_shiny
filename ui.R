# Project name: 3991 final project
# Author: Chengran Li
# This project is an exploratory analysis of coronavius datasets from data.world
# This is my first attempt to accomplish a shiny app, since my html and css knowledge is 
# limited, some of the code seem messy and It will take awfully a lot of time to 
# reorganize the code. I already try my best to make the code at least structurally 
# clear. I am pretty sure i will find this unreadable later. 

library(shiny)
library(shinydashboard)

# header object [UI]
header <- dashboardHeader(
    titleWidth = "330",
    # make header wider and taller
    tags$li(
        class = "dropdown",
        # customization of header and side bar
        tags$style(".main-header {max-height: 75px;}"), 
        tags$style(".skin-purple .main-header .logo {height: 75px; background-color: #4F2683;}"),
        tags$style(".skin-purple .main-header .logo:hover {background-color: #4F2683;} "), 
        tags$style(".skin-purple .main-header .navbar {background-color: #4F2683;}"), 
        tags$style(".sidebar-toggle {height: 75px; padding-top: 25px !important;}"),
        tags$style(".skin-purple .main-sidebar .sidebar .sidebar-menu .active a{background-color: #4F2683;}"), 
        tags$style(".skin-purple .main-header .navbar .sidebar-toggle:hover{background-color: #807F83;}"),
        
    ),
    # add title img and text
    title = tags$a(tags$img(src='header_logo.png', height = "70", width = "300", style = "float: left;"))
)

# sidebar object [BODY]
sidebar <- dashboardSidebar(
    width = "330", 
    sidebarMenu(
        tags$style(".left-side, .main-sidebar {padding-top: 80px;}"),
        menuItem("Introduction", tabName = "intro", menuSubItem("Purpose", tabName = "intro_purpose"), 
                                                    menuSubItem("Data sets", tabName = "intro_data")), 
        menuItem("Data Wrangling", tabName = "wrang"), 
        menuItem("Exploration", tabName = "explore"), 
        menuItem("Discussion", tabName = "discuss"),
        menuItem("Reference", tabName = "ref")
    )
)

# WIGET for displaying map data and bar plot data of population , infected, death rate [BODY]
WIGET_MAP_BAR_PLOT <- wellPanel(uiOutput(outputId = "WIGET"), 
                                conditionalPanel(condition = "input.plot_type == 'Bar plot'", 
                                                 uiOutput(outputId = "plot_slider_interact")), 
                                plotOutput(outputId = "country_plot"))


# body object [UI]
body <- dashboardBody(
    tabItems(
        tabItem(tabName = "intro"),
        # sub items of intro
        tabItem(tabName = "intro_purpose", fluidPage(
                                                    # purpose of the analysis markdown file
                                                    fluidRow(column(width = 10, offset = 0, 
                                                                    includeMarkdown("intro_purpose.md"), 
                                                                    style = 'font-size : 20px;
                                                                            font-family: "Times New Roman", serif;
                                                                            margin-left : 10px;
                                                                            margin-bottom : 20px'),
                                                    # image of the cronavirus
                                                    fluidRow(column(width = 10, offset = 0, 
                                                                tags$table(class = "center", 
                                                                           tags$caption(align="bottom", "Figure 1. An illustration of the COVID_19 virus (https://www.cdc.gov/media/subtopic/images.htm)"), 
                                                                           tags$tr(tags$td(tags$img(src = "cronavirus.jpg", 
                                                                                                    height = "337.5", 
                                                                                                    width = "600px", 
                                                                                                    align = "center"
                                                                                                    )))
                                                                          ),  style = 'margin-left : 150px; 
                                                              ')
                                                                  
                                                               ))
                                                     )
                ), 
        tabItem(tabName = "intro_data", fluidPage(
                                                    # general summary of two datasets
                                                    fluidRow(column(width = 10, offset = 0, 
                                                                    includeMarkdown("data_general_summary.md"), 
                                                                    style = 'font-size : 20px;
                                                                            font-family: "Times New Roman", serif;
                                                                            margin-left : 10px;
                                                                            margin-bottom : 10px')),
                                                    # interactive summary of cronavirus activity datasets
                                                    fluidRow(column(width = 10, offset = 0, 
                                                                    wellPanel(navbarPage(title = "COVID-19 Activity - Location & Population Table", 
                                                                                        tabPanel("Table Preview", DT::dataTableOutput(outputId = 'preview_loca_pop')), 
                                                                                        tabPanel("Summary", verbatimTextOutput(outputId = 'summary_loca_pop')), 
                                                                                        tabPanel("Column Name and Type", tableOutput('colname_loca_pop')))))), 
                                                    # interactive summary of country pop and time datasets
                                                    fluidRow(column(width = 10, offset = 0, 
                                                                    wellPanel(navbarPage(title = "COVID-19 Activity Table", 
                                                                                        tabPanel("Table Preview", DT::dataTableOutput(outputId = 'preview_activity')),
                                                                                        tabPanel("Summary", verbatimTextOutput(outputId = 'summary_activity')), 
                                                                                        tabPanel("Column Name and Type", tableOutput(outputId = 'colname_activity'))))))
                                                  )
                ), 
        
        tabItem(tabName = "wrang", fluidPage(fluidRow(column(width = 10, offset = 0, includeMarkdown("wrangling.md"), style = 'font-size : 20px;
                                                                                                                              font-family: "Times New Roman", serif;
                                                                                                                              margin-left : 10px;
                                                                                                                              margin-bottom : 100px;')))), 
        tabItem(tabName = "explore", fluidPage(fluidRow(column(width = 10, offset = 0, includeMarkdown("explore.md"), style = 'font-size : 20px;
                                                                                                                              font-family: "Times New Roman", serif;
                                                                                                                              margin-left : 10px;
                                                                                                                              margin-bottom : 20px;')), 
                                               fluidRow(column(width = 10, offset = 0, WIGET_MAP_BAR_PLOT)),
                                               fluidRow(column(width = 10, offset = 0, includeMarkdown("explore_following.md"), style = 'font-size : 20px;
                                                                                                                              font-family: "Times New Roman", serif;
                                                                                                                              margin-left : 10px;
                                                                                                                              margin-top : 20px;
                                                                                                                              margin-bottom : 20px')), 
                                               fluidRow(column(width = 10, offset = 0, DT::dataTableOutput(outputId = 'explore_table'), style = 'margin-bottom : 20px;')), 
                                               fluidRow(column(width = 10, offset = 0, plotOutput(outputId = "explore_plot"), style = 'margin-bottom: 100px')))), 
        tabItem(tabName = "discuss", fluidPage(fluidRow(column(width = 10, offset = 0, includeMarkdown("discussion.md"), style = 'font-size : 20px;
                                                                                                                              font-family: "Times New Roman", serif;
                                                                                                                              margin-left : 10px;')))), 
        tabItem(tabName = "ref", fluidPage(fluidRow(column(width = 10, offset = 0, includeMarkdown("reference.md"), style = 'font-size : 20px;
                                                                                                                              font-family: "Times New Roman", serif;
                                                                                                                              margin-left : 10px;'))))))

# create ui with skin purple
ui <- fluidPage(title = "COVID-19 Activity Analysis", fluidRow(column(width = 12, offset = 0, style='padding:0px',  # add the main title
                                titlePanel(tags$div( 
                                    tags$h1("Exploratory Analysis of Open Covid-19 Data", 
                                            style = '
                                                    /*h1 css*/
                                                    padding-bottom: 5px;
                                                    padding-top: 15px;
                                                    margin-top: 0px;
                                                    margin-bottom: 0px; 
                                                    /*text css*/ 
                                                    color : #ffffff;
                                                    text-align : center; 
                                                    font-family: "Times New Roman", serif;
                                                    font-size: 45px;'), 
                                    tags$hr(style = '
                                                    margin-top: 4px;
                                                    margin-bottom: 5px;
                                                    width : 93%; ', 
                                                    ), 
                                    tags$h4("Author: Chengran Li", 
                                            style = '
                                                    /*h2 css*/
                                                    margin-bottom: 0px;
                                                    margin-top: 0px;
                                                    padding-bottom: 10px;
                                                    padding-top: 5px;
                                       
                                                    /*text css*/
                                                    color : #ffffff;
                                                    text-align : center; 
                                                    font-family: "Times New Roman", serif;'), 
                                    
                                    # panel css
                                    style = '
                                            background-image: linear-gradient(to bottom, #7B5CA2, #654193, #4F2683, #4F2683, #654193, #7B5CA2);
                                            margin-top: -20px;
                                            margin-bottom: -13px;
                                            border-style: solid;
                                            border-width: 3px; 
                                            border-color: black;
                                            border-radius: 5px;')))), 
                fluidRow(column(width = 12, offset = 0, style='padding:0px;
                                                               border-style: solid;
                                                               border-width; 3px;
                                                               border-color: black; 
                                                               border-radius: 10px', 
                                dashboardPage(skin = "purple", header, sidebar, body))
    )
)




      #          #
    #   #      #   #
   # # # #    # # # # 
  #       #  #       #



