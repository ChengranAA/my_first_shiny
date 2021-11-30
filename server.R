library(tidyverse)
library(lubridate)
library(knitr)
library(DT)
library(rworldmap)
library(RColorBrewer)
library(ggpubr)

# The following part only run once when server start

# read the datasets
covid_activity_all <- read.csv("Data/COVID-19_Activity.csv", header=TRUE, stringsAsFactors = FALSE)
covid_data_loca_pop <- read.csv("Data/Master_Location_Pop_Table.csv", header=TRUE, stringsAsFactors=FALSE) # need to find faster way to read data
                                                                                                           # and process it

# necesary functions
## for scrollable preview of the data table
PreviewTable <- function(mydata) {
  DT::datatable(mydata, extensions = "Scroller", 
                options = list(dom = 't',
                               scrollY = 350,
                               scrollX = 500,
                               deferRender = TRUE,
                               scoller = TRUE))
}

# create the summary of pop and loca data
preview_dt_loca_pop <- PreviewTable(data.table::setDT(head(covid_data_loca_pop)))
colname_loca_pop <- read_csv("Data/colname_loca_pop.csv")
summary_loca_pop <- summary(covid_data_loca_pop)

# create the summary of activity data
preview_dt_activity <- PreviewTable(data.table::setDT(head(covid_activity_all)))
colname_activity <- read_csv("Data/colname_activity.csv")
summary_activity <- summary(covid_activity_all)

## get relevant information of covid_activity_data
covid_activity_all <- covid_activity_all %>%
  select(-DATA_SOURCE_NAME, -COUNTY_NAME, -PROVINCE_STATE_NAME, -COUNTY_FIPS_NUMBER, -COUNTRY_ALPHA_2_CODE) %>%
  # rm any na
  filter(!is.na(COUNTRY_ALPHA_3_CODE) & !is.na(CONTINENT_NAME)) %>%
  # em empty row
  filter(COUNTRY_ALPHA_3_CODE != "" & CONTINENT_NAME != "") %>%
  group_by(COUNTRY_SHORT_NAME, REPORT_DATE, COUNTRY_ALPHA_3_CODE, CONTINENT_NAME) %>%
  summarise(total_new_positive_by_day = sum(PEOPLE_POSITIVE_NEW_CASES_COUNT), 
            total_new_death_day = sum(PEOPLE_DEATH_NEW_COUNT), .groups = 'drop')

# get relevant country data
covid_data_loca_pop <- covid_data_loca_pop %>%
  select(-DATA_SOURCE_NAME, -GEO_LATITUDE, -GEO_LONGITUDE) %>%
  group_by(COUNTRY_ALPHA_3_CODE) %>%
  summarise(total_population = sum(GEO_REGION_POPULATION_COUNT), .groups = "drop") %>%
  filter(total_population > 0)

# sum up by country
covid_full_by_country <- covid_activity_all %>%
  group_by(COUNTRY_ALPHA_3_CODE, CONTINENT_NAME) %>%
  summarise(total_infected = sum(total_new_positive_by_day),total_death = sum(total_new_death_day), .groups = "drop")

# join the known population to covide data
covid_full_by_country_pop <- left_join(covid_data_loca_pop, covid_full_by_country, by = c("COUNTRY_ALPHA_3_CODE"))

# calculate percentage of infected & death
covid_full_by_country_pop <- covid_full_by_country_pop %>%
  mutate(percent_infected = round(total_infected / total_population * 100, 2), 
         percent_death = round(total_death / total_population * 100, 2), 
         population_million_of = total_population / 1000000)

# plot data for covid activity each day
covid_activity_all_year <- covid_activity_all %>%
  group_by(REPORT_DATE) %>%
  summarise(total_new_death_day = sum(total_new_death_day), 
            total_new_positive_by_day = sum(total_new_positive_by_day)) %>%
  mutate(year = year(REPORT_DATE))

# change the type for plotting preparation
covid_activity_all_year$REPORT_DATE <- as_date(covid_activity_all_year$REPORT_DATE)
covid_activity_all_year$year <- as.factor(covid_activity_all_year$year)

# calculate 8month data before 2020-04-02
eight_month_be <- ymd(as.Date('2021-04-02')) %m-% months(8)

covid_activity_all_year_sub <- covid_activity_all_year %>%
  filter(REPORT_DATE <= as.Date('2021-04-02') & REPORT_DATE>= eight_month_be) %>%
  group_by(year) %>%
  summarise(mu_infected_4m = round(mean(total_new_positive_by_day),2),
            mu_death_4m = round(mean(total_new_death_day),2))

rm(eight_month_be)


# SERVER !!! 

# bug log 001 [MINOR] : when switching variable type, if the origin variable is not on population , this is a warning , can be fix, however, 
# there is limited time for this project

# bug log 002 [MAJOR] [FIXED] : when select bar plot and select any percentage variable, and change the plot type, the widget will crash 
# bug fix 002 : some what fix, there are still warnings, i really don,t know how to fix, just leave it for future me


# The following part run each interaction
server <- function(input, output) {
  # create the preview of location and population data
  output$preview_loca_pop <- DT::renderDT({preview_dt_loca_pop})
  
  # create the summary of location data and population
  output$summary_loca_pop <- renderPrint({summary_loca_pop})
  
  # create the variable name and type output of covid_data_loca_pop
  output$colname_loca_pop <- renderTable({colname_loca_pop})
  
  # create the preview of activity data
  output$preview_activity <- DT::renderDT({preview_dt_activity})
  
  # create the summary of activity data
  output$summary_activity <- renderPrint({summary_activity})
  
  # create the variable name and type output of covid_activity_all
  output$colname_activity <- renderTable({colname_activity})
  
  # render widget ui
  output$WIGET <- renderUI({
    inputPanel(
      uiOutput(outputId = "render_plot_type"),
      uiOutput(outputId = "render_variable"), 
      uiOutput(outputId = "continent_ui_select"), 
      conditionalPanel(condition = "input.plot_type == 'Bar plot'",  
                       selectInput(inputId = "plot_variable_type", 
                                 label = "Vaiebale Type", 
                                 choices = c("Total", "Percentage"))))
  })
  
  # render the plot_type ui
  output$render_plot_type <- renderUI({
    selectInput(inputId = "plot_type", 
                label = "Type of the plot", 
                choices = c("Map", "Bar plot"), 
                selected = "Map")
  })
  
  # make slider depend on continent
  max_slider <- function(data, continent){
    if (!is.null(continent) & continent == "World"){
      max_num <- length(data$CONTINENT_NAME)
    } else {
      max_num <- nrow(data %>% filter(CONTINENT_NAME == continent))
    }
    return(max_num)
  }
  
  # render the slider for select number of country
  output$plot_slider_interact <- renderUI({
    req(input$plot_continent)
    sliderInput("slider_input", "Number of countries", 
                min = 1, 
                step = 1,
                max = max_slider(covid_full_by_country_pop, input$plot_continent), 
                value = 5, width = "50%", ticks = FALSE)
  })
  
  # make continents choices depend on plot type
  choices_continent <- function(type){
    if (!is.null(type) & type == "Map"){
      choices = c("World", "Asia", "Europe", "Africa", "North America","Latin America", "Oceania")
    } else if (!is.null(type) & type == "Bar plot"){
      choices = c("World", "America", "Asia" , "Africa" , "Europe" , "Oceania")
    } 
    return(choices)
  }
  
  # render the continent select input ui
  output$continent_ui_select <- renderUI({
    req(input$plot_type)
    selectInput(inputId = "plot_continent", 
                label = "Continent", 
                choices = choices_continent(input$plot_type), 
                selected = "World")
  })
  
  # make variable choices depend on variable type
  choices_variable <- function(type){
    if (type == "Total" & !is.null(type)) {
      choices = c("Population", "Total infected", "Total death")
    } else if (type == "Percentage" & !is.null(type)) {
      choices = c("Population", "Infected percentage", "Death percentage")
    }
  }
  
  # make variable depend on variable type
  output$render_variable <- renderUI({
    input$plot_type
    req(input$plot_variable_type)
      selectInput(inputId = "plot_variable", 
                  label = "Variable", 
                  choices = choices_variable(input$plot_variable_type), 
                  selected = "Population") 
    })
  
  # map plotTing function
  plot_map <- function(data, continent, variable){
    # I'd admit the code is not elegant
    sink("NULL") ## stream message to /dev/null, silence all the message, a little bit risky 
    
    ## make title fro the map plot
    title_of_map <- paste(variable, "of", continent, seq = " ")
    
    ## create color palette 
    colorpalette <- switch (variable,
                            "Population" = "Blues", 
                            "Total infected" = "YlOrRd", 
                            "Total death"= "Greys")
    colorpalette <- colorpalate <- brewer.pal(9, colorpalette)
    
    ## change variable to variable name in the table
    variable <- switch(variable, 
                       "Population" = "population_million_of", 
                       "Total infected" = "total_infected", 
                       "Total death"= "total_death")
    continent <- str_to_lower(continent)
    
    ## join the data to map data 
    ## note: 7 of the data did not plot 
    joinData <- joinCountryData2Map(data,
                                    joinCode = "ISO3", 
                                    nameJoinColumn = "COUNTRY_ALPHA_3_CODE")
    suppressWarnings(mapCountryData(joinData, nameColumnToPlot = variable, 
                                                     catMethod = "quantiles", 
                                                     mapRegion = continent, 
                                                     addLegend = TRUE, 
                                                     colourPalette = colorpalette,
                                                     mapTitle = title_of_map))
    sink()
  }
  
  # bar plotting function 
  bar_plot <- function(data, continent, variable, variable_type, number_of_country){
    ## create title 
    title_of_bar <- paste("Top",as.character(number_of_country),"of", variable, "for", continent, seq = " ")
    
    ## create y axis title
    if (variable == "Population"){
      y_variable_name <- paste(variable, "(millions)", seq = " ")
    } else {
      y_variabl_name <- variable
      y_variable_name <- variable}
    
    ## switch variable to variable name in the table  
    if (variable_type == "Total"){
      variable <- switch(variable, 
                         "Population" = "population_million_of", 
                         "Total infected" = "total_infected", 
                         "Total death"= "total_death")}
    else if (variable_type == "Percentage"){
      variable <- switch(variable, 
                         "Population" = "population_million_of", 
                         "Infected percentage" = "percent_infected", 
                         "Death percentage" = "percent_death")}
    
    ## plotting function within bar plot function
    ggplot_bar <- function(variable_name){
      ggplot(data, aes(x = reorder(COUNTRY_ALPHA_3_CODE, -UQ(as.name(variable))), y = UQ(as.name(variable)))) +
        geom_bar(stat = 'identity') + 
        labs(title = title_of_bar) +
        ylab(y_variable_name) +
        xlab("Country ISO ALPHA 3 code") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    }
    
    ## check continent variable
    if (continent == "World"){
      data <- data %>%
        arrange(desc(UQ(as.name(variable)))) ## variable name character need to be change as name 
      data <- head(data, number_of_country)
      ggplot_bar(variable)
    } else {
      data <- data %>%
        filter(CONTINENT_NAME == continent) %>%
        arrange(desc(UQ(as.name(variable))))
      data <- head(data, number_of_country)
      ggplot_bar(variable)
    }
  }
  

  
  # render plot
  output$country_plot <- renderPlot({
    req(input$plot_type)
    req(input$plot_continent)
    req(input$plot_variable)
    if (input$plot_type == "Map"){
      plot_map(covid_full_by_country_pop, input$plot_continent, input$plot_variable)
    } else if (input$plot_type == "Bar plot"){
      req(input$plot_variable_type)
      req(input$slider_input)
      bar_plot(covid_full_by_country_pop, input$plot_continent, 
               input$plot_variable, input$plot_variable_type, 
               input$slider_input)
    }
  })
  
  # render explore plot
  output$explore_plot <- renderPlot({
    
    a <- ggplot(covid_activity_all_year, aes(x = REPORT_DATE, y = total_new_positive_by_day, group = year, color = year)) +
      geom_line() +
      scale_x_date(date_breaks ="1 month" ,date_labels = "%Y %b") +
      scale_color_manual(values = c("#429AFF", "#FAAE28")) +
      geom_hline(yintercept = mean(covid_activity_all_year[covid_activity_all_year$year == 2020, ]$total_new_positive_by_day), color = "#429AFF") +
      geom_hline(yintercept = mean(covid_activity_all_year[covid_activity_all_year$year == 2021, ]$total_new_positive_by_day), color = "#FAAE28") +
      labs(title = "Total number of new positive cases by each day") +
      xlab("Report Date") +
      ylab("Total new positive") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            panel.background = element_blank(), 
            axis.line = element_line(color ="black"))
    
    b <- ggplot(covid_activity_all_year, aes(x = REPORT_DATE, y = total_new_death_day, group = year, color = year)) +
      geom_line() +
      scale_x_date(date_breaks ="1 month" ,date_labels = "%Y %b") +
      scale_color_manual(values = c("#429AFF", "#FAAE28")) +
      geom_hline(yintercept = mean(covid_activity_all_year[covid_activity_all_year$year == 2020, ]$total_new_death_day), color = "#429AFF") +
      geom_hline(yintercept = mean(covid_activity_all_year[covid_activity_all_year$year == 2021, ]$total_new_death_day), color = "#FAAE28") +
      labs(title = "Total number of new death cases by each day") +
      xlab("Report Date") +
      ylab("Total new death") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            panel.background = element_blank(), 
            axis.line = element_line(color ="black"))
    
    ggarrange(a,b, ncol = 2, nrow = 1)
  })
  
  # render explore table
  output$explore_table <- DT::renderDT({
    DT::datatable(covid_activity_all_year_sub,  options = list(dom = 't'))
  })
  
}
