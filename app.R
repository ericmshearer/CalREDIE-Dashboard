library(DT)
library(bslib)
library(readr)
library(tidyr)
library(dplyr)
library(shiny)
library(plotly)
library(bsicons)

#set boostrap theme
my_theme = bs_theme(
  bg = "#ffffff",
  fg = "#000000",
  primary = "#113a72",
  secondary = "#0072B2",
  success = "#009E73",
  warning = "#D3D3D3",
  danger = "#EE4B2B"
)

#helper function
decade <- function(x){
  
  x = ifelse(x < 10, "0-9",
             ifelse(x >= 80, "Over 80",
                    ifelse(x >= 10, paste0(substr(x,1,1),"0-", substr(x,1,1),"9"),
                           "Missing/Unknown")))
  
  x = factor(x, levels = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","Over 80","Missing/Unknown"))
  
  return (x)
}


ui <- fluidPage(
  theme = my_theme,
  sidebarLayout(
    sidebarPanel(
      width = 2,
      h3(strong("Control Panel")),
      fileInput("file1", "Choose TSV File"),
      checkboxGroupInput(inputId = "inclusion_criteria", label = "Inclusion Criteria:", choices = c("Confirmed","Probable","Suspect"), selected = c("Confirmed","Probable")),
      br(),
      checkboxGroupInput(inputId = "year_list", label = "Calendar Year:", choices = 2013, selected = 2013),
      br(),
      p("Warning: this data app only ingests complete UDF extracts from the CalREDIE Data Distribution Portal (DDP).")
    ),
    mainPanel(
      width = 10,
      br(),
      htmlOutput("condition", style = "font-size: 48px; text-align: center; font-weight: bold;"),
      hr(),
      fluidRow(
        column(width = 4, align = "center", value_box(title = "Total Cases", value = textOutput("total_cases"), showcase = bs_icon("people-fill"), theme_color = "primary")),
        column(width = 4, align = "center", value_box(title = "Total Hospitalizations", value = textOutput("hospitalizations"), showcase = bs_icon("hospital"), theme_color = "warning")),
        column(width = 4, align = "center", value_box(title = "Total Deaths", value = textOutput("deaths"), showcase = bs_icon("patch-exclamation-fill"), theme_color = "danger"))
      ),
      hr(),
      tabsetPanel(
        tabPanel("Line List", br(), DT::dataTableOutput("contents")),
        tabPanel("Demographics", br(),
                 radioButtons(inputId = "type", label = "Case type:", choices = c("All","Hospitalized","Deaths"), select = "All", inline = TRUE),
                 fluidRow(
                   column(width = 3, tableOutput("gender")),
                   column(width = 3, tableOutput("race")),
                   column(width = 3, tableOutput("age"))
                 )
        ),
        tabPanel("Epidemic Curve", br(), plotlyOutput("epi_curve", height = "600px"))
      )
    )
  )
)

server <- function(input, output, session){
  
  #load user input file
  
  imported_data <- reactive({
    
    inFile <- input$file1
    
    if(is.null(inFile)) return(NULL)
    
    data <- read_delim(inFile$datapath, na = "", delim = "\t", show_col_types = FALSE) %>%
      mutate(across(c("DtEpisode","DtOnset","DtLabCollect","DtDeath"), ~as.Date(., format = "%m/%d/%Y"))) %>%
      mutate(Year = as.numeric(strftime(DtEpisode, "%Y"))) %>%
      select(IncidentID, Disease, Age, Gender = Sex, Race, Ethnicity, City, RStatus, DtEpisode, DtOnset, DtLabCollect, contains("HOSPHOSPITALIZED"), PtDiedIllness, DtDeath, Year) %>%
      mutate(
        Gender = case_when(
          Gender == "F" ~ "Female",
          Gender == "M" ~ "Male",
          Gender == "TF" ~ "Transgender woman",
          Gender == "TM" ~ "Transgender man",
          is.na(Gender) ~ "Missing/Unknown",
          TRUE ~ "Other/Unspecified")
      )
    
  })
  
  final_data <- reactive({
    
    if(is.null(imported_data())) return(NULL)
    
    imported_data() %>%
      filter(RStatus %in% input$inclusion_criteria) %>%
      filter(Year %in% input$year_list)
    
  })
  
  #populate year list from available data
  
  observe({
    
    if(is.null(imported_data())) return(NULL)
    
    year_choices <- imported_data() %>%
      filter(RStatus %in% input$inclusion_criteria) %>%
      select(Year) %>%
      distinct(Year) %>%
      arrange(Year) %>%
      pull()
    
    updateCheckboxGroupInput(session, "year_list", choices = year_choices, selected = year_choices[1])
    
  })
  
  #pull disease name for title
  
  output$condition <- renderText({
    
    if(is.null(final_data())) return("No data uploaded.")
    
    name <- unique(
      final_data() %>%
        select(Disease) %>%
        pull(Disease)
    )
    
    name = ifelse(length(name) > 1, trimws(gsub("-.*|,.*", "", name[1])), name) #for conditions like WNV
    
  })
  
  #value boxes
  
  output$total_cases <- renderText({
    
    if(is.null(final_data())){
      cases = 0}
    else{
      cases = final_data() %>%
        nrow()
    }
    
  })
  
  output$hospitalizations <- renderText({
    
    if(is.null(final_data())){
      admits = 0}
    else{
      admits = final_data() %>%
        select(hospitalized = contains("HOSPHOSPITALIZED")) %>%
        filter(hospitalized %in% c("Y")) %>%
        nrow()
    }
    
  })
  
  output$deaths <- renderText({
    
    if(is.null(final_data())){
      deaths = 0}
    else{
      deaths = final_data() %>%
        drop_na(DtDeath) %>%
        nrow()
    }
    
  })
  
  #table - line list
  
  output$contents <- DT::renderDataTable({
    
    if(is.null(final_data())) return(NULL)
    
    DT::datatable(final_data() %>% select(-Disease, -Year), rownames = FALSE, options = list(searching = FALSE))
    
  })
  
  #plot - epidemic curve
  
  output$epi_curve <- renderPlotly({
    
    if (is.null(final_data())) return(NULL)
    
    build <- final_data() %>%
      mutate(
        episode_month = as.Date(paste0(match(strftime(DtEpisode, "%B"), month.name),"/1/", strftime(DtEpisode, "%Y")), format = "%m/%d/%Y")
      )
    
    data_storage <- list()
    
    year_list <- unique(build$Year)
    
    for(year in year_list){
      
      year_temp <- build %>%
        filter(Year == year)
      
      from_date = as.Date(paste(year, "01", "01", sep = "/"))
      to_date = as.Date(paste(year, "12", "01", sep = "/"))
      
      shell <- data.frame(episode_month = seq(from = from_date, to = to_date, by = "months")) %>%
        mutate(episode_year = year) %>%
        left_join(count(build, episode_month), by = "episode_month") %>%
        mutate(n = replace_na(n, 0))
      
      data_storage[[year]] <- shell
      
    }
    
    data_storage <- do.call(rbind, data_storage)
    rownames(data_storage) <- NULL
    
    year_num <- length(unique(data_storage$episode_year))
    
    data_storage %>%
      group_by(episode_year) %>%
      do(p = plot_ly(., x = ~episode_month, y = ~n, type = "bar", name = ~episode_year)) %>%
      subplot(nrows = year_num, shareX = FALSE, shareY = TRUE)
    
  })
  
  #tables - demographics
  
  output$gender <- renderTable({
    
    if (is.null(final_data())) return(NULL)
    
    if(input$type == "All"){
      final_data() %>%
        count(Gender) %>%
        mutate(Percent = round(n/sum(n) * 100, digits = 1))
    } else {
      if(input$type == "Hospitalized"){
        final_data() %>%
          select(Gender, hospitalized = contains("HOSPHOSPITALIZED")) %>%
          filter(hospitalized %in% c("Y")) %>%
          count(Gender) %>%
          mutate(Percent = round(n/sum(n) * 100, digits = 1))
      } else {
        final_data() %>%
          drop_na(DtDeath) %>%
          count(Gender) %>%
          mutate(Percent = round(n/sum(n) * 100, digits = 1))
      }
    }
    
  })
  
  output$race <- renderTable({
    
    if (is.null(final_data())) return(NULL)
    
    if(input$type == "All"){
      final_data() %>%
        mutate(`Race/Ethnicity` = ifelse(Ethnicity == "Hispanic or Latino", Ethnicity, Race)) %>%
        count(`Race/Ethnicity`) %>%
        mutate(Percent = round(n/sum(n) * 100, digits = 1))
    } else {
      if(input$type == "Hospitalized"){
        final_data() %>%
          mutate(`Race/Ethnicity` = ifelse(Ethnicity == "Hispanic or Latino", Ethnicity, Race)) %>%
          select(`Race/Ethnicity`, hospitalized = contains("HOSPHOSPITALIZED")) %>%
          filter(hospitalized %in% c("Y")) %>%
          count(`Race/Ethnicity`) %>%
          mutate(Percent = round(n/sum(n) * 100, digits = 1))
      } else {
        final_data() %>%
          drop_na(DtDeath) %>%
          mutate(`Race/Ethnicity` = ifelse(Ethnicity == "Hispanic or Latino", Ethnicity, Race)) %>%
          count(`Race/Ethnicity`) %>%
          mutate(Percent = round(n/sum(n) * 100, digits = 1))
      }
    }
    
  })
  
  output$age <- renderTable({
    
    if (is.null(final_data())) return(NULL)
    
    if(input$type == "All"){
      final_data() %>%
        mutate(`Age Group` = decade(Age)) %>%
        count(`Age Group`) %>%
        mutate(Percent = round(n/sum(n) * 100, digits = 1))
    } else {
      if(input$type == "Hospitalized"){
        final_data() %>%
          select(Age, hospitalized = contains("HOSPHOSPITALIZED")) %>%
          filter(hospitalized %in% c("Y")) %>%
          mutate(`Age Group` = decade(Age)) %>%
          count(`Age Group`) %>%
          mutate(Percent = round(n/sum(n) * 100, digits = 1))
      } else {
        final_data() %>%
          drop_na(DtDeath) %>%
          mutate(`Age Group` = decade(Age)) %>%
          count(`Age Group`) %>%
          mutate(Percent = round(n/sum(n) * 100, digits = 1))
      }
    }
    
  })
  
}

shinyApp(ui, server)