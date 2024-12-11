## Shiny UI component for the Dashboard

dashboardPage(
  
  dashboardHeader(title="Exploring AIHW & ABS data with R & Shiny Dashboard", titleWidth = 650, 
                  tags$li(class="dropdown",tags$a(href="https://github.com/", icon("github"), "Source Code", target="_blank"))
                  ),
  
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
      # sidebar: About
      menuItem("About", tabName = "About", icon = icon("circle-info")),
      # sidebar: Geographic Distribution
      menuItem("Geographic Distribution", tabName = "geoDist", icon=icon("map"), selected = TRUE),
      # condition items for prevalence
      conditionalPanel("input.sidebar == 'geoDist' && input.t2 == 'g19'", 
        selectInput(inputId = "dataSrc", 
          label = "Select data source", 
          choices = g19_or_g20, 
          selected = "G19")),
      
      conditionalPanel("input.sidebar == 'geoDist' && input.t2 == 'g19'", 
        selectInput(inputId = "gender" , 
          label ="Select gender" , 
          choices = gender, 
          selected = "P")),
      
      conditionalPanel("input.sidebar == 'geoDist' && input.t2 == 'g19'", 
        selectInput(inputId = "ageGroup" , 
          label ="Select age group" , 
          choices = ageGroup, 
          selected = "0_14")),
      
      conditionalPanel("input.sidebar == 'geoDist' && input.t2 == 'g19'", 
        selectInput(inputId = "cond" , 
          label ="Select condition" , 
          choices = condition, 
          selected = "Arthritis")),
      
      conditionalPanel("input.sidebar == 'geoDist' && input.t2 == 'g19'", 
        selectInput(inputId = "state" , 
          label ="Select state" , 
          choices = states, 
          selected = "New South Wales")),
      
      # condition items for Low urgent care
      conditionalPanel("input.sidebar == 'geoDist' && input.t2 == 'lucmap'", 
                       selectInput(inputId = "luc_type",
                                   label = "Low Urgency Care",
                                   choices = hours,
                                   selected = "All-hours")),
      
      conditionalPanel("input.sidebar == 'geoDist' && input.t2 == 'lucmap'", 
                       selectInput(inputId = "state" , 
                                   label ="Select state" , 
                                   choices = states, 
                                   selected = "New South Wales")),
      
      # sidebar: correlation matrix
      menuItem("Correlation", tabName = "corrmatrix", icon=icon("chart-line")),
      # G19 correlation matrix parameter
      conditionalPanel("input.sidebar == 'corrmatrix' && input.t3 == 'corrmatrix_g19'", 
                       selectInput(inputId = "which_state3",
                                   label = "State",
                                   choices = states2,
                                   selected = "NSW")),
      # G20 correlation matrix parameter
      conditionalPanel("input.sidebar == 'corrmatrix' && input.t3 == 'corrmatrix_g20'", 
                       selectInput(inputId = "which_state2",
                                   label = "State",
                                   choices = states2,
                                   selected = "NSW")),
      # condition items for scatter plot
      conditionalPanel("input.sidebar == 'corrmatrix' && input.t3 == 'scatterplot'", 
                       selectInput(inputId = "cond_name",
                                   label = "Chronic Medical Condition",
                                   choices = g19_condition,
                                   selected = "Heart_disease")), 
      
      conditionalPanel("input.sidebar == 'corrmatrix' && input.t3 == 'scatterplot'", 
                       selectInput(inputId = "luc_type",
                                   label = "Low Urgency Care",
                                   choices = hours,
                                   selected = "All-hours")),
      
      conditionalPanel("input.sidebar == 'corrmatrix' && input.t3 == 'scatterplot'", 
                       selectInput(inputId = "age",
                                   label = "Age Range",
                                   choices = ageGroup2,
                                   selected = "Tot")),
      
      conditionalPanel("input.sidebar == 'corrmatrix' && input.t3 == 'scatterplot'", 
                       selectInput(inputId = "which_state",
                                   label = "State",
                                   choices = states2,
                                   selected = "NSW")),
      # sidebar: Data
      menuItem("Dataset", tabName = "Data", icon = icon("database"))
    )
  ),
  
  dashboardBody(
    tabItems(
      ## First tab: About 
      tabItem(tabName = "About", 
        tabBox(id="t1", width = 12, 
          tabPanel("About", icon=icon("info"),
            fluidRow(column(width = 8, div(includeMarkdown("about.md"), 
                                           align="justify")))
          )
        )
      ),  
      
      # Second Tab: Geographic Distribution
      tabItem(tabName = "geoDist", 
        tabBox(id="t2",  width=12, side = "left",
          tabPanel("Condition Prevalence", value="g19", id="g19", 
                   withSpinner(tmapOutput(outputId = "censusMap")),
                   tmapOutput(outputId = "censusSubset")),
          tabPanel("Low Urgency Care", value="lucmap", id="lucmap", 
                   withSpinner(tmapOutput(outputId = "aihwPlot")),
                   tmapOutput(outputId = "aihwSubset"))
        ),
      ),
      
      # Third Tab: correlation matrix
      tabItem(tabName = "corrmatrix", 
        tabBox(id="t3",  width=12, side = "left",
          tabPanel("Scatterplot", value="scatterplot", id="scatterplot", withSpinner(plotOutput(outputId = "scatterplot"))),
          tabPanel("Amount of Conditions Matrix", value="corrmatrix_g20", id="scatterplot_20", withSpinner(plotlyOutput(outputId = "heatmapPlot_g20"))),
          tabPanel("Specific Conditions Matrix", value="corrmatrix_g19", id="scatterplot_19", withSpinner(plotlyOutput(outputId = "heatmapPlot_g19")))
        ),
      ),
      
      ## Fourth tab: data 
      tabItem(tabName = "Data", 
              tabBox(id="t5", width = 12, 
                     tabPanel("AIHW Data", dataTableOutput("dataT"), icon = icon("table")), 
                     tabPanel("AIHW Structure", verbatimTextOutput("structure"), icon=icon("uncharted")),
                     tabPanel("ABS Data", dataTableOutput("dataT2"), icon = icon("table")), 
                     tabPanel("ABS Structure", verbatimTextOutput("structure2"), icon=icon("uncharted"))
              )
      )
    )
  )
)

  
  
