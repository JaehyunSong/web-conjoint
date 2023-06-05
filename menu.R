menu <- sidebarPanel(
    
    h4("Data"),
    
    helpText("You can download a sample dataset from 'How to Use' page."),
    
    checkboxInput(inputId = "data_legacy", 
                  label   = "Legacy Format (~2017)", 
                  value   = FALSE),
    
    fileInput(inputId = "data_file", 
              label   = "",
              accept  = ".csv",
              width   = "100%"),
    
    textInput("data_prefix", "Prefix:", value = "F"),
    
    selectInput(inputId = "data_responseId", 
                label   = "Respondents' ID:", 
                choices = ""),
    
    selectInput(inputId  = "data_outcome", 
                label    = "Outcomes:", 
                choices  = "", 
                multiple = TRUE),
    
    selectInput(inputId  = "data_covariates", 
                label    = "Covariates:",
                choices  = "", 
                multiple = TRUE),
    
    radioButtons(inputId  = "data_type", 
                 label    = "Outcome Type:",
                 choices  = c("Choice" = "choice",
                              "Rating（未対応）" = "rating",
                              "Rank（未対応）"   = "rank"),
                 selected = "choice"),
    
    actionButton(inputId  = "data_reshape",
                 label    = "Reshape",
                 width    = "100%"),
    
    hr(),
    
    h4("Model"),
    
    selectInput(inputId  = "model_predictor", 
                label    = "Predictors:",
                choices  = "", 
                multiple = TRUE),
    
    selectInput(inputId  = "model_by", 
                label    = "BY:",
                choices  = "",
                multiple = FALSE),
    
    awesomeRadio(inputId = "model_estimator",
                 label = "Estimator:", 
                 choices = c("AMCE" = "amce", 
                             "MM"   = "mm"),
                 selected = "amce",
                 inline   = TRUE),
    
    conditionalPanel(
        condition = "input.model_by != ''",
        
        checkboxInput(inputId = "model_diff", 
                      label   = "Difference", 
                      value   = FALSE),
    ),
    
    uiOutput("baselines"),
    
    uiOutput("baselines2"),
    
    conditionalPanel(
        condition = "input.model_estimator == 'mm'",
        
        numericInput(inputId  = "model_h0", 
                     label    = "h0 (0 ~ 1):",
                     min      = 0.0,
                     max      = 1,
                     value    = 0.5),
    ),
    
    numericInput(inputId  = "model_alpha", 
                 label    = "alpha:",
                 min      = 0,
                 max      = 1,
                 value    = 0.05,
                 step     = 0.01),
    
    actionButton(inputId  = "model_estimate",
                 label    = "Estimation",
                 width    = "100%"),
    
    hr(),

    h4("Table"),
    
    numericInput(inputId = "tbl_digits",
                 label   = "Digits:",
                 value   = 3,
                 min     = 1,
                 max     = 5),
    
    switchInput(inputId    = "tbl_highlight",
                label      = "Highlight", 
                onLabel    = "On",
                offLabel   = "Off",
                width      = "100%",
                value      = FALSE),
    
    hr(),
    
    h4("Plot"),
    
    numericInput(inputId = "plot_width",
                 label   = "Width (px):",
                 value   = 600,
                 min     = 100,
                 max     = 2000),
    
    numericInput(inputId = "plot_height",
                 label   = "Height (px):",
                 value   = 800,
                 min     = 100,
                 max     = 2000),
    
    numericInput(inputId = "plot_size",
                 label   = "Point Size:",
                 value   = 2,
                 min     = 0.5,
                 max     = 5,
                 step    = 0.1),
    
    conditionalPanel(
        condition = "input.model_by != ''",
        
        switchInput(inputId    = "plot_facet",
                    label      = "Facets", 
                    width      = "100%"),
    ),
    
    conditionalPanel(
        condition = "input.plot_facet == 1",
        
        numericInput(inputId  = "plot_rows", 
                     label    = "Rows:",
                     min      = 0,
                     value    = 1),
    ),
    
    switchInput(inputId    = "plot_legend",
                label      = "Legend", 
                onLabel    = "Show",
                offLabel   = "Hide",
                width      = "100%",
                value      = TRUE),
    
    width = 2,
    
    style = "overflow-y:scroll; max-height: 1024px; position:relative;"
)
