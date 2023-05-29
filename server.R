server <- function(input, output) {
    
    # responsdId / outcome / covariatesのリストを取得し、UIをアップデートする。
    observeEvent(input$data_file, { 
        
        inFile <- input$data_file
        
        df <- read_survey(inFile$datapath, 
                          legacy = input$data_legacy)
        
        updateSelectInput(getDefaultReactiveDomain(), 
                          inputId  = "data_responseId",
                          choices  = names(df), 
                          selected = "ResponseId")
        
        updateSelectInput(getDefaultReactiveDomain(), 
                          inputId  = "data_outcome",
                          choices  = names(df),
                          selected = "")
        
        updateSelectInput(getDefaultReactiveDomain(), 
                          inputId  = "data_covariates",
                          choices  = names(df),
                          selected = "")
    }, ignoreInit = TRUE)
    
    # Step 2-1
       # データの読み込み
       # 説明変数とグループ変数を取得し、アップデート
       # データを返す
    data <- reactive({
        req(input$data_reshape)
        
        df <- q2c(data       = input$data_file$datapath,
                  prefix     = input$data_prefix,
                  id         = input$data_responseId,
                  outcome    = input$data_outcome,
                  covariates = input$data_covariates,
                  type       = input$data_type,
                  legacy     = input$data_legacy)
        
        name_df <- df |> 
            select(-(ID:Outcome), -input$data_covariates) |> 
            names()
        
        updateSelectInput(getDefaultReactiveDomain(), 
                          inputId  = "model_predictor",
                          choices  = name_df,
                          selected = name_df)
        
        updateSelectInput(getDefaultReactiveDomain(), 
                          inputId  = "model_by",
                          choices  = input$data_covariates,
                          selected = "")
        
        df
    })
    
    # Step 2-2
       # Step 2-1で読み込んだデータを出力
    output$data <- renderDataTable({
        data()
    })
    
    output$baselines <- renderUI({
        req(input$model_predictor)
        
        n  <- length(input$model_predictor)
        id <- sapply(1:n, function(i){paste0("base", i)})
        
        output = tagList()
        
        for(i in 1:n){
            choices <- data() |> 
                pull(input$model_predictor[i]) |> 
                unique()
            
            output[[i]] <- tagList()
            #output[[i]][[1]] = selectInput(inputId  = id[i], 
            #                               label    = paste0("Baseline (",
            #                                                 input$model_predictor[i], 
            #                                                 "):"), 
            #                               choices  = choices,
            #                               selected = choices[1])
            output[[i]][[1]] <- rank_list(
                input_id = id[i],
                text     = paste0(input$model_predictor[i], ":"), 
                labels   = choices
            )
        }
        
        output
    })
    
    # Step 3-1
    fit <- reactive({
        req(input$model_estimate)
        
        data <- data()
        
        for (i in 1:length(input$model_predictor)) {
            temp <- input$model_predictor[i]
            data[, temp] <- fct_relevel(unlist(data[, temp]),
                                        eval(parse(text = paste0("input$base", i))))
        }
        
        fit_formula <- data |> 
            select(Outcome, input$model_predictor) |> 
            DF2formula()
        
        data <- data |> 
            mutate(across(input$model_predictor, factor))
        
        if (input$model_estimator == "amce") {
            fit_plot <- cj(data, fit_formula,
                           id          = ~ID,
                           estimate    = "amce",
                           alpha       = input$model_alpha,
                           level_order = "descending")
            fit_tbl  <- cj(data, fit_formula,
                           id          = ~ID,
                           estimate    = "amce",
                           alpha       = input$model_alpha)
        } else if (input$model_estimator == "mm") {
            fit_plot <- cj(data, fit_formula,
                           id          = ~ID,
                           estimate    = "mm",
                           alpha       = input$model_alpha,
                           h0          = input$model_h0,
                           level_order = "descending")
            fit_tbl <- cj(data, fit_formula,
                          id          = ~ID,
                          estimate    = "mm",
                          alpha       = input$model_alpha,
                          h0          = input$model_h0)
        }
        
        
        fit <- list("est" = fit_plot,
                    "tbl" = fit_tbl)
        
        fit
    })
    
    # Step 3-2
    observeEvent(input$model_estimate, {
        updateTabsetPanel(getDefaultReactiveDomain(), 
                          "data_panel",
                          selected = "tbl")
    })
    
    # Step 3-3
    output$result_tbl <- render_gt({
        fit()[["tbl"]] |> 
            select(feature, level, estimate, std.error, p, lower, upper) |> 
            group_by(feature) |> 
            gt() |> 
            cols_label("level" = "Levels",
                       "estimate"  = case_when(input$model_estimator == "amce" ~ "AMCEs",
                                               input$model_estimator == "mm"   ~ "MMs"),
                       "std.error" = "SE",
                       "lower"     = "Lower",
                       "upper"     = "Upper") |> 
            cols_align(columns = "level", align = "left") |> 
            fmt_number(columns = estimate:upper, decimals = 3) |> 
            tab_spanner(columns = lower:upper, 
                        label = paste0((1 - input$model_alpha) * 100, "% CI"))
    })
    
    # Step 3-4
    output$result_plot <- renderPlot({
        if (input$model_estimator == "mm") {
            out <- plot(fit()[["est"]],
                        vline      = input$model_h0,
                        size       = input$plot_size)
        } else {
            out <- plot(fit()[["est"]],
                        vline      = 0,
                        size       = input$plot_size)
        }
        
        if (input$plot_facet) {
            out <- out +
                facet_wrap(~feature, 
                           nrow   = input$plot_rows,
                           scales = "free_y")
        }
        
        if (input$plot_legend == FALSE) {
            out <- out +
                theme(legend.position = "none")
        }
        
        out
        
    },
    width  = reactive(input$plot_width),
    height = reactive(input$plot_height),
    res    = 150)
}
