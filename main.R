main <- mainPanel(
    tabsetPanel(id = "data_panel",
                tabPanel("Data",  DTOutput("data"),          value = "shaped"),
                tabPanel("Table", gt_output("result_tbl"),   value = "tbl"),
                tabPanel("Plot",  plotOutput("result_plot"), value = "plot")),
    
    width = 10
)