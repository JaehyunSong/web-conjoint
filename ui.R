source("menu.R")
source("main.R")
source("support.R")

navbarPage(
  title       = "Web Conjoint 0.0.2",
  selected    = "main", 
  position    = "static-top",
  collapsible = TRUE, 
  inverse     = FALSE, 
  theme       = shinythemes::shinytheme("sandstone"),
  
  tabPanel("Main", value = "main", sidebarLayout(menu, main)),
  navbarMenu("More...", Help_Page, About_Author)
)

