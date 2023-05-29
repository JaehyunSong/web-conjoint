Help_Page <- tabPanel(
  "How to use",
  h3("How to use"),
  tags$ul(
    tags$li("同じ名前の水準が複数の属性にあるとエラーが出るぞ"), 
    tags$li("2017年までのQualtricsはLegacyだぞ"), 
    tags$li("Legacyはヘッダーが2行分、現行版は3行分だぞ"), 
  ),
  h3("Sample Data"),
  tags$ul(
    tags$li("以下のダミーデータを使ってください。"), 
    tags$li(tags$a(href = "https://www.jaysong.net/software/Data/webconjoint_sample.csv",
                   "https://www.jaysong.net/software/Data/webconjoint_sample.csv")), 
  ),
  h3("Update Plan"),
  tags$ul(
    tags$li("BY引数への対応"), 
    tags$li("特定のTaskのみを対象とした推定"), 
    tags$li("Tableのカスタマイズ"), 
    tags$li("Rating/Rankへの対応"), 
    tags$li("UI改善（shinyWidgetsとかおしゃれだよね...）"),
  ),
  h3("Repository"),
  tags$ul(
    tags$li(tags$a(href = "https://github.com/JaehyunSong/web-conjoint",
                   "https://github.com/JaehyunSong/web-conjoint"))
  ),
)

About_Author <- tabPanel(
  "About author",
  h3("Jaehyun Song, Ph.D."),
  tags$ul(
    tags$li(tags$b("Affiliation: "), "Faculty of Informatics, Kansai University, Japan"), 
    tags$li(tags$b("Position: "), "Associate Professor"),
    tags$li(tags$b("Homepage: "),a(href = "https://www.jaysong.net",
                                   "https://www.jaysong.net")), 
    tags$li(tags$b("E-mail: "), a(href = "mailto:song@kansai-u.ac.jp",
                                  "song@kansai-u.ac.jp")), 
    tags$li(tags$b("Github: "), a(href = "https://github.com/JaehyunSong",
                                  "https://github.com/JaehyunSong"))
  ),
)