library(shiny)

shinyUI(
  pageWithSidebar(
    headerPanel("That's some catch, that Catch-22", windowTitle = "Catch-22!"),
    
    sidebarPanel(
      helpText(div(HTML("<u>Filter Controls:</u>"))),
      sliderInput("chapters", "Chapter Numbers:", min=1, max=42, value=c(1,42)),
      conditionalPanel(condition = "input.tabName == 'Character Appearances' | input.tabName == 'Character Co-Occurrences'",
                       sliderInput("characters", "Character Rank:", min=1, max=28, value=c(1,28)),
                       br(),
                       helpText(div(HTML("<u>Display Controls:</u>"))),
                       fluidRow(
                         column(6,
                                radioButtons("colours", "Colour by:", c('Frequency','Cluster'))),
                         column(6, 
                                radioButtons("orders", "Order by:", c('Frequency','Cluster','Alphabet')))),
                       conditionalPanel(condition = "input.colours == 'Cluster' | input.orders == 'Cluster'",
                                        sliderInput("clusters", "Number of Clusters:", min=2, max=8, value=5))),
      conditionalPanel(condition = "input.tabName == 'Characteristic Words'",
                       sliderInput("numwords", "Top x Words:", min=1, max=8, value=5),
                       selectInput("wordtype", "Word Type:", c("Adjective"='ADJ',"Adverb"='ADV',"Noun"='N',"Verb")),
                       conditionalPanel(condition = "input.wordtype == 'Verb'",
                                        selectInput("verbtype", "Verb Type:", c("All","Present Tense"='V',"Past Tense"='VD',"Present Participle"='VG',"Past Participle"='VN'))),
                       helpText(div(HTML("<u>Display Controls:</u>"))),
                       radioButtons("wordplot", "Plot type:", c('Bar','Area'))),
      br(),
      helpText(div(HTML("<u>Information:</u>"))),
      conditionalPanel(condition = "input.tabName == 'Mediterranean Travels'",
                       helpText("This visualization shows the locations that are mentioned in the novel.")),
      conditionalPanel(condition = "input.tabName == 'Character Appearances'",
                       helpText(div(HTML("This visualization shows mentions of the main characters throughout the book. It was inspired by Jeff Clark's <a href=\"http://neoformix.com/2013/NovelViews.html\" target=\"_blank\">Novel Views</a>. Clusters are based on a community-finding algorithm that seeks to find characters that appear together often.")))),
      conditionalPanel(condition = "input.tabName == 'Character Co-Occurrences'",
                       helpText(div(HTML("This visualization shows when the main characters are mentioned in the same chapter together. It was inspired by Mike Bostock's <a href=\"http://bost.ocks.org/mike/miserables/\" target=\"_blank\">Co-occurrence</a>. Clusters are based on a community-finding algorithm that seeks to find characters that appear together often.")))),
      conditionalPanel(condition = "input.tabName == 'Characteristic Words'",
                       helpText("This visualizaition shows the main words used in the vicinity of a mention of Yossaran. Word frequencies are normalized by the number of mentions Yossanian recieves in a given chapter.")),
      br(),
      helpText(div(HTML("Visit <a href=\"http://trevorstephens.com/\" target=\"_blank\">Trevor Stephens' blog</a>."))),
      width = 3),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Mediterranean Travels", plotOutput("travels", height="100%")),
        tabPanel("Character Appearances", plotOutput("appearances", height="100%")),
        tabPanel("Character Co-Occurrences", plotOutput("cooccurrences", height="100%")),
        tabPanel("Characteristic Words", plotOutput("pos", height="100%")),
        id="tabName"))))
