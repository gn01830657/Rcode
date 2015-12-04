library(shiny)
library(shinydashboard)
library(httr)
library(XML)
library(jiebaR)
library(qdap)

ui <- dashboardPage(
	dashboardHeader(title = "PTT"),
	dashboardSidebar(
		sidebarMenu(
			menuItem("GetUrl", tabName = "get", icon = icon("dashboard")),
			menuItem("Jieba", tabName = "jieba", icon = icon("th")),
			menuItem("Word Cloud", tabName = "wordcloud", icon = icon("th"))
		)
	),
	dashboardBody(
		tabItems(
			tabItem(tabName = "get",
				fluidRow(
					box(
						textInput("text", "Url:")	
					),
					box(
						verbatimTextOutput("tabset1Selected")
					)
				)
			)
		)
	)
)

server <- function(input, output) {	
	output$tabset1Selected <- renderText({	
		post <- unlist(xpathApply(content(GET(input$text,set_cookies(over18=1)),as = 'parsed'), "//div[@id='main-content']", xmlValue)) 
		post <- gsub("[A-Za-z0-9]", "", post)
		strip(post, apostrophe.remove = FALSE, lower.case = FALSE)
		kill = worker()
		killed <- kill <= post
    })
	
}
