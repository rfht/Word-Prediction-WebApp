##########################
#
# Word Prediction WebApp - ui.R
# --------------
#
# Author: Thomas Frohwein (thfr@)
# 
##########################

library(shiny)


# Start the shinyUI code block that contains everything that is displayed by shiny. Note that
# the corresponding "backend" code is in the server.R file in the same directory. 

shinyUI(fluidPage(

	# This shiny app has 3 panels: titlePanel, the sidebar, and the mainPanel. The
	# latter two are located inside the sidebarLayout() function.

	titlePanel(
		h1("Word Prediction WebApp", align = "center")
	),

	sidebarLayout(
		sidebarPanel(
			helpText("WebApp for predicting the next word. It is build upon a text corpus comprising blogs, news articles, and tweets. A language model based on Kneser-Ney smoothing is used for the prediction.")	
		),

		mainPanel(
                        textInput("input_text", label = "The first prediction will happen automatically. Please be patient.", value = "What would you like to", placeholder = "What would you like to", width = "75%"),
                        submitButton("Submit"),
                        
                        br(),

			h4("Predicted next word:"),
			em(textOutput("output_text")),
			
			br(),
			
			h5("Explanation of Special Results"),
			p("<stop>               End of a sentence"),
			p("<num>                A number"),
			p("<unknown>            No word from the training set was found"),
			
			br(),
			
			p("Note: The delay in the prediction is variable and depends on how easily the algorithm can narrow down the options."),
			
			br(),
			
			h4("Background and References"),
			p("The text corpus that was used to build this prediction model is from HC Corpora and can be found here:"),
			a(href="www.corpora.heliohost.org", "Link to the HC Corpora"),
		        p("Link to Milestone Report that shows the initial preparation and exploratory analysis of the text corpus")
		)
	)
))
