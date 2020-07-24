library(RSQLite)
library(shinythemes)

#quora <- read.csv("quora.tsv", sep = "\t")

#Qlist <- read.csv("Qlist.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Define UI for slider demo app ----
    ui <- fluidPage(theme = shinytheme("cosmo"),
        #table <- "Read these two texts and rate their similarty", 
        # App title ----
        titlePanel("Measuring Text Similarity"),
        
        
        #THIS ALLOW TO INCLUDE TEXTS FROM THE ADMIN
        #fileInput("Json", "Choose Text File",
        #          multiple = FALSE,
        #          accept = c(".json")),
        #DTOutput('tbl'),
        
        # Sidebar layout with input and output definitions ----
        sidebarLayout(
           
            h6(textOutput("save.results")),
            
            

            mainPanel(
                # Main Action is where most everything is happenning in the
                # object (where the welcome message, survey, and results appear)
                uiOutput("MainAction"),
                
                h5(textOutput("counter")),
                
                # This displays the action putton Next.
                actionButton("Click.Counter", "Next")    
            )
            
            
        ))
)


#)

Qlist <- read.csv("quora.tsv", sep = "\t")
#Qlist <- read.csv("Qlist.csv")

# Define server logic required to draw a histogram
server <- function(input,output){
    
    
    output$downloadData <- downloadHandler(
        filename = "IndividualData.csv",
        content = function(file) {
            write.csv(presults, file)
        }
    )
    
    
    #------------------------------------------------------------
    
    results <<- rep("", nrow(Qlist))
    # Name each element of the vector based on the
    # second column of the Qlist
    names(results)  <<- Qlist[,2]
    
    #------------------------------------------------------------
    
    output$counter <- 
        renderText({
            if (!file.exists("counter.Rdata")) counter <- 0
            if (file.exists("counter.Rdata")) load(file="counter.Rdata")
            counter <- counter <<- counter + 1
            
            save(counter, file="counter.Rdata")     
            paste0("Hits: ", counter)
        })
    
    #------------------------------------------------------------
    
    
    # This renderUI function holds the primary actions of the
    # survey area.
    output$MainAction <- renderUI( {
        dynamicUi()
    })
    
    #------------------------------------------------------------
    
    # Dynamic UI is the interface which changes as the survey
    # progresses. 
    
    dynamicUi <- reactive({
        # Initially it shows a welcome message. 
        if (input$Click.Counter==0) 
            return(
                list(
                    h5("Welcome to the text similarity surveil tool!"),
                    
                    textInput("caption", "State your Name", ""),
                    
    
                    
                    h6("by Yassir Al Bahri") #inspired in the work of Francis Smart
                )
            )
        # Once the next button has been clicked once we see each question
        # of the survey.
        if (input$Click.Counter>0 & input$Click.Counter<=nrow(Qlist))  
            return(
                list(
                    h5("Read the following pair of texts and rate their similarty."),
                    
                    h3("Pair",input$Click.Counter,":"),
                    
                    h4(" "),
                    
                    h4(verbatimTextOutput("question1")),
                    
                    h4(verbatimTextOutput("question2")),
                    
                    radioButtons("survey", "Please Select:", 
                                 c("Prefer not to answer", option.list()))
                    
                    
                    
                )
            )
        
        # Finally we see results of the survey as well as a
        # download button.
        if (input$Click.Counter>nrow(Qlist))
            return(
                list(
                    
                    
                    
                    
                    
                    #h4("View aggregate results"),
                    #tableOutput("surveyresults"),
                    #downloadButton('downloadData', 'Download Individual Results'),
                    #br(),
                    #h6("Haven't figured out how to get rid of 'next' button yet"),
                    h4("Thanks for taking the survey!")
                )
            )    
    })
    
 
    
    
    #------------------------------------------------------------
    
    # This reactive function is concerned primarily with
    # saving the results of the survey for this individual.
    

    
    output$save.results <- renderText({
        
        
        
        # After each click, save the results of the radio buttons.
        if ((input$Click.Counter>0)&(input$Click.Counter>!nrow(Qlist)))
            try(results[input$Click.Counter] <<- input$survey)
        # try is used because there is a brief moment in which
        # the if condition is true but input$survey = NULL
        
        # If the user has clicked through all of the survey questions
        # then R saves the results to the survey file.
        if (input$Click.Counter==nrow(Qlist)+1) {
            if (file.exists("survey.results.Rdata")) 
                load(file="survey.results.Rdata")
            if (!file.exists("survey.results.Rdata")) 
                presults<-NULL
            #df <<- rownames(presults)

            presults <- presults <<- rbind(presults, results)
            #presults <<- cbind(User = 2, presults)
            rownames(presults) <- rownames(presults) <<- 
                paste("", 1:nrow(presults))
            
           # presults <- cbind(User = input$caption, presults)
            
            #setattr(presults, "rownames", c("a, b, c, d, e"))
            #presults <- cbind(User = input$caption, presults)
            
            save(presults, file="survey.results.Rdata")
            
            
            
            
            
            username <<- input$caption
            save(username, file="names.results.Rdata")
    
            #df <- rbind(df, username)
            #rownames(presults) <<- df
            
            # Set up database
            
            survey <- dbConnect(SQLite(), "survey.sqlite")
            dbWriteTable(survey, "data", as.data.frame(presults), overwrite = TRUE , row.names = TRUE)
            dbReadTable(survey, "data")
            table <- dbReadTable(survey, "data")
            dbDisconnect(survey)
            
            userid <- dbConnect(SQLite(), "userid.sqlite")
            dbWriteTable(userid, "id", as.data.frame(username), append = TRUE, row.names = FALSE)
            dbReadTable(userid, "id")
            iduser <- dbReadTable(userid, "id")
            dbDisconnect(userid)
            
            
            finaltab <- cbind(iduser, table)
            final <- dbConnect(SQLite(), "final.sqlite")
            dbWriteTable(final, "tab", as.data.frame(finaltab), overwrite = TRUE, row.names = FALSE)
            dbDisconnect(final)
 
            
        }
        # Because there has to be a UI object to call this
        # function I set up render text that distplays the content
        # of this funciton.
        ""
    })
    
    #--------------------------------------------
    

    
    
    #------------------------------------------------------------
    
    # This function renders the table of results from the
    # survey.
    
    #output$surveyresults <- renderTable({
    #    t(summary(presults))
    #})
    
    #------------------------------------------------------------
    
    
    # This renders the data downloader
    #output$downloadData <- downloadHandler(
    #    filename = "IndividualData.csv",
    #    content = function(file) {
    #        write.csv(presults, file)
    #    }
    #)
    
    # The option list is a reative list of elements that
    # updates itself when the click counter is advanced.
    option.list <- reactive({
        qlist <- Qlist[input$Click.Counter,4:ncol(Qlist)]
        # Remove items from the qlist if the option is empty.
        # Also, convert the option list to matrix. 
        as.matrix(qlist[qlist!=""])
    })
    
    output$question1 <- renderText({
        paste0(
            
            "Text 1",": ", 
            Qlist[input$Click.Counter,2]
            
        )
    })
    
    output$question2 <- renderText({
        paste0(
            
            
            "Text 2",": ", 
            Qlist[input$Click.Counter,3]
        )
    })
    
    
}    


# Run the application 
shinyApp(ui = ui, server = server)

