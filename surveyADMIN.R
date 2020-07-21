library(shiny)
library(shinydashboard)
library(shinythemes)
library(RSQLite)
library(irr)
library(dplyr)


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cosmo"),

    # Application title
    titlePanel("Text Similarity Admin Panel"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
             
        fluidRow(
            
            column(5,
                            selectInput("pair", h3("Select Text Pair"), 
                                        choices = list("Pair 1" = 1, "Pair 2" = 2,
                                                       "Pair 3" = 3, "Pair 4" = 4, 
                                                       "Pair 5" = 5, "Pair 6" = 6,
                                                       "Pair 7" = 7, "Pair 8" = 8,
                                                       "Pair 9" = 9, "Pair 10" = 10), selected = 1)),)
        
        ),
        
        
        
        
        # Show a plot of the generated distribution
        mainPanel(
            h4("Overview"),
            #br(),
            
            h6(textOutput("n")),
            h6(textOutput("q")),
            h6(textOutput("kappaf")),
            br(),
            #downloadButton('downloadData', 'Download Results'),
            
            
            
            #tableOutput("table"),
            h6(textOutput("selectedpair")),
            h6(verbatimTextOutput("question1")),
            h6(verbatimTextOutput("question2")),
            h4(textOutput("rowkappa")),
            plotOutput("plot"),
            
            #tableOutput("tbls")
            #dataTableOutput("tbls")
            DT::dataTableOutput("mytable")
            
            
        )
    )
)

#Read the evaluators data
final <- dbConnect(SQLite(), "final.sqlite")
table <- dbReadTable(final, "tab")
tbls <- dbReadTable(final, "tab")
table <- t(table)
table <- table [(3:12),]
n <- ncol(table)
q <- nrow(table)
kappa <- kappam.fleiss(table)
dbDisconnect(final)


colnames(tbls) <- paste("Pair ", seq_along(tbls), sep="") #USE _ INSTEAD OF SPACE AFTER PAIR?
colnames(tbls)[1] <- "Name"
colnames(tbls)[2] <- "User"


#Read the original questions
Qlist <- read.csv("quora.tsv", sep = "\t")



server <- function(input, output) {

    
    
    output$downloadData <- downloadHandler(
        filename = "IndividualData.csv",
        content = function(file) {
            write.cssdv(presults, file)
        }
    )
    
    
    #-------------------------------------------------------------
    
    output$n <- renderText(
        paste("The total number of raters is: ", n)
    )
    
    output$q <- renderText(
        paste("The total number questions (subjects) is: ", q)
    )
    
    output$kappaf <- renderText({
        
        paste("The Overall Fleiss-Kappa value is: ", kappa$value)
        
    })
    

    
    #-------------------------------------------------------------
    
    output$selectedpair <- renderText({
        
        paste("Pair ", input$pair, ":")
})
    
    #-------------------------------------------------------------

    output$mytable = DT::renderDataTable({
        tbls
    })

    

    
    #-------------------------------------------------------------
    
    
    output$plot <- renderPlot({
        #tbl <- with(table, table(table[,3]))
        #tbl <- with(table, table(table[,as.numeric(input$pair)]))
        barplot(table(table[as.numeric(input$pair),]), beside = TRUE, legend = FALSE, xlab="Ratings", ylab="Number of raters", )
        
    })
    
    #-------------------------------------------------------------
    
    #Fleiss Kappa for each pair of questions
    
    output$rowkappa <- renderText({
    
    
    
    srow <- table[as.integer(input$pair),]
    #kapparow <- kappam.fleiss(t(srow))
    
    srow <- t(srow)
    
    sroww <- as.data.frame(table(srow))
    
    PKappa <- sroww$Freq *  sroww$Freq
    
    kapparow <- sum(PKappa) / (ncol(table) * (ncol(table)-1))
    
    
    paste("The Fleiss-Kappa value for Pair ", input$pair, "is: ", kapparow)
    
    })
    
    #-------------------------------------------------------------
    
    #Show the chosen pair of texts to review
    
    output$question1 <- renderText({
        paste0(
            
            "Text 1",": ", 
            Qlist[input$pair,2]
            
        )
    })
    
    
    output$question2 <- renderText({
        paste0(
            
            
            "Text 2",": ", 
            Qlist[input$pair,3]
        )
    })
    
    #---------------------------------------------------------------
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
