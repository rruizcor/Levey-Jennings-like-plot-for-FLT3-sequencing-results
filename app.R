
######################################################
###Title: "APP for FLT3 QA Project"
###Author: RRC
###Date: Version 1.0 "July 25, 2016"
###Output: Shiny App
######################################################


library(shiny)
library(ggplot2)

ui = shinyUI(fluidPage(
  #Header Title Panel  
  titlePanel(title = "Scatter plot of FLT3 - D835 Samples"),
  
  sidebarLayout(
    #Sidebar panel    
    sidebarPanel(
      
      fileInput("file", "Import File"),
      
      helpText("Default max. file size is 9MB"),
      
      tags$hr(),
      
      h4(helpText("Select the type of file to import (.csv recommended)")),
      
      radioButtons(inputId = "sep", label = "", choices = c(Comma=",", Semicolon=";", Tab="\t", Space=""), selected = ","),
      
      br(),
      h4(helpText("Select the type of file to export plot")),
      
      radioButtons(inputId = "var", label = "", choices = list("pdf", "png")),
      
      br(),
      uiOutput("tb")
      
    ),
    #Main Panel
    mainPanel(
      
      plotOutput("plot"),
      
      downloadButton(outputId = "down", label = "Download the Plot"))
    

    )
  )
)



####################

options(shiny.maxRequestsize = 9*1024^2)


server = shinyServer(
  function(input, output){
    
#Code to replace logo with Dataframe    
    output$tb = renderUI({
      if(is.null(submitteddata()))
        h5("Powered by", tags$img(src="XXXX.png", heigth = 200, width = 500))
      else
        tabsetPanel(tabPanel("Data", tableOutput("table")))
    })
    
#.csv File
    submitteddata = reactive({
      file1 = input$file
      if(is.null(file1)){return()}
      df = read.csv(file1$datapath)# to read .csv file as dataframe
      df = df[, c(2, 4, 11, 13, 15)]# to eliminate non-wanted columns
      df = df[!grepl("ITD", df$Sample.Name),]# to eliminate ITD samples
    })

#Data tab displaying dataframe
    output$table = renderTable({
      if(is.null(submitteddata())){return()}
      submitteddata()
    })  
    
#Graph
    output$plot = renderPlot({
     
      plotD835 = (ggplot(aes(x = submitteddata()$Sample.Name, y = submitteddata()$Peak.Area.1), data=submitteddata()) + 
              scale_x_discrete() + 
              scale_y_continuous(limits=c(min(100000),max(300000))) + 
              labs(x = "Sample IDS", y = "Peak Area") +
              ggtitle("FLT3 - D835 samples") +
              geom_linerange(ymin = (mean(subset(submitteddata(), submitteddata()$Sample.Name=="D835_NEGCTRL")$Peak.Area.1)), ymax = (mean(subset(submitteddata(), submitteddata()$Sample.Name=="D835_POSCTRL_1NG")$Peak.Area.1)), color="#ff033e", size = 17, alpha = 0.1) +
              geom_point(colour = "firebrick", size = 3) +
                theme(plot.title=element_text(size=22, face="bold", color = "#494949"),
                      panel.background = element_rect(fill = "#f0f0f0"),
                      plot.background = element_rect(fill = "#d2d2d2"),
                      axis.title.x = element_text(color="white", size = 18, face = "bold"),
                      axis.title.y = element_text(color="white", size = 18, face = "bold"),
                      axis.text.x = element_text(face="bold", color="#494949", size=12, angle=60, hjust = 1),
                      axis.text.y = element_text(face="bold", color="#494949", size=10)))
      print(plotD835)
    })
#Code to save plot as .pdf
    output$down = downloadHandler(
      #Specify file name
      filename = function() { 
        # FLT3.pdf
        # FLT3.png  
        paste("D835", input$var, sep='.') },
      
      content = function(file) {
        #open device
        if(input$var == "pdf")
          pdf(file, width = 10, height = 7)
        else
          png(file, width = 10, height = 7)
        #create plot
        print(ggplot(aes(x = submitteddata()$Sample.Name, y = submitteddata()$Peak.Area.1), data=submitteddata()) + 
          scale_x_discrete() + 
          scale_y_continuous(limits=c(min(100000),max(300000))) + 
          labs(x = "Sample IDS", y = "Peak Area") +
          ggtitle("FLT3 - D835 samples") +
            geom_linerange(ymin = (mean(subset(submitteddata(), submitteddata()$Sample.Name=="D835_NEGCTRL")$Peak.Area.1)), ymax = (mean(subset(submitteddata(), submitteddata()$Sample.Name=="D835_POSCTRL_1NG")$Peak.Area.1)), color="#ff033e", size = 12, alpha = 0.1) +
          geom_point(colour = "firebrick", size = 3) +
          theme(plot.title=element_text(size=22, face="bold", color = "#494949"),
                  panel.background = element_rect(fill = "#f0f0f0"),
                  plot.background = element_rect(fill = "#d2d2d2"),
                  axis.title.x = element_text(color="white", size = 18, face = "bold"),
                  axis.title.y = element_text(color="white", size = 18, face = "bold"),
                  axis.text.x = element_text(face="bold", color="#494949", size=12, angle=60, hjust = 1),
                  axis.text.y = element_text(face="bold", color="#494949", size=10)))
        #close device
        dev.off()
        
        
    })

  })    

#To run the app
shinyApp(ui = ui, server = server)


