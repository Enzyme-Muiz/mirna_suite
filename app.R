library(BiocManager)
options(repos = BiocManager::repositories("multiMiR"))

ui<- fluidPage(
                tabsetPanel(
                  tabPanel(title= "miRNA location",
                fluidRow
               (
                column(6,align="center",tags$em(tags$h1("MicroRNA Suite"))),
                 column(6, align= "center", tags$img(height=150, width=400, src= "miRNA.jpg"))
                 ),
              fluidRow
              (column(6,
               textInput(inputId="num2", label="mirna input", value = "hsa-let-7c-3p"), 
               textOutput("selected_var")
              ),
                column(6, sliderInput(inputId= "num", label= "choose a number", 
                                      value =20, min= 1, max=100))
                
                  
                
                
               ),
               fluidRow(
              column(6,
              textInput('vec1', 'Enter a vector (comma delimited)', "hsa-miR-4665-5p"),
               actionButton(inputId = "go", label= "mirna_target")),
              column(6, 
               textOutput("oid1")
              )
               )
              ),
              tabPanel(title= "cool",
              selectInput("variable", label="select_input",choices=list('PathwayCommonsPathways'=1,'ReactomePathways'=2,'NetPath_Gene_regulation'=3,'KEGG_filtered_canonical_pathways'=4,'DisGeNet'=5,'gene_location'=6)),
              
              
               textOutput("oid2"),
              
                )
               ),
              shinyWidgets::setBackgroundColor("green"))    

server<- function(input, output)
  {
  #output$hist<- renderPlot({hist(rnorm(input$num))})
  output$selected_var<- renderText({
  df1= read.csv("./miRNAlocation.csv")
  answer<- df1[df1$Name == input$num2, ] 
  answer1<- answer[, 2]
  result<- c()
  for (i in c(1: length(answer1)))
       {insert= toString(answer1[i])
       result<- append(result, insert)}
  
  result})
  data= eventReactive(input$go, {miRNAtarget <- multiMiR::get_multimir(mirna= input$vec1, summary=FALSE)
  (unique(miRNAtarget@data[,4])[c(1:input$num)])
  })
  
  output$oid1<- renderPrint({
    print(data())
    }
  )
  
  
   }

shinyApp(ui= ui, server=server)


#sliderInput(inputId= "num", label= "choose a number", 
#           value =25, min= 1, max=100)


#rsconnect::setAccountInfo(name='rajimu',
#                          token='43EFC122B0BA5E1D8E6D8C3520880F14',
#                          secret='r0Q80ufJZpw7w/29yW7yw8tQ6Yq3f9/YTtLQopIX')




