library(BiocManager)
options(repos = BiocManager::repositories("multiMiR"))

ui<- fluidPage(
                tabsetPanel(
                  tabPanel(title= "miRNA location",
                fluidRow
               (
                column(6,align="center", tags$div(class= "fade",
                  tags$em(tags$h1("MicroRNA Suite")))),
                 #column(6, align= "center", tags$img(height=150, width=400, src= "miRNA.jpg"))
                 ),
              sidebarLayout
              (

                sidebarPanel(
               textInput(inputId="num2", label="mirna input", value = "hsa-let-7c-3p")),
               mainPanel(align= "center",
               textOutput("selected_var"))
              ),
                
                
                  
                
                
               
               sidebarLayout(
              sidebarPanel(
              textInput('vec1', 'mirna', "hsa-miR-4665-5p"),
              sliderInput(inputId= "num", label= "choose a number", 
                                      value =20, min= 1, max=100), 
               
               actionButton(inputId = "go", label= "mirna_target")),
              mainPanel(
               textOutput("oid1")
              )
               )
              ),
              tabPanel(title= "miRNA_set_analysis",
              sidebarLayout(
              sidebarPanel( textInput(inputId="num2", label="mirnas_input", value = c("hsa-let-7c-3p,hsa-miR-125b-5p")), 
  
              selectInput("variable", label="select_pathway",choices=list('PathwayCommonsPathways'=1,'ReactomePathways'=2,'NetPath_Gene_regulation'=3,'KEGG_filtered_canonical_pathways'=4,'DisGeNet'=5,'gene_location'=6)),
              actionButton(inputId = "go2", label= "overrepresented_pathways")),
              
              mainPanel(
               textOutput("oid2")),
              
                
               )

                )
               ),
              shinyWidgets::setBackgroundColor("green"),

              tags$head(
              tags$style(HTML("
                    #oid1{
                      color: white;
                      background: blue;
                      font-family: 'Times New Roman', Times, serif;
                      font-size: 10px;
                      font-style: italic;
                    }
                    #selected_var2 {
                      color: blue;
                      background: orange;
                      font-family: 'Times New Roman', Times, serif;
                      font-size: 12px;
                      font-weight: bold;
                    }
                    #rendertext1 {
                      color: red;
                      background: yellow;
                      font-family: Arial, Helvetica, sans-serif;
                      font-size: 19px;
                    }
                    
                    .fade {opacity: 1;
                      transition: opacity .25s ease-in-out;
                      -moz-transition: opacity .25s ease-in-out;
                      }

                    .fade:hover{
                      opacity:0;
                    }


                    .fade{
                      
                       transition: background-color;
                      background-color: red;

                    }

                    .fade h1 {
                      font-family: monospace; /* Web-safe typewriter-like font */
                      overflow: hidden; /* Ensures the content is not revealed until the animation */
                      border-right: .15em solid orange; /* The typwriter cursor */
                      white-space: nowrap; /* Keeps the content on a single line */
                      margin: 0 auto; /* Gives that scrolling effect as the typing happens */
                      letter-spacing: .15em; /* Adjust as needed */
                      animation: 
                        typing 3.5s steps(30, end),
                        blinking-cursor .5s step-end infinite;
                    }

                    /* The typing effect */
                    @keyframes typing {
                      from { width: 0 }
                      to { width: 100% }
                    }

                    /* The typewriter cursor effect */
                    @keyframes blinking-cursor {
                      from, to { border-color: transparent }
                      50% { border-color: orange; }
                    }


                    
                    ")))
)

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
  
  output$oid1<- renderText({
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




