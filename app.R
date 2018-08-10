#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(BFTheme)
library(stringr)
library(data.table)
library(ggplot2)
library(extrafont)
library(plotly)

load("data/cma.tech.concentration.rds")
load("data/city_occ_emp_data.RDS")
load("data/canada_occ_emp.RDS")
load("data/cma_tech_2006.RDS")
load("data/diversity_cma.RDS")
load("data/tech_premium.RDS")
load("data/cma_list.RDS")
load("data/tech_gender.RDS")
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("State of Canada's Tech Workers"),
   tags$style("body{background:linear-gradient(to bottom right,#8AD4DF,#14365D);
              color:#fff}"),
   # Sidebar with a slider input for number of bins 
   navbarPage("Navigation",
              tabPanel("Part 1: General Trends",
                         # Show a plot of the generated distribution
                         fluidRow(align="center",
                                  selectInput("province",
                                              "Select a Province:",
                                              choices = c("British Columbia"="BC","Alberta"="AB","Saskachewan"="SK","Manitoba"="MB",
                                                          "Ontario"="ON","Quebec"="QC","New Brunswick"="NB","Prince Edward Islands"="PE",
                                                          "Nova Scotia"="NS","Newfoundland and Labrador"="NL","Nunavut"="NU","Northwest Territories"="NT",
                                                          "Yukon"="YK"),selected="ON"),
                                  div(style="display: inline-block;vertical-align:middle; height:34px",
                                      p("Tell me more about tech jobs in ")),
                                  div(style="display: inline-block;vertical-align:middle; horizontal-align:left; height: 34px",
                                      uiOutput("CMA_choice")),
                                  plotlyOutput("cmatot", height="600px")
                                ), #EndFluidRow

                       fluidRow(style = "margin-right: 50px ; margin-left: 50px",
                                align="center",
                                tags$style(".selectize-input{border-style: none;
                                                border-bottom-style: solid;
                                                border-radius: 0px ;
                                                padding: 0px 0px 0px 0px;
                                                box-shadow: none;
                                                background: transparent;
                                                background-color: transparent;
                                                color: white}
                                            .selectize-input.dropdown-active{
                                                box-shadow: none;
                                                border-radius: 0px;
                                                background: transparent;
                                                background-color: transparent;
                                                color: white}
                                            .selectize-input.focus{
                                                box-shadow: none;
                                                border-radius: 0px;
                                                border-color: #8AD4DF;
                                                background-color: transparent;
                                                background: transparent;
                                                color: white}
                                           .selectize-input.full{
                                                background-color:transparent;
                                                background:transparent;
                                                color: white}
                                           .select-input.input-active{
                                                background-color:transparent;
                                                background: transparent;
                                                color:white
                                    }") #End tagstyle
                                ), #End FluidRow
                       
                       fluidRow(style="font-size: 25px; margin-right: 50px ; margin-left: 50px",
                                align = "center",
                                p("Technology sector in Canada is diverse - collectively, almost",
                                span(style='color: #F7941E',"1 million"),
                                "workers work in a technology occupation in Canada, forming 5% of the Canadian workforce.
                                This web-app accompanies our main report: State of Canadian Tech Workforce.")
                                ), #EndFluidRow
                       
                       fluidRow(align="center",
                                style="font-size: 18px; margin: 50px",
                                textOutput("CMA_chosen_2")
                                ), #EndFluidRow
                       
                       fluidRow(style = "margin-right: 50px; margin-left: 50px",
                                column(width=2,
                                       radioButtons("pct_or_tot",choices = c("Absolute","Relative"),selected = "Absolute",label="Show me in")), #End Column
                                column(style="border-right: 2px dashed #14365D",
                                       align = "center",
                                       plotlyOutput("cmatopocc",height="800px"),
                                       width=5), #End Column
                                
                                column(align="center",
                                       plotlyOutput("canadatopocc",height="800px"),
                                       width=5) #End Column
                                ), #EndFluidRow
                       
                       fluidRow(style="margin-right: 50px; margin-left: 50px; font-size: 18px",
                                htmlOutput("cmatotnumtext")
                                ), #EndFluidRow
                       
                       fluidRow(style = "margin-right: 50px; margin-left: 50px; font-size: 18px",
                                align = "center",
                                "You might be wondering, how has tech talent changed over the past 10 years?"
                                ), #End FluidRow
                       
                       fluidRow(style = "margin-right: 50px; margin-left: 50px; font-size: 18px; margin-top: 30px",
                                column(width=10,
                                       htmlOutput("cmain2006"))
                                ) #End FluidRow

                       ),#End first tab panel
              
              tabPanel("Part 2: Income and Diversity",
                       fluidRow(align = "center",
                                p(style="font-size: 18px","Now, we take a deeper look into the pay and diversity of tech workers in the biggest cities in the country")
                                ), #End FluidRow
                       
                       fluidRow(align = "center",
                                selectInput("cma_div","Select a Metropolitan Area:",
                                            choices = cma_list[,GEO.NAME])
                                ), #EndFluidRow
                       
                       fluidRow(align = "center",
                                plotlyOutput("tech.pre.scatter")
                                ), #EndFluidRow
                       
                       fluidRow(align = "center",
                                style = "font-size: 18px",
                                htmlOutput("tech.premium.text")
                                ), #EndFluidRow
                       
                       fluidRow(align = "center",
                                plotOutput("gender.waffle.cma")
                                ), #EndFluidRow
                       
                       fluidRow(align = "center",
                                style = "font-size: 18px",
                                htmlOutput("gender.cma.text")
                                ), #EndFluidRow
                       
                       fluidRow(align = "center",
                                plotlyOutput("female.pay.cma"))
                       ) #End second tab panel
              ) #EndNavbarPage

   ) #EndFluidPage


# Define server logic required to draw a histogram
server <- function(input, output) {
  #Text of the chosen city
  output$CMA_choice <- renderUI(
    selectInput("cma",
                label=NULL,
                choice = unique(BFTheme::cma.data[stringr::str_sub(ID,1,2)==province.data[abbrev==input$province,code],Name])))
  #Interactive column plot of all the cities with the chosen city highlighted
  output$CMA_chosen_2 <- renderText(
    paste("This web-app examines the state of tech workers in ", input$cma))

   output$cmatot <- renderPlotly({
     noc.dem.tech.map[,cma.focus:="0"]
     noc.dem.tech.map[ALT.GEO.CODE %in% cma.data[Name==input$cma,ID],cma.focus:="1"]
     column.pct <- BFTheme::plot.column.bf(noc.dem.tech.map[tech==1],"pct","GEO.NAME",
                                           order.bar = "ascending",
                                           group.by="cma.focus",
                                           colours = BFTheme::set.colours(2,categorical.choice = c("light.blue","pink")),
                                           label.unit = "%",
                                           col.invert = TRUE
                                           ) + ggplot2::guides(fill="none",colour = "none") +ggplot2::theme(axis.text.x = element_blank())
     print(ggplotly(column.pct,tooltip=c("y","x"),showlegend=FALSE,showscale=FALSE))
   })
   #Top occupations both Absolute and Relative
   output$cmatopocc <- renderPlotly({
     noc.dem.city.plot <- noc.dem.city[ALT.GEO.CODE %in% cma.data[Name==input$cma,ID]]
     if(nrow(noc.dem.city.plot)>10){
       noc.dem.city.plot <- noc.dem.city.plot[(.N-9):.N]
     }
     if(input$pct_or_tot=="Absolute"){
       print(ggplotly(BFTheme::plot.column.bf(noc.dem.city.plot,"TOT","NOC",
                               order.bar = "ascending",
                               colours = BFTheme::set.colours(1,categorical.choice = "light.blue"),
                               col.invert = TRUE)))
     }
     else{
       print(ggplotly(BFTheme::plot.column.bf(noc.dem.city.plot,"pct","NOC",
                               order.bar = "ascending",
                               label.unit = "%",
                               colours = BFTheme::set.colours(1,categorical.choice = "light.blue"),
                               col.invert = TRUE)))
     }

   })
   #Canada's top occupation both absolute and relative
   output$canadatopocc <- renderPlotly({
     noc.dem.canada.plot <- noc.dem.canada[(.N-9):.N]
     if(input$pct_or_tot == "Absolute"){
       print(ggplotly(BFTheme::plot.column.bf(noc.dem.canada.plot,"TOT","NOC",
                               order.bar="ascending",
                               colours = BFTheme::set.colours(1,categorical.choice = "light.blue"),
                               col.invert = TRUE)))
     }
     else{
       print(ggplotly(BFTheme::plot.column.bf(noc.dem.canada.plot,"pct","NOC",
                               order.bar = "ascending",
                               label.unit = "%",
                               colours = BFTheme::set.colours(1,categorical.choice="light.blue"),
                               col.invert = TRUE)))
     }

   })
   #Change in the tech workforce in text
   output$cmatotnumtext <- renderUI(p("In 2016, there were ",
                                            noc.dem.tech.map[GEO.NAME==input$cma & tech==1,V1],
                                            " tech workers in",
                                            paste(input$cma,
                                            ". This means that ",sep=""),
                                            paste(round(100*noc.dem.tech.map[GEO.NAME==input$cma & tech==1,V1]/891720,2),
                                            "% of tech workers in Canada lived in ",sep=""),
                                            paste(input$cma,".",sep="")))
   
   #Workforce in 2006 vs 2016 comparison text
   output$cmain2006 <- renderUI({
     difference <- noc.dem.tech.map[GEO.NAME==input$cma & tech==1,V1] - noc.2006.city[tech=="Tech Occupation" & GEO.CODE==cma.data[Name==input$cma,ID],V1]
     if(abs(difference)/noc.dem.tech.map[GEO.NAME==input$cma & tech==1,V1]<0.01 | abs(difference)<50){
       
       p("In 2006, there were",
             span(style="color: #F7941E",noc.2006.city[tech=="Tech Occupation" & GEO.CODE==cma.data[Name==input$cma,ID],V1]),
             "tech workers in",
             paste(input$cma,".",sep="")," This means that the number of tech workers have been relatively unchanged over the past 10 years.")
     }
     else{
       if(difference<0){
         p("In 2006, there were",
               span(style="color: #F7941E",noc.2006.city[tech=="Tech Occupation" & GEO.CODE==cma.data[Name==input$cma,ID],V1]),
               "tech workers in",
           paste(input$cma,".",sep="")," This means that the number of tech workers have decreased over the past 10 years.")
       }
       else{
         p("In 2006, there were",
               span(style="color: #F7941E",noc.2006.city[tech=="Tech Occupation" & GEO.CODE==cma.data[Name==input$cma,ID],V1]),
               "tech workers in",
           paste(input$cma,".",sep="")," This means that the number of tech workers have increased over the past 10 years.")         
       }
     }
   })
     
   ################ Output for second tab
   output$tech.pre.scatter <- renderPlotly({
     tech_premium[GEO.NAME=="Canada",color:="0"]
     tech_premium[GEO.NAME!="Canada",color:="1"]
     tech_premium[GEO.NAME==input$cma_div,color:="2"]
     scatter.tech.plot <- BFTheme::plot.scatter.bf(tech_premium,"non.tech.pay","tech.pay",group.by="color",
                                                   colours = set.colours(3,categorical.choice = c("green","dark.blue","pink")),
                                                   unit.x = "$",
                                                   unit.y = "$",
                                                   x.axis = "Average pay for non-tech jobs",
                                                   y.axis = "Average pay for tech jobs")
     ggplotly(scatter.tech.plot)
   })
   
   output$tech.premium.text <- renderUI({
     p("In ",
       paste(input$cma_div,",",sep=""),
       " tech workers were paid ",
       paste("$",round(tech_premium[GEO.NAME==input$cma_div,tech.pay]),sep=""),
       "; this was ",
       paste("$",round(tech_premium[GEO.NAME==input$cma_div,tech.pay-non.tech.pay]),sep=""),
       "more than non-tech workers.")
   })
   
   output$gender.waffle.cma <- renderPlot({
     female.share <- round(tech_gender[GEO.NAME==input$cma_div,share_tech])
     plot.waffle.bf(c("Male"=100-female.share,"Female"=female.share))
   }, bg = "transparent")
   
   output$gender.cma.text <- renderUI({
     female.share.can <- round(tech_gender[GEO.NAME=="Canada",share_tech])
     female.share.cma <- round(tech_gender[GEO.NAME==input$cma_div, share_tech])
     p("In Canada ",
       paste(female.share.can,"%",sep=""),
       "of tech workers are female. This is compared to ",
       paste(female.share.cma,"%",sep=""),
       paste("in ",input$cma_div,sep=""))
   })
   
   output$female.pay.cma <- renderPlotly({
     tech_gender[GEO.NAME=="Canada",color:="0"]
     tech_gender[GEO.NAME!="Canada",color:="1"]
     tech_gender[GEO.NAME==input$cma_div,color:="2"]
     ggplotly(BFTheme::plot.column.bf(tech_gender,"V2","GEO.NAME",
                                      group.by="color",
                                      order.bar = "ascending",
                                      col.invert = TRUE,
                                      colours=set.colours(3,categorical.choice = c("green","dark.blue","pink")),
                                      label.unit = "$"))
   })

     


}

# Run the application 
shinyApp(ui = ui, server = server)

