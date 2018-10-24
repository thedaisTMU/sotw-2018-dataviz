library(shiny) #Main package to load
library(stringr) #Aid in string oprations
library(data.table) #Aid in data manipulation
library(ggplot2) #Aid in plotting
library(plotly) #Aid in interactive plotting
library(scales) #Aid in formatting some scales


#Set up theme elements
source("definitions.R")
#Load up functions to plot first panel graphs
source("first_panel_graph_functions.R")
#Load up functions to plot second panel graphs
source("second_panel_graph_functions.R")

# Define UI for application that draws a histogram
ui <- fluidPage(theme="style.css",
                tags$head(tags$link(rel="shortcut icon", href="www/favicon.ico")),
                

   
   # Application title
   div(class="header",
       style = "background: #fff; margin-left: 0%; margin-right: 0%; font-size: 16px !important; max-width: unset; padding-right: 10%; padding-left: 10%; padding-bottom: 20px; padding-top:16px; border-bottom: 6px solid #d1236c",
       includeHTML("www/header.html")),
   

   titlePanel("",windowTitle = "Brookfield Institute - State of Canada's Tech Workers 2018"), #Name to show up on browser. Favicon is set in css document
   div(style = "margin-right: 10%; margin-left: 10%; padding-bottom: 15px", h1("State of Canada's Tech Sector, 2018")),
   

   navbarPage("",
              tabPanel("Key Statistics (Cities and Towns)",

                         fluidRow(div(class="sectiontitlewrapper",h2("Canada's Tech Workers: Key Statistics")),
                                  div(style="padding-top: 30px; padding-bottom: 30px",
                                      p("Who are the faces behind Canada's tech sector? How many are there? Where do they work? 
                                        Is there a diversity problem in tech? This data visualization, accompanying Brookfield Institute's 
                                        first report in the 2018 State of Canada's Sector series, will answer these questions."))
                                 ), #End FluidRow
                         fluidRow(align="center",
                                  selectInput("province",
                                              "List the cities and towns in the Province/Territory of ",
                                              choices = c("British Columbia"="BC","Alberta"="AB","Saskachewan"="SK","Manitoba"="MB",
                                                          "Ontario"="ON","Quebec"="QC","New Brunswick"="NB","Prince Edward Islands"="PE",
                                                          "Nova Scotia"="NS","Newfoundland and Labrador"="NL","Northwest Territories"="NT"),selected="ON"),
                                  div(style="display: inline-block;vertical-align:middle; height:34px",
                                      p("Tell me more about tech jobs in ")),
                                  div(style="display: inline-block;vertical-align:middle; horizontal-align:left; height: 34px",
                                      uiOutput("CMA_choice"))
                                ), #EndFluidRow
                       
                       fluidRow(align = "center",
                                plotlyOutput("cmatot",height="600px")
                                ), #EndFluidRow
                       
                       fluidRow(class="alignedrow",
                                style="padding-top: 40px",
                                column(style="margin-right: 4%",
                                       width=7, 
                                       p("One in twenty workers in Canada
                                         work in one of 32 tech occupations, from engineers to programmers, totaling almost",
                                          span(style='color: #e24585',"1 million tech workers.")), #End p
                                       p("Depending on where they live, tech workers have different jobs, earnings and backgrounds.")
                                       ), #End Column
                                
                                column(width=4,
                                       style = "background: #d1236c; color: #fff; margin-bottom: 20px; padding-top: 15px; padding-bottom: 15px",
                                       div(h4("Tech Occupation Definition"),
                                           p("We define tech occupations to be occupations that require high competency in tech skills. 
                                              We use the National Occupational Classification (NOC) to define our occupations, and US's O*NET Skills taxonomy to look at each occupation's tech intensity."
                                             ) #End p
                                           ) #End Div
                                       ) #End Column

                                ), #EndFluidRow
                       
                       
                       fluidRow(align="center",
                                style="font-size: 28px; padding-top: 50px; padding-bottom: 50px",
                                textOutput("CMA_chosen_2")
                                ), #EndFluidRow
                       
            #START SHOWING TOPLINE NUMBERS FROM HERE

                       fluidRow(class="sectiontitlewrapper",
                                h3("Tech Employment by Occupation")
                                ), #End FluidRow
                       
                       fluidRow(column(width=2,
                                       class="column-border-right",
                                       radioButtons("pct_or_tot",
                                                    choiceValues = c("Total Tech Workers",
                                                                "Concentration of Tech Workers"),
                                                    choiceNames = list(HTML('<i class="fa fa-circle-o social-icons" style = "font-size:1.1em"></i><i class="fa fa-dot-circle-o" style = "font-size:1.1em"></i> <p style="padding-left:10px; font-size: 14px">Total Tech Workers</p>'),
                                                                    HTML('<i class="fa fa-circle-o social-icons" style="font-size:1.1em"></i><i class="fa fa-dot-circle-o" style = "font-size:1.1em"></i> <p style="padding-left:10px; font-size: 14px">Concentration of Tech Workers</p>')),
                                                    selected = "Total Tech Workers",
                                                    label="Show me"),
                                       fluidRow(style = "margin-left:inherit; margin-right:inherit;padding-left:0; padding-right:0",
                                                div(style="background: #d1236c; padding-top: 15px; padding-bottom: 15px; margin-bottom: 7px; color: #fff; padding-left: 10px; padding-right: 10px",
                                                    h4(style="font-size:14px","Top 10 Occupations Locally"),
                                                    p(style="font-size: 0.7em","These occupations are different from the top 10 tech occupations by employment nationally.")),
                                                div(style="background: #072b49; padding-top: 15px; padding-bottom: 15px; color: #fff; padding-left: 10px; padding-right: 10px",
                                                    h4(style="font-size:14px","Top 10 Occupations Canada-wide"),
                                                    p(style="font-size: 0.7em","These occupations are also represented in the top 10 tech occupations by employment nationally")))), #End Column
                                column(align = "center",
                                       p("Local Tech Talent"),
                                       plotlyOutput("cmatopocc",height="500px"),
                                       width=5), #End Column
                                
                                column(align="center",
                                       style = "padding-bottom: 20px",
                                       p("National Tech Talent"),
                                       plotlyOutput("canadatopocc",height="500px"),
                                       width=5) #End Column
                                ), #EndFluidRow
                       
                       fluidRow(class="alignedrow",
                                column(width=9,htmlOutput("cmatotnumtext"),
                                       htmlOutput("cmain2006")),
                                column(width=3,img(src="test_image_1.png",style="width:100%; min-width:160px"))
                                ), #EndFluidRow
            
        #START SHOWING EDUCATION STUFF FROM HERE

                       fluidRow(class="sectiontitlewrapper",
                                h3("Educational Attainment of Tech Workers")
                                ), #End FluidRow
                       
                       fluidRow(class="alignedrow",
                                column(width=3,
                                       div(h4("How to read this graph"),
                                           p(style="font-size:0.7em","Each", span(style="font-family:rooneysansmedium !important","blue line") ,"represents a city, and the share of tech workers in that city with a particular educational level.
                                             The",
                                             span(style="font-family:rooneysansmedium !important; color: #e24585","magenta line"),
                                             " shows the educational distribution of tech workers in the city you selected, and the", 
                                             span(style="font-family:rooneysansmedium !important; color: #82C458","green line"), 
                                             "shows the educational distribution of tech workers in Canada overall."))),
                                column(width=9,
                                       class = "column-border-left",
                                       style = "・max-width: 700px",
                                       plotlyOutput("educ.column",height="600px")
                                       )#, #End Column
                                
                                ), #EndFluidRow
                      
                      fluidRow(class="alignedrow",
                               column(width=3,
                                      img(src="tech_image_2.png",style="width:100%; min-width:160px")),
                               column(width=8,
                                      htmlOutput("educ.text"),
                                      p("In the [report](https://brookfieldinstitute.ca/), we dove into workers' postsecondary degree specialization.
                                        Though we do not present this data at the city/town level, tech workers came from a variety of backgrounds,
                                        with a disproportionate share coming from traditional STEM programs such as Mathematics, Computer Science, and Physics. A sizeable share of the 
                                        tech workforce specialized in Business, Commerce, and Management related fields."))
                               ), #End FluidRow
                      
        #START SHOWING COMPARISON FROM HERE

                      fluidRow(class="sectiontitlewrapper",
                               h3("Compare between cities/towns")
                      ), #EndFluidRow
                      
                      
                      fluidRow(p("Now that you have a bit of a better grasp of tech workers across the nation, 
                                 use the tool below to compare key statistics for up to 5 cities and towns in Canada.")
                      ), #EndFluidRow
                
                      fluidRow(align = "center",
                               selectizeInput(inputId = "comparison.cma",
                                              label = "Type the name of up to 5 Cities/Towns above the line",
                                              choices = sort(unique(cma.data[,Name])),
                                              selected = NULL,
                                              multiple = TRUE,
                                              options = list(maxItems = 5))
                               ), #EndFluidRow
                      

                      fluidRow(align = "center",
                               tableOutput("comparison.topline")
                               ), #EndFluidRow
        
                      fluidRow(align = "center",
                               downloadButton("download.topline.specific","Download Current Table")
                               )#, #EndFluidRow


                       ),#End first tab panel
  #SECOND TAB PANEL
              tabPanel("Income and Diversity (Metropolitan Areas)",
                       fluidRow(div(class="sectiontitlewrapper",h2("Canada's Tech Workers: Income and Diversity")),
                                div(style="padding-top: 30px; padding-bottom: 30px",
                                  "Diversity has been a focal point of many conversations surrounding tech. Let's explore how different identities 
                                  affect the experiences of working in a tech occupation for Canada's metropolitan areas.")
                                ), #End FluidRow
                       
                       fluidRow(align = "center",
                                selectInput("cma_div","Select a Metropolitan Area:",
                                            choices = sort(cma_list[,GEO.NAME]))
                                ), #EndFluidRow
                       
                       fluidRow(class="alignedrow",
                                column(width = 7,
                                       plotlyOutput("tech.pre.scatter",height = "500px")),
                                column(width = 5,
                                       p("Consistently, tech workers across Canada received higher average pay than non-tech workers.
                                         This pay differential was present in all demographic and geographic groups."),
                                       htmlOutput("tech.premium.text"))
                                
                                ), #EndFluidRow
                       
      #GENDER DIVERSITY STARTS HERE
      fluidRow(class="sectiontitlewrapper",
               h3("Gender diversity")
               
      ), #End FluidRow
      
      fluidRow(column(width = 5,
                      p("Labour force participation among",
                        span(class="tooltiphelp","women",span(class="tooltiptexthelp",
                                                              "In the 2016 Canadian Census, Statistics Canada only collected data pertaining to an individual's sex which may differ from one's gender. We ackowledge this issue but also believe that many insights pertaining for the female population is also relevant for women population in Canada.")
                        ),"in Canada has been steadily increasing. In 2016, women made up 48 percent of the labour market."),
                      p("Despite these trends, in 2016 there were 584,000 more men in tech occupations than women. 
                        Men were almost four times more likely than women to work in a tech occupation—7.8 percent 
                        of men in the labour market worked in tech occupations compared to 2.1 percent of women.")),
               
               column(width = 7,
                      align = "left",
                      tableOutput("gen.comp.table"))
               
      ), #EndFluidRow
      fluidRow(class="alignedrow",
               column(width=5,
                      tableOutput("gen.comp.mf.table")),
               column(width=7,
                      align = "left",
                      htmlOutput("gen.gap.text"))
               
      ), #EndFluidRow
      
      fluidRow(class="alignedrow",
               column(width = 4,
                      img(src="tech_pic_3.png", style="width:100%; min_width: 180px")),
               column(width = 8, 
                      p("When it comes to the gender pay gap in Tech,
                        almost every major city in Canada has a higher gap than the Canadian average."),
                      htmlOutput("pay.gap.text"),
                      p("In our main report, we analyze this gender pay gap and see that this gap is persistent across education,
                        visible minority and immigration status, as well as age."))
        ), #End FluidRow
      
      fluidRow(align = "center",
               plotlyOutput("gen.pay.gap.cma",height="600px")
      ), #End FluidRow

                       

      

                       
                       
    #VISIBLE MINORITY STARTS HERE
    
    fluidRow(class="sectiontitlewrapper",
             h3("Visible Minority(VM)")
    ), #End FluidRow
    
    
    fluidRow(class="alignedrow",
             column(width=7,
                    class="column-border-right",
                    htmlOutput("vismin.comp.table",class = "inverse")),
             column(width=5,
                    p("Almost one-third of Canada's tech workers identify as visible minorities (31.9%).
                                         8.5 percent or almost one in twelve visible minorities workers worked in a tech occupation, totalling 271,000 people"),
                    htmlOutput("vismin.sumtext", class="inverse"))
    ), #End FluidRow
    
    fluidRow(class="alignedrow",
             column(width=5,
                    class="column-border-right",
                    p("Yet, there are important differences between different visible minority groups, and with non-visible minorities.
                      In this application, we focus broadly on the idea of visible minorities. For a detailed look at how different groups
                      (such as those who identify as Black, Chinese, or South Asian) compare to each other, check out our main report.")
                    ), #End column
             
            column(width = 7,
                   tableOutput("vismin.comp.nonvismin.table")
                  ) #End column
    ), #End FluidRow
    
    fluidRow(class="alignedrow",
             column(width=4,
                    class="popoutbox",
                    div(h4("Visible Minority Identities"),
                        p('2016 Canadian Census uses the term visible minority to refer to whether a person belongs to a visible
                          minority group as defined by the Employment Equity Act, which defines visible minorities as 
                          "persons, other than Aboriginal peoples, who are non-Caucasian in race or non-white in colour."'
                          ) #End p
                        ) #End div
                    ), #End Column
             column(width=7,
                    htmlOutput("vismin.paygap.text"))
             ),
    
    fluidRow(plotlyOutput("vismin.paygap",height="600px")
    ), #End FluidRow
    

                       
                       
                       
                       
        #COMPARISON BEGINS HERE
    
    fluidRow(class="sectiontitlewrapper",
             h3("Compare between metropolitan areas")
    ), #EndFluidRow
    
    fluidRow(p("Now that you've gotten a better idea of diversity in tech, you may want to compare between
                                  different metropolitan areas. Use our tool to compare the topline numbers for up to 5 metropolitan areas, 
               and download the result.")
    ), #EndFluidRow
    
    fluidRow(align = "center",
             selectizeInput(inputId = "comparison.cma.div",
                            label = "Type the name of up to 5 metropolitan areas above the line",
                            choices = sort(unique(cma_list[,GEO.NAME])),
                            selected = NULL,
                            multiple = TRUE,
                            options = list(maxItems = 5))
    ), #EndFluidRow
    
    fluidRow(align = "center",
             tableOutput("comparison.diversity")
    ), #EndFluidRow
    
    fluidRow(align = "center",
             downloadButton("download.div.specific","Download Current Table")
    ) #EndFluidRow
    
    
    
              ) #End second tab panel
                       
        
              ), #EndNavbarPage
  
  fluidRow(class="sectiontitlewrapper",
           h3("About this data visualization")),
  fluidRow(style="font-size:1.1em",p("This data visualization was created by the Brookfield Institute for Innovation and Entrepreneurship
             to accompany the State of Canada's Tech Sector 2018: Tech Workers report. [See the source code](https://brookfieldinstitute.ca/)."),
           p("The report was authored by Asher Zafar, Viet Vu, and Creig Lamb. The data visualization was developed by Viet Vu using R and Shiny."),
           p("This report, as well as the data visualization, would not have been possible without support from the following individuals:
             Sean Zohar, Jessica Thomson, Nisa Malli, Annalise Huynh, Andrew Do, Sarah Doyle, as well as our reviewers."),
           p("The main data source for the report and the visualization is the 2016 and 2006 Canadian long form census."),
           p("Please send any feedback, comments, or questions to",a(href="mailto:brookfield.institute@ryerson.ca","brookfield.institute@ryerson.ca")),
           
           div(style="margin-top:25px;border-top: 1px solid #e3e3e3",p(style="font-style: italic","For media enquiries, please contact", 
                 a(href="https://brookfieldinstitute.ca/team/coralie-dsouza","Coralie D’Souza"), 
                 "Director of Communications, Events + Community Relations at the 
                 Brookfield Institute for Innovation + Entrepreneurship.")
               ) #End Div
           ), #End FluidRow
  
  fluidRow(style="background: #072b49; margin-left: 0%; margin-right: 0%; font-size: 12px !important; max-width: unset; padding-top:64px; padding-bottom:24px",
           div(class="footer",
               style = "padding-left: 5%; padding-right: 5%",
                 includeHTML("www/site_footer.html")))

   ) #EndFluidPage



server <- function(input, output) {
  #Text of the chosen city
  output$CMA_choice <- renderUI(
    selectInput("cma",
                label=NULL,
                choice = sort(unique(cma.data[stringr::str_sub(ID,1,2)==province.data[abbrev==input$province,code],Name]))))

  #Just the CMA
  output$CMA_chosen_as_input <- renderUI(
    paste(input$cma,", ",input$province,sep="")
  )
  
  cma.reactive <- reactive({
    p <- "Arnprior"
    if(!is.null(input$cma)){
      p <- input$cma
    }
    p
  })
  
   #Text for focusing on a particular CMA
   output$CMA_chosen_2 <- renderText({
    paste("Let's learn about ", input$cma,", ",input$province,"'s tech workers",sep="")})

   #Interactive column plot of all the cities with the chosen city highlighted
   output$cmatot <- renderPlotly({
     plot.cmatot(cma.reactive())
     })
   
   #Top occupations both Absolute and Relative
   output$cmatopocc <- renderPlotly(plot.cmaocc(cma.reactive(),input$pct_or_tot))
   
   #Canada's top occupation both absolute and relative
   output$canadatopocc <- renderPlotly(plot.canocc(input$pct_or_tot))
   
   #Change in the tech workforce in text
   output$cmatotnumtext <- renderUI({div(p("In 2016, ",
                                      span(style="color:#e24585",
                                           paste(comma(noc.dem.tech.map[ALT.GEO.CODE %in% cma.data[Name %in% input$cma, ID] & tech==1,V1]),
                                            " tech workers worked and lived in ",
                                            paste(input$cma,
                                                  ".",sep=""))),
                                      " This represents ",
                                      span(style = "color:#e24585",
                                           paste(round(100*noc.dem.tech.map[ALT.GEO.CODE %in% cma.data[Name %in% input$cma, ID] & tech==1,V1]/891720,2),
                                                 " percent of all tech workers in Canada.",sep="")
                                           ),
                                      "In the past 10 years, 
                                      over 180,000 workers joined the tech workforce in Canada and 180,000 more are predicted to 
                                      join in the next 10 years according to", 
                                      a(href="http://occupations.esdc.gc.ca/sppc-cops/w.2lc.4m.2@-eng.jsp",
                                        "Economics and Social Development Canada"),
                                      "."
                                       ))
                                      })

   
   #Workforce in 2006 vs 2016 comparison text
   output$cmain2006 <- renderUI({
     if(nrow(noc.2006.city[tech=="Tech Occupation" & GEO.CODE %in% cma.data[Name %in% input$cma,ID]])==0){
       p("Unfortunately,",paste(input$cma,"'s",sep="") ,"population was too low in 2006 to have publicly published data.")
     }
     else{
       difference <- noc.dem.tech.map[GEO.NAME %in% input$cma & tech==1,V1] - noc.2006.city[tech=="Tech Occupation" & GEO.CODE %in% cma.data[Name %in% input$cma,ID],V1]
       if(abs(difference)/noc.dem.tech.map[GEO.NAME %in% input$cma & tech==1,V1] < 0.01 | abs(difference)<50){
         
         p("Looking at the Census in 2006, we find that there were",
           span(style="color: #e24585",
                paste(comma(noc.2006.city[tech=="Tech Occupation" & GEO.CODE %in% cma.data[Name %in% input$cma,ID],V1]),
                      " tech workers in ",
                      input$cma,
                      ".",
                      sep="")
                ),
           " This means that the number of tech workers have been relatively unchanged over the past 10 years."
           )
       }
       else{
         if(difference<0){
           p("Looking at the Census in 2006, we find that there were ",
             span(style="color: #e24585",
                  paste(comma(noc.2006.city[tech=="Tech Occupation" & GEO.CODE %in% cma.data[Name %in% input$cma,ID],V1]),
                        " tech workers in ",
                        input$cma,
                        ".",
                        sep="")
                  ),
             " This means that the number of tech workers have decreased over the past 10 years",
             paste("by ",abs(difference)," workers.",sep="")
             )
         }
         else{
           p("Looking at the Census in 2006, we find that there were",
             span(style="color: #e24585",
                  paste(comma(noc.2006.city[tech=="Tech Occupation" & GEO.CODE %in% cma.data[Name %in% input$cma,ID],V1]),
                        " tech workers in ",
                        input$cma,
                        ".",
                        sep=""
                        )
                  ),
             " This means that the number of tech workers have increased over the past 10 years.",
             paste("by ",abs(difference)," workers.",sep="")
             )         
         }
       }
     }
     
   })
   
   #Plot the main education graph
   output$educ.column <- renderPlotly(plot.educ(cma.reactive()))
   
   #Text for the education section
   output$educ.text <- renderUI({
     p("Tech Workers in Canada are highly educated. 
       In fact, almost 58% of tech workers hold a Bachelor's degree or above, 
       compare to 26% of workers not in tech occupations.",
       span(style="color: #e24585",paste("In ",input$cma,", ",
             round(cma.ca.educ[ALT.GEO.CODE %in% cma.data[Name %in% input$cma,ID] & EDUC15.ID==10,pct]),"%", sep = ""),
       " of tech workers held a Bachelor's degree or above."
       ))
   })
   
   #Comparison table for top line - this creates the reactive content
   comparison.table.topline <- reactive(draw.table.topline(input$comparison.cma))
   
   #For download removes the html elements from the first column that allows for tooltips to appear
   comparison.table.topline.fordl <- reactive({
     base <- comparison.table.topline()
     base[,Metrics:=c("Number of tech workers",
                      "Concentration of Tech Workers",
                      "Number of Tech Workers in 2006",
                      "Share of Tech Workers with Bachelors Degree or higher")]
     base
   })
   
   output$comparison.topline <- renderTable(comparison.table.topline(),sanitize.text.function = function(x) x)
   
   #Download button for topline comparison table
   output$download.topline.specific <- downloadHandler(filename = "tech_topline_selected.csv",
                                                       content=function(file){
                                                         write.csv(comparison.table.topline.fordl(),
                                                                   file,
                                                                   row.names = FALSE)
                                                       })
     
   ################ Output for second tab
   
   #Scatterplot of tech pay vs non tech pay
   output$tech.pre.scatter <- renderPlotly(plot.scatter.income(input$cma_div))
   
   #Tech premium text
   output$tech.premium.text <- renderUI({
     p("In ",
       span(style ="color: #e24585",
            paste(input$cma_div,",",sep=""),
            " tech workers were paid ",
            paste("$",comma(signif(tech_premium[GEO.NAME==input$cma_div,tech.pay])),";",sep=""),
            " on average, ",
            paste("$",comma(signif(tech_premium[GEO.NAME==input$cma_div,tech.pay-non.tech.pay])),sep=""),
            "more than non-tech workers."))
            
       
   })
   
   #Gender CMA text
   output$gender.cma.text <- renderUI({
     female.share.can <- round(tech_gender[GEO.NAME=="Canada",share_tech])
     female.share.cma <- round(tech_gender[GEO.NAME==input$cma_div, share_tech])
     p("In Canada ",
       paste(female.share.can,"%",sep=""),
       "of tech workers are female. This is compared to ",
       paste(female.share.cma,"%",sep=""),
       paste("in ",input$cma_div,sep=""))
   })
   
   #Gender paygap graph
   output$gen.pay.gap.cma <- renderPlotly(plot.gender.paygap(input$cma_div))
   
   #Gender paygap text
   output$pay.gap.text <- renderUI({
     gap.amount <- tech_gender[GEO.NAME %in% input$cma_div,pay.gap]
     ratio <- round(tech_gender[GEO.NAME %in% input$cma_div, male.prop.tech/prop.tech],1)
     div(p("In ",
           span(style = "color: #e24585",
                paste(input$cma_div,",",sep=""),
                " the gender pay gap for tech workers was around",
                paste(" $",comma(signif(gap.amount,3)),".",sep=""))))
           


   })
   
   output$gen.gap.text <- renderUI({
     ratio <- round(tech_gender[GEO.NAME %in% input$cma_div, male.prop.tech/prop.tech],1)
     div(p("In",
           paste(input$cma_div,sep=""),
           "Men were",
           span(style = "color: #e24585", 
                paste(ratio, " times more likely",sep="")),
           " to work in a tech occupation,",
         "where",
         span(style = "color: #e24585",
              paste(round(tech_gender[GEO.NAME %in% input$cma_div,prop.tech],1),
                    "%",
                    " of women")), " worked in tech occupations.",sep=""))
   })
   #Gender table in main text chosen CMA and Canada
   output$gen.comp.table <- renderTable(draw.gender.table(input$cma_div),sanitize.text.function = function(x) x)
   
   #Comparison table for chosen CMA between male and female
   output$gen.comp.mf.table <- renderTable(draw.male.female.comp.table(input$cma_div),sanitize.text.function=function(x) x)
   
   #Visible minority comparison table chosen CMA and Canada
   output$vismin.comp.table <- renderTable(draw.vismin.table(input$cma_div),sanitize.text.function = function(x) x)
   
   #Comparison table for chosen CMA between VM and non-VM
   
   output$vismin.comp.nonvismin.table <- renderTable(draw.comp.vismin.table(input$cma_div),sanitize.text.function = function(x) x)
   
   #Visible minority main text
   output$vismin.sumtext <- renderUI({
     num.vismin <- signif(tech_vismin[GEO.NAME %in% input$cma_div & VIS.MIN15.ID == 2, V1],3)
     p("In 2016, ",
       span(style ="color: #e24585" ,
            paste(comma(num.vismin)," visible minorities worked",sep=""),
                   " in tech occupations in ",
                   paste(input$cma_div,".",sep="")))

   })
   
   #Paygap graph for visible minority
   output$vismin.paygap <- renderPlotly(plot.vismin.paygap(input$cma_div))
   
   #Paygap text
   output$vismin.paygap.text <- renderUI({
     pay.gap <- signif(tech_vismin[GEO.NAME %in% input$cma_div & VIS.MIN15.ID==2, pay.gap],2)
     p("In terms of the pay difference that exists between tech workers with a visible minority identity,",
       "we found that",
       paste("tech workers in ",input$cma_div," make",sep=""),
       span(style="color: #e24585",
            paste("$",comma(pay.gap)," less",sep="")),
       "than their non-visible minority counterparts.")
   })
   
   #CMA Comparison table for diverity 
   comparison.table.diversity <- reactive(draw.table.div(input$comparison.cma.div))
   
   #Remove HTML elements from the downloadable version of this table
   comparison.table.diversity.fordl <- reactive({
     tablefordl <- comparison.table.diversity()
     tablefordl[,Metrics:=c("Overall Tech Worker Pay",
                           "Overall Non-Tech Worker Pay",
                           "Share of Tech Workers who are Female",
                           "Share of Female who are Tech Workers",
                           "Pay Difference by Sex",
                           "Share of Tech Workers who are in a Visible Minority Group",
                           "Share of Visible Minorities who are Tech Workers",
                           "Pay Difference by Visible Minority Identities")]
     tablefordl
   })
   output$comparison.diversity <- renderTable(comparison.table.diversity(),sanitize.text.function = function(x) x)
   
   #Doanlod button for diversity table generated
   output$download.div.specific <- downloadHandler(filename = "tech_diversity_selected.csv",
                                                   content=function(file){
                                                     write.csv(comparison.table.diversity.fordl(),
                                                               file,
                                                               row.names = FALSE)
                                                   })

}

# Run the application 
shinyApp(ui = ui, server = server)

