
library(shiny) #Main package to load
library(stringr) #Aid in string oprations
library(data.table) #Aid in data manipulation
library(ggplot2) #Aid in plotting
library(plotly) #Aid in interactive plotting
library(scales) #Aid in formatting some scales


#Loads up all the data files used for this data visualization
load("data/cma.tech.concentration.rds") #Loads noc.dem.tech.map
load("data/city_occ_emp_data.RDS") #Loads noc.dem.city
load("data/canada_occ_emp.RDS") #Loads noc.dem.canada
load("data/cma_tech_2006.RDS") #Loads noc.2006.city
load("data/diversity_cma.RDS") #Loads can_thing
load("data/tech_premium.RDS") #Loads tech_premium
load("data/cma_list.RDS") #Loads cma_list
load("data/tech_gender.RDS") #Loads tech_gender
load("data/cma_data.RDS") #Loads cma.data
load("data/province_data.RDS") #Loads province.data
load("data/cma_ca_education.RDS") #Loads cma.ca.educ
load("data/tech_vismin.RDS") #Loads tech_vismin

#Set up theme elements
source("definitions.R")
#Load up functions to plot first panel graphs
source("first_panel_graph_functions.R")
#Load up functions to plot second panel graphs
source("second_panel_graph_functions.R")


# Define UI for application that draws a histogram
ui <- fluidPage(theme="style.css",
   
   # Application title
   titlePanel(div(style="padding-right: 10%; padding-left: 10%", #Add some nice padding
                  img(src="biie-logo-web-500width.png",width="250px"), #Add Brookfield logo to the header
                  "2018 State of Canada's Tech Sector"), #Name of the viz
              windowTitle = "Brookfield Institute - State of Canada's Tech Workers 2018"), #Name to show up on browser. Favicon is set in css document
   # Sidebar with a slider input for number of bins 
   navbarPage("Navigation",
              tabPanel("General Trends (Cities and Towns)",
                         # Show a plot of the generated distribution
                         fluidRow(align = "center",
                                  style = "background: #8AD4DF; color:#14365D",
                                  h2("Canada's Tech Workers"),
                                  div(style="padding-top: 30px; padding-bottom: 30px",
                                      p("Who are the people making up the Tech sector in Canada? How many are there? Where do they work? 
                                        Is there a diversity problem in tech? These are some of the questions you may have asked while 
                                        trying to understand the Tech sector in Canada. This data visualization, accompanying our 
                                        2018 State of Tech Workers report, will help you dive deeper into these issues for cities around Canada."))
                                 ), #End FluidRow
                         fluidRow(align="center",
                                  style = "background: #8AD4DF; color:#14365D !important",
                                  selectInput("province",
                                              "Select a Province:",
                                              choices = c("British Columbia"="BC","Alberta"="AB","Saskachewan"="SK","Manitoba"="MB",
                                                          "Ontario"="ON","Quebec"="QC","New Brunswick"="NB","Prince Edward Islands"="PE",
                                                          "Nova Scotia"="NS","Newfoundland and Labrador"="NL","Nunavut"="NU","Northwest Territories"="NT",
                                                          "Yukon"="YK"),selected="ON"),
                                  div(style="display: inline-block;vertical-align:middle; height:34px",
                                      p("Tell me more about tech jobs in ")),
                                  div(style="display: inline-block;vertical-align:middle; horizontal-align:left; height: 34px",
                                      uiOutput("CMA_choice"))
                                ), #EndFluidRow
                       
                       fluidRow(align = "center",
                                style = "background: #8AD4DF; color: #14365D",
                                plotlyOutput("cmatot",height="600px")
                                ), #EndFluidRow
                       
                       fluidRow(style="padding-top: 40px; background: #8AD4DF; color:#14365D; display:flex; align-items:center",
                                column(style="padding-right: 15px",
                                       align="center",
                                       width=7, 
                                       p("The technology sector in Canada is diverse - almost ",
                                          span(style='color: #DD347A',"1 million"),
                                          "Canadians are in a tech occupation - that's 1 in 20 workers in Canada overall.
                                         They work in many industries across cities and towns in Canada.") #End p
                                       ), #End Column
                                
                                column(width=4,
                                       style = "background: #DD347A; color: #fff; margin-bottom: 20px; padding-top: 10px; padding-bottom: 10px",
                                       div(h4("Tech Occupation Definition"),
                                           p("For this report, we define Tech occupations to be occupations with high requirement in Tech Skills. 
                                              We use the National Occupational Classification to define our occupations, and US's O*Net's Skills taxonomy to look at each occupation's tech intensity."
                                             ) #End p
                                           ) #End Div
                                       ) #End Column

                                ), #EndFluidRow
                       
                       
                       fluidRow(align="center",
                                style="font-size: 28px; padding-top: 50px; padding-bottom: 50px; color: #14365D; background: #CEE6C1",
                                textOutput("CMA_chosen_2")
                                ), #EndFluidRow
                       
            #START SHOWING TOPLINE NUMBERS FROM HERE
                       fluidRow(style = "background: #14365D; color: #fff;",
                                h3("Tech Employment by Occupations")
                                ), #End FluidRow
                       
                       fluidRow(style = "background: #14365D; color: #fff",
                                column(width=2,
                                       style="font-size:18px",
                                       radioButtons("pct_or_tot",choices = c("Total Tech Workers","Concentration of Tech Workers"),selected = "Total Tech Workers",label="Show me")), #End Column
                                column(style="border-right: 2px dashed #8AD4DF; padding-bottom: 20px",
                                       align = "center",
                                       p(style ="font-size: 18px", "Local Tech Talent"),
                                       plotlyOutput("cmatopocc",height="500px"),
                                       width=5), #End Column
                                
                                column(align="center",
                                       style = "padding-bottom: 20px",
                                       p(style ="font-size: 18px", "Canada's Tech Talent"),
                                       plotlyOutput("canadatopocc",height="500px"),
                                       width=5) #End Column
                                ), #EndFluidRow
                       
                       fluidRow(style="background: #14365D; color: #fff; display:flex; align-items:center",
                                column(width=9,htmlOutput("cmatotnumtext"),
                                       div(style="margin-bottom: 10px","How has this tech workforce changed since 2006?"),
                                       htmlOutput("cmain2006")),
                                column(width=3,img(src="test_image_1.png",style="width:100%; min-width:160px"))
                                ), #EndFluidRow
            
        #START SHOWING EDUCATION STUFF FROM HERE
                       fluidRow(style = "background: #8AD4DF; color: #14365D;",
                                h3("Education Attainment of Tech Workers by City/Town")
                                ), #End FluidRow
                       
                       #fluidRow(style = "padding-right: 50px; padding-left: 50px; font-size: 18px; padding-top: 30px; background: #14365D; color: #fff",
                      #          htmlOutput("cmain2006")
                      #          ), #End FluidRow
                       
                       fluidRow(style = "background: #8AD4DF; color:#14365D;",
                                align = "center",
                                column(width=12,
                                       style = "max-width: 800px; float:none",
                                       align = "center",
                                       plotlyOutput("educ.column",height="800px")
                                       )#, #End Column
                                
                                ), #EndFluidRow
                      
                      fluidRow(style = "background: #8AD4DF; color: #14365D; display:flex; align-items:center",
                               column(width=3,
                                      img(src="tech_image_2.png",style="width:100%; min-width:160px")),
                               column(width=8,
                                      htmlOutput("educ.text"),
                                      p("In the main report, we also explored what programs students specialized in.
                                        Though we don't present this data at the city/town level, tech workers came from a variety of backgrounds,
                                        with a disproportionate share coming from traditional STEM programs. There was also a sizeable number who studied
                                        Business degrees who are now a part of tech workers."))
                               ), #End FluidRow
                      
        #START SHOWING COMPARISON FROM HERE
                      fluidRow(style = "background: #CEE6C1; color: #14365D",
                               h3("Compare between cities/towns")
                      ), #EndFluidRow
                      
                      
                      fluidRow(style = "background: #CEE6C1; color: #14365D",
                               p("Now that you've gotten a better idea of the topline numbers, you may want to compare between
                                 different cities and/or towns. Use our tools to compare the topline numbers for up to 5 cities/towns, 
                                 and download the resulting table.")
                      ), #EndFluidRow
                
                      fluidRow(style = "background: #CEE6C1; color: #14365D",
                               align = "center",
                               selectizeInput(inputId = "comparison.cma",
                                              label = "Select up to 5 cities or towns",
                                              choices = sort(unique(cma.data[,Name])),
                                              selected = NULL,
                                              multiple = TRUE,
                                              options = list(maxItems = 5))
                               ), #EndFluidRow
                      

                      fluidRow(style = "background: #CEE6C1; color: #14365D",
                               align = "center",
                               tableOutput("comparison.topline")
                               ), #EndFluidRow
        
                      fluidRow(style = "background: #CEE6C1; color: #14365D",
                               align = "center",
                               downloadButton("download.topline.specific","Download Current Table")
                               )#, #EndFluidRow


                       ),#End first tab panel
  #SECOND TAB PANEL
              tabPanel("Income and Diversity (Metropolitan Areas)",
                       fluidRow(align = "center",
                                style = "background: #8AD4DF; color: #14365D",
                                p(style="font-size: 18px",
                                  "Diversity has been a focal point of many conversation surrounding tech. Let's explore how different groups of people
                                  may have difference experiences working in tech for some of Canada's largest cities.")
                                ), #End FluidRow
                       
                       fluidRow(align = "center",
                                style = "background: #8AD4DF; color: #14365D",
                                selectInput("cma_div","Select a Metropolitan Area:",
                                            choices = cma_list[,GEO.NAME])
                                ), #EndFluidRow
                       
                       fluidRow(style = "background: #8AD4DF; color: #14365D; display:flex; align-items:center",
                                column(width = 7,
                                       plotlyOutput("tech.pre.scatter",height = "500px")),
                                column(width = 5,
                                       p("Consistently, tech workers across Canada received higher average pay than non-tech workers.
                                         This pay increase was present in every demographic and geographic groups."),
                                       htmlOutput("tech.premium.text"))
                                
                                ), #EndFluidRow
                       
      #GENDER DIVERSITY STARTS HERE
      
                       fluidRow(style = "background: #79133E; color: #fff",
                                h3("Gender diversity")
                                
                                ), #End FluidRow
                       
                       fluidRow(style = "background: #79133E; color: #fff",
                                
                                column(width = 6,
                                       p("Labour force participation among women in Canada has been steadily increasing.
                                         In 2016, women made up 48 percent of the labour market."),
                                       p("Despite these trends, in 2016 there were 584,000 more men in tech occupations than women. 
                                         Men were almost four times more likely than women to work in a tech occupation—7.8 percent 
                                         of men in the labour market were in tech occupations compared to 2.1 percent of women.")),
                                
                                column(width = 6,
                                       align = "center",
                                       tableOutput("gen.comp.table"))
                                
                                ), #EndFluidRow
                       
                       fluidRow(style = "background: #79133E; color: #fff; display:flex; align-items:center",
                                column(width = 4,
                                       img(src="tech_pic_3.png", style="width:100%; min_width: 180px")),
                                column(width = 8, 
                                       p("When it comes to the gender pay gap in Tech,
                                          almost every major city in Canada has a higher gap than the Canadian average."),
                                       htmlOutput("pay.gap.text"),
                                       p("In our main report, we analyze this gender pay gap and see that this gap is persistent across education,
                                         visible minority and immigration status, as well as age. Were you surprised with the gap
                                         in the city you chose?"))
                                ), #End FluidRow
                       
                       fluidRow(align = "center",
                                style = "background: #79133E",
                                plotlyOutput("gen.pay.gap.cma",height="600px")
                                ), #End FluidRow
                       
                       
    #VISIBLE MINORITY STARTS HERE
                       fluidRow(style = "background: #14365D; color: #fff",
                                h3("Visible Minority")
                                ), #End FluidRow
 

                       fluidRow(style = "background: #14365D; color: #fff",
                                column(width=6,
                                       htmlOutput("vismin.comp.table",class = "inverse")),
                                column(width=6,
                                       p("Almost one-third of Canada's tech workers have visibile minority identities (31.9%).
                                         8.5 percent, or almost one in twelve visible minorities was a tech worker, totalling 271,000 people"),
                                       htmlOutput("vismin.sumtext", class="inverse"),
                                       p("Yet, there are important differences between different visible minority groups, and with non-visible minorities.
                                         In this application, we focus broadly on the idea of visible minorities. For a detailed look on how different groups
                                         (such as those who identify as Black, Chinese, or South Asian) compare to each other, check out our main report."))
                                ), #End FluidRow
                       
                       fluidRow(style = "background: #14365D; color: #fff",
                                column(width = 7,
                                       p("When it comes to the pay gap that exists between visible minorities groups and non-visible minorities, the .")),
                                column(width = 4,
                                       p("blah blah text"))
                                ), #End FluidRow
                       
                       fluidRow(style = "background: #14365D; color: #fff",
                                plotlyOutput("vismin.paygap",height="600px")
                                ), #End FluidRow
                       
        #COMPARISON BEGINS HERE
                       fluidRow(style = "background: #CEE6C1; color: #14365D",
                                h3("Compare between cities/towns")
                       ), #EndFluidRow
                       
                       
                       fluidRow(style = "background: #CEE6C1; color: #14365D",
                                p("Now that you've gotten a better idea of diversity in tech, you may want to compare between
                                  different cities and/or towns. Use our tools to compare the topline numbers for up to 5 cities/towns, 
                                  or download the entire dataset.")
                                ), #EndFluidRow
                       
                       fluidRow(style = "background: #CEE6C1; color: #14365D",
                                align = "center",
                                selectizeInput(inputId = "comparison.cma.div",
                                               label = "Select up to 5 metropolitan areas",
                                               choices = sort(unique(cma_list[,GEO.NAME])),
                                               selected = NULL,
                                               multiple = TRUE,
                                               options = list(maxItems = 5))
                       ), #EndFluidRow
                       
                       fluidRow(style = "background: #CEE6C1; color: #14365D",
                                align = "center",
                                tableOutput("comparison.diversity")
                                ), #EndFluidRow
    
                       fluidRow(style = "background: #CEE6C1; color: #14365D",
                                align = "center",
                                downloadButton("download.div.specific","Download Current Table")
                                ) #EndFluidRow
                       ) #End second tab panel
        
              ), #EndNavbarPage
  fluidRow(style="background: #14365D; padding-left: 0%; padding-right: 0%; font-size: 12px",
           div(class="footer",
               style = "padding-left: 5%; padding-right: 5%",
                 includeHTML("www/site_footer.html")))

   ) #EndFluidPage


# Define server logic required to draw a histogram
server <- function(input, output) {
  #Text of the chosen city
  output$CMA_choice <- renderUI(
    selectInput("cma",
                label=NULL,
                choice = sort(unique(cma.data[stringr::str_sub(ID,1,2)==province.data[abbrev==input$province,code],Name]))))

  #Just the CMA
  output$CMA_chosen_as_input <- renderUI(
    paste(input$cma)
  )
  
   #Text for focusing on a particular CMA
   output$CMA_chosen_2 <- renderText({
    paste("Let's focus on ", input$cma,"'s story.",sep="")})

   #Interactive column plot of all the cities with the chosen city highlighted
   output$cmatot <- renderPlotly(plot.cmatot(input$cma))
   
   #Top occupations both Absolute and Relative
   output$cmatopocc <- renderPlotly(plot.cmaocc(input$cma,input$pct_or_tot))
   
   #Canada's top occupation both absolute and relative
   output$canadatopocc <- renderPlotly(plot.canocc(input$pct_or_tot))
   
   #Change in the tech workforce in text
   output$cmatotnumtext <- renderUI({p("In 2016, ",
                                      span(style="color:#DD347A",
                                           paste(comma(noc.dem.tech.map[ALT.GEO.CODE %in% cma.data[Name %in% input$cma, ID] & tech==1,V1]),
                                            " tech workers worked in ",
                                            paste(input$cma,
                                                  ".",sep=""))),
                                      " That represents ",
                                      span(style = "color:#DD347A",
                                           paste(round(100*noc.dem.tech.map[ALT.GEO.CODE %in% cma.data[Name %in% input$cma, ID] & tech==1,V1]/891720,2),
                                                 " percent of all tech workers in Canada.",sep="")
                                           ),
                                      "Did it exceed your expectations? In the past 10 years, 
                                      over 180,000 workers joined the tech workforce in Canada and 180,000 more are predicted to 
                                      join in the next 10 years according to Economics and Social Development Canada."
                                       )
                                      })

   
   #Workforce in 2006 vs 2016 comparison text
   output$cmain2006 <- renderUI({
     if(nrow(noc.2006.city[tech=="Tech Occupation" & GEO.CODE %in% cma.data[Name %in% input$cma,ID]])==0){
       p("Unfortunately, this city's population was too low in 2006 to have data published without confidentiality issues.")
     }
     else{
       difference <- noc.dem.tech.map[GEO.NAME %in% input$cma & tech==1,V1] - noc.2006.city[tech=="Tech Occupation" & GEO.CODE %in% cma.data[Name %in% input$cma,ID],V1]
       if(abs(difference)/noc.dem.tech.map[GEO.NAME %in% input$cma & tech==1,V1] < 0.01 | abs(difference)<50){
         
         p("Looking at the Census in 2006, we find that there were",
           span(style="color: #DD347A",
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
             span(style="color: #DD347A",
                  paste(comma(noc.2006.city[tech=="Tech Occupation" & GEO.CODE %in% cma.data[Name %in% input$cma,ID],V1]),
                        " tech workers in ",
                        input$cma,
                        ".",
                        sep="")
                  ),
             " This means that the number of tech workers have decreased over the past 10 years."
             )
         }
         else{
           p("Looking at the Census in 2006, we find that there were",
             span(style="color: #DD347A",
                  paste(comma(noc.2006.city[tech=="Tech Occupation" & GEO.CODE %in% cma.data[Name %in% input$cma,ID],V1]),
                        " tech workers in ",
                        input$cma,
                        ".",
                        sep=""
                        )
                  ),
             " This means that the number of tech workers have increased over the past 10 years."
             )         
         }
       }
     }
     
   })
   
   #Plot the main education graph
   output$educ.column <- renderPlotly(plot.educ(input$cma))
   
   #Text for the education section
   output$educ.text <- renderUI({
     p("Tech Workers in Canada are highly educated. 
       In fact, almost 58% of tech workers hold a Bachelor's degree or above. 
       Workers not in tech occupations were almost half as likely to hold a Bachelor's degree or above (26%).",
       span(style="color: #DD347A",paste("In ",input$cma,", ",
             round(cma.ca.educ[ALT.GEO.CODE %in% cma.data[Name %in% input$cma,ID] & EDUC15.ID==10,pct]),"%", sep = ""),
       " of tech workers held a Bachelor's degree or above."
       ))
   })
   
   #Comparison table for top line - this creates the reactive content
   comparison.table.topline <- reactive(draw.table.topline(input$comparison.cma))
   
   output$comparison.topline <- renderTable(comparison.table.topline())
   
   #Download button for topline comparison table
   output$download.topline.specific <- downloadHandler(filename = "tech_topline_selected.csv",
                                                       content=function(file){
                                                         write.csv(comparison.table.topline(),
                                                                   file,
                                                                   row.names = FALSE)
                                                       })
     
   ################ Output for second tab
   
   #Scatterplot of tech pay vs non tech pay
   output$tech.pre.scatter <- renderPlotly(plot.scatter.income(input$cma_div))
   
   #Tech premium text
   output$tech.premium.text <- renderUI({
     p("In ",
       span(style ="color: #DD347A",
            paste(input$cma_div,",",sep=""),
            " tech workers were paid ",
            paste("$",comma(signif(tech_premium[GEO.NAME==input$cma_div,tech.pay])),sep=""),
            "; this was ",
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
     p("In ",
       span(style = "color: #8AD4DF",
            paste(input$cma_div,",",sep=""),
            " the gender pay gap for tech workers was around",
            paste(" $",comma(signif(gap.amount,3)),sep=""),"."))

   })
   
   #Gender table in main text
   output$gen.comp.table <- renderTable(draw.gender.table(input$cma_div))
   
   #Visible minority comparison table
   output$vismin.comp.table <- renderTable(draw.vismin.table(input$cma_div))
   
   #Cisible minority main text
   output$vismin.sumtext <- renderUI({
     num.vismin <- signif(tech_vismin[GEO.NAME %in% input$cma_div & VIS.MIN15.ID == 2, V1],3)
     p("In 2016, ",
       span(style ="color: #DD347A" ,
            paste(comma(num.vismin)," visible minorities worked",sep=""),
                   " in tech occupations in ",
                   paste(input$cma_div,".",sep="")))

   })
   
   #Paygap graph for visible minority
   output$vismin.paygap <- renderPlotly(plot.vismin.paygap(input$cma_div))
   
   #CMA Comparison table for diverity 
   comparison.table.diversity <- reactive(draw.table.div(input$comparison.cma.div))
   
   output$comparison.diversity <- renderTable(comparison.table.diversity())
   
   #Doanlod button for diversity table generated
   output$download.div.specific <- downloadHandler(filename = "tech_diversity_selected.csv",
                                                   content=function(file){
                                                     write.csv(comparison.table.diversity(),
                                                               file,
                                                               row.names = FALSE)
                                                   })

}

# Run the application 
shinyApp(ui = ui, server = server)

