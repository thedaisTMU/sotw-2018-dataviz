
library(shiny)
library(stringr)
library(data.table)
library(ggplot2)
library(plotly)
library(scales)



load("data/cma.tech.concentration.rds")
load("data/city_occ_emp_data.RDS")
load("data/canada_occ_emp.RDS")
load("data/cma_tech_2006.RDS")
load("data/diversity_cma.RDS")
load("data/tech_premium.RDS")
load("data/cma_list.RDS")
load("data/tech_gender.RDS")
load("data/cma_data.RDS")
load("data/province_data.RDS")
load("data/cma_ca_education.RDS")
load("data/tech_vismin.RDS")

#Set up theme elements
source("definitions.R")

# Define UI for application that draws a histogram
ui <- fluidPage(theme="style.css",
   
   # Application title
   titlePanel(div(style="padding-right: 10%; padding-left: 10%",img(src="biie-logo-web-500width.png",width="250px"),"State of Canada's Tech Sector"),
              windowTitle = "Brookfield Institute - State of Canada's Tech Workers 2018"),
   tags$style("body{background:#CEE6C1;
              color:#14365D}"),
   # Sidebar with a slider input for number of bins 
   navbarPage("Navigation",
              tabPanel("General Trends (Cities and Towns)",
                         # Show a plot of the generated distribution
                         fluidRow(align = "center",
                                  style = "padding-right: 10%; padding-left: 10%; background: #8AD4DF; color:#14365D; font-size: 18px; padding-top: 20px",
                                  h2("Canada's Tech Workers"),
                                  div(style="padding-top: 30px; padding-bottom: 30px",
                                      p("Who are the people making up the Tech sector in Canada? How many are there? Where do they work? 
                                        Is there a diversity problem in tech? These are some of the questions you may have asked while 
                                        trying to understand the Tech sector in Canada. This data visualization, accompanying our 
                                        2018 State of Tech Workers report, will help you dive deeper into these issues for cities around Canada."))
                                 ), #End FluidRow
                         fluidRow(align="center",
                                  style = "background: #8AD4DF; color:#14365D !important; font-size: 18px",
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
                                style = "background: #8AD4DF; color: #14365D; padding-left: 10%; padding-right: 10%; padding-bottom: 15px",
                                plotlyOutput("cmatot",height="600px")
                                ), #EndFluidRow
                       
                       fluidRow(style="padding-right: 10%; padding-left: 10%; padding-top: 40px; background: #8AD4DF; color:#14365D; display:flex; align-items:center",
                                column(style="font-size: 20px; padding-right: 15px",
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
                                style="font-size: 28px; padding-right: 10%; padding-left: 10%; padding-top: 50px; padding-bottom: 50px; color: #14365D; background: #CEE6C1",
                                textOutput("CMA_chosen_2")
                                ), #EndFluidRow
                       
            #START SHOWING TOPLINE NUMBERS FROM HERE
                       fluidRow(style = "font-side: 18px; background: #14365D; color: #fff;",
                                h3("Tech Employment by Occupations")
                                ), #End FluidRow
                       
                       fluidRow(style = "padding-right: 10%; padding-left: 10%; background: #14365D; color: #fff",
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
                       
                       fluidRow(style="padding-top: 30px; padding-bottom: 30px; padding-right: 10%; padding-left: 10%; font-size: 18px; background: #14365D; color: #fff; display:flex; align-items:center",
                                column(width=9,htmlOutput("cmatotnumtext"),
                                       div(style="margin-bottom: 10px","How has this tech workforce changed since 2006?"),
                                       htmlOutput("cmain2006")),
                                column(width=3,img(src="test_image_1.png",style="width:100%; min-width:160px"))
                                ), #EndFluidRow
            
        #START SHOWING EDUCATION STUFF FROM HERE
                       fluidRow(style = "font-size: 18px; background: #8AD4DF; color: #14365D;",
                                h3("Education Attainment of Tech Workers by City/Town")
                                ), #End FluidRow
                       
                       #fluidRow(style = "padding-right: 50px; padding-left: 50px; font-size: 18px; padding-top: 30px; background: #14365D; color: #fff",
                      #          htmlOutput("cmain2006")
                      #          ), #End FluidRow
                       
                       fluidRow(style = "background: #8AD4DF; color:#14365D; padding-right: 10%; padding-left: 10%",
                                align = "center",
                                column(width=12,
                                       style = "max-width: 800px; float:none",
                                       align = "center",
                                       plotlyOutput("educ.column",height="800px")
                                       )#, #End Column
                                
                                ), #EndFluidRow
                      
                      fluidRow(style = "padding-top: 30px; padding-bottom: 30px; padding-right: 10%; padding-left: 10%; background: #8AD4DF; color: #14365D; font-size: 18px; display:flex; align-items:center",
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
                      fluidRow(style = "background: #CEE6C1, color: #14365D; font-size: 18px",
                               h3("Compare between cities/towns")
                      ), #EndFluidRow
                      
                      
                      fluidRow(style = "padding-bottom: 30px; padding-right: 10%; padding-left: 10%; background: #CEE6C1, color: #14365D; font-size: 18px",
                               p("Now that you've gotten a better idea of the topline numbers, you may want to compare between
                                 different cities and/or towns. Use our tools to compare the topline numbers for up to 5 cities/towns, 
                                 or download the entire dataset.")
                      ), #EndFluidRow
                
                      fluidRow(style = "padding-bottom: 30px; padding-right: 10%; padding-left: 10%; background: #CEE6C1, color: #14365D; font-size: 18px",
                               align = "center",
                               selectizeInput(inputId = "comparison.cma",
                                              label = "Select up to 5 cities or towns",
                                              choices = sort(unique(cma.data[,Name])),
                                              selected = NULL,
                                              multiple = TRUE,
                                              options = list(maxItems = 5))
                               ), #EndFluidRow
                      

                      fluidRow(style = "padding-top: 30px; padding-bottom: 30px; padding-right: 10%; padding-left: 10%; background: #CEE6C1, color: #14365D; font-size: 18px",
                               align = "center",
                               tableOutput("comparison.topline")
                               ), #EndFluidRow
        
                      fluidRow(style = "padding-bottom: 30px; padding-right: 10%; padding-left: 10%; background: #CEE6C1, color: #14365D; font-size: 18px",
                               align = "center",
                               downloadButton("download.topline.specific","Download Current Table")
                               )#, #EndFluidRow


                       ),#End first tab panel
  #SECOND TAB PANEL
              tabPanel("Income and Diversity (Metropolitan Areas)",
                       fluidRow(align = "center",
                                style = "padding-left: 10%; padding-right: 10%; padding-top: 20px; background: #8AD4DF; color: #14365D, font-size: 18px",
                                p(style="font-size: 18px",
                                  "Diversity has been a focal point of many conversation surrounding tech. Let's explore how different groups of people
                                  may have difference experiences working in tech for some of Canada's largest cities.")
                                ), #End FluidRow
                       
                       fluidRow(align = "center",
                                style = "padding-left: 10%; padding-right: 10%; background: #8AD4DF; color: #14365D",
                                selectInput("cma_div","Select a Metropolitan Area:",
                                            choices = cma_list[,GEO.NAME])
                                ), #EndFluidRow
                       
                       fluidRow(style = "padding-left: 10%; padding-right: 10%; padding-bottom: 20px; background: #8AD4DF; color: #14365D; font-size: 18px; display:flex; align-items:center",
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
                       
                       fluidRow(style = "padding-left: 10%; padding-right: 10%; background: #79133E; padding-top: 20px; padding-bottom: 20px; color: #fff; font-size: 18px",
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
                       
                       fluidRow(style = "bakground: #79133E; padding-left: 10%; padding-right: 10%; color: #fff; background: #79133E; font-size: 18px",
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
                                style = "background: #79133E; padding-left: 10%; padding-right: 10%; padding-bottom: 20px",
                                plotlyOutput("gen.pay.gap.cma",height="600px")
                                ), #End FluidRow
                       
                       
    #VISIBLE MINORITY STARTS HERE
                       fluidRow(style = "background: #14365D; color: #fff",
                                h3("Visible Minority")
                                ), #End FluidRow
 

                       fluidRow(style = "padding-left: 10%; padding-right: 10%; background: #14365D; padding-top: 20px; padding-bottom: 10px; color: #fff; font-size: 18px",
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
                       
                       fluidRow(style = "padding-left: 10%; padding-right: 10%; background: #14365D; padding-top: 20px; padding-bottom: 10px; color: #fff; font-size: 18px",
                                column(width = 7,
                                       p("Blah blah text")),
                                column(width = 4,
                                       p("blah blah text"))
                                ), #End FluidRow
                       
                       fluidRow(style = "padding-left: 10%; padding-right: 10%; background: #14365D; padding-top: 20px; padding-bottom: 10px; color: #fff; font-size: 18px",
                                plotlyOutput("vismin.paygap",height="600px")
                                ), #End FluidRow
                       
        #COMPARISON BEGINS HERE
                       fluidRow(style = "background: #CEE6C1, color: #14365D; font-size: 18px",
                                h3("Compare between cities/towns")
                       ), #EndFluidRow
                       
                       
                       fluidRow(style = "padding-top: 30px; padding-bottom: 30px; padding-right: 10%; padding-left: 10%; background: #CEE6C1, color: #14365D; font-size: 18px",
                                p("Now that you've gotten a better idea of diversity in tech, you may want to compare between
                                  different cities and/or towns. Use our tools to compare the topline numbers for up to 5 cities/towns, 
                                  or download the entire dataset.")
                                ), #EndFluidRow
                       
                       fluidRow(style = "padding-top: 30px; padding-bottom: 30px; padding-right: 10%; padding-left: 10%; background: #CEE6C1, color: #14365D; font-size: 18px",
                                align = "center",
                                selectizeInput(inputId = "comparison.cma.div",
                                               label = "Select up to 5 metropolitan areas",
                                               choices = sort(unique(cma_list[,GEO.NAME])),
                                               selected = NULL,
                                               multiple = TRUE,
                                               options = list(maxItems = 5))
                       ), #EndFluidRow
                       
                       fluidRow(style = "padding-top: 30px; padding-bottom: 30px; padding-right: 10%; padding-left: 10%; background: #CEE6C1, color: #14365D; font-size: 18px",
                                align = "center",
                                tableOutput("comparison.diversity")
                                ), #EndFluidRow
    
                       fluidRow(style = "padding-top: 30px; padding-bottom: 30px; padding-right: 10%; padding-left: 10%; background: #CEE6C1, color: #14365D; font-size: 18px",
                                align = "center",
                                downloadButton("download.div.specific","Download Current Table")
                                ) #EndFluidRow
                       ) #End second tab panel
        
              ), #EndNavbarPage
  fluidRow(style="background: #14365D",
           div(class="footer",
                 includeHTML("www/site_footer.html")))

   ) #EndFluidPage


# Define server logic required to draw a histogram
server <- function(input, output) {
  #Text of the chosen city
  output$CMA_choice <- renderUI(
    selectInput("cma",
                label=NULL,
                choice = sort(unique(cma.data[stringr::str_sub(ID,1,2)==province.data[abbrev==input$province,code],Name]))))
  #Interactive column plot of all the cities with the chosen city highlighted
  
  output$CMA_chosen_as_input <- renderUI(
    paste(input$cma)
  )
  
  output$CMA_chosen_2 <- renderText({
    paste("Let's focus on ", input$cma,"'s story.",sep="")})

   output$cmatot <- renderPlotly({
     if(!is.null(input$cma)){
       noc.dem.tech.map[,cma.focus:="0"]
       noc.dem.tech.map[ALT.GEO.CODE %in% cma.data[Name %in% input$cma,ID],cma.focus:="1"]
       column.pct <- ggplot(data=noc.dem.tech.map[tech==1],aes(reorder(GEO.NAME,pct),pct,fill=cma.focus)) + 
         geom_col(aes(text=paste(GEO.NAME,
                                 "<br>",
                                 "Concentration of Tech Workers:",
                                 str_c(signif(pct,2),"%"))),
                  width=0.6) + 
         BF.Base.Theme + 
         scale_y_continuous(expand=c(0,0),breaks = c(0,2.5,5,7.5,10),labels = c("0%","2.5%","5%","7.5%","10%")) + 
         theme(axis.text.x = element_blank(),
               axis.title.x = element_text(size=9, color="#14365D"),
               axis.title.y = element_text(size=9, color="#14365D"),
               axis.ticks.x = element_blank(),
               axis.text.y = element_text(size=9, margin=ggplot2::margin(r=2),color="#14365D"),
               axis.line = ggplot2::element_line(size=0.25, colour = "#14365D"),
               legend.text = ggplot2::element_text(size=9,margin=ggplot2::margin(r=2),color = "#14365D"),
               axis.ticks = ggplot2::element_line(size=0.15,colour = "#14365D")) +
         scale_fill_manual(values = c("#14365D","#DD347A")) +
         guides(fill="none",colour = "none") +
         labs(y="Tech Workers as a Share of Local Workforce",x="Hover over each bar to learn about a city")
       graph <- config(layout(ggplotly(column.pct,tooltip=c("text"),showlegend=FALSE,showscale=FALSE),
                              legend = list(orientation = 'h'),
                              xaxis=list(fixedrange=TRUE), 
                              yaxis=list(fixedrange=TRUE)),
                       displayModeBar=F)
       graph$x$data[[1]]$name <- "Other Cities/Towns"
       graph$x$data[[2]]$name <- input$cma
       print(graph)
     }
   })
   #Top occupations both Absolute and Relative
   output$cmatopocc <- renderPlotly({
     noc.dem.city.plot <- noc.dem.city[ALT.GEO.CODE %in% cma.data[Name %in% input$cma,ID]]
     if(nrow(noc.dem.city.plot)>10){
       noc.dem.city.plot <- noc.dem.city.plot[(.N-9):.N]
     }
     if(input$pct_or_tot=="Total Tech Workers"){
       plot_pct_or_tot <- ggplot(data=noc.dem.city.plot,aes(reorder(NOC,TOT),TOT),fill="#8AD4DF") +
         geom_col(aes(text=paste(reorder(NOC,TOT),
                                 "<br>",
                                 "Total Employed:",
                                 scales::comma(sort(TOT)))),
                  fill="#8AD4DF",
                  width=0.6) +
         BF.Base.Theme + 
         scale_y_continuous(expand=c(0,0)) + 
         guides(fill="none",colour = "none") +
         theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
         labs(y="Total Employment",x="Hover over each bar to learn about an occupation.")
       print(config(layout(ggplotly(plot_pct_or_tot,tooltip=c("text")),
                           xaxis = list(fixedrange=TRUE),
                           yaxis = list(fixedrange=TRUE)),
                    displayModeBar=F))
       #print(ggplotly(BFTheme::plot.column.bf(noc.dem.city.plot,"TOT","NOC",
        #                       order.bar = "ascending",
        #                       colours = BFTheme::set.colours(1,categorical.choice = "light.blue"),
        #                       col.invert = TRUE)))
     }
     else{
       plot_pct_or_tot <- ggplot(data=noc.dem.city.plot,aes(reorder(NOC,pct),pct),fill="#8AD4DF") +
         geom_col(aes(text=paste(reorder(NOC,TOT),
                                 "<br>",
                                 "Share of Tech Workforce:",
                                 str_c(round(sort(pct),2),"%"))),
                  fill="#8AD4DF",
                  width=0.6) +
         BF.Base.Theme + 
         scale_y_continuous(expand=c(0,0)) + 
         guides(fill="none",colour = "none") +
         theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
         labs(y="Share of Local Tech Workforce",x="Hover over each bar to learn about an occupation")
       print(config(layout(ggplotly(plot_pct_or_tot,tooltip=c("text")),
                           xaxis = list(fixedrange=TRUE),
                           yaxis = list(fixedrange=TRUE)),
                    displayModeBar=F))
       #print(ggplotly(BFTheme::plot.column.bf(noc.dem.city.plot,"pct","NOC",
      #                         order.bar = "ascending",
      #                         label.unit = "%",
      #                         colours = BFTheme::set.colours(1,categorical.choice = "light.blue"),
      #                         col.invert = TRUE)))
     }

   })
   #Canada's top occupation both absolute and relative
   output$canadatopocc <- renderPlotly({
     noc.dem.canada.plot <- noc.dem.canada[(.N-9):.N]
     if(input$pct_or_tot == "Total Tech Workers"){
       plot_pct_or_tot <- ggplot(data=noc.dem.canada.plot,aes(reorder(NOC,TOT),TOT),fill="#8AD4DF") +
         geom_col(aes(text=paste(reorder(NOC,TOT),
                                 "<br>",
                                 "Total Employed:",
                                 scales::comma(sort(TOT)))),
                  fill="#8AD4DF",
                  width=0.6) +
         BF.Base.Theme + 
         scale_y_continuous(expand=c(0,0)) + 
         guides(fill="none",colour = "none") +
         theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
         labs(y="",x="")
       print(config(layout(ggplotly(plot_pct_or_tot,tooltip=c("text")),
                           xaxis = list(fixedrange=TRUE),
                           yaxis = list(fixedrange=TRUE)),
                    displayModeBar=F))
       
       #print(ggplotly(BFTheme::plot.column.bf(noc.dem.canada.plot,"TOT","NOC",
      #                         order.bar="ascending",
      #                         colours = BFTheme::set.colours(1,categorical.choice = "light.blue"),
      #                         col.invert = TRUE)))
     }
     else{
       plot_pct_or_tot <- ggplot(data=noc.dem.canada.plot,aes(reorder(NOC,pct),pct),fill="#8AD4DF") +
         geom_col(aes(text=paste(reorder(NOC,TOT),
                                 "<br>",
                                 "Share of Tech Workforce:",
                                 str_c(round(sort(pct),2),"%"))),
                  fill="#8AD4DF",
                  width=0.6) +
         BF.Base.Theme + 
         scale_y_continuous(expand=c(0,0)) + 
         guides(fill="none",colour = "none") +
         theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
         labs(y="",x="")
       print(config(layout(ggplotly(plot_pct_or_tot,tooltip=c("text")),
                           xaxis = list(fixedrange=TRUE),
                           yaxis = list(fixedrange=TRUE)),
                    displayModeBar=F))
       #print(ggplotly(BFTheme::plot.column.bf(noc.dem.canada.plot,"pct","NOC",
      #                         order.bar = "ascending",
      #                         label.unit = "%",
      #                         colours = BFTheme::set.colours(1,categorical.choice="light.blue"),
      #                         col.invert = TRUE)))
     }

   })
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
   
   output$educ.column <- renderPlotly({
     cma.ca.educ[,EDUC15:=str_wrap(EDUC15,30)]
     cma.ca.educ[,EDUC15:=reorder(EDUC15,EDUC15.ID)]
     cma.ca.educ[,dum:=0]
     cma.ca.educ[ALT.GEO.CODE %in% cma.data[Name %in% input$cma,ID],dum:=1]
     plot.educ.share <- ggplot(data=cma.ca.educ[dum==0],aes(EDUC15,pct)) +
       BF.Base.Theme +
       theme(axis.text.x = element_text(size=9,colour="#14365D"),
             axis.text.y = element_text(size=9,colour="#14365D"),
             panel.grid.major.y = element_line(size=0.1,colour = "#14365D"),
             axis.line = ggplot2::element_line(size=0.25, colour = "#14365D"),
             axis.ticks = ggplot2::element_line(size=0.15,colour = "#14365D"),
             axis.title.x = element_text(size=10,colour = "#14365D"),
             axis.title.y = element_blank()) +
       geom_line(aes(group=GEO.NAME),color = "#14365D",alpha=0.3) +
       scale_y_continuous(breaks = c(0,25,50,75,100),
                          limits = c(0,100),
                          labels = c("0%","25%","50%","75%","100%")) +
       geom_point(data=cma.ca.educ[dum == 1],
                  aes(EDUC15,pct, text = paste(GEO.NAME,"<br>","Share of Tech Workforce: ",round(pct),"%")),
                  color = "#DD347A",
                  size=2) +
       geom_line(data=cma.ca.educ[dum == 1],
                 aes(EDUC15,pct,group = GEO.NAME),
                 color = "#DD347A",
                 size=1) +
       labs(y = "Share of Tech Workers \n Each line represents a city/town") +
       coord_flip()
     
     plotly.plot.educ.share <- config(layout(ggplotly(plot.educ.share,tooltip=c("text")),
                                             xaxis = list(fixedrange=TRUE),
                                             yaxis = list(fixedrange=TRUE),
                                             margin = list(l=200)),
                                      displayModeBar=F)
     
     plotly.plot.educ.share$x$data[[1]]$hoverinfo <- "skip"
     
     plotly.plot.educ.share
   })
   
   output$educ.text <- renderUI({
     p("Tech Workers in Canada are highly educated. 
       In fact, almost 58% of tech workers hold a Bachelor's degree or above. 
       Workers not in tech occupations were almost half as likely to hold a Bachelor's degree or above (26%).",
       span(style="color: #DD347A",paste("In ",input$cma,", ",
             round(cma.ca.educ[ALT.GEO.CODE %in% cma.data[Name %in% input$cma,ID] & EDUC15.ID==10,pct]),"%", sep = ""),
       " of tech workers held a Bachelor's degree or above."
       ))
   })
   
   comparison.table.topline <- reactive({
     topline.vars <- c("Number of Tech Workers",
                       "Concentration of Tech Workers",
                       "Number of Tech Workers in 2006",
                       "Share of Tech Workers with Bachelors Degree or higher")
     final.table <- data.table(first.col = topline.vars)
     names(final.table) <- c("Metrics")
     if(length(input$comparison.cma)>0){
       #Set up first column
       if(!is.na(input$comparison.cma[1][[1]])){
         first.city <- c(comma(round(noc.dem.tech.map[tech==1 & ALT.GEO.CODE %in% cma.data[Name == input$comparison.cma[1][[1]],ID],V1])),
                         str_c(signif(noc.dem.tech.map[tech==1 & ALT.GEO.CODE %in% cma.data[Name == input$comparison.cma[1][[1]],ID],pct],2),"%"),
                         ifelse(length(noc.2006.city[tech == "Tech Occupation" & GEO.CODE %in% cma.data[Name == input$comparison.cma[1][[1]],ID],V1])==0,
                                "NA",
                                comma(round(noc.2006.city[tech == "Tech Occupation" & GEO.CODE %in% cma.data[Name == input$comparison.cma[1][[1]],ID],V1]))),
                         str_c(signif(cma.ca.educ[EDUC15.ID == 10 & ALT.GEO.CODE %in% cma.data[Name %in% input$comparison.cma[1][[1]],ID],pct],2),"%"))
         final.table[,first:=first.city]
       }
       #Set up second column
       if(!is.na(input$comparison.cma[2][[1]])){
         second.city <- c(comma(round(noc.dem.tech.map[tech == 1 & ALT.GEO.CODE %in% cma.data[Name == input$comparison.cma[2][[1]],ID],V1])),
                          str_c(signif(noc.dem.tech.map[tech == 1 & ALT.GEO.CODE %in% cma.data[Name == input$comparison.cma[2][[1]],ID],pct],2),"%"),
                          ifelse(length(noc.2006.city[tech == "Tech Occupation" & GEO.CODE %in% cma.data[Name == input$comparison.cma[2][[1]],ID],V1])==0,
                                 "NA",
                                 comma(round(noc.2006.city[tech == "Tech Occupation" & GEO.CODE %in% cma.data[Name == input$comparison.cma[2][[1]],ID],V1]))),
                          str_c(signif(cma.ca.educ[EDUC15.ID == 10 & ALT.GEO.CODE %in% cma.data[Name %in% input$comparison.cma[2][[1]],ID],pct],2),"%"))
         final.table[,second:=second.city]
       }
       #Set up third column
       if(!is.na(input$comparison.cma[3][[1]])){
         third.city <- c(comma(round(noc.dem.tech.map[tech == 1 & ALT.GEO.CODE %in% cma.data[Name == input$comparison.cma[3][[1]],ID],V1])),
                         str_c(signif(noc.dem.tech.map[tech == 1 & ALT.GEO.CODE %in% cma.data[Name == input$comparison.cma[3][[1]],ID],pct],2),"%"),
                         ifelse(length(noc.2006.city[tech == "Tech Occupation" & GEO.CODE %in% cma.data[Name == input$comparison.cma[3][[1]],ID],V1])==0,
                                "NA",
                                comma(round(noc.2006.city[tech == "Tech Occupation" & GEO.CODE %in% cma.data[Name == input$comparison.cma[3][[1]],ID],V1]))),
                         str_c(signif(cma.ca.educ[EDUC15.ID == 10 & ALT.GEO.CODE %in% cma.data[Name %in% input$comparison.cma[3][[1]],ID],pct],2),"%"))
         final.table[,third:=third.city]
       }
       #Set up fourth column
       if(!is.na(input$comparison.cma[4][[1]])){
         fourth.city <- c(comma(round(noc.dem.tech.map[tech == 1 & ALT.GEO.CODE %in% cma.data[Name == input$comparison.cma[4][[1]],ID],V1])),
                          str_c(signif(noc.dem.tech.map[tech == 1 & ALT.GEO.CODE %in% cma.data[Name == input$comparison.cma[4][[1]],ID],pct],2),"%"),
                          ifelse(length(noc.2006.city[tech == "Tech Occupation" & GEO.CODE %in% cma.data[Name == input$comparison.cma[4][[1]],ID],V1])==0,
                                 "NA",
                                 comma(round(noc.2006.city[tech == "Tech Occupation" & GEO.CODE %in% cma.data[Name == input$comparison.cma[4][[1]],ID],V1]))),
                          str_c(signif(cma.ca.educ[EDUC15.ID == 10 & ALT.GEO.CODE %in% cma.data[Name %in% input$comparison.cma[4][[1]],ID],pct],2),"%"))
         final.table[,fourth:=fourth.city]
       }
       #Set up fifth column
       if(!is.na(input$comparison.cma[5][[1]])){
         fifth.city <- c(comma(round(noc.dem.tech.map[tech == 1 & ALT.GEO.CODE %in% cma.data[Name == input$comparison.cma[5][[1]],ID],V1])),
                         str_c(signif(noc.dem.tech.map[tech == 1 & ALT.GEO.CODE %in% cma.data[Name == input$comparison.cma[5][[1]],ID],pct],2),"%"),
                         ifelse(length(noc.2006.city[tech == "Tech Occupation" & GEO.CODE %in% cma.data[Name == input$comparison.cma[5][[1]],ID],V1])==0,
                                "NA",
                                comma(round(noc.2006.city[tech == "Tech Occupation" & GEO.CODE %in% cma.data[Name == input$comparison.cma[5][[1]],ID],V1]))),
                         str_c(signif(cma.ca.educ[EDUC15.ID == 10 & ALT.GEO.CODE %in% cma.data[Name %in% input$comparison.cma[5][[1]],ID],pct],2),"%"))
         final.table[,fifth:=fifth.city]
       }
       names(final.table) <- c("Metrics", input$comparison.cma)
     }
     final.table
   })
   
   output$comparison.topline <- renderTable(comparison.table.topline())
   
   output$download.topline.specific <- downloadHandler(filename = "tech_topline_selected.csv",
                                                       content=function(file){
                                                         write.csv(comparison.table.topline(),
                                                                   file,
                                                                   row.names = FALSE)
                                                       })
     
   ################ Output for second tab
   output$tech.pre.scatter <- renderPlotly({
     tech_premium[GEO.NAME=="Canada",color:="0"]
     tech_premium[GEO.NAME!="Canada",color:="1"]
     tech_premium[GEO.NAME==input$cma_div,color:="2"]
     
     scatter.tech.plot <- ggplot(data=tech_premium, aes(non.tech.pay,tech.pay,colour=color)) +
       geom_point(aes(text=paste(GEO.NAME,
                                 "<br>",
                                 "Average Pay in Tech Occupations:",
                                 str_c("$",comma(tech.pay)),
                                 "<br>",
                                 "Average Pay in Non-Tech Occupations:",
                                 str_c("$",comma(non.tech.pay)))),
                  size=2.3) +
       BF.Base.Theme +
       theme(axis.text.x = element_text(size = 9, colour = "#14365D"),
             axis.text.y = element_text(size = 9, colour = "#14365D"),
             axis.title.x = element_text(size = 9, colour = "#14365D"),
             axis.title.y = element_text(size = 9, colour = "#14365D"),
             axis.line.y = element_line(size = 0.25, colour = "#14365D"),
             axis.line.x = element_line(size = 0.25, colour = "#14365D"),
             axis.ticks.x = element_line(size = 0.25, colour = "#14365D"),
             axis.ticks.y = element_line(size = 0.25, colour = "#14365D"),
             legend.text = ggplot2::element_text(size=9,margin=ggplot2::margin(r=2),color = "#14365D")) +
       scale_colour_manual(values=c("#14365D","#707D85","#DD347A")) +
       scale_y_continuous(breaks = c(60000,75000,90000,105000),labels = c("$60,000","$75,000","$90,000","$105,000")) +
       scale_x_continuous(breaks = c(40000,50000,60000,70000),labels = c("$40,000","$50,000","$60,000","$70,000")) +
       labs(y="Average Pay in Tech Occupations",x="Average Pay in Non-Tech Occupations")
     graph <- config(layout(ggplotly(scatter.tech.plot,tooltip=c("text")),
                            legend = list(orientation = 'h'),
                            xaxis = list(fixedrange=TRUE),
                            yaxis = list(fixedrange=TRUE)),
                     displayModeBar=F)
     graph$x$data[[1]]$name <- "Canada"
     graph$x$data[[2]]$name <- "Other metropolitan areas"
     graph$x$data[[3]]$name <- input$cma_div
     print(graph)
   })
   
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
   
   output$gender.waffle.cma <- renderPlot({
     female.share <- round(tech_gender[GEO.NAME==input$cma_div,share_tech])
     plot.waffle.bf(c("Male"=100-female.share,"Female"=female.share),row.num = 10)
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
   
   output$gen.pay.gap.cma <- renderPlotly({
     tech_gender[GEO.NAME=="Canada",color:="0"]
     tech_gender[GEO.NAME!="Canada",color:="1"]
     tech_gender[GEO.NAME==input$cma_div,color:="2"]
     plot.female.pay <- ggplot(data=tech_gender,aes(reorder(GEO.NAME,pay.gap),pay.gap)) +
       geom_col(aes(fill=color,
                    text= paste(GEO.NAME,
                                "<br>",
                                "Gender Pay Gap in Tech Occupations:",
                                str_c("$",scales::comma(pay.gap)))),
                width=0.6) +
       BF.Base.Theme +
       scale_y_continuous(expand=c(0,0)) + 
       scale_fill_manual(values=c("#82C458","#14365D","#DD347A")) +
       guides(fill="none",colour = "none") +
       theme(axis.text.x = ggplot2::element_text(size=9, margin=ggplot2::margin(t=2),color="white",angle=90)) +
       labs(y="Gender Pay Gap in Tech Occupations",x="Each bar is a Metropolitan Area.")
     print(config(layout(ggplotly(plot.female.pay,tooltip=c("text")),
                         xaxis = list(fixedrange=TRUE),
                         yaxis = list(fixedrange=TRUE),
                         showlegend = FALSE),
                  displayModeBar=F))
     

   })
   
   output$pay.gap.text <- renderUI({
     gap.amount <- tech_gender[GEO.NAME %in% input$cma_div,pay.gap]
     p("In ",
       span(style = "color: #8AD4DF",
            paste(input$cma_div,",",sep=""),
            " the gender pay gap for tech workers was around",
            paste(" $",comma(signif(gap.amount,3)),sep=""),"."))

   })
   
   output$gen.comp.table <- renderTable({
     measure.vector <- c("Number of Female in Tech",
                         "Share of Female in Tech",
                         "Female Participation in Tech",
                         "Female Pay in Tech",
                         "Female Pay in non-Tech")
     city.vector <- c(comma(round(tech_gender[GEO.NAME %in% input$cma_div,V1])),
                      str_c(signif(tech_gender[GEO.NAME %in% input$cma_div,share_tech],2),"%"),
                      str_c(signif(tech_gender[GEO.NAME %in% input$cma_div,prop.tech],2),"%"),
                      str_c("$",comma(signif(tech_gender[GEO.NAME %in% input$cma_div,V2],3))),
                      str_c("$",comma(signif(tech_gender[GEO.NAME %in% input$cma_div,non.tech.pay],3))))
     
     can.vector <- c(comma(round(tech_gender[GEO.NAME == "Canada",V1])),
                     str_c(signif(tech_gender[GEO.NAME == "Canada",share_tech],2),"%"),
                     str_c(signif(tech_gender[GEO.NAME == "Canada",prop.tech],2),"%"),
                     str_c("$",comma(signif(tech_gender[GEO.NAME == "Canada",V2],3))),
                     str_c("$",comma(signif(tech_gender[GEO.NAME == "Canada",non.tech.pay],3))))
     
     table.to.display <- data.table("Measure"=measure.vector,"City"=city.vector,"Canada"=can.vector)
     names(table.to.display) <- c("Measure",stringr::str_wrap(input$cma_div,15),"Canada")
     table.to.display
   })
   
   output$vismin.comp.table <- renderTable({
     measure.vector <- c("Number of Visible Minority in Tech",
                         "Share of Visible Minority in Tech",
                         "Visible Minority Participation in Tech",
                         "Visible Minority Pay in Tech",
                         "Visible Minority Pay in non-Tech")
     city.vector <- c(comma(round(tech_vismin[GEO.NAME %in% input$cma_div & VIS.MIN15.ID == 2,V1])),
                      str_c(signif(tech_vismin[GEO.NAME %in% input$cma_div & VIS.MIN15.ID == 2,share.tech],2),"%"),
                      str_c(signif(tech_vismin[GEO.NAME %in% input$cma_div & VIS.MIN15.ID == 2,prop.tech],2),"%"),
                      str_c("$",comma(signif(tech_vismin[GEO.NAME %in% input$cma_div & VIS.MIN15.ID == 2,V2],3))),
                      str_c("$",comma(signif(tech_vismin[GEO.NAME %in% input$cma_div & VIS.MIN15.ID == 2,non.tech.pay],3))))
     
     can.vector <- c(comma(round(tech_vismin[GEO.NAME == "Canada" & VIS.MIN15.ID == 2,V1])),
                     str_c(signif(tech_vismin[GEO.NAME == "Canada" & VIS.MIN15.ID == 2,share.tech],2),"%"),
                     str_c(signif(tech_vismin[GEO.NAME == "Canada" & VIS.MIN15.ID == 2,prop.tech],2),"%"),
                     str_c("$",comma(signif(tech_vismin[GEO.NAME == "Canada" & VIS.MIN15.ID == 2,V2],3))),
                     str_c("$",comma(signif(tech_vismin[GEO.NAME == "Canada" & VIS.MIN15.ID == 2,non.tech.pay],3))))
     
     table.to.display <- data.table("Measure"=measure.vector,"City"=city.vector,"Canada"=can.vector)
     names(table.to.display) <- c("Measure",stringr::str_wrap(input$cma_div,15),"Canada")
     table.to.display
   })
   
   output$vismin.sumtext <- renderUI({
     num.vismin <- signif(tech_vismin[GEO.NAME %in% input$cma_div & VIS.MIN15.ID == 2, V1],3)
     p("In 2016, ",
       span(style ="color: #DD347A" ,
            paste(comma(num.vismin)," visible minorities worked",sep=""),
                   " in tech occupations in ",
                   paste(input$cma_div,".",sep="")))

   })
   
   
   output$vismin.paygap <- renderPlotly({
     tech_vismin[GEO.NAME=="Canada",color:="0"]
     tech_vismin[GEO.NAME!="Canada",color:="1"]
     tech_vismin[GEO.NAME %in% input$cma_div,color:="2"]
     plot.vismin.pay <- ggplot(data=tech_vismin[VIS.MIN15.ID==2],aes(reorder(GEO.NAME,pay.gap),pay.gap)) +
       geom_col(aes(fill=color,
                    text= paste(GEO.NAME,
                                "<br>",
                                "Visible Minority Pay Gap in Tech Occupations:",
                                str_c("$",scales::comma(pay.gap)))),
                width=0.6) +
       BF.Base.Theme +
       scale_y_continuous(expand=c(0,0)) + 
       scale_fill_manual(values=c("#82C458","#8AD4DF","#DD347A")) +
       guides(fill="none",colour = "none") +
       theme(axis.text.x = ggplot2::element_text(size=9, margin=ggplot2::margin(t=2),color="white",angle=90)) +
       labs(y="Visible Minority Pay Gap in Tech Occupations",x="Each bar is a Metropolitan Area.")
     print(config(layout(ggplotly(plot.vismin.pay,tooltip=c("text")),
                         xaxis = list(fixedrange=TRUE),
                         yaxis = list(fixedrange=TRUE),
                         showlegend = FALSE),
                  displayModeBar=F))
     

   })
   
   comparison.table.diversity <- reactive({
     topline.vars <- c("Overall Tech Worker Pay",
                       "Overall Non-Tech Worker Pay",
                       "Share of Tech Workers who are Female",
                       "Share of Female who are Tech Workers",
                       "Pay Difference by Sex",
                       "Share of Tech Workers who are in a Visible Minority Group",
                       "Share of Visible Minorities who are Tech Workers",
                       "Pay Difference by Visible Minority Identities")
     final.table.div <- data.table(first.col = topline.vars)
     names(final.table.div) <- c("Metrics")
     if(length(input$comparison.cma.div)>0){
       #Set up first column
       if(!is.na(input$comparison.cma.div[1][[1]])){
         first.city <- c(str_c("$",comma(signif(tech_premium[GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[1][[1]],GEO.CODE],tech.pay],3))),
                         str_c("$",comma(signif(tech_premium[GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[1][[1]],GEO.CODE],non.tech.pay],3))),
                         str_c(signif(tech_gender[ALT.GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[1][[1]],GEO.CODE],share_tech],2),"%"),
                         str_c(signif(tech_gender[ALT.GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[1][[1]],GEO.CODE],prop.tech],2),"%"),
                         str_c("$",comma(signif(tech_gender[ALT.GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[1][[1]],GEO.CODE],pay.gap],3))),
                         str_c(signif(tech_vismin[ALT.GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[1][[1]],GEO.CODE] & VIS.MIN15.ID == 2, share.tech],2),"%"),
                         str_c(signif(tech_vismin[ALT.GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[1][[1]],GEO.CODE] & VIS.MIN15.ID == 2, prop.tech],2),"%"),
                         str_c("$",comma(signif(tech_vismin[ALT.GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[1][[1]],GEO.CODE] & VIS.MIN15.ID == 2, pay.gap],3)))
         )
         final.table.div[,first:=first.city]
       }
       #Set up second column
       if(!is.na(input$comparison.cma.div[2][[1]])){
         second.city <- c(str_c("$",comma(signif(tech_premium[GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[2][[1]],GEO.CODE],tech.pay],3))),
                          str_c("$",comma(signif(tech_premium[GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[2][[1]],GEO.CODE],non.tech.pay],3))),
                          str_c(signif(tech_gender[ALT.GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[2][[1]],GEO.CODE],share_tech],2),"%"),
                          str_c(signif(tech_gender[ALT.GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[2][[1]],GEO.CODE],prop.tech],2),"%"),
                          str_c("$",comma(signif(tech_gender[ALT.GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[2][[1]],GEO.CODE],pay.gap],3))),
                          str_c(signif(tech_vismin[ALT.GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[2][[1]],GEO.CODE] & VIS.MIN15.ID == 2, share.tech],2),"%"),
                          str_c(signif(tech_vismin[ALT.GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[2][[1]],GEO.CODE] & VIS.MIN15.ID == 2, prop.tech],2),"%"),
                          str_c("$",comma(signif(tech_vismin[ALT.GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[2][[1]],GEO.CODE] & VIS.MIN15.ID == 2, pay.gap],3)))
         )
         final.table.div[,second:=second.city]
       }
       #Set up third column
       if(!is.na(input$comparison.cma.div[3][[1]])){
         third.city <- c(str_c("$",comma(signif(tech_premium[GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[3][[1]],GEO.CODE],tech.pay],3))),
                         str_c("$",comma(signif(tech_premium[GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[3][[1]],GEO.CODE],non.tech.pay],3))),
                         str_c(signif(tech_gender[ALT.GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[3][[1]],GEO.CODE],share_tech],2),"%"),
                         str_c(signif(tech_gender[ALT.GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[3][[1]],GEO.CODE],prop.tech],2),"%"),
                         str_c("$",comma(signif(tech_gender[ALT.GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[3][[1]],GEO.CODE],pay.gap],3))),
                         str_c(signif(tech_vismin[ALT.GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[3][[1]],GEO.CODE] & VIS.MIN15.ID == 2, share.tech],2),"%"),
                         str_c(signif(tech_vismin[ALT.GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[3][[1]],GEO.CODE] & VIS.MIN15.ID == 2, prop.tech],2),"%"),
                         str_c("$",comma(signif(tech_vismin[ALT.GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[3][[1]],GEO.CODE] & VIS.MIN15.ID == 2, pay.gap],3)))
         )
         final.table.div[,third:=third.city]
       }
       #Set up fourth column
       if(!is.na(input$comparison.cma.div[4][[1]])){
         fourth.city <- c(str_c("$",comma(signif(tech_premium[GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[4][[1]],GEO.CODE],tech.pay],3))),
                          str_c("$",comma(signif(tech_premium[GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[4][[1]],GEO.CODE],non.tech.pay],3))),
                          str_c(signif(tech_gender[ALT.GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[4][[1]],GEO.CODE],share_tech],2),"%"),
                          str_c(signif(tech_gender[ALT.GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[4][[1]],GEO.CODE],prop.tech],2),"%"),
                          str_c("$",comma(signif(tech_gender[ALT.GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[4][[1]],GEO.CODE],pay.gap],3))),
                          str_c(signif(tech_vismin[ALT.GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[4][[1]],GEO.CODE] & VIS.MIN15.ID == 2, share.tech],2),"%"),
                          str_c(signif(tech_vismin[ALT.GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[4][[1]],GEO.CODE] & VIS.MIN15.ID == 2, prop.tech],2),"%"),
                          str_c("$",comma(signif(tech_vismin[ALT.GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[4][[1]],GEO.CODE] & VIS.MIN15.ID == 2, pay.gap],3)))
         )
         final.table.div[,fourth:=fourth.city]
       }
       #Set up fifth column
       if(!is.na(input$comparison.cma.div[5][[1]])){
         fifth.city <- c(str_c("$",comma(signif(tech_premium[GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[5][[1]],GEO.CODE],tech.pay]),3)),
                         str_c("$",comma(signif(tech_premium[GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[5][[1]],GEO.CODE],non.tech.pay]),3)),
                         str_c(signif(tech_gender[ALT.GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[5][[1]],GEO.CODE],share_tech],2),"%"),
                         str_c(signif(tech_gender[ALT.GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[5][[1]],GEO.CODE],prop.tech],2),"%"),
                         str_c("$",comma(signif(tech_gender[ALT.GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[5][[1]],GEO.CODE],pay.gap],3))),
                         str_c(signif(tech_vismin[ALT.GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[5][[1]],GEO.CODE] & VIS.MIN15.ID == 2, share.tech],2),"%"),
                         str_c(signif(tech_vismin[ALT.GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[5][[1]],GEO.CODE] & VIS.MIN15.ID == 2, prop.tech],2),"%"),
                         str_c("$",comma(signif(tech_vismin[ALT.GEO.CODE %in% cma_list[GEO.NAME == input$comparison.cma.div[5][[1]],GEO.CODE] & VIS.MIN15.ID == 2, pay.gap],3)))
         )
         final.table.div[,fifth:=fifth.city]
       }
       names(final.table.div) <- c("Metrics", input$comparison.cma.div)
     }
     final.table.div
   })
   
   output$comparison.diversity <- renderTable(comparison.table.diversity())
   
   output$download.div.specific <- downloadHandler(filename = "tech_diversity_selected.csv",
                                                   content=function(file){
                                                     write.csv(comparison.table.diversity(),
                                                               file,
                                                               row.names = FALSE)
                                                   })
   

     


}

# Run the application 
shinyApp(ui = ui, server = server)

