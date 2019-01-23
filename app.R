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
                tags$head(HTML('<script async src="https://www.googletagmanager.com/gtag/js?id=UA-79862704-1"></script>
<script>
 window.dataLayer = window.dataLayer || [];
 function gtag(){dataLayer.push(arguments);}
 gtag("js", new Date());

 gtag("config", "UA-79862704-1");
</script>')),
                
                
                
                # Application title
                div(class="header",
                    style = "background: #fff; margin-left: 0%; margin-right: 0%; font-size: 16px !important; max-width: unset; padding-right: 10%; padding-left: 10%; padding-bottom: 20px; padding-top:16px; border-bottom: 6px solid #d1236c",
                    includeHTML("www/header.html")),
                
                
                titlePanel("",windowTitle = "Brookfield Institute - Canada's Tech Compass"), #Name to show up on browser. Favicon is set in css document
                #div(style = "margin-right: 10%; margin-left: 10%; padding-bottom: 15px", h1("Canada's Tech Diary")),
               
                div(class="nonmobile",
                    style="padding-left:10%; padding-right:10%;position:relative;",
                    div(style="margin-left:auto;margin-right:auto;position:relative;display:block",
                        img(class="techimage4",src="tech_pic_4.png"),
                        p(style="margin-bottom:0px; font-size:25px; font-family:'rooneysansbold' !important; color: #d1236c; padding-right: 40px","Explore") #End p
                        ) #End div
                    ), #End div
                div(class="nonmobile",
                    style="padding-left:10%; padding-right:10%;position:relative;",
                    img(style="margin-left:auto;margin-right:auto;position:relative;display:block; height:70px",
                        src="Arrows.svg")),
                
                div(class="filler"),
                navbarPage("",
                           tabPanel("Canada's Tech Dashboard (Cities)",
                                    
                                    fluidRow(div(class="sectiontitlewrapper",h2("Canada's Tech Dashboard (Cities)")),
                                             div(style="padding-top: 30px; padding-bottom: 30px",
                                                 p("Who are the people that make up Canada’s tech industries? How many are there? Where do they work? What cities can you find them in? How educated are they?"),
                                                 p("Journey through our latest insights on how tech talent is shaping up across Canada and find our more about how your city stacks up to different cities and regions across the nation. Or,
                                                   if you'd like,",a("jump",href="#generalcomparison")," to the end to compare between different cities and towns."))
                                                 ), #End FluidRow
                                    div(class="filler_sel_1"), #Filler for the navbar
                                    fluidRow(align="center",
                                             class="selectize-city",
                                             div(div(style="display: inline-block;vertical-align:middle; height:34px",
                                                     p("Of the cities in the province/territory of")),
                                                 div(style="display: inline-block; height: 34px",
                                                     selectInput("province",
                                                                 label = "",
                                                                 choices = c("British Columbia"="BC",
                                                                             "Alberta"="AB",
                                                                             "Saskatchewan"="SK",
                                                                             "Manitoba"="MB",
                                                                             "Ontario"="ON",
                                                                             "Quebec"="QC",
                                                                             "New Brunswick"="NB",
                                                                             "Prince Edward Islands"="PE",
                                                                             "Nova Scotia"="NS",
                                                                             "Newfoundland and Labrador"="NL",
                                                                             "Northwest Territories"="NT"),
                                                                 selected="ON"))),
                                             div(div(style="display: inline-block;vertical-align:middle; height:34px",
                                                     p("Tell me more about tech jobs in ")),
                                                 div(style="display: inline-block;vertical-align:middle; height: 34px",
                                                     uiOutput("CMA_choice")))
                                             
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
                                                      span(style='color: #e24585',"1 million tech workers"),"."), #End p
                                                    p("Depending on where they live, tech workers have different jobs, earnings and backgrounds.")
                                             ), #End Column
                                             
                                             column(width=4,
                                                    class="popupbox",
                                                    div(h4("What's a tech worker!?"),
                                                        p("In our report, we define tech occupations to be occupations that require high competency in tech skills. 
                                                          We use the", 
                                                          a(href="http://noc.esdc.gc.ca/English/noc/welcome.aspx?ver=16","National Occupational Classification (NOC)"), 
                                                          "to define our occupations, and US's" ,
                                                          a(href="https://www.onetonline.org/","O*NET Skills"), 
                                                          "taxonomy to look at each occupation's tech intensity."
                                                        ) #End p
                                                        ) #End Div
                                                    ) #End Column
                                             
                                             ), #EndFluidRow
                                    
                                    img(src="tech_pic_8.png",class="techimage8"),
                                    fluidRow(align="center",
                                             style="font-size: 28px; padding-top: 50px; padding-bottom: 50px; position:relative; z-index:1; background: transparent",
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
                                                                 choiceNames = list(HTML('<i class="fa fa-circle-o social-icons" style = "font-size:1.1em"></i><i class="fa fa-dot-circle-o" style = "font-size:1.1em"></i> <div class=tooltiphelp style="border-bottom: none"><p style="padding-left:10px; font-size: 14px">Total Tech Workers</p><span class=tooltiptexthelp>Total number of workers in tech occupations in a geographic area</span></div>'),
                                                                                    HTML('<i class="fa fa-circle-o social-icons" style="font-size:1.1em"></i><i class="fa fa-dot-circle-o" style = "font-size:1.1em"></i> <div class=tooltiphelp style="border-bottom: none"><p style="padding-left:10px; font-size: 14px">Concentration of Tech Workers</p><span class=tooltiptexthelp>Share of all workers in a geographic region who are tech workers</span></div>')),
                                                                 selected = "Total Tech Workers",
                                                                 label="Show me"),
                                                    fluidRow(style = "margin-left:inherit; margin-right:inherit;padding-left:0; padding-right:0",
                                                             div(style="background: #d1236c; padding-top: 15px; padding-bottom: 15px; margin-bottom: 7px; color: #fff; padding-left: 10px; padding-right: 10px",
                                                                 h4(style="font-size:14px","Top 10 Occupations Locally"),
                                                                 p(style="font-size: 0.7em","These occupations are not amongst the national top 10 tech occupations by employment.")),
                                                             div(style="background: #072b49; padding-top: 15px; padding-bottom: 15px; color: #fff; padding-left: 10px; padding-right: 10px",
                                                                 h4(style="font-size:14px","Top 10 Occupations Canada-wide"),
                                                                 p(style="font-size: 0.7em","These occupations are also in the national top 10 tech occupations by employment.")))), #End Column
                                             column(align = "center",
                                                    textOutput("CMA_chosen_3"),
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
                                             column(width=3,img(src="tech_pic_1.png",style="width:100%; min-width:160px"))
                                    ), #EndFluidRow
                                    
                                    #START SHOWING EDUCATION STUFF FROM HERE
                                    
                                    fluidRow(class="sectiontitlewrapper",
                                             h3("Educational Attainment of Tech Workers")
                                    ), #End FluidRow
                                    img(src="tech_pic_7.png",class="techimage7"),
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
                                             column(width=4,
                                                    img(src="tech_pic_2.png",style="width:100%; min-width:160px")),
                                             column(width=8,
                                                    htmlOutput("educ.text"),
                                                    p("In the", a(href="https://brookfieldinstitute.ca/report/who-are-canadas-tech-workers", "report,"), "we dove into workers' postsecondary degree specialization.
                                                      Though we do not present this data at the city/town level, tech workers came from a variety of backgrounds,
                                                      with a largest shares coming from traditional STEM programs such as Mathematics, Computer Science and Physics. A sizeable share of the 
                                                      tech workforce also specialized in Business, Commerce and Management related fields."))
                                                    ), #End FluidRow
                                    
                                    #START SHOWING COMPARISON FROM HERE
                                    
                                    fluidRow(class="sectiontitlewrapper finalsection",
                                             h3("Compare Between Cities/Towns")
                                    ), #EndFluidRow
                                    a(id="generalcomparison"),
                                    
                                    fluidRow(p("Now that you have a bit of a better grasp of tech workers across the nation, 
                                               use the tool below to compare key statistics for up to 5 cities and towns across Canada.")
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
                           tabPanel("Diversity Compass (Metropolitan Areas)",
                                    fluidRow(div(class="sectiontitlewrapper",h2("Canada's Tech Workers: Diversity Compass")),
                                             div(style="padding-top: 30px; padding-bottom: 30px",
                                                 p("Diversity is a focal point for many conversations in tech. Let's explore how different identities, like gender and visible minority identities,  
                                                 affect participation and earnings in tech occupations in Canada's metropolitan areas. Or, if you'd like,",a("jump",href="#diversityselection")," to the end to compare different metropolitan areas."))
                                             ), #End FluidRow
                                    div(class="filler_sel_2"), #Filler for the navbar
                                    fluidRow(align = "center",
                                             class = "selectize-metro",
                                             selectInput("cma_div","Select a Metropolitan Area:",
                                                         choices = sort(cma_list[,GEO.NAME]),
                                                         selected="Toronto")
                                    ), #EndFluidRow
                                    
                                    fluidRow(class="alignedrow",
                                             column(width = 7,
                                                    plotlyOutput("tech.pre.scatter",height = "500px")),
                                             column(width = 5,
                                                    p("Consistently, tech workers across Canada received higher average pay than non-tech workers.
                                                      This pay differential was consistent across all demographic and geographic groups."),
                                                    htmlOutput("tech.premium.text"))
                                             
                                             ), #EndFluidRow
                                    
                                    #GENDER DIVERSITY STARTS HERE
                                    fluidRow(class="sectiontitlewrapper",
                                             h3("Gender Diversity")
                                             
                                    ), #End FluidRow
                                    
                                    fluidRow(column(width = 5,
                                                    class = "column-border-right",
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
                                                    class="column-border-right",
                                                    tableOutput("gen.comp.mf.table")),
                                             column(width=7,
                                                    align = "left",
                                                    htmlOutput("gen.gap.text"))
                                             
                                    ), #EndFluidRow
                                    
                                    fluidRow(class="alignedrow",
                                             column(width = 5,
                                                    img(src="tech_pic_3.png", style="width:100%; min_width: 180px")),
                                             column(width = 7, 
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
                                    img(class="specialimage2",src="tech_pic_6.png"),
                                        
                                    fluidRow(class="sectiontitlewrapper",
                                             h3("Visible Minority (VM)")
                                    ), #End FluidRow
                                    
                                    
                                    fluidRow(class="alignedrow",
                                             column(width=8,
                                                    style = "margin-right:3%",
                                                    class="column-border-right",
                                                    htmlOutput("vismin.comp.table")),
                                             column(width=4,
                                                    class="popoutbox",
                                                    div(h4("Visible Minority Identities"),
                                                        p('2016 Canadian Census uses the term visible minority to refer to whether a person belongs to a visible
                                                          minority group as defined by the Employment Equity Act, which defines visible minorities as 
                                                          "persons, other than Aboriginal peoples, who are non-Caucasian in race or non-white in colour."'
                                                        ) #End p
                                                        ) #End div
                                                        ) #End Column

                                             ), #End FluidRow
                                    
                                    fluidRow(class="alignedrow",
                                             column(width=8,
                                                    p("Almost one-third of Canada's tech workers identify as visible minorities (31.9 percent).
                                                      8.5 percent or almost one in twelve visible minority workers worked in a tech occupation, totalling 271,000 people."),
                                                    htmlOutput("vismin.sumtext"),
                                                    p("Yet, there are important differences between visible minority groups.
                                                      In this application, we focus broadly on the idea of visible minorities. For a detailed look at how specific groups
                                                      compare to each other, check out our main report.")
                                             ), #End column
                                             
                                             column(width=4,
                                                    img(src="tech_pic_4.png", style="width:100%; min_width: 180px")
                                                    ) #End Column
                                                    ), #End FluidRow
                                    
                                    fluidRow(class="alignedrow",
                                             column(width = 6,
                                                    class="column-border-right",
                                                    tableOutput("vismin.comp.nonvismin.table")
                                             ), #End column
                                             column(width=6,
                                                    htmlOutput("vismin.paygap.text")
                                                    ) #End Column
                                                    ), #EndFluidRow
                                    
                                    fluidRow(plotlyOutput("vismin.paygap",height="600px")
                                    ), #End FluidRow
                                    
                                    
                                    
                                    
                                    
                                    
                                    #COMPARISON BEGINS HERE
                                    
                                    fluidRow(class="sectiontitlewrapper finalsection",
                                             h3("Compare Between Metropolitan Areas")
                                    ), #EndFluidRow
                                    a(id="diversityselection"),
                                    fluidRow(p("Now that you've gotten a better idea of the diversity among Canadian tech workers, you may want to compare between
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
                         h3("About This Data Visualization")),
                fluidRow(style="font-size:1.1em",p("This data visualization was created by the Brookfield Institute for Innovation and Entrepreneurship
                                                   to accompany the", a(href="https://brookfieldinstitute.ca/report/who-are-canadas-tech-workers","State of Canada's Tech Sector 2018: Tech Workers report."), "See the",a(href="https://github.com/BrookfieldIIE/sotw-2018-dataviz", "source code on GitHub.")),
                         p("The report was authored by Asher Zafar, Viet Vu, and Creig Lamb. The data visualization was developed by Viet Vu using R, Shiny, and Javascript."),
                         p("This report, as well as the data visualization, would not have been possible without support from the following individuals:
                           Sean Zohar, Jessica Thomson, Nisa Malli, Annalise Huynh, Andrew Do, Sarah Doyle, as well as our reviewers."),
                         p("We also thank ",a(href="https://www.spencerflock.com/", "Spencer Flock"),", a Toronto based artist, for the illustration used in this data visualization."),
                         p("The main data source for the report and the visualization is the 2016 and 2006 Canadian long form census."),
                         p("Please send any feedback, comments, or questions to", a(href="mailto:brookfield.institute@ryerson.ca", "brookfield.institute@ryerson.ca")),
                         
                         div(style="margin-top:25px;border-top: 1px solid #e3e3e3",p(style="font-style: italic","For media enquiries, please contact", 
                                                                                     a(href="https://brookfieldinstitute.ca/team/coralie-dsouza","Coralie D’Souza"), 
                                                                                     "Director of Communications, Events + Community Relations at the 
                                                                                     Brookfield Institute for Innovation + Entrepreneurship.")
                         ) #End Div
                         ), #End FluidRow
                
                fluidRow(style="background: #072b49; margin-left: 0%; margin-right: 0%; font-size: 12px !important; max-width: unset; padding-top:64px; padding-bottom:24px",
                         div(class="footer",
                             style = "padding-left: 5%; padding-right: 5%",
                             includeHTML("www/site_footer.html"))),
                
                tags$script(src = "choose_city.js")
                
                         ) #EndFluidPage



server <- function(input, output) {
  #Text of the chosen city
  output$CMA_choice <- renderUI(
    selectInput("cma",
                label=NULL,
                choice = sort(unique(cma.data[stringr::str_sub(ID,1,2)==province.data[abbrev==input$province,code],Name])),
                selected = "Toronto"))
  
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
  
  
  
  #This reactive element isolates the ID for the cma element that will be used throughout so it only needs to update once for every change
  #in city as opposed to once in every single update function.
  cma.id.reactive <- reactive({
    p <- "35507"
    if(!is.null(input$cma)){
      p <- cma.data[Name %in% input$cma,ID]
    }
    p
  })
  
  #Text for focusing on a particular CMA
  output$CMA_chosen_2 <- renderText({
    paste("Let's learn about ", input$cma,", ",input$province,"'s story.",sep="")})
  
  output$CMA_chosen_3 <- renderText({
    paste(input$cma,"'s Tech Talent",sep="")
  })
  
  #Interactive column plot of all the cities with the chosen city highlighted
  output$cmatot <- renderPlotly({
    plot.cmatot(name_to_use=cma.reactive(),
                cma.id=cma.id.reactive())
  })
  
  #Top occupations both Absolute and Relative
  output$cmatopocc <- renderPlotly(plot.cmaocc(cma.reactive(),
                                               input$pct_or_tot,
                                               cma.id=cma.id.reactive()))
  
  #Canada's top occupation both absolute and relative
  output$canadatopocc <- renderPlotly(plot.canocc(input$pct_or_tot))
  
  #Change in the tech workforce in text
  output$cmatotnumtext <- renderUI({
    list.largest.city <- noc.dem.tech.map[tech==1,max(V1),by=str_sub(ALT.GEO.CODE,1,2)]
    largest.city.pop <- list.largest.city[str_sub %in% province.data[abbrev==input$province],V1]
    largest.city.name <- noc.dem.tech.map[tech==1 & str_sub(ALT.GEO.CODE,1,2) %in% province.data[abbrev==input$province,code] & V1 == largest.city.pop,GEO.NAME]
    times.tech <- signif(largest.city.pop/noc.dem.tech.map[ALT.GEO.CODE %in% cma.id.reactive() & tech==1,V1],3)
    if(input$cma==largest.city.name){
      div(p("In 2016, ",
            span(class="focused-text",
                 style="color:#e24585",
                 paste(comma(noc.dem.tech.map[ALT.GEO.CODE %in% cma.id.reactive() & tech==1,V1]),
                       " tech workers worked and lived in ",
                       paste(input$cma,
                             ".",sep=""))),
            " This represents ",
            span(class="focused-text",
                 style = "color:#e24585",
                 paste(round(100*noc.dem.tech.map[ALT.GEO.CODE %in% cma.id.reactive() & tech==1,V1]/891720,2),
                       " percent of all tech workers in Canada.",sep="")
            ),
            "In the past 10 years, 
            over 180,000 workers joined the tech workforce in Canada and 180,000 more are predicted to 
            join in the next 10 years according to", 
            a(href="http://occupations.esdc.gc.ca/sppc-cops/w.2lc.4m.2@-eng.jsp",
              "Employment and Social Development Canada's"),
            "projections."),
          p(paste(largest.city.name,
                  " was the city with",sep=""), #end paste
            span(class="focused-text",
                 style="color:#e24585",
                 "the largest tech workforce"),
            paste(" in ",
                  province.data[abbrev==input$province,province],
                  ".",
                  sep="")
            ) #end p
          )
    }
    else{
      div(p("In 2016, ",
            span(class="focused-text",
                 style="color:#e24585",
                 paste(comma(noc.dem.tech.map[ALT.GEO.CODE %in% cma.id.reactive() & tech==1,V1]),
                       " tech workers worked and lived in ",
                       paste(input$cma,
                             ",",sep=""))),
            " representing ",
            span(class="focused-text",
                 style = "color:#e24585",
                 paste(round(100*noc.dem.tech.map[ALT.GEO.CODE %in% cma.id.reactive() & tech==1,V1]/891720,2),
                       " percent of all tech workers in Canada.",sep="")
            ),
            "In the past 10 years, 
            over 180,000 workers joined the tech workforce in Canada. 180,000 more are predicted to 
            join in the next 10 years according to", 
            a(href="http://occupations.esdc.gc.ca/sppc-cops/w.2lc.4m.2@-eng.jsp",
              "Employment and Social Development Canada's"),
            "projections."),
          p(paste(largest.city.name,
                  ", the city with the most tech workers in ",
                  province.data[abbrev==input$province,province],
                  sep=""),
            "had",
            span(class="focused-text",
                 style="color:#e24585",
                 paste(times.tech,
                       " times",
                       sep="")),
            paste("the tech workers in ",
                  input$cma,
                  ".",
                  sep="")
            ) #End p
          ) #End div
                  
    }
    

  })
  
  
  #Workforce in 2006 vs 2016 comparison text
  output$cmain2006 <- renderUI({
    if(nrow(noc.2006.city[tech=="Tech Occupation" & GEO.CODE %in% cma.id.reactive()])==0){
      p("Unfortunately,",paste(input$cma,"'s",sep="") ,"population was too low in 2006 to have publicly published data.")
    }
    else{
      difference <- noc.dem.tech.map[ALT.GEO.CODE %in% cma.id.reactive() & tech==1,V1] - noc.2006.city[tech=="Tech Occupation" & GEO.CODE %in% cma.id.reactive(),V1]
      if(abs(difference)/noc.dem.tech.map[ALT.GEO.CODE %in% cma.id.reactive() & tech==1,V1] < 0.01 | abs(difference)<50){
        
        p("Looking at the Census in 2006, we find that there were",
          span(class="focused-text",
               style="color: #e24585",
               paste(comma(noc.2006.city[tech=="Tech Occupation" & GEO.CODE %in% cma.id.reactive(),V1]),
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
            span(class="focused-text",
                 style="color: #e24585",
                 paste(comma(noc.2006.city[tech=="Tech Occupation" & GEO.CODE %in% cma.id.reactive(),V1]),
                       " tech workers in ",
                       input$cma,
                       ".",
                       sep="")
            ),
            " This means that the number of tech workers has decreased over the past 10 years",
            paste("by ",comma(abs(difference))," workers.",sep="")
          )
        }
        else{
          pct_change <- 100*difference/noc.dem.tech.map[ALT.GEO.CODE %in% cma.id.reactive() & tech==1,V1]
          p("Looking at the Census in 2006, we find that there were",
            span(class="focused-text",
                 style="color: #e24585",
                 paste(comma(noc.2006.city[tech=="Tech Occupation" & GEO.CODE %in% cma.id.reactive(),V1]),
                       " tech workers in ",
                       input$cma,
                       ".",
                       sep=""
                 )
            ),
            " This means that the number of tech workers have increased over the past 10 years",
            paste("by ",comma(abs(difference))," workers.",sep=""),
            " In other words,",
            paste(signif(pct_change,2),"% of tech workers in ",input$cma," joined the tech workforce in the past 10 years.",sep="")
          )         
        }
      }
    }
    
  })
  
  #Plot the main education graph
  output$educ.column <- renderPlotly(plot.educ(cma.reactive(),cma.id=cma.id.reactive()))
  
  #Text for the education section
  output$educ.text <- renderUI({
    p("Tech Workers in Canada are highly educated. 
      In fact, almost 58 percent of tech workers hold a Bachelor's degree or above, 
      compared to 26 percent of workers not in tech occupations.",
      span(class="focused-text",
           style="color: #e24585",paste("In ",input$cma,", ",
                                        round(cma.ca.educ[ALT.GEO.CODE %in% cma.id.reactive() & EDUC15.ID==10,pct])," percent", sep = ""),
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
    p(span(class="focused-text",
           style ="color: #e24585",
           "In ",
           paste(input$cma_div,",",sep=""),
           " tech workers were paid ",
           paste("$",comma(signif(tech_premium[GEO.NAME==input$cma_div,tech.pay])), sep=""),
           " on average, ",
           paste("$",comma(signif(tech_premium[GEO.NAME==input$cma_div,tech.pay-non.tech.pay])), sep=""),
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
          span(class="focused-text",
               style = "color: #e24585",
               paste(input$cma_div,",",sep=""),
               " the gender pay gap for tech workers was around",
               paste(" $",comma(signif(gap.amount,3)),".",sep=""))))
    
    
    
  })
  
  output$gen.gap.text <- renderUI({
    ratio <- round(tech_gender[GEO.NAME %in% input$cma_div, male.prop.tech/prop.tech],1)
    div(p("In",
          paste(input$cma_div,",",sep=""),
          "men were",
          span(class="focused-text",
               style = "color: #e24585", 
               paste(ratio, " times more likely",sep="")),
          " to work in a tech occupation,",
          "where",
          span(class="focused-text",
               style = "color: #e24585",
               paste(round(tech_gender[GEO.NAME %in% input$cma_div,prop.tech],1),
                     " percent",
                     " of women",sep = "")), " worked in tech occupations.",sep=""))
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
      span(class="focused-text",
           style ="color: #e24585" ,
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
      span(class="focused-text",
           style="color: #e24585",
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

