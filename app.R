#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#library(BFTheme)
library(stringr)
library(data.table)
library(ggplot2)
#library(extrafont)
library(plotly)


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

#Set up theme elements
BF.Base.Theme <- ggplot2::theme(panel.background = ggplot2::element_rect(fill="transparent", colour=NA), #Make sure plot area background is transparent
                                plot.background = ggplot2::element_rect(fill="transparent", colour=NA), #Make sure render area background is transparent
                                axis.line = ggplot2::element_line(size=0.25, colour = "#B1B8BC"), #Set axis line width and set colour to grey
                                axis.ticks = ggplot2::element_line(size=0.25, colour = "#B1B8BC"), #Set axis tick width and set colour to grey
                                panel.grid.major = ggplot2::element_blank(), #Remove the panel grid lines
                                panel.grid.minor = ggplot2::element_blank(), #Remove the panel grid lines
                                text = ggplot2::element_text(color="white"), #Set the font for every text element (except for geom elements)
                                plot.title = ggplot2::element_text(size=9,color="white"), #Format figure number
                                plot.subtitle = ggplot2::element_text(size=12,color="white"), #Format main title
                                plot.caption = ggplot2::element_text(face="italic", size=8.2, margin=ggplot2::margin(t=10),hjust=0,colour="#B1B8BC"), #Format for caption and other notes
                                legend.background = ggplot2::element_rect(fill="transparent",colour=NA), #Make sure legend box is transparent (for export)
                                legend.key = ggplot2::element_blank(), #Background on each key is transparent
                                legend.box.margin = ggplot2::margin(b=4,t=6), #Set margin for the box for appropriate distance
                                legend.title = ggplot2::element_text(size=10,hjust=0.5,color="white"), #Legend title text settings, also make centre it. Light so it's not as prominent
                                legend.title.align = 0.5,
                                legend.text = ggplot2::element_text(size=9,margin=ggplot2::margin(r=2),color="white"), #Legend text settings. Light so it's not as prominent
                                legend.margin = ggplot2::margin(b=1), #Small margin in the bottom
                                legend.position = "top", #Set the legend to top so panel can be full width (for export)
                                legend.box.spacing = ggplot2::unit(c(0,0,0,0),units=c("cm","cm","cm","cm")), #Legend box spacing - maybe not needed?
                                axis.text.x = ggplot2::element_text(size=9, margin=ggplot2::margin(t=2),color="white"), #Set axis text. Light to make it less prominent - margin is also precise
                                axis.text.y = ggplot2::element_text(size=9, margin=ggplot2::margin(r=2),color="white"), #Set axis text. Light to make it less prominent - margin is also precise
                                axis.title.x = ggplot2::element_text(size=10, margin=ggplot2::margin(t=4),color="white"), #Set axis title. Margin is also precise
                                axis.title.y = ggplot2::element_text(size=10, margin=ggplot2::margin(r=4),color="white"))

set.colours <- function(n,
                        type = "categorical",
                        gradient.choice = "dark.blue",
                        categorical.choice = NULL,
                        special = NULL){
  #Setting all the base vectors to refer to - precise because I don't trust R's generation of gradients
  base.set <- c("dark.blue"="#14365D","light.blue"="#8AD4DF","pink"="#DD347A","yellow"="#FFC800",
                "magenta"="#79133E","orange"="#F7941E","green"="#82C458","teal"="#005F61","grey"="#707D85")
  dark.blue <- c("#14365D","#29486B","#3E5A7A","#546C89","#697F97","#7E91A6","#94A3B5","#A9B5C4")
  light.blue <- c("#8AD4DF","#94D7E1","#9FDBE4","#A9DFE7","#B4E3EA","#BFE7ED","#C9EBF0","#D4EFF3")
  pink <- c("#DD347A","#E04686","#E35892","#E66B9E","#E97DAA","#EC90B6","#EFA2C2","#F2B5CE")
  yellow <- c("#FFC800","#FFCD17","#FFD22E","#FFD745","#FFDC5C","#FFE173","#FFE68B","#FFEBA2")
  magenta <- c("#79133E","#85284F","#913D61","#9D5372","#A96884","#B57E95","#C293A7","#CEA9B8")
  orange <- c("#F7941E","#F79D32","#F8A746","#F9B15B","#F9BA6F","#FAC484","#FBCE98","#FCD8AD")
  green <- c("#82C458","#8DC967","#98CE76","#A4D485","#AFD994","#BADEA3","#C6E4B3","#D1E9C2")
  teal <- c("#005F61","#176D6F","#2E7C7D","#458A8C","#5C999A","#73A7A8","#8BB6B7","#A2C4C5")
  grey <- c("#707D85","#7D8890","#8A949B","#97A0A6","#A4ACB1","#B1B8BC","#B1B8BC","#B1B8BC")
  #Check if you have way too many categories - 7 is the absolute max!
  if(n > 7){
    stop("You have way too many categories. Reduce it!")
  }
  #Check if the type is categorical
  if(type == "categorical"){
    if(is.null(categorical.choice)){ #Check if a specific colour set was requested
      return(unname(base.set[1:n])) #If not then return sequential from dark blue,light blue to pink
    }
    else{
      if(length(categorical.choice)!=n){ #Check if the length of choice matches requested number of colours
        stop("You didn't have the same number of colours as you specified. Change n or categorical.choice") #This is for sanity check, not because of code
      }
      return(unname(base.set[categorical.choice])) #Return the corresponding set of colours
    }
  }
  if(type == "gradient"){ #On the otherhand, if it's a gradient
    #Set up all the gradient choices
    gra2 <- c(1, 5)
    gra3 <- c(1, 4, 7)
    gra4 <- c(1, 3, 5, 7)
    gra5 <- c(1, 2, 4, 6, 7)
    gra6 <- c(1, 2, 3, 4, 6, 7)
    gra7 <- c(1, 2, 3, 4, 5, 6, 7)
    return(get(gradient.choice)[get(str_c("gra",n))]) #Get the right number of gradients.
  }
}

plot.waffle.bf <- function(named.vector,
                           row.num=5,
                           colours = NULL,
                           plot.title="",
                           plot.fig.num="",
                           plot.cat.title="",
                           caption = "",
                           x.axis="",
                           dividing.line=FALSE,
                           labels=FALSE,
                           label.unit = "",
                           export = FALSE,
                           export.name = "Rplot") {
  
  #Set up the main theme element
  waffle.theme <- BF.Base.Theme +
    theme(panel.spacing = ggplot2::unit(c(0,0,0,0),units=c("cm","cm","cm","cm")),
          axis.text = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          axis.line = ggplot2::element_blank())
  #Calculate the coordinates for the waffle chart
  num.square <- sum(named.vector) #Get the number of squares needed
  most.x <- floor(num.square/row.num) #Find the maximum x value that fits in a square
  left.x.max <- num.square-most.x*row.num #Find out how many squares don't fit
  label.vector <- rep(names(named.vector),named.vector) #Create a vector of labels enumerated by labels
  x.vector <- c(rep(seq(1,most.x),rep(row.num,most.x))) #Get the divisible part of the x coordinates
  y.vector <- c(rep(seq(1,row.num),most.x)) #Get the divisible part of the y coordinates
  id <- seq(1,num.square) #Get id for reference later on if needed
  if(left.x.max!=0){ #If there are non divisible part
    x.vector <- c(x.vector,rep(most.x+1,left.x.max)) #Add the x coordinate for the last few squares
    y.vector <- c(y.vector,seq(1,left.x.max)) #Add the y coordinate for the last few squares
  }
  main.data <- data.table::data.table(id=id,label=label.vector,xc=x.vector,yc=y.vector) #Get the squares into a data table
  main.data <- as.data.table(main.data)
  if(is.null(colours)){
    colours <- set.colours(length(named.vector)) #Get the colour vector for fill
  }
  #Set up base plot
  p <- ggplot2::ggplot(main.data,aes(xc,yc,fill=label)) +
    waffle.theme +
    ggplot2::geom_tile(width=0.75,height=0.75) +
    ggplot2::scale_fill_manual(values=colours,labels = str_c(sort(names(named.vector)),"    ")) +
    ggplot2::coord_fixed(ratio=1)
  #Dealing with dividing line problems - inefficient and can be improved (Future)
  if(dividing.line){
    length.vec <- length(named.vector) - 1 #Only need divicing lines for everything but the last category
    for(n in names(named.vector[1:length.vec])){
      max.x <- max(main.data[label==n,xc])
      max.y <- max(main.data[label==n & xc==max.x,yc])
      min.y <- min(main.data[label==n & xc==max.x,yc])
      p <- p + ggplot2::annotate("segment",x=max.x+0.5,y=min.y-0.5,xend=max.x+0.5,yend=max.y+0.5,linetype=2,colour="#727D84") #Draw all the right lines to border cells
      if(max.y < row.num){ #Check if the top border cell is the top row.
        p <- p + ggplot2::annotate("segment",x=max.x-0.5,y=max.y+0.5,xend=max.x+0.5,yend=max.y+0.5,linetype=2,colour="#727D84") #Draw a border to the top cell
        p <- p + ggplot2::annotate("segment",x=max.x-1+0.5,y=max.y+1-0.5,xend=max.x-1+0.5,yend=row.num+0.5,linetype=2,colour="#727D84") #Draw the right border for rest of the cells
      }
    }
  }
  #Dealing with label stuff
  if(labels){
    prev.x <- 0 #This is to see if labels will be too close to each other - approximate
    prev.y <- -1 #This is to adjust the y height if labels are too close to each other - approximate
    for(n in names(named.vector)){
      random <- sample(0:1,1) #Sometimes put label on the bottom, sometimes on top
      if(random==1){
        max.y <- min(main.data[label==n,yc])
      }
      else{
        max.y <- max(main.data[label==n,yc])
      }
      max.x <- max(main.data[label==n & yc==max.y,xc]) #This jointly with max.y determines which cell the arrow will come from
      label <- stringr::str_c(named.vector[n],label.unit," ",n) #Generate label texts
      if(max.y<row.num/2){
        y.dest <- 0
      }
      else{
        y.dest <- row.num+1
      }
      if(max.x-prev.x<=2 & prev.y==y.dest){ #Check to see if labels are too close to each other
        if(y.dest==0){
          y.dest <- y.dest-0.75
        }
        else{
          y.dest <- y.dest+0.75
        }
      }
      size.text <- nchar(label)/3.7 #Approximate text size so it won't collide with the segments
      p <- p +
        ggplot2::annotate("segment",x = max.x, y = max.y, xend = max.x - 1, yend = y.dest, colour="#727D84", size=0.25) + #Add the diagonal segment
        ggplot2::annotate("segment",x=max.x-1,y=y.dest,xend=max.x-2,yend=y.dest,colour="#727D84",size=0.25) + #Add the horizontal segment
        ggplot2::annotate("text",x=max.x-1-size.text,y=y.dest,label=label,size=11*0.352777778,family="RooneySans-Regular") #Add the text
      prev.x <- max.x
      prev.y <- y.dest
    }
  }
  #Add labels
  p <- p +
    ggplot2::scale_x_continuous(expand=c(0,0),limits=c(0.5,most.x+0.5)) +
    ggplot2::scale_y_continuous(limits=c(-1,row.num+1)) +
    labs(title=plot.fig.num,subtitle=plot.title,x=x.axis,caption=caption) +
    guides(fill=guide_legend(title=plot.cat.title,title.position = "top"))
  #Export things
  if(export){
    ratio = round(row.num/most.x,2)
    f.height = 12*ratio
    export.bf.plot(export.name,p,p.height=f.height,p.width=12)
  }
  return(p)
}

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
                choice = unique(cma.data[stringr::str_sub(ID,1,2)==province.data[abbrev==input$province,code],Name])))
  #Interactive column plot of all the cities with the chosen city highlighted
  output$CMA_chosen_2 <- renderText(
    paste("This web-app examines the state of tech workers in ", input$cma))

   output$cmatot <- renderPlotly({
     noc.dem.tech.map[,cma.focus:="0"]
     noc.dem.tech.map[ALT.GEO.CODE %in% cma.data[Name %in% input$cma,ID],cma.focus:="1"]
     column.pct <- ggplot(data=noc.dem.tech.map[tech==1],aes(reorder(GEO.NAME,pct),pct,fill=cma.focus)) + 
       geom_col(width=0.6) + 
       BF.Base.Theme + 
       scale_y_continuous(expand=c(0,0)) + 
       theme(axis.text.x = element_blank()) +
       scale_fill_manual(values = c("#14365D","#DD347A")) +
       guides(fill="none",colour = "none") +
       labs(y="",x="")
       
     #column.pct <- BFTheme::plot.column.bf(noc.dem.tech.map[tech==1],"pct","GEO.NAME",
      #                                     order.bar = "ascending",
       #                                    group.by="cma.focus",
        #                                   colours = BFTheme::set.colours(2,categorical.choice = c("light.blue","pink")),
         #                                  label.unit = "%",
          #                                 col.invert = TRUE
           #                                ) + ggplot2::guides(fill="none",colour = "none") + ggplot2::theme(axis.text.x = element_blank())
     print(ggplotly(column.pct,tooltip=c("y","x"),showlegend=FALSE,showscale=FALSE))
   })
   #Top occupations both Absolute and Relative
   output$cmatopocc <- renderPlotly({
     noc.dem.city.plot <- noc.dem.city[ALT.GEO.CODE %in% cma.data[Name %in% input$cma,ID]]
     if(nrow(noc.dem.city.plot)>10){
       noc.dem.city.plot <- noc.dem.city.plot[(.N-9):.N]
     }
     if(input$pct_or_tot=="Absolute"){
       plot_pct_or_tot <- ggplot(data=noc.dem.city.plot,aes(reorder(NOC,TOT),TOT),fill="#8AD4DF") +
         geom_col(fill="#8AD4DF",width=0.6) +
         BF.Base.Theme + 
         scale_y_continuous(expand=c(0,0)) + 
         guides(fill="none",colour = "none") +
         theme(axis.text.x = ggplot2::element_text(size=9, margin=ggplot2::margin(t=2),color="white",angle=90)) +
         labs(y="",x="")
       print(ggplotly(plot_pct_or_tot))
       #print(ggplotly(BFTheme::plot.column.bf(noc.dem.city.plot,"TOT","NOC",
        #                       order.bar = "ascending",
        #                       colours = BFTheme::set.colours(1,categorical.choice = "light.blue"),
        #                       col.invert = TRUE)))
     }
     else{
       plot_pct_or_tot <- ggplot(data=noc.dem.city.plot,aes(reorder(NOC,pct),pct),fill="#8AD4DF") +
         geom_col(fill="#8AD4DF",width=0.6) +
         BF.Base.Theme + 
         scale_y_continuous(expand=c(0,0)) + 
         guides(fill="none",colour = "none") +
         theme(axis.text.x = ggplot2::element_text(size=9, margin=ggplot2::margin(t=2),color="white",angle=90)) +
         labs(y="",x="")
       print(ggplotly(plot_pct_or_tot))
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
     if(input$pct_or_tot == "Absolute"){
       plot_pct_or_tot <- ggplot(data=noc.dem.canada.plot,aes(reorder(NOC,TOT),TOT),fill="#8AD4DF") +
         geom_col(fill="#8AD4DF",width=0.6) +
         BF.Base.Theme + 
         scale_y_continuous(expand=c(0,0)) + 
         guides(fill="none",colour = "none") +
         theme(axis.text.x = ggplot2::element_text(size=9, margin=ggplot2::margin(t=2),color="white",angle=90)) +
         labs(y="",x="")
       print(ggplotly(plot_pct_or_tot))
       
       #print(ggplotly(BFTheme::plot.column.bf(noc.dem.canada.plot,"TOT","NOC",
      #                         order.bar="ascending",
      #                         colours = BFTheme::set.colours(1,categorical.choice = "light.blue"),
      #                         col.invert = TRUE)))
     }
     else{
       plot_pct_or_tot <- ggplot(data=noc.dem.canada.plot,aes(reorder(NOC,pct),pct),fill="#8AD4DF") +
         geom_col(fill="#8AD4DF",width=0.6) +
         BF.Base.Theme + 
         scale_y_continuous(expand=c(0,0)) + 
         guides(fill="none",colour = "none") +
         theme(axis.text.x = ggplot2::element_text(size=9, margin=ggplot2::margin(t=2),color="white",angle=90)) +
         labs(y="",x="")
       print(ggplotly(plot_pct_or_tot))
       #print(ggplotly(BFTheme::plot.column.bf(noc.dem.canada.plot,"pct","NOC",
      #                         order.bar = "ascending",
      #                         label.unit = "%",
      #                         colours = BFTheme::set.colours(1,categorical.choice="light.blue"),
      #                         col.invert = TRUE)))
     }

   })
   #Change in the tech workforce in text
   output$cmatotnumtext <- renderUI(p("In 2016, there were ",
                                            noc.dem.tech.map[GEO.NAME %in% input$cma & tech==1,V1],
                                            " tech workers in",
                                            paste(input$cma,
                                            ". This means that ",sep=""),
                                            paste(round(100*noc.dem.tech.map[GEO.NAME %in% input$cma & tech==1,V1]/891720,2),
                                            "% of tech workers in Canada lived in ",sep=""),
                                            paste(input$cma,".",sep="")))
   
   #Workforce in 2006 vs 2016 comparison text
   output$cmain2006 <- renderUI({
     difference <- noc.dem.tech.map[GEO.NAME %in% input$cma & tech==1,V1] - noc.2006.city[tech=="Tech Occupation" & GEO.CODE %in% cma.data[Name %in% input$cma,ID],V1]
     if(abs(difference)/noc.dem.tech.map[GEO.NAME %in% input$cma & tech==1,V1] < 0.01 | abs(difference)<50){
       
       p("In 2006, there were",
             span(style="color: #F7941E",noc.2006.city[tech=="Tech Occupation" & GEO.CODE %in% cma.data[Name %in% input$cma,ID],V1]),
             "tech workers in",
             paste(input$cma,".",sep="")," This means that the number of tech workers have been relatively unchanged over the past 10 years.")
     }
     else{
       if(difference<0){
         p("In 2006, there were",
               span(style="color: #F7941E",noc.2006.city[tech=="Tech Occupation" & GEO.CODE %in% cma.data[Name %in% input$cma,ID],V1]),
               "tech workers in",
           paste(input$cma,".",sep="")," This means that the number of tech workers have decreased over the past 10 years.")
       }
       else{
         p("In 2006, there were",
               span(style="color: #F7941E",noc.2006.city[tech=="Tech Occupation" & GEO.CODE %in% cma.data[Name %in% input$cma,ID],V1]),
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
     
     scatter.tech.plot <- ggplot(data=tech_premium, aes(non.tech.pay,tech.pay,colour=color)) +
       geom_point(size=2.3) +
       BF.Base.Theme +
       scale_colour_manual(values=c("#14365D","#DD347A","#82C458"))
     #scatter.tech.plot <- BFTheme::plot.scatter.bf(tech_premium,"non.tech.pay","tech.pay",group.by="color",
    #                                               colours = set.colours(3,categorical.choice = c("green","dark.blue","pink")),
    #                                               unit.x = "$",
    #                                               unit.y = "$",
    #                                               x.axis = "Average pay for non-tech jobs",
    #                                               y.axis = "Average pay for tech jobs")
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
     plot.female.pay <- ggplot(data=tech_gender,aes(reorder(GEO.NAME,V2),V2)) +
       geom_col(aes(fill=color)) +
       BF.Base.Theme +
       scale_y_continuous(expand=c(0,0)) + 
       scale_fill_manual(values=c("#DD347A","#14365D","#82C458")) +
       guides(fill="none",colour = "none") +
       theme(axis.text.x = ggplot2::element_text(size=9, margin=ggplot2::margin(t=2),color="white",angle=90)) +
       labs(y="",x="")
       
     #ggplotly(BFTheme::plot.column.bf(tech_gender,"V2","GEO.NAME",
    #                                  group.by="color",
    #                                  order.bar = "ascending",
    #                                  col.invert = TRUE,
    #                                  colours=set.colours(3,categorical.choice = c("green","dark.blue","pink")),
    #                                  label.unit = "$"))
   })

     


}

# Run the application 
shinyApp(ui = ui, server = server)

