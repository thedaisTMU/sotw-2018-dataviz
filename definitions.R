################################
# This file sets some of the global definitions needed - set colour function and BF plot theme maily - will adjust as needed.


library(shiny)
library(stringr)
library(data.table)
library(ggplot2)
library(plotly)
library(scales)


#Loads up all the data files used for this data visualization

load("data/cma.tech.concentration.rds") #Loads noc.dem.tech.map
load("data/city_occ_emp_data.RDS") #Loads noc.dem.city
load("data/canada_occ_emp.RDS") #Loads noc.dem.canada
load("data/cma_tech_2006.RDS") #Loads noc.2006.city
#load("data/diversity_cma.RDS") #Loads can_thing - only needed for development. No need in production
load("data/tech_premium.RDS") #Loads tech_premium
load("data/cma_list.RDS") #Loads cma_list
load("data/tech_gender.RDS") #Loads tech_gender
load("data/cma_data.RDS") #Loads cma.data
load("data/province_data.RDS") #Loads province.data
load("data/cma_ca_education.RDS") #Loads cma.ca.educ
load("data/tech_vismin.RDS") #Loads tech_vismin
load("data/can_educ.RDS") #Load education data for Canada


BF.Base.Theme <- ggplot2::theme(panel.background = ggplot2::element_rect(fill="transparent", colour=NA), #Make sure plot area background is transparent
                                plot.background = ggplot2::element_rect(fill="transparent", colour=NA), #Make sure render area background is transparent
                                axis.line = ggplot2::element_line(size=0.25, colour = "#B1B8BC"), #Set axis line width and set colour to grey
                                axis.ticks = ggplot2::element_line(size=0.25, colour = "#B1B8BC"), #Set axis tick width and set colour to grey
                                panel.grid.major = ggplot2::element_blank(), #Remove the panel grid lines
                                panel.grid.minor = ggplot2::element_blank(), #Remove the panel grid lines
                                text = ggplot2::element_text(color="white"), #Set the font for every text element (except for geom elements)
                                plot.title = ggplot2::element_text(size=9,color="#072b49"), #Format figure number
                                plot.subtitle = ggplot2::element_text(size=12,color="#072b49"), #Format main title
                                plot.caption = ggplot2::element_text(face="italic", size=8.2, margin=ggplot2::margin(t=10),hjust=0,colour="#072b49"), #Format for caption and other notes
                                legend.background = ggplot2::element_rect(fill="transparent",colour=NA), #Make sure legend box is transparent (for export)
                                legend.key = ggplot2::element_blank(), #Background on each key is transparent
                                legend.box.margin = ggplot2::margin(b=4,t=6), #Set margin for the box for appropriate distance
                                legend.title = ggplot2::element_text(size=10,hjust=0.5,color="#072b49"), #Legend title text settings, also make centre it. Light so it's not as prominent
                                legend.title.align = 0.5,
                                legend.text = ggplot2::element_text(size=9,margin=ggplot2::margin(r=2),color="#072b49"), #Legend text settings. Light so it's not as prominent
                                legend.margin = ggplot2::margin(b=1), #Small margin in the bottom
                                legend.position = "top", #Set the legend to top so panel can be full width (for export)
                                legend.box.spacing = ggplot2::unit(c(0,0,0,0),units=c("cm","cm","cm","cm")), #Legend box spacing - maybe not needed?
                                axis.text.x = ggplot2::element_text(size=9, margin=ggplot2::margin(t=2),color="#072b49"), #Set axis text. Light to make it less prominent - margin is also precise
                                axis.text.y = ggplot2::element_text(size=9, margin=ggplot2::margin(r=2),color="#072b49"), #Set axis text. Light to make it less prominent - margin is also precise
                                axis.title.x = ggplot2::element_text(size=10, margin=ggplot2::margin(t=4),color="#072b49"), #Set axis title. Margin is also precise
                                axis.title.y = ggplot2::element_text(size=10, margin=ggplot2::margin(r=4),color="#072b49"))

set.colours <- function(n,
                        type = "categorical",
                        gradient.choice = "dark.blue",
                        categorical.choice = NULL,
                        special = NULL){
  #Setting all the base vectors to refer to - precise because I don't trust R's generation of gradients
  base.set <- c("dark.blue"="#072b49","light.blue"="#9cdae7","pink"="#e24585","yellow"="#FFC800",
                "magenta"="#79133E","orange"="#F7941E","green"="#82C458","teal"="#005F61","grey"="#707D85")
  dark.blue <- c("#072b49","#29486B","#3E5A7A","#546C89","#697F97","#7E91A6","#94A3B5","#A9B5C4")
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



set.ticks.seq <- function(max,min,unit,num.ticks=5){
  if(unit==""){ #If there are no unit
    ticks <- scales::cbreaks(c(max,min),labels= comma_format(unit=unit,sep="",big.mark = ","))
  }
  if(unit=="$"){
    ticks <- scales::cbreaks(c(max,min),labels = dollar_format(largest_with_cents = 100)) #Format money
    return(ticks)
  }
  if(unit=="%" & max >= 75){
    ticks <- scales::cbreaks(c(100,0),labels = unit_format(unit="%",sep="")) #Format percentage
    return(ticks)
  }
  else{
    ticks <- scales::cbreaks(c(max,min),labels=unit_format(unit=unit,sep=" ",big.mark = ",")) #Format percentage without the percentage sign
    return(ticks)
  }
}
