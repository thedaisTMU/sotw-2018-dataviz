################################
# This file sets some of the global definitions needed - set colour function and BF plot theme maily - will adjust as needed.


library(shiny)
library(stringr)
library(data.table)
library(ggplot2)
library(plotly)
library(scales)


BF.Base.Theme <- ggplot2::theme(panel.background = ggplot2::element_rect(fill="transparent", colour=NA), #Make sure plot area background is transparent
                                plot.background = ggplot2::element_rect(fill="transparent", colour=NA), #Make sure render area background is transparent
                                axis.line = ggplot2::element_line(size=0.25, colour = "#B1B8BC"), #Set axis line width and set colour to grey
                                axis.ticks = ggplot2::element_line(size=0.25, colour = "#B1B8BC"), #Set axis tick width and set colour to grey
                                panel.grid.major = ggplot2::element_blank(), #Remove the panel grid lines
                                panel.grid.minor = ggplot2::element_blank(), #Remove the panel grid lines
                                text = ggplot2::element_text(color="white"), #Set the font for every text element (except for geom elements)
                                plot.title = ggplot2::element_text(size=9,color="white"), #Format figure number
                                plot.subtitle = ggplot2::element_text(size=12,color="white"), #Format main title
                                plot.caption = ggplot2::element_text(face="italic", size=8.2, margin=ggplot2::margin(t=10),hjust=0,colour="#14365D"), #Format for caption and other notes
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



