library(shiny)
library(stringr)
library(data.table)
library(ggplot2)
library(plotly)
library(scales)


#Function returns the first graph used in the dataviz - y-axis is share of tech workers for every single
#CMA and CA in Canada
plot.cmatot <- function(name_to_use){
  if(!is.null(name_to_use)){
    noc.dem.tech.map[,cma.focus:="0"]
    noc.dem.tech.map[ALT.GEO.CODE %in% cma.data[Name %in% name_to_use,ID],cma.focus:="1"]
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
    graph$x$data[[2]]$name <- name_to_use
    graph
  }
}

#Function returns the second graph used in the dataviz - y-axis is the number of workers or share of workers depending on button chosen and
#x axis is top 10 occupations in that city
plot.cmaocc <- function(name_to_use, pct_or_tot){
    noc.dem.city.plot <- noc.dem.city[ALT.GEO.CODE %in% cma.data[Name %in% name_to_use,ID]]
    if(nrow(noc.dem.city.plot)>10){
      noc.dem.city.plot <- noc.dem.city.plot[(.N-9):.N]
    }
    if(pct_or_tot=="Total Tech Workers"){
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
    }
    
}

#Function returns the third graph used in the dataviz - y-axis is the Canadian plot of the second graph
plot.canocc <- function(pct_or_tot){
  noc.dem.canada.plot <- noc.dem.canada[(.N-9):.N]
  if(pct_or_tot == "Total Tech Workers"){
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
  }
  
}

#Plot the line graph for educational attainment shares for each CMA/CA
plot.educ <- function(name_to_use){
  cma.ca.educ[,EDUC15:=str_wrap(EDUC15,30)]
  cma.ca.educ[,EDUC15:=reorder(EDUC15,EDUC15.ID)]
  cma.ca.educ[,dum:=0]
  cma.ca.educ[ALT.GEO.CODE %in% cma.data[Name %in% name_to_use,ID],dum:=1]
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
}

#Draw the first comparison table between cities
draw.table.topline <- function(comparison.cma){
  topline.vars <- c("Number of Tech Workers",
                    "Concentration of Tech Workers",
                    "Number of Tech Workers in 2006",
                    "Share of Tech Workers with Bachelors Degree or higher")
  final.table <- data.table(first.col = topline.vars)
  names(final.table) <- c("Metrics")
  if(length(comparison.cma)>0){
    #Set up first column
    if(!is.na(comparison.cma[1][[1]])){
      first.city <- c(comma(round(noc.dem.tech.map[tech==1 & ALT.GEO.CODE %in% cma.data[Name == comparison.cma[1][[1]],ID],V1])),
                      str_c(signif(noc.dem.tech.map[tech==1 & ALT.GEO.CODE %in% cma.data[Name == comparison.cma[1][[1]],ID],pct],2),"%"),
                      ifelse(length(noc.2006.city[tech == "Tech Occupation" & GEO.CODE %in% cma.data[Name == comparison.cma[1][[1]],ID],V1])==0,
                             "NA",
                             comma(round(noc.2006.city[tech == "Tech Occupation" & GEO.CODE %in% cma.data[Name == comparison.cma[1][[1]],ID],V1]))),
                      str_c(signif(cma.ca.educ[EDUC15.ID == 10 & ALT.GEO.CODE %in% cma.data[Name %in% comparison.cma[1][[1]],ID],pct],2),"%"))
      final.table[,first:=first.city]
    }
    #Set up second column
    if(!is.na(comparison.cma[2][[1]])){
      second.city <- c(comma(round(noc.dem.tech.map[tech == 1 & ALT.GEO.CODE %in% cma.data[Name == comparison.cma[2][[1]],ID],V1])),
                       str_c(signif(noc.dem.tech.map[tech == 1 & ALT.GEO.CODE %in% cma.data[Name == comparison.cma[2][[1]],ID],pct],2),"%"),
                       ifelse(length(noc.2006.city[tech == "Tech Occupation" & GEO.CODE %in% cma.data[Name == comparison.cma[2][[1]],ID],V1])==0,
                              "NA",
                              comma(round(noc.2006.city[tech == "Tech Occupation" & GEO.CODE %in% cma.data[Name == comparison.cma[2][[1]],ID],V1]))),
                       str_c(signif(cma.ca.educ[EDUC15.ID == 10 & ALT.GEO.CODE %in% cma.data[Name %in% comparison.cma[2][[1]],ID],pct],2),"%"))
      final.table[,second:=second.city]
    }
    #Set up third column
    if(!is.na(comparison.cma[3][[1]])){
      third.city <- c(comma(round(noc.dem.tech.map[tech == 1 & ALT.GEO.CODE %in% cma.data[Name == comparison.cma[3][[1]],ID],V1])),
                      str_c(signif(noc.dem.tech.map[tech == 1 & ALT.GEO.CODE %in% cma.data[Name == comparison.cma[3][[1]],ID],pct],2),"%"),
                      ifelse(length(noc.2006.city[tech == "Tech Occupation" & GEO.CODE %in% cma.data[Name == comparison.cma[3][[1]],ID],V1])==0,
                             "NA",
                             comma(round(noc.2006.city[tech == "Tech Occupation" & GEO.CODE %in% cma.data[Name == comparison.cma[3][[1]],ID],V1]))),
                      str_c(signif(cma.ca.educ[EDUC15.ID == 10 & ALT.GEO.CODE %in% cma.data[Name %in% comparison.cma[3][[1]],ID],pct],2),"%"))
      final.table[,third:=third.city]
    }
    #Set up fourth column
    if(!is.na(comparison.cma[4][[1]])){
      fourth.city <- c(comma(round(noc.dem.tech.map[tech == 1 & ALT.GEO.CODE %in% cma.data[Name == comparison.cma[4][[1]],ID],V1])),
                       str_c(signif(noc.dem.tech.map[tech == 1 & ALT.GEO.CODE %in% cma.data[Name == comparison.cma[4][[1]],ID],pct],2),"%"),
                       ifelse(length(noc.2006.city[tech == "Tech Occupation" & GEO.CODE %in% cma.data[Name == comparison.cma[4][[1]],ID],V1])==0,
                              "NA",
                              comma(round(noc.2006.city[tech == "Tech Occupation" & GEO.CODE %in% cma.data[Name == comparison.cma[4][[1]],ID],V1]))),
                       str_c(signif(cma.ca.educ[EDUC15.ID == 10 & ALT.GEO.CODE %in% cma.data[Name %in% comparison.cma[4][[1]],ID],pct],2),"%"))
      final.table[,fourth:=fourth.city]
    }
    #Set up fifth column
    if(!is.na(comparison.cma[5][[1]])){
      fifth.city <- c(comma(round(noc.dem.tech.map[tech == 1 & ALT.GEO.CODE %in% cma.data[Name == comparison.cma[5][[1]],ID],V1])),
                      str_c(signif(noc.dem.tech.map[tech == 1 & ALT.GEO.CODE %in% cma.data[Name == comparison.cma[5][[1]],ID],pct],2),"%"),
                      ifelse(length(noc.2006.city[tech == "Tech Occupation" & GEO.CODE %in% cma.data[Name == comparison.cma[5][[1]],ID],V1])==0,
                             "NA",
                             comma(round(noc.2006.city[tech == "Tech Occupation" & GEO.CODE %in% cma.data[Name == comparison.cma[5][[1]],ID],V1]))),
                      str_c(signif(cma.ca.educ[EDUC15.ID == 10 & ALT.GEO.CODE %in% cma.data[Name %in% comparison.cma[5][[1]],ID],pct],2),"%"))
      final.table[,fifth:=fifth.city]
    }
    names(final.table) <- c("Metrics", comparison.cma)
  }
  final.table
}