library(shiny)
library(stringr)
library(data.table)
library(ggplot2)
library(plotly)
library(scales)


#Function returns the first graph used in the dataviz - y-axis is share of tech workers for every single
#CMA and CA in Canada
plot.cmatot <- function(name_to_use,cma.id){
    #This first bit ensures that during initial loading something is displayed instead of returning an error
    if(is.null(name_to_use)){
      noc.dem.tech.map[cma.focus=="1",cma.focus:="0"]
      noc.dem.tech.map[ALT.GEO.CODE %in% cma.id,cma.focus:="1"]
      column.pct <- ggplot(data=noc.dem.tech.map[tech==1],aes(reorder(GEO.NAME,pct),pct,fill=cma.focus)) + 
        geom_col(aes(text=paste(GEO.NAME,
                                "<br>",
                                "Concentration of Tech Workers:",
                                str_c(signif(pct,2),"%"),
                                "<br>",
                                "Number of Tech Workers:",
                                comma(signif(V1,3)))),
                 width=0.6) + 
        BF.Base.Theme + 
        scale_y_continuous(expand=c(0,0),
                           breaks = c(0,2.5,5,7.5,10),
                           labels = c("0%","2.5%","5%","7.5%","10%"),
                           limits = c(0,12)) + 
        theme(axis.text.x = element_blank(),
              axis.title.x = element_text(size=9, 
                                          color="#072b49"),
              axis.title.y = element_text(size=9, 
                                          color="#072b49"),
              axis.ticks.x = element_blank(),
              axis.text.y = element_text(size=9, 
                                         margin=ggplot2::margin(r=2),
                                         color="#072b49"),
              axis.line = ggplot2::element_line(size=0.25, 
                                                colour = "#072b49"),
              legend.text = ggplot2::element_text(size=9,
                                                  margin=ggplot2::margin(r=2),
                                                  color = "#072b49"),
              axis.ticks = ggplot2::element_line(size=0.15,
                                                 colour = "#072b49")) +
        scale_fill_manual(values = c("#072b49","#e24585")) +
        guides(fill="none",colour = "none") +
        labs(y="Tech Workers as a Share of Local Workforce",x="Hover over each bar to learn about a city")
      graph <- config(layout(ggplotly(column.pct,
                                      tooltip=c("text"),
                                      showlegend=FALSE,
                                      showscale=FALSE),
                             legend = list(orientation = 'h'),
                             xaxis=list(fixedrange=TRUE), 
                             yaxis=list(fixedrange=TRUE)),
                      displayModeBar=F)
      
      graph$x$data[[1]]$name <- "Other Cities/Towns"
      graph$x$data[[2]]$name <- "Arnprior"
      graph
    }
    #Now the actual dynamic bit begins...
    else{
      noc.dem.tech.map.to.use <- noc.dem.tech.map[tech==1]
      noc.dem.tech.map.to.use[cma.focus=="1",cma.focus:="0"]
      noc.dem.tech.map.to.use[ALT.GEO.CODE %in% cma.id,cma.focus:="1"]
      x.focus <- noc.dem.tech.map.to.use[cma.focus=="1" ,rownum]
      y.focus <- noc.dem.tech.map.to.use[cma.focus=="1" ,pct]
      column.pct <- ggplot(data=noc.dem.tech.map.to.use,aes(GEO.NAME,pct,fill=cma.focus)) + 
        geom_col(aes(text=paste(GEO.NAME,
                                "<br>",
                                "Concentration of Tech Workers:",
                                str_c(signif(pct,2),"%"),
                                "<br>",
                                "Number of Tech Workers:",
                                comma(signif(V1,3))
                                )
                     ),
                 width=0.6) + 
        BF.Base.Theme + 
        scale_y_continuous(expand=c(0,0),
                           breaks = c(0,2.5,5,7.5,10),
                           labels = c("0%","2.5%","5%","7.5%","10%"),
                           limits = c(0,12)) + 
        theme(axis.text.x = element_blank(),
              axis.title.x = element_text(size=9, color="#072b49"),
              axis.title.y = element_text(size=9, color="#072b49"),
              axis.ticks.x = element_blank(),
              axis.text.y = element_text(size=9, margin=ggplot2::margin(r=2),color="#072b49"),
              axis.line = ggplot2::element_line(size=0.25, colour = "#072b49"),
              legend.text = ggplot2::element_text(size=9,margin=ggplot2::margin(r=2),color = "#072b49"),
              axis.ticks = ggplot2::element_line(size=0.15,colour = "#072b49")) +
        scale_fill_manual(values = c("#072b49","#e24585")) +
        guides(fill="none",colour = "none") +
        labs(y="Tech Workers as a Share of Local Workforce",x="Hover over each bar to learn about a city") +
        annotate("segment",
                 y = y.focus+0.02,
                 x = x.focus,
                 xend = x.focus,
                 yend = floor(y.focus)+2,
                 linetype = "dotted",
                 colour = "#e24585") +
        annotate("text",
                 x = x.focus,
                 y = floor(y.focus)+2,
                 colour = "#e24585",
                 label = name_to_use,
                 hjust = 0,
                 size = 15*0.352777778)
      graph <- config(layout(ggplotly(column.pct,tooltip=c("text"),showlegend=FALSE,showscale=FALSE),
                             legend = list(orientation = 'h'),
                             xaxis=list(fixedrange=TRUE), 
                             yaxis=list(fixedrange=TRUE)),
                      displayModeBar=F)
      graph$x$data[[4]]$hoverinfo <- "none"
      graph$x$data[[3]]$hoverinfo <- "none"
      if(x.focus<50){
        graph$x$data[[4]]$textposition <- "top right"
      }
      else if(x.focus>=100){
        graph$x$data[[4]]$textposition <- "top left"
      }
      else{
        graph$x$data[[4]]$textposition <- "top center"
      }
      graph$x$data[[1]]$name <- "Other Cities/Towns"
      graph$x$data[[2]]$name <- name_to_use
      graph
    }

}

#Function returns the second graph used in the dataviz - y-axis is the number of workers or share of workers depending on button chosen and
#x axis is top 10 occupations in that city
plot.cmaocc <- function(name_to_use, pct_or_tot, cma.id){
    noc.dem.city.plot <- noc.dem.city[ALT.GEO.CODE %in% cma.id]
    noc.dem.city.plot[,cat:="Top 10 Occupations Locally"]
    noc.dem.city.plot[NOC %in% noc.dem.canada[,NOC],cat:="Top 10 Occupations Canada-wide"]
    if(nrow(noc.dem.city.plot)>10){
      noc.dem.city.plot <- noc.dem.city.plot[(.N-9):.N]
    }
    if(pct_or_tot=="Total Tech Workers"){
      max.plot <- max(noc.dem.city.plot[,TOT])
      min.plot <- 0
      ticks <- set.ticks.seq(max.plot,min.plot,unit="")
      plot_pct_or_tot <- ggplot(data=noc.dem.city.plot,aes(reorder(NOC,TOT),TOT)) +
        geom_col(aes(text=paste(reorder(NOC,TOT),
                                "<br>",
                                "Total Employed:",
                                scales::comma(sort(TOT))),fill=cat),
                 width=0.6) +
        BF.Base.Theme + 
        scale_y_continuous(expand=c(0,0),breaks = ticks$breaks, label = ticks$labels) + 
        scale_fill_manual(values = set.colours(2, categorical.choice = c("dark.blue","pink"))) +
        guides(fill="none",colour = "none") +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
        labs(y="Total Employment",x="Hover over each bar to learn about an occupation.")
    }
    else{
      max.plot <- max(noc.dem.city.plot[,pct])
      min.plot <- 0
      ticks <- set.ticks.seq(max.plot,min.plot,unit="%")
      plot_pct_or_tot <- ggplot(data=noc.dem.city.plot,aes(reorder(NOC,pct),pct)) +
        geom_col(aes(text=paste(reorder(NOC,TOT),
                                "<br>",
                                "Share of Tech Workforce:",
                                str_c(round(sort(pct),2),"%")),
                     fill=cat),
                 width=0.6) +
        BF.Base.Theme + 
        scale_y_continuous(expand=c(0,0),breaks = ticks$breaks, labels = ticks$labels) + 
        scale_fill_manual(values = set.colours(2, categorical.choice = c("dark.blue","pink"))) +
        guides(fill="none",colour = "none") +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
        labs(y="Share of Local Tech Workforce",x="Hover over each bar to learn about an occupation")
    }
    if(nrow(noc.dem.city.plot[cat=="Top 10 Occupations Locally"])==0){
      plot_pct_or_tot <- plot_pct_or_tot + scale_fill_manual(values=set.colours(1, categorical.choice = c("dark.blue")))
    }
    print(config(layout(ggplotly(plot_pct_or_tot,tooltip=c("text")),
                        xaxis = list(fixedrange=TRUE),
                        yaxis = list(fixedrange=TRUE),
                        margin = list(l = 100, b = 100),
                        legend = list(x=0.05,y=0.95)),
                 displayModeBar=F))
    
}

#Function returns the third graph used in the dataviz - y-axis is the Canadian plot of the second graph
plot.canocc <- function(pct_or_tot){
  if(pct_or_tot == "Total Tech Workers"){
    max.plot <- max(noc.dem.canada[,TOT])
    min.plot <- 0
    ticks <- set.ticks.seq(max.plot,min.plot,unit="")
    plot_pct_or_tot <- ggplot(data=noc.dem.canada,aes(reorder(NOC,TOT),TOT)) +
      geom_col(aes(text=paste(reorder(NOC,TOT),
                              "<br>",
                              "Total Employed:",
                              scales::comma(sort(TOT)))),
               fill="#072b49",
               width=0.6) +
      BF.Base.Theme + 
      scale_y_continuous(expand=c(0,0), breaks = ticks$breaks, labels = ticks$labels) + 
      guides(fill="none",colour = "none") +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
      labs(y="",x="")
  }
  else{
    max.plot <- max(noc.dem.canada[,pct])
    min.plot <- 0
    ticks <- set.ticks.seq(max.plot,min.plot,unit="%")
    plot_pct_or_tot <- ggplot(data=noc.dem.canada,aes(reorder(NOC,pct),pct)) +
      geom_col(aes(text=paste(reorder(NOC,TOT),
                              "<br>",
                              "Share of Tech Workforce:",
                              str_c(round(sort(pct),2),"%"))),
               fill="#072b49",
               width=0.6) +
      BF.Base.Theme + 
      scale_y_continuous(expand=c(0,0), breaks = ticks$breaks, labels = ticks$labels) + 
      guides(fill="none",colour = "none") +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
      labs(y="",x="")
  }
  print(config(layout(ggplotly(plot_pct_or_tot,tooltip=c("text")),
                      xaxis = list(fixedrange=TRUE),
                      yaxis = list(fixedrange=TRUE),
                      margin = list(l = 100, b = 100)),
               displayModeBar=F))
}

#Plot the line graph for educational attainment shares for each CMA/CA
plot.educ <- function(name_to_use,cma.id){
  cma.ca.educ[dum==1,dum:=0]
  cma.ca.educ[ALT.GEO.CODE %in% cma.id,dum:=1]
  plot.educ.share <- ggplot(data=cma.ca.educ[dum==0],aes(EDUC15,pct)) +
    BF.Base.Theme +
    theme(axis.text.x = element_text(size=9,colour="#072b49"),
          axis.text.y = element_text(size=9,colour="#072b49"),
          panel.grid.major.y = element_line(size=0.1,colour = "#072b49"),
          axis.line = ggplot2::element_line(size=0.25, colour = "#072b49"),
          axis.ticks = ggplot2::element_line(size=0.15,colour = "#072b49"),
          axis.title.x = element_text(size=10,colour = "#072b49"),
          axis.title.y = element_blank()) +
    geom_line(aes(group=GEO.NAME),color = "#072b49",alpha=0.3) +
    scale_y_continuous(breaks = c(0,25,50,75,100),
                       limits = c(0,100),
                       labels = c("0%","25%","50%","75%","100%")) +
    scale_x_discrete(expand = c(0.03,0)) +
    geom_point(data=can.educ.sum.narrow,
               aes(EDUC15,pct,text = paste("Canada <br> Share of Tech Workforce: ",round(pct),"%")),
               color = "#82C458",
               size=2) +
    geom_line(data=can.educ.sum.narrow,
              aes(EDUC15,pct, group=GEO.NAME),
              color = "#82C458",
              size=1) +
    geom_point(data=cma.ca.educ[dum == 1],
               aes(EDUC15,pct, text = paste(GEO.NAME,"<br>","Share of Tech Workforce: ",round(pct),"%")),
               color = "#e24585",
               size=2) +
    geom_line(data=cma.ca.educ[dum == 1],
              aes(EDUC15,pct,group = GEO.NAME),
              color = "#e24585",
              size=1) +
    labs(y = "Share of Tech Workers") +
    coord_flip()
  plotly.plot.educ.share <- config(layout(ggplotly(plot.educ.share,tooltip=c("text")),
                                          xaxis = list(fixedrange=TRUE),
                                          yaxis = list(fixedrange=TRUE),
                                          margin = list(l=200, b=100)),
                                   displayModeBar=F)
  
  plotly.plot.educ.share$x$data[[1]]$hoverinfo <- "skip"
  plotly.plot.educ.share
}

#Draw the first comparison table between cities
draw.table.topline <- function(comparison.cma){
  topline.vars <- c("<div class=tooltiphelp> Number of Tech Workers<span class=tooltiptexthelp> Total number of workers in tech occupations in a geographic area</span> </div>",
                    "<div class=tooltiphelp>Concentration of Tech Workers<span class=tooltiptexthelp>Share of all workers in a geographic region who are tech workers </span> </div>",
                    "<div class=tooltiphelp>Number of Tech Workers in 2006<span class=tooltiptexthelp> Total number of workers in tech occupations in a geographic area in 2006 </span></div>",
                    "<div class=tooltiphelp>Share of Tech Workers With Bachelors Degree or Higher<span class=tooltiptexthelp> Share of tech workers with a Bachelors degree or above as their highest degree </span> </div>")
  final.table <- data.table(first.col = topline.vars)
  names(final.table) <- c("Metrics")
  if(length(comparison.cma)>0){
    #Set up first column
    if(!is.na(comparison.cma[1][[1]])){
      first.city.id <- cma.data[Name == comparison.cma[1][[1]],ID]
      first.city <- c(comma(round(noc.dem.tech.map[tech==1 & ALT.GEO.CODE %in% first.city.id,V1])),
                      str_c(signif(noc.dem.tech.map[tech==1 & ALT.GEO.CODE %in% first.city.id,pct],2),"%"),
                      ifelse(length(noc.2006.city[tech == "Tech Occupation" & GEO.CODE %in% first.city.id,V1])==0,
                             "NA",
                             comma(round(noc.2006.city[tech == "Tech Occupation" & GEO.CODE %in% first.city.id,V1]))),
                      str_c(signif(cma.ca.educ[EDUC15.ID == 10 & ALT.GEO.CODE %in% first.city.id,pct],2),"%"))
      final.table[,first:=first.city]
    }
    #Set up second column
    if(!is.na(comparison.cma[2][[1]])){
      second.city.id <- cma.data[Name == comparison.cma[2][[1]],ID]
      second.city <- c(comma(round(noc.dem.tech.map[tech == 1 & ALT.GEO.CODE %in% second.city.id,V1])),
                       str_c(signif(noc.dem.tech.map[tech == 1 & ALT.GEO.CODE %in% second.city.id,pct],2),"%"),
                       ifelse(length(noc.2006.city[tech == "Tech Occupation" & GEO.CODE %in% second.city.id,V1])==0,
                              "NA",
                              comma(round(noc.2006.city[tech == "Tech Occupation" & GEO.CODE %in% second.city.id,V1]))),
                       str_c(signif(cma.ca.educ[EDUC15.ID == 10 & ALT.GEO.CODE %in% second.city.id,pct],2),"%"))
      final.table[,second:=second.city]
    }
    #Set up third column
    if(!is.na(comparison.cma[3][[1]])){
      third.city.id <- cma.data[Name == comparison.cma[3][[1]],ID]
      third.city <- c(comma(round(noc.dem.tech.map[tech == 1 & ALT.GEO.CODE %in% third.city.id,V1])),
                      str_c(signif(noc.dem.tech.map[tech == 1 & ALT.GEO.CODE %in% third.city.id,pct],2),"%"),
                      ifelse(length(noc.2006.city[tech == "Tech Occupation" & GEO.CODE %in% third.city.id,V1])==0,
                             "NA",
                             comma(round(noc.2006.city[tech == "Tech Occupation" & GEO.CODE %in% third.city.id,V1]))),
                      str_c(signif(cma.ca.educ[EDUC15.ID == 10 & ALT.GEO.CODE %in% third.city.id,pct],2),"%"))
      final.table[,third:=third.city]
    }
    #Set up fourth column
    if(!is.na(comparison.cma[4][[1]])){
      fourth.city.id <- cma.data[Name == comparison.cma[4][[1]],ID]
      fourth.city <- c(comma(round(noc.dem.tech.map[tech == 1 & ALT.GEO.CODE %in% fourth.city.id,V1])),
                       str_c(signif(noc.dem.tech.map[tech == 1 & ALT.GEO.CODE %in% fourth.city.id,pct],2),"%"),
                       ifelse(length(noc.2006.city[tech == "Tech Occupation" & GEO.CODE %in% fourth.city.id,V1])==0,
                              "NA",
                              comma(round(noc.2006.city[tech == "Tech Occupation" & GEO.CODE %in% fourth.city.id,V1]))),
                       str_c(signif(cma.ca.educ[EDUC15.ID == 10 & ALT.GEO.CODE %in% fourth.city.id,pct],2),"%"))
      final.table[,fourth:=fourth.city]
    }
    #Set up fifth column
    if(!is.na(comparison.cma[5][[1]])){
      fifth.city.id <- cma.data[Name == comparison.cma[5][[1]],ID]
      fifth.city <- c(comma(round(noc.dem.tech.map[tech == 1 & ALT.GEO.CODE %in% fifth.city.id,V1])),
                      str_c(signif(noc.dem.tech.map[tech == 1 & ALT.GEO.CODE %in% fifth.city.id,pct],2),"%"),
                      ifelse(length(noc.2006.city[tech == "Tech Occupation" & GEO.CODE %in% fifth.city.id,V1])==0,
                             "NA",
                             comma(round(noc.2006.city[tech == "Tech Occupation" & GEO.CODE %in% fifth.city.id,V1]))),
                      str_c(signif(cma.ca.educ[EDUC15.ID == 10 & ALT.GEO.CODE %in% fifth.city.id,pct],2),"%"))
      final.table[,fifth:=fifth.city]
    }
    names(final.table) <- c("Metrics", comparison.cma)
  }
  final.table
}