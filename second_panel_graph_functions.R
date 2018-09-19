library(shiny)
library(stringr)
library(data.table)
library(ggplot2)
library(plotly)
library(scales)

#Plot scatterplot of pay in tech and pay in non-tech for each CMA
plot.scatter.income <- function(name_to_use){
  tech_premium[GEO.NAME=="Canada",color:="0"]
  tech_premium[GEO.NAME!="Canada",color:="1"]
  tech_premium[GEO.NAME==name_to_use,color:="2"]
  
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
  graph$x$data[[3]]$name <- name_to_use
  print(graph)
}

#Plot gender pay gap for each CMA - column graph
plot.gender.paygap <- function(name_to_use){
  tech_gender[GEO.NAME=="Canada",color:="0"]
  tech_gender[GEO.NAME!="Canada",color:="1"]
  tech_gender[GEO.NAME==name_to_use,color:="2"]
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
}

#Draw table that compares gender number for chosen CMA with Canada
draw.gender.table <- function(name_to_use){
  measure.vector <- c("Number of Female in Tech",
                      "Share of Female in Tech",
                      "Female Participation in Tech",
                      "Female Pay in Tech",
                      "Female Pay in non-Tech")
  city.vector <- c(comma(round(tech_gender[GEO.NAME %in% name_to_use,V1])),
                   str_c(signif(tech_gender[GEO.NAME %in% name_to_use,share_tech],2),"%"),
                   str_c(signif(tech_gender[GEO.NAME %in% name_to_use,prop.tech],2),"%"),
                   str_c("$",comma(signif(tech_gender[GEO.NAME %in% name_to_use,V2],3))),
                   str_c("$",comma(signif(tech_gender[GEO.NAME %in% name_to_use,non.tech.pay],3))))
  
  can.vector <- c(comma(round(tech_gender[GEO.NAME == "Canada",V1])),
                  str_c(signif(tech_gender[GEO.NAME == "Canada",share_tech],2),"%"),
                  str_c(signif(tech_gender[GEO.NAME == "Canada",prop.tech],2),"%"),
                  str_c("$",comma(signif(tech_gender[GEO.NAME == "Canada",V2],3))),
                  str_c("$",comma(signif(tech_gender[GEO.NAME == "Canada",non.tech.pay],3))))
  
  table.to.display <- data.table("Measure"=measure.vector,"City"=city.vector,"Canada"=can.vector)
  names(table.to.display) <- c("Measure",stringr::str_wrap(name_to_use,15),"Canada")
  table.to.display
}

#Draw table that compares Visible Minority numbers for chosen CMA with Canada
draw.vismin.table <- function(name_to_use){
  measure.vector <- c("Number of Visible Minority in Tech",
                      "Share of Visible Minority in Tech",
                      "Visible Minority Participation in Tech",
                      "Visible Minority Pay in Tech",
                      "Visible Minority Pay in non-Tech")
  city.vector <- c(comma(round(tech_vismin[GEO.NAME %in% name_to_use & VIS.MIN15.ID == 2,V1])),
                   str_c(signif(tech_vismin[GEO.NAME %in% name_to_use & VIS.MIN15.ID == 2,share.tech],2),"%"),
                   str_c(signif(tech_vismin[GEO.NAME %in% name_to_use & VIS.MIN15.ID == 2,prop.tech],2),"%"),
                   str_c("$",comma(signif(tech_vismin[GEO.NAME %in% name_to_use & VIS.MIN15.ID == 2,V2],3))),
                   str_c("$",comma(signif(tech_vismin[GEO.NAME %in% name_to_use & VIS.MIN15.ID == 2,non.tech.pay],3))))
  
  can.vector <- c(comma(round(tech_vismin[GEO.NAME == "Canada" & VIS.MIN15.ID == 2,V1])),
                  str_c(signif(tech_vismin[GEO.NAME == "Canada" & VIS.MIN15.ID == 2,share.tech],2),"%"),
                  str_c(signif(tech_vismin[GEO.NAME == "Canada" & VIS.MIN15.ID == 2,prop.tech],2),"%"),
                  str_c("$",comma(signif(tech_vismin[GEO.NAME == "Canada" & VIS.MIN15.ID == 2,V2],3))),
                  str_c("$",comma(signif(tech_vismin[GEO.NAME == "Canada" & VIS.MIN15.ID == 2,non.tech.pay],3))))
  
  table.to.display <- data.table("Measure"=measure.vector,"City"=city.vector,"Canada"=can.vector)
  names(table.to.display) <- c("Measure",stringr::str_wrap(name_to_use,15),"Canada")
  table.to.display
}

#Plot pay gap for visible minority groups for each CMA - column graph
plot.vismin.paygap <- function(name_to_use){
  tech_vismin[GEO.NAME=="Canada",color:="0"]
  tech_vismin[GEO.NAME!="Canada",color:="1"]
  tech_vismin[GEO.NAME %in% name_to_use,color:="2"]
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
  
  
}

#Draw table for comparing diversity & income numbers for CMAs
draw.table.div <- function(comparison.cma.div){
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
  if(length(comparison.cma.div)>0){
    #Set up first column
    if(!is.na(comparison.cma.div[1][[1]])){
      first.city <- c(str_c("$",comma(signif(tech_premium[GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[1][[1]],GEO.CODE],tech.pay],3))),
                      str_c("$",comma(signif(tech_premium[GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[1][[1]],GEO.CODE],non.tech.pay],3))),
                      str_c(signif(tech_gender[ALT.GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[1][[1]],GEO.CODE],share_tech],2),"%"),
                      str_c(signif(tech_gender[ALT.GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[1][[1]],GEO.CODE],prop.tech],2),"%"),
                      str_c("$",comma(signif(tech_gender[ALT.GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[1][[1]],GEO.CODE],pay.gap],3))),
                      str_c(signif(tech_vismin[ALT.GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[1][[1]],GEO.CODE] & VIS.MIN15.ID == 2, share.tech],2),"%"),
                      str_c(signif(tech_vismin[ALT.GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[1][[1]],GEO.CODE] & VIS.MIN15.ID == 2, prop.tech],2),"%"),
                      str_c("$",comma(signif(tech_vismin[ALT.GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[1][[1]],GEO.CODE] & VIS.MIN15.ID == 2, pay.gap],3)))
      )
      final.table.div[,first:=first.city]
    }
    #Set up second column
    if(!is.na(comparison.cma.div[2][[1]])){
      second.city <- c(str_c("$",comma(signif(tech_premium[GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[2][[1]],GEO.CODE],tech.pay],3))),
                       str_c("$",comma(signif(tech_premium[GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[2][[1]],GEO.CODE],non.tech.pay],3))),
                       str_c(signif(tech_gender[ALT.GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[2][[1]],GEO.CODE],share_tech],2),"%"),
                       str_c(signif(tech_gender[ALT.GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[2][[1]],GEO.CODE],prop.tech],2),"%"),
                       str_c("$",comma(signif(tech_gender[ALT.GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[2][[1]],GEO.CODE],pay.gap],3))),
                       str_c(signif(tech_vismin[ALT.GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[2][[1]],GEO.CODE] & VIS.MIN15.ID == 2, share.tech],2),"%"),
                       str_c(signif(tech_vismin[ALT.GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[2][[1]],GEO.CODE] & VIS.MIN15.ID == 2, prop.tech],2),"%"),
                       str_c("$",comma(signif(tech_vismin[ALT.GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[2][[1]],GEO.CODE] & VIS.MIN15.ID == 2, pay.gap],3)))
      )
      final.table.div[,second:=second.city]
    }
    #Set up third column
    if(!is.na(comparison.cma.div[3][[1]])){
      third.city <- c(str_c("$",comma(signif(tech_premium[GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[3][[1]],GEO.CODE],tech.pay],3))),
                      str_c("$",comma(signif(tech_premium[GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[3][[1]],GEO.CODE],non.tech.pay],3))),
                      str_c(signif(tech_gender[ALT.GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[3][[1]],GEO.CODE],share_tech],2),"%"),
                      str_c(signif(tech_gender[ALT.GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[3][[1]],GEO.CODE],prop.tech],2),"%"),
                      str_c("$",comma(signif(tech_gender[ALT.GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[3][[1]],GEO.CODE],pay.gap],3))),
                      str_c(signif(tech_vismin[ALT.GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[3][[1]],GEO.CODE] & VIS.MIN15.ID == 2, share.tech],2),"%"),
                      str_c(signif(tech_vismin[ALT.GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[3][[1]],GEO.CODE] & VIS.MIN15.ID == 2, prop.tech],2),"%"),
                      str_c("$",comma(signif(tech_vismin[ALT.GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[3][[1]],GEO.CODE] & VIS.MIN15.ID == 2, pay.gap],3)))
      )
      final.table.div[,third:=third.city]
    }
    #Set up fourth column
    if(!is.na(comparison.cma.div[4][[1]])){
      fourth.city <- c(str_c("$",comma(signif(tech_premium[GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[4][[1]],GEO.CODE],tech.pay],3))),
                       str_c("$",comma(signif(tech_premium[GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[4][[1]],GEO.CODE],non.tech.pay],3))),
                       str_c(signif(tech_gender[ALT.GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[4][[1]],GEO.CODE],share_tech],2),"%"),
                       str_c(signif(tech_gender[ALT.GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[4][[1]],GEO.CODE],prop.tech],2),"%"),
                       str_c("$",comma(signif(tech_gender[ALT.GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[4][[1]],GEO.CODE],pay.gap],3))),
                       str_c(signif(tech_vismin[ALT.GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[4][[1]],GEO.CODE] & VIS.MIN15.ID == 2, share.tech],2),"%"),
                       str_c(signif(tech_vismin[ALT.GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[4][[1]],GEO.CODE] & VIS.MIN15.ID == 2, prop.tech],2),"%"),
                       str_c("$",comma(signif(tech_vismin[ALT.GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[4][[1]],GEO.CODE] & VIS.MIN15.ID == 2, pay.gap],3)))
      )
      final.table.div[,fourth:=fourth.city]
    }
    #Set up fifth column
    if(!is.na(comparison.cma.div[5][[1]])){
      fifth.city <- c(str_c("$",comma(signif(tech_premium[GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[5][[1]],GEO.CODE],tech.pay]),3)),
                      str_c("$",comma(signif(tech_premium[GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[5][[1]],GEO.CODE],non.tech.pay]),3)),
                      str_c(signif(tech_gender[ALT.GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[5][[1]],GEO.CODE],share_tech],2),"%"),
                      str_c(signif(tech_gender[ALT.GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[5][[1]],GEO.CODE],prop.tech],2),"%"),
                      str_c("$",comma(signif(tech_gender[ALT.GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[5][[1]],GEO.CODE],pay.gap],3))),
                      str_c(signif(tech_vismin[ALT.GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[5][[1]],GEO.CODE] & VIS.MIN15.ID == 2, share.tech],2),"%"),
                      str_c(signif(tech_vismin[ALT.GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[5][[1]],GEO.CODE] & VIS.MIN15.ID == 2, prop.tech],2),"%"),
                      str_c("$",comma(signif(tech_vismin[ALT.GEO.CODE %in% cma_list[GEO.NAME == comparison.cma.div[5][[1]],GEO.CODE] & VIS.MIN15.ID == 2, pay.gap],3)))
      )
      final.table.div[,fifth:=fifth.city]
    }
    names(final.table.div) <- c("Metrics", comparison.cma.div)
  }
  final.table.div
}