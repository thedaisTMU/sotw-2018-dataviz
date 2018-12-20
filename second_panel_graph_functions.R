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
    theme(axis.text.x = element_text(size = 9, colour = "#072b49"),
          axis.text.y = element_text(size = 9, colour = "#072b49"),
          axis.title.x = element_text(size = 9, colour = "#072b49"),
          axis.title.y = element_text(size = 9, colour = "#072b49"),
          axis.line.y = element_line(size = 0.25, colour = "#072b49"),
          axis.line.x = element_line(size = 0.25, colour = "#072b49"),
          axis.ticks.x = element_line(size = 0.25, colour = "#072b49"),
          axis.ticks.y = element_line(size = 0.25, colour = "#072b49"),
          legend.text = ggplot2::element_text(size=9,margin=ggplot2::margin(r=2),color = "#072b49")) +
    scale_colour_manual(values=c("#072b49","#9cdae7","#e24585")) +
    scale_y_continuous(breaks = c(25000,50000,75000,100000,125000),labels = c("$25,000","$50,000","$75,000","$100,000","$125,000"),limits = c(25000,140000),expand=c(0,0)) +
    scale_x_continuous(breaks = c(25000,37500,50000,62500),labels = c("$25,000","$37,500","$50,000","$62,500"),limits = c(25000,70000),expand=c(0,0)) +
    labs(y="Average Pay in Tech Occupations",x="Average Pay in Non-Tech Occupations")
  graph <- config(layout(ggplotly(scatter.tech.plot,tooltip=c("text")),
                         legend = list(orientation = 'h'),
                         xaxis = list(fixedrange=TRUE),
                         yaxis = list(fixedrange=TRUE)),
                  displayModeBar=F)
  graph$x$data[[1]]$name <- "Canada"
  graph$x$data[[2]]$name <- "Other Metropolitan Areas"
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
    scale_y_continuous(expand=c(0,0),breaks = c(0,10000,20000,30000),labels = c("$0","$10,000","$20,000","$30,000")) + 
    scale_fill_manual(values=c("#82C458","#072b49","#e24585")) +
    guides(fill="none",colour = "none") +
    theme(axis.text.x = ggplot2::element_text(size=9, margin=ggplot2::margin(t=2),color="#072b49")) +
    labs(y="Each bar is a Metropolitan Area",x="",title="Gender Pay Gap in Tech Occupations") +
    coord_flip()
  print(config(layout(ggplotly(plot.female.pay,tooltip=c("text")),
                      xaxis = list(fixedrange=TRUE),
                      yaxis = list(fixedrange=TRUE),
                      margin = list(l = 200,t=100),
                      showlegend = FALSE),
               displayModeBar=F))
}

#Draw table that compares gender number for chosen CMA with Canada
draw.gender.table <- function(name_to_use){
  measure.vector <- c("<div class=tooltiphelp>Number of women in Tech <span class=tooltiptexthelp>Total number of tech workers in a geographic area who identifies as female</span></div>",
                      "<div class=tooltiphelp>Share of women in Tech <span class=tooltiptexthelp>Share of tech workforce in a geographic area who identifies as female </span> </div>",
                      "<div class=tooltiphelp>Women participation in Tech <span class=tooltiptexthelp>Share of all female workers in a geographic area who works in tech occupations</span></div>",
                      "<div class=tooltiphelp>Average women pay in Tech<span class=tooltiptexthelp>Mean pay for female tech workers in a geographic area</span></div>",
                      "<div class=tooltiphelp>Average women pay in non-Tech<span class=tooltiptexthelp>Mean pay for female non-tech workers in a geographic area</span></div>")
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


#Draw table that compares participation stats for male and female
draw.male.female.comp.table <- function(name_to_use){
  measure.vector <- c("<div class=tooltiphelp>Number of people in Tech <span class=tooltiptexthelp>Total number of tech workers in a geographic area who identifies as male or female</span></div>",
                      "<div class=tooltiphelp>Share of People in Tech <span class=tooltiptexthelp>Share of tech workforce in a geographic area who identifies as male or female</span> </div>",
                      "<div class=tooltiphelp>Participation in Tech <span class=tooltiptexthelp>Share of all workers in a geographic area who works in tech occupations that is either male or female</span></div>",
                      "<div class=tooltiphelp>Average Pay in Tech<span class=tooltiptexthelp>Average pay workers in tech jobs received</span></div>",
                      "<div class=tooltiphelp>Average Pay in Non-Tech<span class=tooltiptexthelp>Average pay workers in non-tech jobs received.</span></div>")
  female.vector <- c(comma(round(tech_gender[GEO.NAME %in% name_to_use,V1])),
                     str_c(signif(tech_gender[GEO.NAME %in% name_to_use,share_tech],2),"%"),
                     str_c(signif(tech_gender[GEO.NAME %in% name_to_use,prop.tech],2),"%"),
                     str_c("$",comma(signif(tech_gender[GEO.NAME %in% name_to_use,V2],3))),
                     str_c("$",comma(signif(tech_gender[GEO.NAME %in% name_to_use, non.tech.pay],3)))
                     )

  male.vector <- c(comma(round(tech_gender[GEO.NAME %in% name_to_use,i.V1])),
                   str_c(signif(tech_gender[GEO.NAME %in% name_to_use,share.male],2),"%"),
                   str_c(signif(tech_gender[GEO.NAME %in% name_to_use,male.prop.tech],2),"%"),
                   str_c("$",comma(signif(tech_gender[GEO.NAME %in% name_to_use, male.tech.pay],3))),
                   str_c("$",comma(signif(tech_gender[GEO.NAME %in% name_to_use,male.non.tech.pay],3)))
                   )
  table.to.display <- data.table("Measure"=measure.vector,"Female"=female.vector,"Male"=male.vector)
  names(table.to.display) <- c(name_to_use, "Female","Male")
  table.to.display
                                                     
}

#Draw table that compares Visible Minority numbers for chosen CMA with Canada
draw.vismin.table <- function(name_to_use){
  measure.vector <- c("<div class=tooltiphelp> Number of VM in Tech <span class=tooltiptexthelp>Total number of tech workers in a geographic area who identifies with a visible minority group</span></div>",
                      "<div class=tooltiphelp> Share of VM in Tech <span class=tooltiptexthelp>Share of tech workforce in a geographic area who identifies with a visible minority group </span> </div>",
                      "<div class=tooltiphelp> VM Participation in Tech <span class=tooltiptexthelp>Share of all visible minority workers in a geographic area who works in tech occupations</span></div>",
                      "<div class=tooltiphelp> Average VM Pay in Tech <span class=tooltiptexthelp>Mean pay for visible minority tech workers in a geographic area</span></div>",
                      "<div class=tooltiphelp> Average VM Pay in Non-Tech <span class=tooltiptexthelp>Mean pay for visible minority tech workers in a geographic area</span></div>")
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

draw.comp.vismin.table <- function(name_to_use){
  measure.vector <- c("<div class=tooltiphelp> Number in Tech <span class=tooltiptexthelp>Total number of tech workers in a geographic area</span></div>",
                      "<div class=tooltiphelp> Share in Tech <span class=tooltiptexthelp>Share of tech workforce in a geographic area </span> </div>",
                      "<div class=tooltiphelp> Participation in Tech <span class=tooltiptexthelp>Share that works in a geographic area who works in tech occupations</span></div>",
                      "<div class=tooltiphelp>Average Pay in Tech<span class=tooltiptexthelp>Average pay workers in tech jobs received</span></div>",
                      "<div class=tooltiphelp>Average Pay in Non-Tech<span class=tooltiptexthelp>Average pay workers in non-tech jobs received.</span></div>")
  vm.vector <- c(comma(round(tech_vismin[GEO.NAME %in% name_to_use & VIS.MIN15.ID == 2,V1])),
                   str_c(signif(tech_vismin[GEO.NAME %in% name_to_use & VIS.MIN15.ID == 2,share.tech],2),"%"),
                   str_c(signif(tech_vismin[GEO.NAME %in% name_to_use & VIS.MIN15.ID == 2,prop.tech],2),"%"),
                 str_c("$",comma(signif(tech_vismin[GEO.NAME %in% name_to_use & VIS.MIN15.ID==2,V2],3))),
                 str_c("$",comma(signif(tech_vismin[GEO.NAME %in% name_to_use & VIS.MIN15.ID ==2,non.tech.pay],3))))
  
  non.vm.vector <- c(comma(round(tech_vismin[GEO.NAME %in% name_to_use & VIS.MIN15.ID == 15,V1])),
                  str_c(signif(tech_vismin[GEO.NAME %in% name_to_use & VIS.MIN15.ID == 15,share.tech],2),"%"),
                  str_c(signif(tech_vismin[GEO.NAME %in% name_to_use & VIS.MIN15.ID == 15,prop.tech],2),"%"),
                  str_c("$",comma(signif(tech_vismin[GEO.NAME %in% name_to_use & VIS.MIN15.ID == 15,V2],3))),
                  str_c("$",comma(signif(tech_vismin[GEO.NAME %in% name_to_use & VIS.MIN15.ID == 15,non.tech.pay],3))))
  
  table.to.display <- data.table("Measure"=measure.vector,"Visible Minority"=vm.vector,"Not a Visible Minority"=non.vm.vector)
  names(table.to.display) <- c(name_to_use,"Visible Minority","Not a Visible Minority")
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
    scale_y_continuous(expand=c(0,0),breaks = c(0,20000,40000,60000),labels=c("$0","$20,000","$40,000","$60,000")) + 
    scale_fill_manual(values=c("#82C458","#072b49","#e24585")) +
    guides(fill="none",colour = "none") +
    theme(axis.text.x = ggplot2::element_text(size=9, margin=ggplot2::margin(t=2),color="#072b49")) +
    labs(y="Each Bar is a Metropolitan Area",x="",title="Visible Minority Pay Gap in Tech Occupations") +
    coord_flip()
  print(config(layout(ggplotly(plot.vismin.pay,tooltip=c("text")),
                      xaxis = list(fixedrange=TRUE),
                      yaxis = list(fixedrange=TRUE),
                      margin = list(l = 200,t=100),
                      showlegend = FALSE),
               displayModeBar=F))
  
  
}

#Draw table for comparing diversity & income numbers for CMAs
draw.table.div <- function(comparison.cma.div){
  topline.vars <- c("<div class=tooltiphelp>Overall Tech Worker Pay <span class=tooltiptexthelp>Average pay for tech workers in a metropolitan area</span></div>",
                    "<div class=tooltiphelp>Overall Non-Tech Worker Pay <span class=tooltiptexthelp>Average pay for non-tech workers in a metropolitan area</span></div>",
                    "<div class=tooltiphelp>Share of Women Tech Workers <span class=tooltiptexthelp>Share of all tech workers in a metropolitan area who identifies as female</span></div>",
                    "<div class=tooltiphelp>Share of Women Who are Tech Workers <span class=tooltiptexthelp>Share of all female in a metropolitan area who works in tech occupations</span></div>",
                    "<div class=tooltiphelp>Gender Pay Difference <span class=tooltiptexthelp>Difference between average tech occupation pay by gender in a metropolitan area</span></div>",
                    "<div class=tooltiphelp>Share of Tech Workers Who are in a VM Group <span class=tooltiptexthelp>Share of all tech workers in a metropolitan area with Visible Minority identities</span></div>",
                    "<div class=tooltiphelp>Share of VM Who are Tech Workers<span class=tooltiptexthelp>Share of all visible minorities in a metropolitan area who works in tech occupations</span></div>",
                    "<div class=tooltiphelp>Pay Difference by VM<span class=tooltiptexthelp>Difference between average tech occupation pay between non-visible minorities and visible minorities in a metropolitan area</span></div>")
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