# Load libraries ====
if(!require(shiny)){
  install.packages('shiny')
}
if(!require(gtrendsR)){
  install.packages('gtrendsR')
}
if(!require(reshape2)){
  install.packages('reshape2')
}
if(!require(ggplot2)){
  install.packages('ggplot2')
}

library(shiny)
library(gtrendsR)
library(reshape2)
library(ggplot2)

data(countries)

# Start shiny application

shinyServer(function(input, output) {
  
  
  
  out <- reactive({
    if(length(input$terms)>0){
      
      unlist(strsplit(input$terms,","))
    }
  })
  
  start_date<-reactive({
    
    if(input$period=="2004-present"){as.Date("2004-01-01")}
    
    else if (input$period=="Past90Days"){as.Date(Sys.time())-90}
    
    else if (input$period=="Past12Months"){
      m=as.POSIXlt(as.Date(Sys.time()))
      m$year=m$year-1
      m}
    
    else if (input$period=="2011"){as.Date("2011-01-01")}
    else if (input$period=="2012"){as.Date("2012-01-01")}
    else if (input$period=="2013"){as.Date("2013-01-01")}
    else if (input$period=="2014"){as.Date("2014-01-01")}
    else if (input$period=="2015"){as.Date("2015-01-01")}
    
    
    
  })
  
  
  end_date<-reactive({
    
    if(input$period %in% c("2004-present",
                           "Past90Days","Past12Months"))
    {
      as.Date(Sys.time())}
    
    else if (input$period=="2011"){as.Date("2011-12-31")}
    else if (input$period=="2012"){as.Date("2012-12-31")}
    else if (input$period=="2013"){as.Date("2013-12-31")}
    else if (input$period=="2014"){as.Date("2014-12-31")}
    else if (input$period=="2015"){as.Date(Sys.time())} 
    
  })
  
  geo<-reactive({
    if(input$geography=="Worldwide"){""}
    
    else{
      
      countries$CODE[countries$COUNTRY==input$geography]
    }
    
  })
  
  data<-reactive({
    if(length(out()>0))
    {
      
      #out2<-gtrends(query=out(),start_date=start_date(),end_date=end_date(),geo=geo())
      out2<-gtrends(keyword = out(),
                   time = paste0(start_date()," ",end_date()),geo=geo())
      
    }
    
  })
  
  
  
  
  output$myplot <- renderPlot({
    if(length(out()>0)){
      z=data()
      trend=z$interest_over_time
      
      #if("end"%in%names(date)==T)
      #{
       # trend=select(trend,-end)}
      
      #trend <- melt(trend, id='start')
      
      ggplot(trend, aes(date,hits, color=keyword)) + geom_line()+ggtitle("Interest over time")+
        ylab("Relative Trend")+
        theme(plot.title = element_text(size = 18,colour="black"))+
        xlab('')+theme(axis.title.y = element_text(colour="#00007A",size=14,angle=90,hjust=.5,vjust=1),
                       axis.text.y = element_text(colour="darkred",size=14,angle=0,hjust=1,vjust=0),
                       axis.text.x = element_text(colour="darkred",size=14,angle=0,hjust=1,vjust=0))+
        theme(legend.title = element_text(colour="black", size=15, 
                                          face="bold"))+
        theme(legend.text = element_text(colour="blue", size=14, 
                                         face="bold"))
      
    }
    
  })
  
  
  corr<-reactive({
    
    if(input$corr==T & length(out()>1)){
      
      z=data()
      trend=z$trend
      trend=trend[,3:ncol(trend)]
      cor(trend)
      
    }
  }) 
  
  
  output$myplot3 <- renderPlot({
    if(length(corr()>0)){
      data=corr()
      
      qplot(x=Var1, y=Var2, data=melt(cor(data)), fill=value, geom="tile")+
        ggtitle('Correlation Matrix')+theme(axis.title.y =element_blank(),axis.title.x =element_blank(),
                                            axis.text.y = element_text(colour="darkred",size=14,angle=0,hjust=1,vjust=0),
                                            axis.text.x = element_text(colour="darkred",size=14,angle=0,hjust=1,vjust=0))+
        theme(legend.title=element_blank())+
        theme(legend.text = element_text(colour="black", size=14))+scale_fill_gradient2(limits=c(-1, 1),low="skyblue", high="blue")+
        theme(plot.title = element_text(size = 20,colour="black"))
    }
  })
  
  
  output$myplot2 <- renderPlot({
    if(length(out()>0)){
      data=data()
      
      
      z=data$searches
      rr=data$regions
      
      for (i in 1:length(z)){
        n=z[i]
        n=as.data.frame(n)
        names(n)=c("searches","hits")
        n$searches <- factor(n$searches, levels = n$searches[order(n$hits,decreasing =T)])
        
        colors=c("orange","skyblue","#999966")
        
        col=sample(c(1,2,3),1,replace=T)
        
        x11()
        
        print(ggplot(n, aes(searches,hits))+  
                geom_bar(stat='identity',fill=colors[col],color='black')+
                ggtitle(data$headers[2+2*length(z)+i])+ylab('Hits')+
                theme(plot.title = element_text(size = 18,colour="blue"))+
                theme(axis.title.x=element_blank(),axis.title.y = element_text(colour="blue",size=14),axis.text.x = element_text(colour="grey20",size=14,angle=60,hjust=.5,vjust=.5,face="plain"))
              
              
        )
        
        
        if(geo()=='')
        {
          x11()
          
          
          regions = as.data.frame(rr)[c(1,i+1)]
          
          names(regions)=c('region','hits')
          
          regions$region[regions$region=="United States"] = "USA"
          
          world_map = map_data("world")
          
          world_map =merge(world_map, regions, by="region",all.x = TRUE)
          
          world_map = world_map[order(world_map$group, world_map$order),]
          
          g=ggplot(world_map, aes(x=long, y=lat, group=group))+
            geom_polygon(aes(fill=hits), color="gray70") 
          
          print(g+theme(axis.text.y   = element_blank(),
                        axis.text.x   = element_blank(),
                        axis.title.y  = element_blank(),
                        axis.title.x  = element_blank(),
                        panel.background = element_blank(),
                        panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank())+
                  scale_fill_gradient(low = "skyblue", high = "blue", guide = "colorbar",na.value="white")+ggtitle(data$headers[2+2*length(z)+i])+ylab('Hits')+
                  theme(legend.key.size = unit(1, "cm"),
                        legend.title = element_text(size = 12, colour = "blue"),
                        legend.title.align=0.3,legend.text = element_text(size = 10))+
                  theme(panel.border = element_rect(colour = "gray70", fill=NA, size=0.5))
          )
        }
      }
      
    }
    
  }) 
  
}) 