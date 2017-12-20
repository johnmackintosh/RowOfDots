#use the same data frame as previous static plot
library(gganimate)
library(tidyverse)
library(lubridate)
library(scales)
library(readxl)
library(extrafont)
library(hrbrthemes)
library(animation)
library(ggExtra)



data <- read_xlsx("RedGreenGreyDots.xlsx", sheet = 1) #read raw data from Excel

str(data) 


plot_data <- data %>% 
  mutate(Movement15 = lubridate::floor_date(MovementDateTime,"15 minutes")) %>% 
  group_by(IN_OUT, Movement_Type,Staging_Post,Movement15) %>% 
  mutate(counter = case_when (
    IN_OUT == 'IN' ~ 1,
    IN_OUT == 'OUT' ~ -1)) %>% 
  mutate(Movement_15_SEQNO =cumsum(counter)) %>% 
  ungroup() 


# Change "Tranfer In"  or "Transfer Out" to "Transfer"
plot_data$Movement_Type <- gsub("Transfer.*","Transfer",x=plot_data$Movement_Type)

# Set limits for plotting
lims <- as.POSIXct(strptime(c("2014-09-03 00:00","2014-09-03 24:00")
                            , format = "%Y-%m-%d %H:%M"))  


animation::ani.options(interval = .2,ani.width = 900, ani.height = 550, ani.res = 300)



p <- ggplot(plot_data,aes(Movement15,Movement_15_SEQNO, colour=Movement_Type, frame = Movement15,cumulative = TRUE))+
  geom_jitter(width=0.10)+
  scale_colour_manual(values=c("#D7100D","#40B578","grey60"))+
  facet_grid(Staging_Post~.,switch = "both")+
  scale_x_datetime(date_labels="%H:%M",date_breaks = "3 hours",
                   limits = lims,
                   timezone = Sys.timezone(),
                   expand = c(0,0))+
  labs(x = NULL, 
       y=NULL,
       caption="@HighlandDataSci | johnmackintosh.com  Source: Neil Pettinger | @kurtstat | kurtosis.co.uk")+
  ggtitle(label = "Anytown General Hospital | Wednesday 3rd September 2014 00:00 to 23:59\n",
          subtitle="A&E AND INPATIENT ARRIVALS, DEPARTURES AND TRANSFERS")+
  theme_ipsum(base_size = 10,base_family = "Tahoma")+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme(axis.text.x=element_text(size=7)) +
  theme(axis.ticks.x=element_blank())+
  guides(color=guide_legend("Movement Type"))+
  theme(legend.position="bottom")+ 
  theme(strip.text.y = element_text(angle = 0))+
  ggExtra::removeGrid() 


gganimate(p,filename = "row_of_dots.gif")#,#interval = .2)
