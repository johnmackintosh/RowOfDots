library(tidyverse)
library(lubridate)
library(scales)
library(readxl)
library(extrafont)
library(hrbrthemes)



data <- read_xlsx("RedGreenGreyDots.xlsx", sheet = 1) #read raw data from Excel

str(data) 


plot_data <- data %>% 
  mutate(Movement15 = lubridate::floor_date(MovementDateTime,"15 minutes")) %>% 
  group_by(IN_OUT, Movement_Type,Staging_Post,Movement15) %>% 
  mutate(counter = case_when(
    IN_OUT == 'IN' ~ 1,
    IN_OUT == 'OUT' ~ -1)) %>% 
  mutate(Movement_15_SEQNO = cumsum(counter)) %>% 
  ungroup() 


# Change "Tranfer In"  or "Transfer Out" to "Transfer"
plot_data$Movement_Type <- gsub("Transfer.*","Transfer",x = plot_data$Movement_Type)

# Set limits for plotting
lims <- as.POSIXct(strptime(c("2014-09-03 00:00","2014-09-04 01:00")
                            , format = "%Y-%m-%d %H:%M"))  


ggplot(plot_data,aes(Movement15,Movement_15_SEQNO, colour = Movement_Type)) +
  geom_jitter(width = 0.10) +
  scale_colour_manual(values = c("#D7100D","#40B578","grey60")) +
  facet_grid(Staging_Post~.) +
  scale_x_datetime(date_labels = "%H:%M",date_breaks = "1 hour",
                   limits = lims,
                   timezone = "UTC",
                   expand = c(0,0)) +
  ggtitle(label = "Anytown General Hospital | Wednesday 3rd September 2014 00:00 to 23:59\n",
          subtitle = "A&E AND INPATIENT ARRIVALS, DEPARTURES AND TRANSFERS") +
  labs(x = NULL, y = NULL) +
  theme_ipsum(base_family = "Arial Narrow") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(axis.text.x = element_text(size = 7)) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  theme(strip.text.y = element_text(angle = 0)) +
  guides(color = guide_legend("Movement Type"))

ggsave("row_of_dots.png", height = 5.99, width = 9.46)


