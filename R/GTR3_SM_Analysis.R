setwd("")
list.files()

library(readxl)
library(tidyverse)

# open the data
dat <- read.csv("gtr_3_dataset.csv")


### -- Cleanup

# set species to remove (already removed from baskets)
species_to_remove <- c("Cascabel Rattlesnake", "Eastern Diamondback Rattlesnake",
                       "Ivory", "Mexican Lancehead Rattlesnake", "Middle American Rattlesnake",
                       "Mako Shark", "White Shark", "Humpback Whale",
                       "Rose-ringed Parakeet", "Western Diamondback Rattlesnake", "Shark", "Whale")

# remove species
dat <- subset(dat, !sale.item.species %in% species_to_remove)

# fix publicads name
dat$website.name <- gsub("PublicAds","Publicads", dat$website.name)

#-----------------------------------------------------------------
### -- Website Summary
# summarize web records
perc.tab <- dat%>%
  group_by(website.name)%>%
  count()

# add percentage
perc.tab$perc <- perc.tab$n/sum(perc.tab$n)*100

# save the table
write.csv(perc.tab, "web_perc.csv")

#------------------------------------------
### -- Frequent and rare species summary
freq.tab <- dat%>%
  group_by(website.name, sold.in)%>%
  count(sale.item.species)%>%
  arrange(website.name, sold.in, desc(n))

# write to file
write.csv(freq.tab, "freq_spec.csv")

#--------------------------------------------

# create a cumulative sum line graph for detection records
cum.sum.tab <- dat %>%
  arrange(chronology.datetamp) %>%  # Ensure data is sorted by date
  group_by(chronology.datetamp) %>%
  summarise(n = n())%>%
  mutate(cumulative_count = cumsum(n))

# set date format
cum.sum.tab$chronology.datetamp <- as.Date(cum.sum.tab$chronology.datetamp)
range(cum.sum.tab$chronology.datetamp)

# when did each hub start contributing?
dat$chronology.datetamp <- as.Date(dat$chronology.datetamp)
date.ranges <- dat%>%
  group_by(sold.in)%>%
  summarise(start_date = min(chronology.datetamp),
            last_date = max(chronology.datetamp))
date.ranges

#make plot
ggplot(cum.sum.tab, aes(x=chronology.datetamp, y=cumulative_count, group = ))+
  geom_line(size=4, color="grey")+
  geom_point()+
  theme_bw()

# highlight Thailands start date
# Define Thailand's start and end dates
thailand_start <- as.Date("2024-09-01")
thailand_end <- as.Date("2024-11-01")

# Create the plot
ggplot(cum.sum.tab, aes(x = chronology.datetamp, y = cumulative_count)) +
  # Highlight Thailand's contribution period
  geom_rect(aes(xmin = thailand_start, xmax = thailand_end, ymin = -Inf, ymax = Inf), 
            fill = "gold", alpha = 0.008) +  
  # Add cumulative count line and points
  geom_line(size = 4, color = "grey") +
  geom_point() +
  annotate("text", x = thailand_start, y = max(cum.sum.tab$cumulative_count) * 0.57, 
           label = "Thailand hub starts here", color = "black", size = 5, fontface = "bold", hjust = 1) +
  # Add curved arrow
  geom_curve(aes(x = thailand_start - 10, y = max(cum.sum.tab$cumulative_count) * 0.5, 
                 xend = thailand_start, yend = max(cum.sum.tab$cumulative_count) * 0.1),
             arrow = arrow(length = unit(0.02, "npc")), curvature = -0.3, color = "black", size = 1) +
  # Labels and theme
  labs(title = "Increase in Records Since Last Global Trend Report",
       x = "Month",
       y = "Cumulative Count") +
  theme_bw(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16))  # Center the title

# save the plot
ggsave("cumulative_trend_Aug-Nov_2024.jpg", width = 10, height=3)
