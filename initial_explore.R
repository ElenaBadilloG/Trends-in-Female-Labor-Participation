######## ######## ######## ######## ######## ######## ######## ######## ######## ######## ######## ######## ######## ######## 
# Exploring Trends in Labor Measures

# Elena Badillo Goicoechea
# January 20, 2019
######## ######## ######## ######## ######## ######## ######## ######## ######## ######## ######## ######## ######## ######## 

library(tidyverse)
library(ggplot2)
library(here)

## I. Hours Worked and Skill Mismatch

# Hours worked dataset:
hoursw <- read.csv(here("data", "oecd_hoursw.csv"))

# Create 'region' column:
hoursw <- mutate(hoursw, Region = ifelse(Country %in% c('Sweden', 'Norway',
                                                  'Denmark','Finland'), "Nordic",
                                   ifelse(Country %in% c('OECD countries'), 'OECD',                  
                                      ifelse(Country %in% c('Spain', 'Italy', 'Portugal', 'Greece'), "Mediterranean",
                                         ifelse(Country %in% c('Russian Federation', 'Lithuania','Latvia',
                                                           'Estonia',  'Slovenia', 'Slovak Republic'), 'Eastern Europe',
                                            ifelse(Country %in% c('Japan', 'Korea'), 'Asia',
                                                   ifelse(Country %in% c('Germany', 'Netherlands','Luxembourg', 'France', 'United Kingdom',
                                                                         'Hungary', 'Poland', 'Austria', 'Belgium', 'Czech Republic', 'Switzerland', 'Ireland' ),
                                                          'Continental Europe', 'Rest')))))))

# Obtain mean weekly hours worked on average over 1980-2017 for each country:
means <- hoursw %>% 
  group_by(Country, Region) %>% 
  summarise(av_hours = mean(Value, na.rm = TRUE)) %>%
  arrange(av_hours)

# Add job mismatch data to the small summary dataset:
mism <- read.csv(here("data","mism.csv"))
mism <- filter(mism, Indicator %in% c('Qualification mismatch'))
means <- merge(means, mism, by = c("Country"), all.x = TRUE)

# Build arbitrary color palette to preserve region colors along all plotting tasks:
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}
mycols <- gg_color_hue(length(unique(means$Region)))
names(mycols) <- unique(means$Region)
mycols["OECD"] <- "black"
mycols["Rest"] <- "pink"
mycols["Nordic"] <- "blue"
mycols["Asia"] <- "red"
mycols["Mediterranean"] <- "magenta"
mycols["Continental Europe"] <- "light blue"
mycols["Eastern Europe"] <- "#33CC33"

########################### PLOT 1) Hours Worked and Skill Mismatch

ggplot(data = means, mapping = aes(x=reorder(Country,av_hours),
                                   y = av_hours), y=av_hours) + 
  geom_bar(stat="identity", aes(fill=Region)) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values=mycols) +
  geom_point(data=means, aes(x=Country, y =Value/.02),  fill='dark gray',
             size =  2.5, alpha =0.6) +
  scale_y_continuous("Hours Worked (No.)",sec.axis = sec_axis(~ . *.02, name = "Mismatch (%)")) +
  ggtitle("Average Hours Worked and Qualification Mismatch \n") +
  xlab("")  + 
  theme(plot.title = element_text(color="black", size=16, face="bold", hjust = 0.5, family = "Trebuchet MS"),
        axis.title.y = element_text(color="black", size=12, family = "Trebuchet MS")) +
  labs(subtitle = "  Countries with more working hours per week have, on average, lower levels of job skill mismatch. This trend seems to \n group countries into geographic regions, with Nordic and Western European countries standing out for their low amount\n of worked hours and low skill mismatch.\n ",
       caption = "Source: OECD Labor Statistics, 1980-2017. Qualification mismatch data corresponds to 2016.\n
       Hours Worked = Annual average weekly hours worked per worker \n
       Qualification Mismatch = Percentage of workers with an educational \n attainment higher or lower than that required by their job") +
  theme(plot.subtitle=element_text(size=11, hjust=0, color="black", family = "Trebuchet MS")) +
  theme(plot.caption = element_text(size=9, color="black", family = "Trebuchet MS")) +
  coord_cartesian(ylim = c(1000, 2500)) 

## II. Public Sector Size, Population, and LFP

# Load and process employment participation rate data:
epr <- read.csv(here("data", "EPR.csv"))
epr_allage <- filter(epr, AGE %in% c('1564'))

epr_w <- filter(epr_allage, SEX %in% c('WOMEN'))
epr_mw <- filter(epr_allage, SEX %in% c('MEN'))
epr_mw$EPR_W <- epr_w$EPR
epr_mw <- mutate(epr_mw, mw_dif = EPR - EPR_W)
epr_mw <- mutate(epr_mw, Region = ifelse(Country %in% c('Sweden', 'Norway',
                                                        'Denmark','Finland'), "Nordic",
                                         ifelse(Country %in% c('OECD countries'), 'OECD',                  
                                                ifelse(Country %in% c('Spain', 'Italy', 'Portugal', 'Greece'), "Mediterranean",
                                                       ifelse(Country %in% c('Russian Federation', 'Lithuania','Latvia',
                                                                             'Estonia',  'Slovenia', 'Slovak Republic'), 'Eastern Europe',
                                                              ifelse(Country %in% c('Japan', 'Korea'), 'Asia',
                                                                     ifelse(Country %in% c('Germany', 'Netherlands','Luxembourg', 'France', 'United Kingdom',
                                                                                           'Hungary', 'Poland', 'Austria', 'Belgium', 'Czech Republic', 'Switzerland', 'Ireland' ),
                                                                            'Continental Europe', 'Rest')))))))

########################### PLOT 2) Women Labor Participation Over Time

ggplot(data = epr_mw, mapping = aes(x = Time, y = EPR_W)) + 
  geom_smooth(se = FALSE, aes(color=Region), span=0.05, method='loess', size = 0.5) + # we smooth (very lightly) to correct strong 
  # seasonal patterns
  geom_smooth(data = epr_mw, se = FALSE, aes(y = EPR, color=Region),
              span=0.05, method='loess', linetype="dashed", size = 0.5) +
  scale_color_manual(values=mycols) +
  scale_x_continuous(breaks=seq(1980,2018, 6)) +
  ggtitle("Labor Force Participation Rate (LFP) by Gender \n") +
  xlab("Year") + ylab("LFP (%)") +
  theme(plot.title = element_text(color="black", size=16, face="bold", hjust = 0.5, family = "Trebuchet MS"),
        axis.title.y = element_text(color="black", size=12, family = "Trebuchet MS")) +
  labs(subtitle = "While male LFP has remained steady and at higher levels than female LFP, the latter has continuously increased \n in virtually all regions.\n",
       caption = "Source: OECD Labor Statistics, 1980-2017 \n
       Labor Force Participation Rate = Number of people available for work as a percentage of the total working age population.") +
  theme(plot.subtitle=element_text(size=11, hjust=0, color="black", family = "Trebuchet MS")) +
  theme(plot.caption = element_text(size=9, color="black", family = "Trebuchet MS")) +
  coord_cartesian(ylim = c(30, 60)) 

####  Women LFP, Goverment Size, and Job Quality

# Government Size: measured as the ratio of total taxation to GDP

tax <- read.csv(here("data", "tax.csv"))
total_tax <- filter(tax, TAX %in% c('TOTALTAX'))
total_tax <- filter(tax, EPR_W %in% c(20, 100))
names(total_tax)[names(total_tax) == 'Year'] <- 'Time'
df <- merge(epr_mw, total_tax, by = c("Country", "Time"))
names(df)[names(df) == 'Value'] <- 'Tax'
df <- filter(df, EPR_W > 20.0)

# Load and process job strain data:
jobq <- read.csv(here("data", "jobq.csv"))
jobq <- filter(jobq, Age== 'Total')
jobq <- filter(jobq, Education == 'Total')
jobq <- filter(jobq, Sex == 'Total')
jobq <- filter(jobq, Components == 'Job Strain')

JSindex<- jobq %>% 
  group_by(Country) %>% 
  summarise(JobStrain = mean(Value))

# Add job strain index to the dataset:
df3 <- merge(df, JSindex, by = c("Country"), all.x = TRUE)

######################### PLOT 3): Women LFP, Goverment Size, and Job Quality

ggplot(data = df3, mapping = aes(x = Tax, y = EPR_W, fill= Region, colour=Region, 
                                 size=JobStrain)) +
  geom_jitter(alpha=0.55, pch=21) + 
  scale_colour_manual(values=mycols) +
  scale_fill_manual(values=mycols) +
  ggtitle("Female Labor Participation, Size of Public Sector, and Job Quality \n") +
  xlab("Tax Revenues (% GDP)") + ylab("Female LFP (%)") +
  theme(
    plot.title = element_text(color="black", size=16, face="bold", hjust = 0.5, family = "Trebuchet MS"),
    axis.title.y = element_text(color="black", size=12, family = "Trebuchet MS")) +
  labs(subtitle = "   Countries with higher female labor participation have, on average, larger public sectors, and higher job quality levels.
         A regional clustering pattern similar of that in first graph is also clear, with Nordic countries at the upper right corner 
       and lower reported job strain levels.\n
         Whether and the extent to which this relations are meaningful, of course, requires more careful analysis and more granular data.
       For instance, are there specific types of public spending that might explain both large government size 
       and women participation in the labor market? This and related issues can be explored in later stages of the analysis.\n",
       caption = "Source: OECD Statistics, 1980-2017 \n
       Job strain index = Proportion of workers facing more job demands than the number of resources they have at their disposal.
       Taking into account of data availability, two types of job demands are identified: i) time pressure which encompasses long working hours,
       high work intensity and working time inflexibility; and ii) physical health risk factors.")  +
  theme(plot.subtitle=element_text(size=11, hjust=0, color="black", family = "Trebuchet MS")) +
  theme(plot.caption = element_text(size=9, color="black", family = "Trebuchet MS")) 