# DATA MANAGEMENT AND VISUALITION EXAMPLES - using 'Forest Research' data describing the provisional woodland area of countries within the United Kingdom (UK) from 1998-2024.

# When it comes to engagement, people tend to be more intrigued by data which shows dramatic, notable differences between its independent conditions and across its covariates. To 
# challenge both myself and these perceptions, I've chosen to explore how relatively uneventful data can be interestingly visualised to facilitate curiosity in otherwise tedious
# findings. In this repository, the below code is designed to demonstrate interesting changes in woodland regions within the United Kingdom, alongside interpretations of visual 
# correlates with change taken from other related data gathered by the same organisations. Visualisations do not utilise statistical analyses as the purpose of this code is to
# simply explore different ways in which data can be visualised to provoke interest.

## The initial code below has been designed to extract data on the total provisional woodland area within each country within the United Kingdom (UK) from 1998-2024, calculate
## the percentage increase for each country across this time period, then visualise the growth of woodland area in the UK from 1998-2024 both in terms of absolute value numeric, 
## and statistically proportional changes. This facilitates more appropriate comparisons of woodland development in the UK than simple change from absolute value amounts in 1998.

# The first plot, aptly named 'Basic Plot', was coded while at the University of Sheffield as part of novice attempts at data visualisation, examined during the PSY6422 Module.
# Because of this, I have added the original version of the plot (to "/figs"), which spanned available data from 1998-2023 :) I may update this yearly to show changes!

## DATA EXTRACTION: 
# Extraction of necessary variables from processed dataframe

country_names<-c("England", "Wales", "Scotland", "Northern Ireland")
woodland_area<-select(processed_extracted_data, ends_with(" total (thousand ha)")) # Total Woodland Area
for (country in country_names){
  assign(country, woodland_area[[paste0(country, " total (thousand ha)")]])
} # creates numeric variables for the woodland area of each country, to be labelled per year

woodland_area<-data.frame(England,Wales,Scotland,`Northern Ireland`) # which is then put back into the dataframe
woodland_area<-woodland_area%>% rename(Northern_Ireland=`Northern.Ireland`) # *corrects for interaction issues now caused by spacing in `Northern Ireland`
country_names<-c("England", "Wales", "Scotland", "Northern_Ireland") # *

woodland_by_year<-cbind(year_ending_March_31st,woodland_area) # Total Woodland Area, By Year

table(is.na(woodland_by_year)) # checks for any missing data - none!

# Calculation of proportional change in woodland area per country

percentage_results<-data.frame(country=character(),percentage_increase=numeric(),stringsAsFactors=FALSE) # creates an empty dataframe
for(country in country_names){
  min_v<-min(woodland_area[[country]])
  max_v<-max(woodland_area[[country]])
  percentage_increase<-(((max_v-min_v)/min_v)*100)
  percentage_results<-rbind(percentage_results, data.frame(country=country,percentage_increase=percentage_increase))
} # this loop calculates the percentage increase in woodland area of each country from 1998-2024
percentage_results[1:4,2]<-round(percentage_results[1:4,2],2) # then rounds the data to 2 decimal places

## PRELIMINARY DATA CHECK: 

woodland_by_year
percentage_results

## FINAL DATAFRAME:

woodland_growth_over_time<-data.frame(
  country=c(rep("England",27),rep("Wales",27),rep("Scotland",27),rep("Northern Ireland",27)),
  woodland=c(woodland_by_year$England,woodland_by_year$Wales,
             woodland_by_year$Scotland,woodland_by_year$Northern_Ireland),
  year=c(woodland_by_year$year_ending_March_31st)) # creates a final dataframe amenable to the upcoming visualisation

mapping<-aes(x=year,y=woodland,colour=country) # creates the mapping for the visualisations

## BASIC STATIC PLOT:

# Creates a plot mapping the amount of woodland area development from 1998-2024, as divisable by country, and as annotated with percentage increase

BasicPlot<-woodland_growth_over_time %>%
  ggplot(mapping=mapping)+
  geom_smooth(method="gam")+
  geom_point()+
  labs(x="Year (ending March 31st)",
       y="Woodland area (in thousand hectares)",
       colour="Country",
       title="The Growth of Total Woodland Area within the United Kingdom",
       subtitle="As annotated with percentage increase from 1998-2024",
       caption="Data retrieved from: Forest Research, 2024")+
  annotate("label",x=2010,y=1210,label=paste(percentage_results[1,2],"%"),colour="#EE0000",size=3,fontface="bold")+
  annotate("label",x=2010,y=375,label=paste(percentage_results[2,2],"%"),colour="#00CD00",size=3,fontface="bold")+
  annotate("label",x=2010,y=1450,label=paste(percentage_results[3,2],"%"),colour="#0000CD",size=3,fontface="bold")+
  annotate("label",x=2010,y=175,label=paste(percentage_results[4,2],"%"),colour="#FFA500",size=3,fontface="bold")+
  scale_colour_manual(values=c(England="#EE0000",Wales="#00CD00",
                               Scotland="#0000CD",`Northern Ireland`="#FFA500"))+
  scale_x_continuous(breaks=seq(1998,2024,5))+
  scale_y_continuous(breaks=seq(0,1500,150))+
  theme(panel.border=element_rect(colour="#8B7355",fill=NA,linewidth=2),
        panel.grid.minor=element_line(colour="#8B7355",linewidth=0.5),
        panel.grid.major=element_line(colour="#CAFF70",linewidth=0.7),
        panel.background=element_rect(fill="#FFFFF0"),
        axis.line=element_line(linewidth=2,colour="#8B7355"),
        plot.title=element_text(face="bold"),
        plot.subtitle=element_text(face="italic"),
        text=element_text(family="serif"),
        legend.title=element_text(face="bold"),
        legend.box.background=element_rect(colour="#8B7355"),
        legend.box.margin=margin(1,1,1,1),
        legend.key=element_rect(colour="#8B7355"))

# To demonstrate the ability to do so, this code adds the official logo to sit alongside the data source

BasicPlot<-ggdraw(BasicPlot)+
  draw_image(logo_file, scale=.2,x=1,hjust=1,halign=1,valign=0)

## Visualisation of the Growth of Total Woodland Area within the United Kingdom, from 1998 to 2024

BasicPlot

## SAVES BASIC PLOT

filename<-paste("The Growth of Woodland Area in the UK from 1998 to 2024.png",sep="")
ggsave(file.path(fig_path,filename),plot=BasicPlot,width=7,height=6.37)
