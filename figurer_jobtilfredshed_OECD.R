library(tidyverse)
library(ggplot2)
library(ggthemes)
library(scales)

job = read_csv("C:/Users/Emma/Desktop/JOBQ_06052020102203571.csv")

job$Time = lubridate::ymd(job$Time, truncated = 2L)
min = as.Date("2005-1-1")
max = NA

job$Components[job$Components == "Job Strain"] = "Jobbelasting"
job$Components[job$Components == "Earnings quality (in constant prices, at constant PPPs)"] = "Indkomstkvalitet"
job$Components[job$Components == "Labour market insecurity"] = "Arbejdsmarkedsusikkerhed"
job$Education[job$Education == "High skilled"] = "Højt"
job$Education[job$Education == "Medium skilled"] = "Medium"
job$Education[job$Education == "Low skilled"] = "Lavt"
job$Education[job$Education == "Total"] = "Gennemsnit"


job1 = job %>% filter(Country == "Denmark", Components == "Jobbelasting")

{ggplot(data=job1, aes(x=Time, y=Value, group=Education, colour=Education)) + 
    geom_point() + 
    geom_line() +
    xlab("Tid") + ylab("Jobbelasting, pct.") +
    ggtitle("Jobbelasting") +
    guides(colour=guide_legend(title="Kvalifikationsniveau")) +
    theme_economist() +
    theme(legend.position="right") +
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    scale_x_date(limits = c(min, max)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1))
  scale_color_economist()
}

job2 = job %>% filter(Country == "Denmark", Components == "Indkomstkvalitet")

{ggplot(data=job2, aes(x=Time, y=Value, group=Education, colour=Education)) + 
    geom_point() + 
    geom_line() +
    xlab("Tid") + ylab("Indkomstkvalitet") +
    ggtitle("Indkomstkvalitet") +
    guides(colour=guide_legend(title="Kvalifikationsniveau")) +
    theme_economist() +
    theme(legend.position="right") +
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    scale_x_date(limits = c(min, max)) +
    scale_color_economist()
}

job3 = job %>% filter(Country == "Denmark", Components == "Arbejdsmarkedsusikkerhed")

job3$Time = lubridate::ymd(job$Time, truncated = 2L)
min = as.Date("2007-1-1")
max = NA

{ggplot(data=job3, aes(x=Time, y=Value, group=Education, colour=Education)) + 
    geom_point() + 
    geom_line() +
    xlab("Tid") + ylab("Arbejdsmarkedsusikkerhed, pct.") +
    ggtitle("Arbejdsmarkedsusikkerhed") +
    guides(colour=guide_legend(title="Kvalifikationsniveau")) +
    theme_economist() +
    theme(legend.position="right") +
    scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
    scale_x_date(limits = c(min, max)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_color_economist()
}

