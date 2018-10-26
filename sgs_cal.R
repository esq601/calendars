library(timevis)
library(readxl)
library(reshape2)
library(scales)
library(dplyr)
library(tidyr)
library(stringr)
library(htmltools)
library(lubridate)
library(janitor)

sgs1 <- read_csv("/data/rsworkspace/Eskew/LRTC/SGSCalendar.csv",
                 col_types = cols(`End Date` = col_datetime(format = "%m/%d/%Y %H:%M"),
                                        `Start Date` = col_datetime(format = "%m/%d/%Y %H:%M"),
                                        Unit = col_factor(levels = c("10MD HQ","HHBN", "86 IBCT", "1 IBCT",
                                                                               "2 IBCT", "3 IBCT", "DIVARTY","10 CAB",
                                                                               "10 SBDE", "NET/NEF T&E","LFS/Leader Schools", "Visiting Units"))))

cals <- data.frame(c("CG","DCSM","DCG-O","DCG-R","CoS","DCoS","CoSA","GC","GCSM","DGC","TBD or N/A"))
types <- data.frame(c("Engagements","Battle Rhythm","Unit Social Events"))

sgs2 <- sgs1 %>%
  clean_names() %>%
  tidyr::separate(col=sr_ldr_coverage,into=paste("ldr",seq(1:12),sep=""),sep=";#") %>%
  tidyr::separate(col=executive_options,into=paste("type",seq(1:3),sep=""),sep=";#") %>%
  gather(Category,Attend,c(type1:type3,ldr1:ldr12)) %>%
  filter(!is.na(Attend) | summary_event == TRUE) %>%
  select(-Category) %>%
  distinct()

sgs2a <- sgs2 %>%
  filter(!is.na(Attend)) %>%
  mutate(groupcontent=Attend) %>%
  mutate(type=NA) %>%
  mutate(sub_event=event) %>%
  mutate(title = paste("Start: " , as.character(start_date) , "End: " , as.character(end_date))) %>%
  mutate(subgroup = 1) %>%
  mutate(className="staff") %>%
  mutate(style = case_when(
    exec_legend %in% "FORSCOM" ~ "background-color: Red;
         width: calc(0px + 100%);
         font-size: 70%;",
    exec_legend %in% "XVIII ABN Corps" ~ "background-color: Orange;
         width: calc(0px + 100%);
         font-size: 70%;",
    exec_legend %in% "Division/Other" ~ "background-color: Gray;
         width: calc(0px + 100%);
         font-size: 70%;",
    exec_legend %in% "Engagements" ~ "background-color: LightBlue;
         width: calc(0px + 100%);
         font-size: 70%;",
    exec_legend %in% "Informational/Opportunity" ~ "background-color: Yellow;
         width: calc(0px + 100%);
         font-size: 70%;",
    TRUE ~ "background-color: Gray;
         width: calc(0px + 100%);
         font-size: 70%;"
    ))

sgs2b <- sgs2 %>%
  filter(summary_event == TRUE & unit %in% "10MD HQ") %>%
  mutate(sub_event=event) %>%
  mutate(groupcontent="Training") %>%
  mutate(title="Traning Event") %>%
  mutate(className="lrtc") %>%
  mutate(style = "
         background-color: LightGreen;
         border-color: gray;
         border-style: dashed;
         border-width: 1px;
         width: calc(0px + 100%);
         font-size: 65%;
         text-align: center;
         ")

sgs2b <- sgs2b %>%
  mutate(subgroup=rep(1:3,length.out=nrow(sgs2b)))

sgs3 <- bind_rows(sgs2a,sgs2b)

sgs3 <- sgs3 %>%
  mutate(group= case_when(
    groupcontent %in% "Training" ~ 1,
    groupcontent %in% "Battle Rhythm" ~ 2,
    groupcontent %in% "Engagements" ~ 3,
    groupcontent %in% "Unit Social Events" ~ 4,
    groupcontent %in% "CG" ~ 5,
    groupcontent %in% "DCSM" ~ 6,
    groupcontent %in% "DCG-O" ~ 7,
    groupcontent %in% "DCG-S" ~ 8,
    groupcontent %in% "DCG-R" ~ 9,
    groupcontent %in% "CoS" ~ 10,
    groupcontent %in% "DCoS" ~ 11,
    groupcontent %in% "CoSA" ~ 12,
    groupcontent %in% "GC" ~ 13,
    groupcontent %in% "GCSM" ~ 14,
    groupcontent %in% "DGC" ~ 15,
    groupcontent %in% "TBD or N/A" ~ 16,
    TRUE ~17
  ))

timedatasgs <- data.frame(
  content=sgs3$sub_event,
  start=sgs3$start_date,
  end=sgs3$end_date,
  group=sgs3$group,
  subgroup=sgs3$subgroup,
  title=sgs3$title,
  className=sgs3$className,
  type=sgs3$type,
  style=sgs3$style,stringsAsFactors = FALSE)

timedatasgs <- timedatasgs %>%
  arrange(group)

groupssgs=data.frame(id=sgs3$group, content=sgs3$groupcontent,order=sgs3$group)

groupssgs <- groupssgs %>%
  distinct(id,.keep_all=TRUE)


t2 <- timevis(timedatasgs, groups=groupssgs, showZoom=TRUE,options=list(
  start=as.POSIXct(cut(Sys.Date(), "month")),
  end=as.POSIXct(cut(Sys.Date(), "month")) + months(4),
  #maxHeight = "1000px",
  orientation = "both",
  verticalScroll = "true",
  horizontalScroll = "true",
  zoomKey = "ctrlKey",
  stack = "false",
  verticalScroll = "true",
  width="175%"
))

styles <- "
  .vis-item .vis-item-overflow { overflow: visible; }
  .vis-time-axis .vis-text.vis-saturday,
  .vis-time-axis .vis-text.vis-sunday { background: Grey; }
  .vis-time-axis .vis-text.vis-sunday { background: Grey; }
  .vis-item .vis-item-content { padding: 0px; }
"

t2 <- tagList(list(tags$head(tags$style(styles, type="text/css")), t2))

html_print(t2)
