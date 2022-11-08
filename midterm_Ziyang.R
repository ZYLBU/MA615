library(tidyverse)
library(magrittr)
library(readxl)
library(Rmisc)

strawb <- read_xlsx("strawberries-2022oct30-a.xlsx",col_names = TRUE)

cnames <- colnames(strawb)

x <- 1:dim(strawb)[2]
T <- NULL
for(i in x){T <- c(T, dim(unique(strawb[i]))[1])}
drop_cols <- cnames[which(T == 1)]

strawb %<>% dplyr::select(!all_of(drop_cols))
strawb %<>% arrange(Year, State)

strawb %<>% separate(col=`Data Item`,
                     into = c("Strawberries", "type", "items", "units"),
                     sep = ",",
                     fill = "right")

temp1 <- strawb %>% select(Strawberries) %>% 
  distinct()

pr_rec <- grep("STRAWBERRIES - PRICE RECEIVED", 
               strawb$Strawberries, 
               ignore.case = T)

type_organic <- grep("organic", 
                     strawb$type, 
                     ignore.case = T)

items_organic <- grep("organic", 
                      strawb$items, 
                      ignore.case = T)  ## nothing here

Domain_organic <- grep("organic", 
                       strawb$Domain, 
                       ignore.case = T)


Domain_Category_organic <- grep("organic", 
                                strawb$`Domain Category`, 
                                ignore.case = T)
org_rows <- intersect(type_organic, Domain_organic)

strawb_organic <- strawb %>% slice(org_rows, preserve = FALSE)

strawb_non_organic <- strawb %>% filter(!row_number() %in% org_rows)

#Q1
a1 <- which(strawb$Value==285)
#Corresponding sales value is 87015, measured in $.

#Q2
strawSub <- subset(strawb, Year == 2016 & State == "CALIFORNIA" & Domain == "ORGANIC STATUS")
Ca_Org_2016 <- as.numeric(strawSub$Value)
a2 <- CI(Ca_Org_2016)
# Confidence Interval: [-39779720,194947329]

#Q4
chemical <- filter(strawb, Domain != 'ORGANIC STATUS' & Domain != 'TOTAL' )
TT <- grep("TOTAL",
          chemical$`Domain Category`,
          ignore.case = T)
unique(chemical$`Domain Category`)
a4 <- length(unique(chemical$`Domain Category`)) - length(TT)
#139

#Q5
Ca <- filter(chemical,State == "CALIFORNIA")
Cachem <- length(unique(Ca$`Domain Category`))
Fl <- filter(chemical,State == "FLORIDA")
Flchem <- length(unique(Fl$`Domain Category`))
TTCa <- grep("TOTAL",Ca$`Domain Category`,ignore.case = T)
TTFl <-grep("TOTAL",Fl$`Domain Category`,ignore.case = T)
a5 <- (Cachem-length(TTCa))-(Flchem-length(TTFl))
# 23


