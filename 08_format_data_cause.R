# head(d_surv)

# format_data <- function(lowtag,e,r,s,censored,p1){
#     x <- rbind(rep(0,5))
#     for(i in 1:length(e)){
#         if(censored[i] == 0){
#             y <- c(lowtag[i],e[i],r[i],1,p1[i])
#             z <- c(lowtag[i],r[i],s[i],0,p1[i])
#             x <- rbind(x,y,z)
#         }else{
#             y <- c(lowtag[i],e[i],r[i],0,p1[i])
#             x <- rbind(x,y)
#         }
#     }
#     x <- x[-1,]
#     rownames(x) = NULL
#     x
# }


# d_fit <- format_data(lowtag = d_surv$lowtag,
#             e = d_surv$left_period_e,
#             r = d_surv$right_period_r,
#             s = d_surv$right_period_s,
#             censored = d_surv$censored,
#             p1 = d_surv$p1)
# d_fit



d_fit <- d_surv[d_surv$censored == 0,]
age_class_indx <- c(52,#fawns
                    52*2,#1
                    52*3,#2
                    52*4,#3
                    52*6,#4-5
                    52*9#6-8
                    )#9.5+

d_fit$ageclassmort <- c()
for(i in 1:nrow(d_fit)){
    if(d_fit$right_age_s[i] > 52*9){
        d_fit$ageclassmort[i] <- 7
    }else{
        d_fit$ageclassmort[i] <- min(which(d_fit$right_age_s[i]<age_class_indx))
    }
}

#females are 1
#males are 0

#creating the response for the cause-specific likelihood
#mort_h == 0 means that deer are not hunter harvest
#mort_h == 1 means that deer are hunter harvested 
d_fit$mort_h <- 0
d_fit$mort_h[d_fit$p1==1] <- 1

records_cause <- nrow(d_fit)
n_ageclassf <- length(unique(d_fit$ageclassmort))

#change the sex for antlerless males (i.e. male fawns) to sex == 1
d_fit$sex[d_fit$sex==0 & d_fit$ageclassmort==1] <- 1
n_ageclassm <- length(unique(d_fit$ageclassmort[d_fit$sex==0]))

table(d_fit$right_age_s[d_fit$sex==0])

###########################################
###
### Read data - hunting season dates
###
###########################################

d_huntseason <- read_xlsx("~/Documents/Data/Harvest/Hunting_SeasonDates.xlsx",1)
# c(d_huntseason[d_huntseason$SeasonType=="Gun",1])
# c(d_huntseason[d_huntseason$SeasonType=="nineday",1])

d_huntseason <- d_huntseason %>% filter(Year>2016)
startdate <- min(df_cap$date_cap)

startng <- c()
endng <- c()
startgun <- c()
endgun <- c()
for (i in 1:5) {
    startng[i] <- interval(startdate,
        min(d_huntseason$OpenDate[d_huntseason$Year == (i + 2016)])) %/% weeks(1)+1
    endng[i] <- interval(startdate,
        max(d_huntseason$CloseDate[d_huntseason$Year == (i + 2016)])) %/% weeks(1)+1
    startgun[i] <- interval(startdate,
        d_huntseason$OpenDate[d_huntseason$Year == (i + 2016) & 
                                d_huntseason$SeasonType == "nineday"]) %/% weeks(1)+1
    endgun[i] <- interval(startdate,
        d_huntseason$CloseDate[d_huntseason$Year == (i + 2016) & 
                                d_huntseason$SeasonType == "nineday"]) %/% weeks(1)+1
}

startng
endng
startgun
endgun

###########################################
###
### Preliminaries - hunting season dates
###
###########################################


#rep(c("ng","gun"))
#"ng"
startdate <- min(df_cap$date_cap)

# interval(df_cap$date_cap[!is.na(df_cap$recap_cwd)],df_cap$recapdate_cap[!is.na(df_cap$recap_cwd)]) %/% months(1)
year  <- 2017:2021

# startng <- c(interval(startdate,"2017-09-16") %/% weeks(1)+1,
#              interval(startdate,"2018-09-15") %/% weeks(1)+1,
#              interval(startdate,"2019-09-14") %/% weeks(1)+1,
#              interval(startdate,"2020-09-12") %/% weeks(1)+1,
#              interval(startdate,"2021-09-18") %/% weeks(1)+1
#             )

# endng <- c(interval(startdate,"2018-01-07") %/% weeks(1)+1,
#            interval(startdate,"2019-01-06") %/% weeks(1)+1,
#            interval(startdate,"2020-01-05") %/% weeks(1)+1,
#            interval(startdate,"2021-01-03") %/% weeks(1)+1,
#            interval(startdate,"2022-01-09") %/% weeks(1)+1
#             )

# startgun <- c(interval(startdate,"2017-11-18") %/% weeks(1)+1,
#               interval(startdate,"2018-11-17") %/% weeks(1)+1,
#               interval(startdate,"2019-11-23") %/% weeks(1)+1,
#               interval(startdate,"2020-11-21") %/% weeks(1)+1,
#               interval(startdate,"2021-11-20") %/% weeks(1)+1
#             )

# endgun <- c(interval(startdate,"2017-11-26") %/% weeks(1)+1,
#             interval(startdate,"2018-11-25") %/% weeks(1)+1,
#             interval(startdate,"2019-12-01") %/% weeks(1)+1,
#             interval(startdate,"2020-11-29") %/% weeks(1)+1,
#             interval(startdate,"2021-11-28") %/% weeks(1)+1
#             )

Z_ng <- rep(0,nT_period)
Z_gun <- rep(0,nT_period)
for(i in 1:5){
    Z_ng[startng[i]:endng[i]] <- 1
    Z_gun[startgun[i]:endgun[i]] <- 1
}

Z_ng
Z_gun


################################################################
###
### preliminaries for hunter harvest Z at a monthly time step
###
################################################################


# startdate <- min(df_cap$date_cap)

# # interval(df_cap$date_cap[!is.na(df_cap$recap_cwd)],df_cap$recapdate_cap[!is.na(df_cap$recap_cwd)]) %/% months(1)
# year  <- 2017:2021
# startng <- c(interval(startdate,"2017-09-16") %/% months(1)+1,
#              interval(startdate,"2018-09-15") %/% months(1)+1,
#              interval(startdate,"2019-09-14") %/% months(1)+1,
#              interval(startdate,"2020-09-12") %/% months(1)+1,
#              interval(startdate,"2021-09-18") %/% months(1)+1
#             )

# endng <- c(interval(startdate,"2018-01-07") %/% months(1)+1,
#            interval(startdate,"2019-01-06") %/% months(1)+1,
#            interval(startdate,"2020-01-05") %/% months(1)+1,
#            interval(startdate,"2021-01-03") %/% months(1)+1,
#            interval(startdate,"2022-01-09") %/% months(1)+1
#             )

# startgun <- c(interval(startdate,"2017-11-18") %/% months(1)+1,
#               interval(startdate,"2018-11-17") %/% months(1)+1,
#               interval(startdate,"2019-11-23") %/% months(1)+1,
#               interval(startdate,"2020-11-21") %/% months(1)+1,
#               interval(startdate,"2021-11-20") %/% months(1)+1
#             )

# endgun <- c(interval(startdate,"2017-11-26") %/% months(1)+1,
#             interval(startdate,"2018-11-25") %/% months(1)+1,
#             interval(startdate,"2019-12-01") %/% months(1)+1,
#             interval(startdate,"2020-11-29") %/% months(1)+1,
#             interval(startdate,"2021-11-28") %/% months(1)+1
#             )


# Z_ng <- rep(0,nT_period_month)
# Z_gun <- rep(0,nT_period_month)
# for(i in 1:5){
#     Z_ng[startng[i]:endng[i]] <- 1
#     Z_gun[startgun[i]:endgun[i]] <- 1
# }

# Z_ng
# Z_gun