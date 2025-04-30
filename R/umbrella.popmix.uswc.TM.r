# import libraries
# pak::pkg_install("kacurtis/CMRutils",force=TRUE)
# devtools::install("C://Users/kacurtis/Data/Research/Github/CMRutils")
library(CMRutils)
library(dplyr)
library(sf)

# import data
mnid <- import_crcmnid(datdir = "../../Humpbacks/data/external/CRC/", mnidfile = "MN ID 20250303.mdb")

# initial data filtering, editing, and selection
source("selectfields_crcdata_gh.r")

# get any IDs from feeding areas north and west of USWC/SBC, regardless of quality or region
match.nw <- sight %>% filter(!is.na(lat) & ((lat>51) | (lon<(-141) & lat>30))) %>%   # (2nd geog subset does not currently add animals)
  select(id) %>% distinct()

# edit sightings for end date, quality, location, redundant records, etc
source("edit_sight.r")

# set years of interest for wg and wc, get matches per stock, addtl filters, ref df
yy.wg <- 2019:2024   # use abbreviation for "wintering ground" so not confused with Washington State
yy.mex <- 2019:2024   # may be out of sync with other wgs wrt updates to HW - check annual IDs, see plotting section below
yy.wc <- 2019:2024   # "West Coast" (not "feeding area" or ground), as it includes through-migrants
source("subset_rgn_matches.r")
## merge stock df to USWC
sight.uswcyy.stock <- sight.uswc.yy %>% left_join(df.stock.yywg) %>%   # same within yy.wc as using df.stockyywg.uswcyy 
  left_join(df.stock.all)

# ## num / % individuals excluded because sigthed in HI and other wg
# sum(ids.mstock %in% sight.uswc.yy$id)
# 100*sum(ids.mstock %in% sight.uswc.yy$id)/(dim(df.stock.yywg)[1] + sum(ids.mstock %in% sight.uswc.yy$id))

# set up parameter data frame
stock.pars <- data.frame(stock=c("CASM","MX-COW","MX-NP","HI"),
                         abbr=c("C","M","P","H"),
                         Nstarx=rep(NA,4), se.Nstarx=rep(NA,4),
                         nx=rep(NA,4), px=rep(NA,4))

# Estimate wintering area capture probabilities
## CASM
sight.casm.yy <- sight.casm %>% filter(occ %in% yy.wg) %>% filter(id %in% sight.uswc$id)
ch <- array2binom(unclass(with(sight.casm.yy, table(id, occ))))
mrc <- run_CAPTURE(ch, keep.out=FALSE)
stock.pars$nx[1] <- dim(ch)[1]
stock.pars$Nstarx[1] <- mrc$N.ChaoMth
stock.pars$se.Nstarx[1] <- mrc$seN.ChaoMth
rm(ch, mrc, sight.casm.yy)
## MX-COW
sight.mxcow.yy <- sight.mxcow %>% filter(occ %in% yy.mex) %>% filter(id %in% sight.uswc$id)
ch <- array2binom(unclass(with(sight.mxcow.yy, table(id, occ))))
mrc <- run_CAPTURE(ch, keep.out=FALSE)
stock.pars$nx[2] <- dim(ch)[1]
stock.pars$Nstarx[2] <- mrc$N.ChaoMth
stock.pars$se.Nstarx[2] <- mrc$seN.ChaoMth
rm(ch, mrc, sight.mxcow.yy)
## MX-NP
sight.mxnp.yy <- sight.mxnp %>% filter(occ %in% yy.mex) %>% filter(id %in% sight.uswc$id)
ch <- array2binom(unclass(with(sight.mxnp.yy, table(id, occ))))
mrc <- run_CAPTURE(ch, keep.out=FALSE)
stock.pars$nx[3] <- dim(ch)[1]
stock.pars$Nstarx[3] <- mrc$N.ChaoMth
stock.pars$se.Nstarx[3] <- mrc$seN.ChaoMth
rm(ch, mrc, sight.mxnp.yy)
## HAWAII
sight.hi.yy <- sight.hi %>% filter(occ %in% yy.wg) # USWC filter redundant for HI
ch <- array2binom(unclass(with(sight.hi.yy, table(id, occ))))
mrc <- run_CAPTURE(ch, keep.out=FALSE)
stock.pars$nx[4] <- dim(ch)[1]
stock.pars$Nstarx[4] <- mrc$N.ChaoMth
stock.pars$se.Nstarx[4] <- mrc$seN.ChaoMth
rm(ch, mrc, sight.hi.yy)
## calculate px (cumulative probability of being sighted)
stock.pars$px <- stock.pars$nx/stock.pars$Nstarx
## Account for misassigned individuals
### probability of CAm animals being sighted off MX
### conservative calculation using only cam-sighted animals (excluding SM, with SM is slightly higher)
p12 <- mean(match.cam.yy$id %in% match.mex.all.yy$id)
###  correction factor for mxi.c = mxi.u-mxi.cam*fmis
fmis <- p12*(1-stock.pars$px[1])/stock.pars$px[1]


# Get binwise summaries
latbreaks = c(30.5,34,35.5,37.1,38,41.1,42.84,44.3,46.3,48.5,49)

sight.lxi <- sight.uswcyy.stock %>% 
  # add season variable
  mutate(ssn = if_else(mm %in% (6:10), "summer", "winter")) %>% 
  # cut into lat bins and Salish Sea
  mutate(salish = (lat>48 & lon>(-124.65)) | (lat>47 & lon>(-123.5)),
         latbin = cut(lat, breaks=latbreaks),   
         latbin = if_else(latbin %in% c("(46.3,48.5]","(48.5,49]") & salish, "(47,49] Salish", latbin)) %>%
  # add monthly sighting levels
  mutate(yymmi=paste0(year,mm,id))
lxi  <- sight.lxi %>%
  # seasonal binwise summaries
  group_by(latbin, ssn) %>%
  # individual-year-month stats
  mutate(ei=as.numeric(n_distinct(yymmi))) %>%
  mutate(mxi.cam=as.numeric(n_distinct(yymmi[!is.na(stock) & stock=="CASM"]))) %>%  
  ungroup() %>%
  filter(!is.na(stockall)) %>%
  group_by(latbin, ssn, ei, mxi.cam, stockall) %>%
  mutate(mxi.all=as.numeric(n_distinct(yymmi)), ni.all=as.numeric(n_distinct(id))) %>%
  ungroup() %>% 
  group_by(latbin, ssn, ei, mxi.cam, stockall, mxi.all, ni.all, stock) %>%
  summarize(mxi.u = as.numeric(n_distinct(yymmi[!is.na(stock)]))) %>%
  ungroup() %>%
  group_by(latbin, ssn, ei, mxi.cam, stock) %>%
  mutate(mxi.all = if (any(!is.na(stock) & stockall==stock))
                         { mxi.all[!is.na(stock) & stockall==stock] }  else { mxi.all },
         ni.all = if (any(!is.na(stock) & stockall==stock))
           { ni.all[!is.na(stock) & stockall==stock] }  else { ni.all },
         mxi.u = sum(mxi.u)) %>%
  ungroup() %>%
  group_by(latbin, ssn, ei, mxi.cam, stockall) %>%
  mutate(keep = if (any(!is.na(stock) & stockall==stock))
    { !is.na(stock) & stockall==stock } else { is.na(stock) }) %>%
  ungroup() %>%
  filter(keep) %>% 
  select(-c(stock,keep)) %>% rename(stock=stockall) %>%
  # complete stock summaries for each bin 
  tidyr::complete(tidyr::nesting(latbin, ssn, ei, mxi.cam), tidyr::nesting(stock), 
                  fill=list(mxi.all=0, mxi.u=0, ni.all=0))
## add a year-round data frame
lxi.all <- lxi %>% group_by(latbin, stock) %>% 
  summarize(ssn="all", ei=sum(ei), mxi.cam=sum(mxi.cam), 
            mxi.all=sum(mxi.all), ni.all=NA, mxi.u=sum(mxi.u))
# combine seasonal and yrround data frames, add stock pars
lxi %<>% bind_rows(lxi.all) %>% 
  filter(ssn!="winter") %>% 
  arrange(latbin, ssn, stock) %>% 
  # merge with stock parameters
  left_join(stock.pars)
rm(lxi.all)
## calculate mxi and raw lxi
lxi %<>% mutate(mxi.c = if_else(stock=="MX-COW", pmax(0, mxi.u-mxi.cam*fmis), mxi.u),
                lxi.u = mxi.u/(ei*px),
                lxi.c = mxi.c/(ei*px),
                lxi.s = mxi.all/ei,
                cpi = mxi.all/ni.all) %>%  # yymmii captures per indiv per bin/stock/ssn
  ## calculate standardized lxi
  group_by(latbin, ssn) %>%
  mutate(sumlxi.u=sum(lxi.u), sumlxi.c=sum(lxi.c), 
         lxi.u.std=lxi.u/sumlxi.u, lxi.c.std=lxi.c/sumlxi.c,
         sumlxi.s=sum(lxi.s), lxi.s.std=lxi.s/sumlxi.s) %>%
  ungroup()
## get bin data and add to lxi
lbin <- lxi %>% select(latbin) %>% distinct() %>% 
  rowwise() %>% 
  mutate(minlat=readr::parse_number(strsplit(latbin,",")[[1]][1]),
         maxlat=readr::parse_number(strsplit(latbin,",")[[1]][2]),
         binwidth=if_else(grepl("Salish",latbin), NA, maxlat-minlat),
         midpt=if_else(grepl("Salish",latbin), 49, (maxlat+minlat)/2)) %>% 
  ungroup()
lxi %<>% left_join(lbin)
save(lxi, file="lxi.rda")

# get annual summaries
source("get.lxi.ann.r")

# Monte Carlo simulations for uncertainty
# source("popmix.uswc.mc.r")

# combine lxi and lxi.mc
load("lxi.mc.rda")
lxi %<>% left_join(lxi.mc.stats) %>% 
  select(latbin, ssn, stock, everything())
