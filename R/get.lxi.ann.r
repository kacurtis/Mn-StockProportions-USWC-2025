# get annual binwise summaries and calculated proportions

# Get annual binwise summaries
lxi.ann <- sight.lxi %>% left_join(lbin) %>% 
  group_by(latbin, ssn, year) %>%
  # individual-month stats
  mutate(ei=as.numeric(n_distinct(yymmi))) %>%
  mutate(mxi.cam=as.numeric(n_distinct(yymmi[!is.na(stock) & stock=="CASM"]))) %>%  
  ungroup %>%
  filter(!is.na(stockall)) %>%
  group_by(latbin, ssn, year, ei, mxi.cam, stockall) %>%
  mutate(mxi.all=as.numeric(n_distinct(yymmi))) %>%
  ungroup() %>% 
  group_by(latbin, ssn, year, ei, mxi.cam, stockall, mxi.all, stock) %>%
  summarize(mxi.u = as.numeric(n_distinct(yymmi[!is.na(stock)]))) %>%
  ungroup() %>%
  group_by(latbin, ssn, year, ei, mxi.cam, stock) %>%
  mutate(mxi.all = if (any(!is.na(stock) & stockall==stock))
  { mxi.all[!is.na(stock) & stockall==stock] }  else { mxi.all },
  mxi.u = sum(mxi.u)) %>%
  ungroup() %>%
  group_by(latbin, ssn, year, ei, mxi.cam, stockall) %>%
  mutate(keep = if (any(!is.na(stock) & stockall==stock))
  { !is.na(stock) & stockall==stock } else { is.na(stock) }) %>%
  ungroup() %>%
  filter(keep) %>% 
  select(-c(stock,keep)) %>% rename(stock=stockall) %>%
  # complete stock summaries for each bin 
  tidyr::complete(tidyr::nesting(latbin, ssn, year, ei, mxi.cam), tidyr::nesting(stock), 
                  fill=list(mxi.all=0, mxi.u=0))
## add a year-round data frame
lxi.ann.all <- lxi.ann %>% group_by(latbin, year, stock) %>% 
  summarize(ssn="all", ei=sum(ei), mxi.cam=sum(mxi.cam), 
            mxi.all=sum(mxi.all), mxi.u=sum(mxi.u))
# combine seasonal and yrround data frames
lxi.ann %<>% bind_rows(lxi.ann.all) %>% 
  filter(ssn!="winter") %>% 
  arrange(latbin, ssn, year, stock)
rm(lxi.ann.all)

# add proportions calculated by year, merge with lbin
lxi.ann.prop <- lxi.ann %>% 
  # replace years with less than 100 sightings with NA
  mutate(mxi.u=if_else(ei<30, NA_real_, mxi.u)) %>%
  # merge with stock parameters
  left_join(stock.pars %>% select(stock, px)) %>% 
  # calculate mxi.c and raw lxi
  mutate(mxi.c = if_else(stock=="MX-COW", pmax(0, mxi.u-mxi.cam*fmis), mxi.u),
         lxi.c = mxi.c/(ei*px),
         lxi.s = mxi.all/ei) %>% 
  # calculate standardized lxi
  group_by(latbin, ssn, year) %>%
  mutate(sumlxi.c=sum(lxi.c), lxi.c.std=lxi.c/sumlxi.c,
         sumlxi.s=sum(lxi.s), lxi.s.std=lxi.s/sumlxi.s) %>%
  ungroup() %>% 
  left_join(lbin)

