# get tables data for TM

# first run popmix.uswc.r

# text
## total monthly sightings
lxi %>% select(latbin, ssn, ei) %>% distinct() %>% group_by(ssn) %>% 
  summarize(ei=sum(ei))
## total individuals sighted
sight.lxi %>% summarize(ni=n_distinct(id))
sight.lxi %>% filter(ssn=="summer") %>% summarize(ni=n_distinct(id))

# Table 1: model parameters
round(c(stock.pars$px, p12), 3)
## run top portion of popmix.uswc.mc.r to get stock.pars.mc
stock.pars.mc %>% group_by(stock) %>% summarize(px.x=round(mean(psam),3), px.se=round(sd(psam),3))

# Table 2, 3
lxi.tab <- lxi %>% left_join(lxi.mc.stats) %>% arrange(ssn, desc(latbin), stock) %>% 
  select(-c(mxi.cam, mxi.all, abbr:lxi.u.std, sumlxi.s:midpt)) %>% 
  mutate(lxi.c.std=paste0(round(lxi.c.std,4), " (", round(sdlxi.c.std,4), ")"), 
         q025.lxi.c=round(q025.lxi.c,4), q975.lxi.c=round(q975.lxi.c,4))
write.csv(lxi.tab, file="manuscript/table2and3.csv", row.names=FALSE)

# Table 4: statewise proportions
library(ApportionMnStocks)
library(dplyr)
library(tidyr)
a <- get_mnstockprops(x=data.frame(minlat=rep(c(30.5, 46.3, 42, 30.5, 38.769, 34.45, 30.5), 2), 
                              maxlat=rep(c(49, 49, 46.3, 42, 42, 38.769, 34.45), 2),
                              ssn=c(rep("all",7),rep("summer",7)), pv=rep(1,14)), 
                 props.ind = TRUE) %>% 
  mutate(prop.sd = paste0(prop, " (",sd, ")")) %>% select(id:stock, prop.sd) %>% 
  pivot_wider(names_from=stock, values_from=prop.sd) %>% 
  arrange(id)
write.csv(a, file="manuscript/table4.csv", row.names=FALSE)

# Table 5: RAMP Fishing Zone proportions
ramp <- read.csv("CDFW_RAMP_Fishing_Zones.csv")
b <- get_mnstockprops(x=data.frame(minlat=rep(ramp$minlat,2), 
                                   maxlat=rep(ramp$maxlat,2), 
                                   ssn=c(rep("all",7),rep("summer",7)), pv=rep(1,14)), 
                      props.ind = TRUE) %>% 
  mutate(prop.sd = paste0(prop, " (",sd, ")")) %>% select(id:stock, prop.sd) %>% 
  pivot_wider(names_from=stock, values_from=prop.sd) %>% 
  arrange(id)
write.csv(b, file="manuscript/table5.csv", row.names=FALSE)

# Appendix B
a <- get_mnstockprops(x=data.frame(minlat=rep(c(46.3, 34.45, 36.8, 36.8, 30.5)), 
                                   maxlat=rep(c(48.5, 42, 48.5, 36.8, 49)),
                                   ssn=rep("all",5), pv=rep(1,5)), 
                      props.ind = TRUE) %>% 
  mutate(prop.sd = paste0(prop, " (",sd, ")")) %>% select(id:stock, prop.sd) %>% 
  pivot_wider(names_from=stock, values_from=prop.sd) %>% 
  arrange(id)

