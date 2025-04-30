# run Monte Carlo simulations for uncertainty in stock proportions

nsim <- 2000

# sample Nstarx for each stock and recalculate px
stock.pars.mc <- stock.pars %>% 
  mutate(sdl = sqrt(log((se.Nstarx/Nstarx)^2+1)), 
         mu = log(Nstarx) - 0.5*sdl^2) %>% 
  tidyr::nest(.by=stock) %>% 
  mutate(sims = lapply(data, 
                       function(x) 
                         data.frame(
                           Nsam = rlnorm(nsim, meanlog=x$mu, sdlog=x$sdl),
                           sim = 1:nsim))) %>% 
  tidyr::unnest(cols=c(data, sims)) %>% 
  mutate(psam = nx/Nsam) %>% 
  select(-c(px, nx, Nstarx, se.Nstarx, Nsam, sdl, mu)) %>% 
  # resample correction factor for misassigned individuals
  group_by(sim) %>% 
  mutate(fmis.sam=p12*(1-psam[stock=="CASM"])/psam[stock=="CASM"]) %>% 
  ungroup()

# resample years and summarize by bins
lxi.mc <- data.frame()
for (si in 1:nsim) {
  yysam <- sample(yy.wc, replace=TRUE)
  temp <- data.frame(year=yysam) %>% 
    left_join(lxi.ann, relationship="many-to-many") %>% 
    group_by(latbin, ssn, stock) %>% 
    summarize(ei=sum(ei), mxi.cam=sum(mxi.cam), mxi.all=sum(mxi.all), mxi.u=sum(mxi.u)) %>% 
    # merge with stock parameters
    left_join(stock.pars.mc %>% filter(sim==si)) %>% 
    # calculate mxi.c and raw lxi
    mutate(mxi.c = if_else(stock=="MX-COW", pmax(0, mxi.u-mxi.cam*fmis.sam), mxi.u),
           lxi.c = mxi.c/(ei*psam),
           lxi.s = mxi.all/ei) %>% 
    # calculate standardized lxi
    group_by(latbin, ssn) %>%
    mutate(sumlxi.c=sum(lxi.c), lxi.c.std=lxi.c/sumlxi.c,
           sumlxi.s=sum(lxi.s), lxi.s.std=lxi.s/sumlxi.s) %>%
    ungroup()
  lxi.mc <- bind_rows(lxi.mc, temp)
  rm(temp)
}

# tried dropping bin-sim combinations with small sample sizes, but made no visual difference in results
# lxi.mc %<>% filter(ei>=80) 

# drop extraneous fields
lxi.mc %<>% select(-c(mxi.cam, abbr, psam, fmis.sam, lxi.c, lxi.s, sumlxi.c, sumlxi.s))

# calculate statistics
lxi.mc.stats <- lxi.mc %>% group_by(latbin, ssn, stock) %>% 
  summarize(sdlxi.c.std=sd(lxi.c.std), sdlxi.s.std=sd(lxi.s.std),
            q025.lxi.c = quantile(lxi.c.std, 0.025, names=FALSE), 
            q25.lxi.c = quantile(lxi.c.std, 0.25, names=FALSE), 
            q75.lxi.c = quantile(lxi.c.std, 0.75, names=FALSE), 
            q975.lxi.c = quantile(lxi.c.std, 0.975, names=FALSE),
            q025.lxi.s = quantile(lxi.s.std, 0.025, names=FALSE), 
            q25.lxi.s = quantile(lxi.s.std, 0.25, names=FALSE), 
            q75.lxi.s = quantile(lxi.s.std, 0.75, names=FALSE), 
            q975.lxi.s = quantile(lxi.s.std, 0.975, names=FALSE)) %>% 
  ungroup()

# save lxi.mc, lxi.mc.stats as RDA for later use
save(lxi.mc, lxi.mc.stats, file="lxi.mc.rda")
tools::resaveRdaFiles("lxi.mc.rda")

