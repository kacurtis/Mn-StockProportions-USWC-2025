# plots for TM

# first run popmix.uswc.r

# plots
library(ggplot2)
colors.stocks <- c("#D86F27", "#481a45", "#884B73", "#98D1CB")
names(colors.stocks) <- c("CASM", "MX-COW", "MX-NP", "HI")
lxi %<>% mutate(stock=factor(stock, levels=c("HI", "MX-COW", "MX-NP", "CASM")))
lxi.ann.prop %<>% mutate(stock=factor(stock, levels=c("HI", "MX-COW", "MX-NP", "CASM")))

# plot main results
g4 <- lxi %>% 
  ggplot(aes(x=midpt, group=interaction(stock,midpt), col=stock, fill=stock)) + 
  geom_boxplot(aes(ymin=q025.lxi.c, lower=q25.lxi.c, middle=lxi.c.std, 
                   upper=q75.lxi.c, ymax=q975.lxi.c), 
               stat="identity", position = position_dodge2(width = 0.7, reverse=TRUE), alpha = 0.3) + 
  geom_point(aes(y=lxi.s.std, shape = I(13), size=I(2)), 
             position = position_dodge2(width = 1.1, reverse=TRUE), show.legend=FALSE) +
  geom_line(aes(y=lxi.c.std, group=stock),
            position = position_dodge2(width = 1.1, reverse=TRUE), linewidth=0.5) +
  geom_vline(xintercept=c(latbreaks[-length(latbreaks)]), color=I("#555"), linetype=I(2)) +
  annotate("text",label="Salish Sea", x=49, y=0.85, size=3) +  
  coord_flip(expand=FALSE) +
  facet_grid(cols=vars(ssn)) +
  scale_x_continuous(limits=c(30,50)) +
  scale_y_continuous(limits=c(0,1), labels = function(x) if_else(x%%1 > 0, round(x,2), round(x))) +
  scale_color_manual(values=colors.stocks, name="Stock") +
  scale_fill_manual(values=colors.stocks, name="Stock") + 
  xlab("Latitude (°N)") + ylab("Proportion") +
  theme_bw() +
  theme(panel.spacing = unit(1, "lines", data = NULL))
ggsave(plot=g4, filename="manuscript/Fig4_StockProportions.tiff", 
       width=160, height=120, units="mm", dpi = 1200, compression = "lzw")

# stacked bar plot of mean proportions and per year
g5 <- lxi.ann.prop %>% 
  bind_rows(lxi) %>% mutate(year=if_else(is.na(year), "2019-2024", as.character(year))) %>% 
  mutate(year=factor(year, levels=c("2019-2024", as.character(2019:2024)))) %>% 
  filter(ssn=="summer") %>%
  ggplot(aes(x=midpt, group=interaction(stock,midpt), col=stock, fill=stock)) +
  geom_bar(aes(y=lxi.c.std), stat="identity", position = "stack") +
  geom_vline(xintercept=c(latbreaks[-length(latbreaks)]), color=I("#555"), linetype=I(2)) +
  annotate("text",label="Sal. Sea", x=49, y=0.8, size=3) +
  coord_flip(expand=FALSE) +
  facet_grid(cols=vars(year)) +
  scale_x_continuous(limits=c(30,50)) +
  scale_y_continuous(limits=c(0,1), labels = function(x) if_else(x%%1 > 0, round(x,2), round(x))) +
  scale_color_manual(values=colors.stocks, name="Stock") +
  scale_fill_manual(values=colors.stocks, name="Stock") +
  xlab("Latitude (°N)") + ylab("Cumulative Proportion") +
  theme_bw() +
  theme(panel.spacing = unit(1, "lines", data = NULL))
ggsave(plot=g5, filename="manuscript/Fig5_AnnualCumulativeStockProportions.tiff",
       width=300, height=120, units="mm", dpi = 1200, compression = "lzw")

# plot bins and effort
# library(gridExtra)
# library(grid)
library(sf)
library(rnaturalearth)

world <- ne_coastline(scale = "medium", returnclass = "sf")
lon.range <- c(-130, -116)
lat.range <- c(30, 50)
load("USWestCoastEEZ.rda")
### create Salish Sea bounding box
bounding_poly <- st_as_sf(data.frame(id = 1),
                          geometry = st_sfc(st_polygon(list(rbind(c(-124.65,49), c(-124.65,48), c(-123.5,48), c(-123.5,47),c(-118,47),c(-118,49),c(-124.65,49))))),
                          crs = st_crs(useez.wc))
### subset Salish Sea polygon
salish_eez <- st_intersection(useez.wc, bounding_poly)
### get state boundaries
states <- ne_states(country = 'United States of America', returnclass = 'sf')

g3a <- ggplot() +
  geom_sf(data=world, linewidth=0.2) +
  geom_sf(data=useez.wc, fill="gray90") + 
  geom_sf(data = states, fill="white", colour = "gray40") +
  labs(x = "Longitude", y = "Latitude") +
  geom_hline(yintercept=latbreaks[1:(length(latbreaks)-1)], linetype=2) +
  geom_point(data=sight.lxi, aes(lon, lat), size=1) +
  scale_x_continuous(breaks = c(-128,-124,-120)) +
  coord_sf(xlim = lon.range, ylim = lat.range, expand=FALSE) +
  theme(panel.background = element_rect(fill = 'white'), 
        panel.border = element_rect(linetype = 1, fill = NA), 
        axis.text=element_text(size=8), axis.title=element_text(size=10),
        plot.margin = margin(0.1, 0, 0.1, 0, "cm"))
ggsave(plot=g3a, filename="manuscript/Fig3a_Map.tiff", 
       width=72, height = 120, units="mm", dpi = 1200, compression = "lzw")

# plot total sightings (effort) and percent known in wc.yy based on wg.yy and all time
## calculate percent known for 2019-2024 wg dataset
pk.wcyywgyy <- lxi %>% 
  group_by(latbin, midpt, ei, ssn) %>% 
  summarize(pknown=sum(mxi.u)/ei[1]) 
nbin <- n_distinct(pk.wcyywgyy$latbin)
## plot total sightings per bin
g3b <- pk.wcyywgyy %>% 
  ggplot(aes(midpt, sqrt(ei), fill = ssn)) +
  geom_bar(stat = "identity", position = "dodge", width=0.8) +
  scale_x_continuous(limits = lat.range, expand = expansion(0)) +
  scale_y_continuous(limits = c(0, 100), expand = expansion(0),
                     breaks = c(0, 22.4, 50, 77.46, 100), 
                     labels = c("0","500","2500","6000","10000")) +
  labs(y = "Monthly identifications", fill = "Season") + 
  scale_fill_manual(values = c("all" = "black", "summer" = "darkgray")) +
  geom_vline(xintercept=latbreaks[1:(length(latbreaks)-1)], linetype=2) +
  annotate("text", x=49, y=80, label="Salish Sea", size=3.5) +
  theme(panel.background = element_rect(fill = 'white'), 
        panel.border = element_rect(linetype = 1, fill = NA), 
        axis.text.x=element_text(size=8), axis.title.x=element_text(size=10),
        axis.text.y=element_blank(), axis.title.y = element_blank(),
        legend.title=element_text(size=10),
        legend.box.background = element_rect(color="black", linewidth=0.8),
        legend.position = c(0.997, 0.003),
        legend.justification = c("right", "bottom"),
        plot.margin = margin(0.1, 0.4, 0.1, 0.2, "cm")) +
  coord_flip() 
ggsave(plot=g3b, filename="manuscript/Fig3b_Effort.tiff", 
       width=70, height = 120, units="mm", dpi = 1200, compression = "lzw")

## plot percent knowns in summer (all year is very similar, with max drop of 4% in southernmost bin)
### % known from study period
opar <- par(no.readonly=TRUE)
par(mar = c(3, 1, 0, 0) + 0.1)
pk.wcyywgyy <- pk.wcyywgyy %>% filter(ssn=="all")
plot(pk.wcyywgyy$pknown, pk.wcyywgyy$midpt,
     xlab="",ylab="", 
     xlim=c(0.35,0.75),  ylim=c(30,50), xaxs="i", yaxs="i", cex.axis=0.8)
mtext("Proportion classified to stock", side=1, outer=TRUE, line=-1.1, hjust=0)
lines(pk.wcyywgyy$pknown[1:(nbin-1)], pk.wcyywgyy$midpt[1:(nbin-1)], lty=1)
abline(h=latbreaks[1:nbin], lty=2)
### % known from any year per bin in uswc.yy
pk.wcyywgall <- lxi %>% 
  filter(ssn=="all") %>% 
  select(latbin, midpt, ei, sumlxi.s) %>% distinct()
points(pk.wcyywgall$sumlxi.s, pk.wcyywgall$midpt)
lines(pk.wcyywgall$sumlxi.s[1:(nbin-1)], pk.wcyywgall$midpt[1:(nbin-1)], lty=2)
text(0.58,49, "Salish Sea", cex=0.8, pos=4)
legend(0.35, 39, c("2019-2024","all time"), title="Ref years", lty=c(1,2),cex=0.8)
par(opar)
# currently just saving image from graphics window in RStudio set to narrow