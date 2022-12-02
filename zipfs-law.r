library(tidyverse)

# read in data
msa <- read.csv("msapop2020.csv")
# create new data to conform with Gabaix (1999)
msa135 <- msa %>%
  filter(rankpop20<=135)

# plot -- all MSAs
p <- ggplot(msa)+
  aes(x = log(pop20), y = log(rankpop20))+
  geom_point(alpha = 0.5, size = 2, color = "#E69F00") +
  geom_smooth(method = "lm", se = FALSE, size=0.5, color="#2b2b2b", alpha=0.5) +
  # Theming
  labs(
    title="Zipf's law",
    subtitle="Log population and log population rank, All U.S. metropolitan areas, 2020",
    caption="Author: Chris Goodman (@cbgoodman), Data: U.S. Census Bureau & Author's calculations.",
    y="Log of Rank",
    x="Log of Population") +
  theme_minimal(base_family="Open Sans Condensed Light") +
  # light, dotted major y-grid lines only
  theme(panel.grid=element_line())+
  theme(panel.grid.major.y=element_line(color="#2b2b2b", linetype="dotted", size=0.15))+
  theme(panel.grid.major.x=element_blank())+
  theme(panel.grid.minor.x=element_blank())+
  theme(panel.grid.minor.y=element_blank())+
  # light x-axis line only
  theme(axis.line=element_line())+
  theme(axis.line.y=element_blank())+
  theme(axis.line.x=element_blank())+
  # tick styling
  theme(axis.ticks=element_line())+
  theme(axis.ticks.x=element_blank())+
  theme(axis.ticks.y=element_blank())+
  theme(axis.ticks.length=unit(5, "pt"))+
  # x-axis labels
  theme(axis.text.x=element_text(size=10, hjust=0.95,vjust=0.2))+
  # breathing room for the plot
  theme(plot.margin=unit(rep(0.5, 4), "cm"))+
  # move the y-axis tick labels over a bit
  theme(axis.text.y=element_text(margin=margin(r=-5)))+
  theme(axis.text.x=element_text(margin=margin(r=-5)))+
  # make the plot title bold and modify the bottom margin a bit
  theme(plot.title=element_text(family="Open Sans Condensed Bold", margin=margin(b=15)))+
  # make the subtitle italic
  theme(plot.subtitle=element_text(family="Open Sans Condensed Light Italic"))+
  theme(plot.caption=element_text(size=8, hjust=0, margin=margin(t=15)))
ggsave(plot=p, "zipf.png", width=8, height=6, units="in", dpi="retina")

summary(lm(data=msa, log(rankpop20)~log(pop20)))

# plot -- 135 largest
p1 <- ggplot(msa135) +
  aes(x = log(pop20), y = log(rankpop20)) +
  # points
  geom_point(alpha = 0.5, size = 2, color = "#E69F00") +
  # point labels for the largest cities -- still deciding
  # geom_text(aes(label=ifelse(rank2010<5,as.character(msa),'')),hjust=0,vjust=0) +
  # trend line
  geom_smooth(method = "lm", se = FALSE, size=0.5, color="#2b2b2b", alpha=0.5) +
  # theming
  labs(
    title="Zipf's law",
    subtitle="Log population and log population rank, 135 largest U.S. metropolitan areas, 2020",
    caption="Author: Chris Goodman (@cbgoodman), Data: U.S. Census Bureau & Author's calculations.",
    y="Log of Rank",
    x="Log of Population") +
  theme_minimal(base_family="Public Sans Light") +
  # light, dotted major y-grid lines only
  theme(panel.grid=element_line())+
  theme(panel.grid.major.y=element_line(color="#2b2b2b", linetype="dotted", size=0.15))+
  theme(panel.grid.major.x=element_blank())+
  theme(panel.grid.minor.x=element_blank())+
  theme(panel.grid.minor.y=element_blank())+
  # light x-axis line only
  theme(axis.line=element_line())+
  theme(axis.line.y=element_blank())+
  theme(axis.line.x=element_blank())+
  # tick styling
  theme(axis.ticks=element_line())+
  theme(axis.ticks.x=element_blank())+
  theme(axis.ticks.y=element_blank())+
  theme(axis.ticks.length=unit(5, "pt"))+
  # x-axis labels
  theme(axis.text.x=element_text(size=10, hjust=0.95,vjust=0.2))+
  # breathing room for the plot
  theme(plot.margin=unit(rep(0.5, 4), "cm"))+
  # move the xy-axis tick labels over a bit
  theme(axis.text.y=element_text(margin=margin(r=-5)))+
  theme(axis.text.x=element_text(margin=margin(r=-5)))+
  # make the plot title bold and modify the bottom margin a bit
  theme(plot.title=element_text(family="Public Sans SemiBold", margin=margin(b=15)))+
  # make the subtitle italic
  theme(plot.subtitle=element_text(family="Public Sans Light Italic"))+
  theme(plot.caption=element_text(size=8, hjust=0, margin=margin(t=15)))
ggsave(plot=p1, "zipf135_20.png", width=8, height=6, units="in", dpi="retina")

summary(lm(data=msa135, log(rank2010)~log(pop2010)))
