library(ggplot2)
library(RColorBrewer)
library(scales)
library(reshape2)

# figure 1

df1 <- data.frame("year" = c("1986-1990",
                             "1991-1995",
                             "1996-2000",
                             "2001-2005",
                             "2006-2010",
                             "2011-2015"),

                  "study" = c(2,
                              2,
                              1,
                              1,
                              9,
                              16))

# color_ = "darkblue"
color_ = brewer.pal(5, "Set1")[2]

ggplot(df1, aes(x=year, y=study, group=1)) +
    geom_point(stat='summary', fun.y=sum, color = color_) +
    stat_summary(fun.y=sum, geom="line", size = 1, color = color_) +
    xlab("Year") + ylab("Number of Studies") +
    scale_y_continuous(breaks = seq(0, 20, 2)) +
    theme_bw() +
    theme(text = element_text(size=28))
    # theme_classic()
    # scale_color_manual(brewer.pal(9, "PuOr")[5])

ggsave("img/figure1_color.png", plot = last_plot(), device = NULL, path = NULL,
       scale = 1, width = 12, height = 8, units = "in",
       dpi = 300, limitsize = TRUE)

#Figure 2
df2 <- read.csv("fig2.csv", stringsAsFactors = F)
df2 <- melt(df2, id.vars = "Study")
names(df2) <- c("study", "race", "pct")
df2$race <- factor(df2$race)
levels(df2$race) <- c("White/Caucasian", "Black/African",
                     "Hispanic/Latino", "Asian", "Other")
df2$study <- factor(df2$study, levels = df2$study[12:1])

ggplot(data = df2) +
    geom_bar(aes(x = study, y = pct, fill = race), stat = "identity") +
    # scale_fill_brewer(palette = "Set2") +
    scale_fill_manual(values = brewer.pal(5, "Set1")[c(2,1,3,4,5)],
    # scale_fill_manual(values = gray.colors(6, start = 0.1)[c(6,1,5,2,4)],
    # scale_fill_manual(values = gray.colors(5, start = 0.1)[c(5,2,4,1,3)],
                      name = "") +
    # scale_fill_grey(name = "Ethnic Groups") +
    scale_y_continuous(labels=percent, limits = c(0, 1)) +
    ylab("Percentage") +
    theme_bw() +
    theme(text = element_text(size=20)) +
    theme(legend.position="top") +
    coord_flip() +
    theme(axis.title.y = element_blank())

ggsave("img/figure2_color.png", plot = last_plot(), device = NULL, path = NULL,
       scale = 1, width = 12, height = 7, units = "in",
       dpi = 300, limitsize = TRUE)
