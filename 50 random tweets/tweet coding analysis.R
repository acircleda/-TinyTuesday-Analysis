library(tidyverse)
library(readxl)

tweets <- read_excel("50 random tweets/tweet findings.xlsx", sheet="Sheet2")

tweets %>%
  ggplot()+
  geom_bar(aes(x=1, y=pct, fill=reorder(category, pct)), stat="identity", width=.5)+
  geom_text(aes(1, y=pct, group=reorder(category, pct), label=
                  paste0(str_to_title(category),
                         " - ",
                         round(pct*100, digits=0),
                         "%")), position = position_stack(vjust=.5), size=6)+
  theme_void()+
  scale_fill_brewer(palette="Blues")+
  theme(legend.position = "none",
        plot.title = element_text(size=20))+
  ggtitle(label="Major Themes in Tweets")

retweets <- read_excel("50 random tweets/tweet findings.xlsx", sheet="Sheet4")
            

retweets %>%
  ggplot()+
  geom_bar(aes(x=1, y=pct, fill=reorder(category, pct)), stat="identity", width=.5)+
  geom_text(aes(1, y=pct, group=reorder(category, pct), label=
                  paste0(str_to_title(category),
                         " - ",
                         round(pct*100, digits=0),
                         "%")), position = position_stack(vjust=.5), size=6)+
  theme_void()+
  scale_fill_brewer(palette="Blues")+
  theme(legend.position = "none",
        plot.title = element_text(size=20))+
  ggtitle(label="Major Themes in Retweets")
