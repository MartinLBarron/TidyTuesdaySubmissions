library(tidyverse)
library(gghighlight)
library(ggthemes)


# Get the Data
game_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv')

#Clean up age
df <- game_goals %>%
  mutate(years=as.numeric(substr(age,1,2)),
         days=as.numeric(substr(age,4,7)),
         ageInDays=(years*365)+days,
         age=ageInDays/365,) %>%
  select(player, date,goals, age)

# Calculate cumulative goals
df <-df %>%
  group_by(player) %>%
  mutate(cumGoals=cumsum(goals))

#limit data to top 10 players
top10 <- df %>%
  filter(row_number()==n()) %>%
  ungroup() %>%
  arrange(desc(cumGoals)) %>%
  top_n(10,cumGoals) %>%
  select(player) %>%
  inner_join(.,df, by="player")
unique(df$player)
Ovechkin <- filter(top10, player=="Alex Ovechkin")

OvechkinPoint <- filter(Ovechkin, row_number()==n()) %>%
mutate(label = paste0(player, "\n", cumGoals, " goals")) 
top4 <- df %>%
  filter(row_number()==n()) %>%
  ungroup() %>%
  filter(player %in% c("Wayne Gretzky", "Jaromir Jagr")) %>%
  mutate(label = paste0(player, "\n", cumGoals, " goals")) %>%
  select(player, label, age, cumGoals)
  

ggplot(top10) +
  geom_hline(yintercept=700, linetype="dashed", col="gray10")+
  geom_line(aes(x=age, y=cumGoals, group=player), col="light gray")+
  geom_line(data=Ovechkin, aes(x=age, y=cumGoals), col="maroon")+
  geom_point(data=top4, aes(x=age, y=cumGoals), col="gray")+
  geom_point(data=OvechkinPoint, aes(x=age, y=cumGoals), col="maroon")+
  geom_text(data=OvechkinPoint, aes(x=age, y=cumGoals, label=label), col="maroon", hjust=1, vjust=0)+
  geom_text(data=top4, aes(x=age, y=cumGoals, label=label), col="gray40",hjust=0)+
  scale_x_continuous(limits=c(20,50), breaks=seq(20,50, by=5), labels = c("Age 20","25","30","35","40","45","50"))+
  scale_y_continuous(limits=c(0,1000), breaks=c(300,600,900), labels = c("300", "600", "900 career goals"))+
  xlab("")+
  ylab("")+
  labs(caption = "data: https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv\ncode: https://github.com/MartinLBarron/TidyTuesdaySubmissions/tree/master/Week10")

  theme_minimal()+
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.line.x = element_line(color="black", size = .5),
        axis.ticks.x=element_line(),
        axis.ticks.length.x = unit(3, "pt"))

ggsave("Week10/output/output.png")
       