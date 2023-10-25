install.packages("Lahman")
library(Lahman)
library(dplyr)
library(ggplot2)
library(gt)

data("Batting")
batting_2022 <- subset(Batting, yearID == 2022) 

batting_2022 <- battingStats(batting_2022)
batting_2022 <- batting_2022 %>%
  group_by(playerID) %>%
  summarize(across(-c(,1:5), sum), .groups = "keep")
batting_2022 <- batting_2022 %>%
  mutate(wOBA = (0.689 * (BB-IBB) + 0.720 * HBP + 0.884 * (H - X2B - X3B - HR) + 1.261 * X2B + 1.601 * X3B + 2.072 * HR)/(AB + BB - IBB + SF + HBP)) 

batting_2022 <- subset(batting_2022, PA >= 502)
data("People")

people <- People %>%
  mutate(player = paste0(nameFirst, " ", nameLast)) %>%
  select(playerID, player)

batting_2022 <- inner_join(batting_2022, people, by = "playerID")

data("Appearances")

app <- Appearances %>%
  filter(yearID == 2022) %>%
  group_by(playerID) %>%
  summarize(across(-c(,1:4), sum), .groups = "keep")

pos_app <- app[,5:17] 

pos_app <- subset(pos_app, select = -G_of)

max_columns <- apply(pos_app, 1, function(row) {
  colnames(pos_app)[which.max(row)]
})

app$PrimaryPOS <- max_columns

app$PrimaryPOS <- toupper(gsub("^G_", "", app$PrimaryPOS))

app <- app %>%
  select(playerID, PrimaryPOS)

batting_2022 <- inner_join(batting_2022, app, by = "playerID")

avg_batting_2022 <- batting_2022 %>%
  group_by(PrimaryPOS) %>%
  summarize(mean_wOBA = mean(wOBA), num = n())

violin_plot <- ggplot(batting_2022, aes(x = PrimaryPOS, y = wOBA, fill = PrimaryPOS)) + 
  geom_violin() +   
  scale_fill_brewer(palette = "Set1") +  
  theme_minimal() +
  labs(x = "Primary Position", y = "wOBA", title = "Analyzing wOBA For Qualified Hitters Based On Their Primary Position", subtitle = "2022 Season - 129 Qualified Hitters (Violin Plot)") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5), plot.subtitle = element_text(face = "italic", hjust = 0.5), legend.position = "none")
ggsave("violin_plot.png", plot = violin_plot, width = 8, height = 6, units = "in", dpi = 300, bg = "white")

density_plot <- ggplot(batting_2022, aes(x = wOBA, group = PrimaryPOS, fill = PrimaryPOS)) + 
  geom_density(size = 1, linetype = "solid") +   
  scale_fill_brewer(palette = "Set1") +  
  theme_minimal() +
  labs(x = "wOBA", y = "Density", title = "Analyzing wOBA For Qualified Hitters Based On Their Primary Position", subtitle = "2022 Season - 129 Qualified Hitters (Density Plot)", fill = "Primary Position") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5), plot.subtitle = element_text(face = "italic", hjust = 0.5))
ggsave("density_plot.png", plot = density_plot, width = 8, height = 6, units = "in", dpi = 300, bg = "white")

table <- avg_batting_2022 %>% arrange(-mean_wOBA) %>% gt() %>% 
  cols_align(
    align = "center",
    columns = c(PrimaryPOS, mean_wOBA, num)
  ) %>%
  data_color(
    columns = mean_wOBA,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    PrimaryPOS = md("**Primary Position**"),
    mean_wOBA = md("**Average wOBA Across Qualified Batters**"),
    num = md("**Number of Qualiifed Hitters**")
  ) %>%
  tab_header(
    title = md("**Analyzing wOBA For Qualified Hitters Based On Their Primary Position**"),
    subtitle = "2022 Season - 129 Qualified Hitters (Average Table)"
  )
gtsave(table, "table.png")
