standings_with_epa2 <- standings_with_epa %>%
  left_join(teams_colors_logos, by = c("team" = "team_abbr"))

stafford_teams <- standings_with_epa2 %>%
  filter(team == "DET") %>%
  filter(season >= 2011)

standings_with_epa2 %>%
  ggplot(aes(x = total_help, y = total_wins)) +
  geom_jitter(aes(y = total_wins, fill = team_color), size = 3, 
              width = 0.005, show.legend = FALSE, alpha = 0.3) +
  theme_bw() +
  geom_hline(yintercept = mean(standings_with_epa2$total_wins), color = "orange", linetype = "dashed", alpha=0.8) +
  geom_vline(xintercept =  mean(standings_with_epa2$total_help), color = "orange", linetype = "dashed", alpha=0.8) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  geom_image(aes(image = team_logo_espn, x = total_help, y = total_wins),
             size = 0.05, asp = 16/9, data = stafford_teams) +
  labs(x = "Total Help = EPA/Rush + Def. EPA/Play",
       y = "Team Wins",
       title = "Matthew Stafford Rarely Had Help",
       subtitle = "All teams since 2006, Lions logo is for teams Stafford was on",
       caption = "By Tej Seth | @mfbanalytics") +
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 12),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    legend.position = "none"
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
ggsave("give-staff-help.png", height = 10, width = 16, dpi = "retina")