# Setup ----
library(dplyr)
library(ggplot2)

# Data importation ----
meetings <- read.csv2(file = "D:\\IRD\\5-Projets_&_themes\\1-RECOLAPE\\5-Git\\WP5\\T5.4\\data_wp5_t5.4.csv",
                      stringsAsFactors = FALSE)

# Data selection ----
meetings <- filter(.data = meetings, data_expected == TRUE, year >= 2015) %>%
  group_by(year, month, structur) %>%
  summarise(number_meeting = n()) %>%
  ungroup() %>%
  mutate(month =  as.character(month),
         month = ifelse(nchar(month) == 1,
                        paste0("0", month),
                        month),
         year_month = paste(year, month, sep = "/")) %>%
  arrange(year, month, structur)

meetings_final <- as.data.frame(matrix(data = NA,
                                       nrow = length(unique(meetings$year)) * 12,
                                       ncol = 3,
                                       dimnames = list(seq(from = 1,
                                                           to = length(unique(meetings$year)) * 12),
                                                       c("year","month", "year_month")))) %>%
  mutate(year = rep(unique(meetings$year), each = 12),
         month = as.character(rep(1:12, length(unique(meetings$year)))),
         month = ifelse(nchar(month) == 1,
                        paste0("0", month),
                        month),
         year_month = paste(year, month, sep = "/")) %>%
  left_join(meetings) %>%
  mutate(number_meeting = ifelse(is.na(number_meeting),
                                 0,
                                 number_meeting))

# Data plot ----
meetings_plot <- ggplot(data = meetings_final,
                        aes(x = year_month, y = number_meeting, fill = structur)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(drop = FALSE) +
  scale_fill_discrete(drop = FALSE,
                      limits=c("ICCAT", "ICCAT/IOTC", "WCPFC", "IOTC", "IATTC/AIDCP", "EU")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Number of meetings where data were possibly necessary by month, year and structure/organization.",
       x = "Year/month",
       y = "Number of meetings",
       fill = "Structure/organization")

# Plot export ----
ggsave(filename = "plot_wp5_t5.4.png",
       plot = meetings_plot,
       dpi = 300,
       width = 25,
       height = 15,
       units = "cm")
