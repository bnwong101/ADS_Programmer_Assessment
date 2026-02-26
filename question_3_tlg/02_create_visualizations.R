#ADS Programmer Coding Assessment
#Question 3: Visualizations - Brandon Wong

#Install packages
#install.packages(c("admiral", "sdtm.oak", "gt", "ggplot2", "pharmaverseraw", "pharmaversesdtm", "pharmaverseadam", "gtsummary", "purrr"))

#Read in data
library(ggplot2)
library(dplyr)
library(purrr)

#Input datasets
adae <- pharmaverseadam::adae

View(adae)

#Visualizations Plot 1
#Summary of AE severity counts
sev_summary <- adae %>%
  dplyr::count(ACTARM, AESEV)

#AE severity distribtuion by treatment
p1 <- ggplot(sev_summary,
             aes(x = ACTARM, y = n, fill = AESEV)) +
  geom_col(position = "stack") +
  labs(
    title = "AE severity distribution by treatment",
    x = "Treatment Arm",
    y = "Counts of AEs",
    fill = "Severity/Intensity"
  ) +
  theme_minimal()

#Saving plot
ggsave("ae_severity_distribution.png", p1, width = 8, height = 6)


#Visualizations Plot 2

#Filter by subjects who experienced an AE
N_ae_pop <- n_distinct(adae$USUBJID)

ae_summary <- adae %>%
  group_by(AETERM) %>%
  summarise(n_subj = n_distinct(USUBJID)) %>%
  # Filter for 10 most frequent AE's
  slice_max(order_by = n_subj, n = 10, with_ties = FALSE) %>%
  # Calculate Clopper-Pearson CI's
  rowwise() %>%
  mutate(
    rate = n_subj / N_ae_pop,
    ci_low = binom.test(n_subj, N_ae_pop)$conf.int[1],
    ci_up  = binom.test(n_subj, N_ae_pop)$conf.int[2],
    AETERM = toupper(AETERM)
  ) %>%
  ungroup()

#Forest plot
p2 <- ggplot(ae_summary, aes(x = reorder(AETERM, rate), y = rate)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_up), width = 0.3, color = "black") +
  geom_point(size = 3, color = "black") +
  coord_flip() +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    breaks = seq(0, 0.3, 0.1),
    limits = c(0, 0.35)
  ) +
  labs(
    title = "Top 10 Most Frequent Adverse Events",
    subtitle = paste0("n = ", N_ae_pop, " subjects; 95% Clopper-Pearson CIs"),
    x = NULL,
    y = "Percentage of Patients (%)"
  ) +
  theme_bw() + # Matches the grey grid background
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black", size = 10)
  )

ggsave("ae_frequency_top10.png", p2, width = 8, height = 6)