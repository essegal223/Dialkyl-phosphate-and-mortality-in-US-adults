
# Compute min and max for each SDDSRVYR and Quartile
quartile_summary <- analysis_URXOP6_crt.imputed.cont$variables %>%
  group_by(SDDSRVYR, quartile) %>%
  summarise(
    Min_URXOP6_crt    = min(URXOP6_crt, na.rm = TRUE),
    Median_URXOP6_crt = quantile(URXOP6_crt, 0.50, na.rm = TRUE),
    Max_URXOP6_crt    = max(URXOP6_crt, na.rm = TRUE),
    .groups = "drop"
  )
View(quartile_summary)

#Check to see if quartile boundaries overlap
quartile_summary |> group_by(SDDSRVYR) |> 
  summarise(Overlapping_boundaries=sum(Max_URXOP6_crt==lead(Min_URXOP6_crt), na.rm = TRUE))
  
  
quartile_summary_wide= quartile_summary |> mutate(
    median_min_max = paste0(Median_URXOP6_crt, " (", Min_URXOP6_crt, ", ", Max_URXOP6_crt, ")")
  ) %>%
  select(SDDSRVYR, quartile, median_min_max) %>%
  pivot_wider(
    names_from = quartile,
    values_from = median_min_max,
    names_prefix = "Quartile_"
  )

  
# View result
View(quartile_summary_wide)



