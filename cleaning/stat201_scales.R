# Get just prejudice items
prejudice_df <- dat %>% 
  select(starts_with("prejudice")) %>%
  na.omit()

# Run the PCA
prejudice_pca <- prcomp(df_prejudice, center = TRUE, scale = TRUE)

# Summary
summary(prejudice_pca)
screeplot(prejudice_pca, type = "lines")

# Long format loadings
prejudice_pca$rotation %>%
  as_tibble(rownames = "item") %>%
  pivot_longer(-item, names_to = "component", values_to = "loading") %>%
  ggplot(aes(x=item, y=loading)) +
  geom_line(group=1) +
  facet_wrap(~component)
