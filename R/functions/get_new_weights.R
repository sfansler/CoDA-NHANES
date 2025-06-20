get_new_weights = function(demo) {
  
svy_weights <- demo %>%
    select(SEQN, SDDSRVYR, WTMEC2YR)

new_weights = demo %>%
  group_by(SDDSRVYR) %>%
  mutate(probs = 1 / WTMEC2YR,
         probs_norm = probs / sum(probs),
         weights_norm = 1 / probs_norm) %>%
  ungroup() %>%
  mutate(new_weights = weights_norm / 2) %>%
  select(-probs, -probs_norm, -weights_norm)

return(new_weights)
}
