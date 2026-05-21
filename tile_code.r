library(tidyverse)
df <- readxl::read_xlsx("КАВКАЗ_ГУСЕНИЦА_version23.xlsx")

df |>
  filter(meaning_translation != "НЕИЗВЕСТНО") |>
  select(language, meaning_translation) |>
  mutate(feature = if_else(meaning_translation == "непроизводное", "непроизводное", "производное")) |>
  distinct(language, feature) |>
  pivot_wider(names_from = feature, values_from = feature) |>
  mutate(across(everything(), function(x) ifelse(is.na(x), "", x))) |>
  mutate(feature = str_c(производное, " и ", непроизводное),
         feature = str_remove(feature, " и $"),
         feature = str_remove(feature, "^ и "),
         feature = factor(feature, levels = c("производное", "производное и непроизводное",
                                             "непроизводное"))) |>
  RCaucTile::ec_tile_map() ->
  plot

ggsave(plot = plot, "value1.png", bg = "white", width = 12, height = 9)


df |>
  filter(meaning_translation != "НЕИЗВЕСТНО") |>
  select(language, meaning_translation, `value2 (snake/worm)`) |>
  rename(feature = `value2 (snake/worm)`) |>
  mutate(feature = case_when(feature == "yes" ~ "змея/червь",
                             meaning_translation == "непроизводное" ~ "непроизводное",
                             TRUE ~ "другое")) |>
  distinct(language, feature) |>
  pivot_wider(names_from = feature, values_from = feature) |>
  mutate(feature = case_when(!is.na(`змея/червь`) ~ "змея/червь",
                             is.na(`змея/червь`) & !is.na(другое) ~ "другое",
                             is.na(`змея/червь`) & is.na(другое) ~ "непроизводное"),
         feature = factor(feature, levels = c("змея/червь", "другое", "непроизводное"))) |>
  RCaucTile::ec_tile_map() ->
  plot

ggsave(plot = plot, "value2.png", bg = "white", width = 12, height = 9)

df |>
  distinct(language, idiom)
  filter(meaning_translation != "НЕИЗВЕСТНО") |>
  select(language, meaning_translation, `value3 (god)`) |>
  rename(feature = `value3 (god)`) |>
  mutate(feature = case_when(feature == "yes" ~ "Аллах",
                             meaning_translation == "непроизводное" ~ "непроизводное",
                             TRUE ~ "другое")) |>
  distinct(language, feature) |>
  pivot_wider(names_from = feature, values_from = feature) |>
  mutate(feature = case_when(!is.na(Аллах) ~ "Аллах",
                             is.na(Аллах) & !is.na(другое) ~ "другое",
                             is.na(Аллах) & is.na(другое) ~ "непроизводное"),
         feature = factor(feature, levels = c("Аллах", "другое", "непроизводное"))) |>
  RCaucTile::ec_tile_map() ->
  plot

ggsave(plot = plot, "value3.png", bg = "white", width = 12, height = 9)