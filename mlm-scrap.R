dfmlm <- dfch_lc %>% 
  pivot_longer(., cols = c(prac2_1_sc, arctic20_ch, arctic10_ch, temp20_ch,
                           temp10_ch, ozone20_ch, ozone10_ch, airqual20_ch,
                           airqual10_ch, diatho20_ch, diatho10_ch, co220_ch,
                           co210_ch, icesheets20_ch, icesheets10_ch, 
                           quake2_1_sc),
               names_to = "item",
               values_to = "response",
               values_drop_na = T)



dfmlm <- dfmlm %>% 
  select(id, con, conservatism_f, item, response, sc_scored, sc_scoredz, 
         rav_scored, rav_scoredz, aot_scored, aot_zscored, nfc_scored,
         nfc_zscored, vl_scored, vl_zscored, contains("10_ch")) %>% 
  mutate(item_slope = factor(case_when(
    item == "arctic2_1_sc" | item == "ozone2_1_sc" | item == "ice_sheets2_1_sc" |
      item == "air_qual2_1_sc" | item == "arctic1_1_sc" | 
      item == "ozone1_1_sc" | item == "ice_sheets1_1_sc" | 
      item == "air_qual1_1_sc" ~ "neg",
    item == "temp2_1_sc" | item == "dia_tho2_1_sc" | item == "co22_1_sc" |
      item == "temp1_1_sc" | item == "dia_tho1_1_sc" | 
      item == "co21_1_sc" ~ "pos",
    item == "prac2_1_sc" | item == "quake2_1_sc" | item == "prac1_1_sc" | 
      item == "quake1_1_sc" ~ "zero"
  )),
  item_ambiguous = factor(case_when(
    item == "arctic2_1_sc" | item == "temp2_1_sc" | item == "ozone2_1_sc" | 
      item == "air_qual2_1_sc" | item == "dia_tho2_1_sc" |
      item == "arctic1_1_sc" | item == "temp1_1_sc" | item == "ozone1_1_sc" | 
      item == "air_qual1_1_sc" | item == "dia_tho1_1_sc" ~ "1",
    item == "quake2_1_sc" | item == "co22_1_sc" | item == "ice_sheets2_1_sc" |
      item == "prac2_1_sc" | item == "quake1_1_sc" | item == "co21_1_sc" |
      item == "ice_sheets1_1_sc" | item == "prac1_1_sc" ~ "0" 
  )),
  item_ideo = factor(case_when(
    item == "arctic2_1_sc" | item == "temp2_1_sc" | item == "co22_1_sc" | 
      item == "ice_sheets2_1_sc" | item == "arctic1_1_sc" | 
      item == "temp1_1_sc" | item == "co21_1_sc" | 
      item == "ice_sheets1_1_sc"~ "con",
    item == "ozone2_1_sc" | item == "air_qual2_1_sc" | item == "ozone1_1_sc" 
    | item == "air_qual1_1_sc" ~ "lib",
    item == "dia_tho2_1_sc" | item == "quake2_1_sc" | item == "prac2_1_sc" |
      item == "dia_tho1_1_sc" | item == "quake1_1_sc" | 
      item == "prac1_1_sc"~ "filler" 
  )),
  item_political = factor(case_when(
    item == "arctic2_1_sc" | item == "temp2_1_sc" | item == "co22_1_sc" | 
      item == "ice_sheets2_1_sc" | item == "arctic1_1_sc" | 
      item == "temp1_1_sc" | item == "co21_1_sc" | 
      item == "ice_sheets1_1_sc" | item == "ozone2_1_sc" | item ==
      "air_qual2_1_sc" | item == "ozone1_1_sc" | 
      item == "air_qual1_1_sc" ~ "1",
    item == "dia_tho2_1_sc" | item == "quake2_1_sc" | item == "prac2_1_sc" |
      item == "dia_tho1_1_sc" | item == "quake1_1_sc" | 
      item == "prac1_1_sc" ~ "0" 
  )),
  item = factor(item),
  response = as.numeric(response),
  id = factor(id),
  conservatism_f = as.numeric(conservatism_f),
  sc_zscored = as.numeric(sc_scoredz),
  rav_zscored = as.numeric(rav_scoredz),
  aot_zscored = as.numeric(aot_zscored),
  nfc_zscored = as.numeric(nfc_zscored),
  vl_zscored = as.numeric(vl_zscored)
  )