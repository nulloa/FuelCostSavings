carinfo %>%
  merge(., data.frame(miles = seq(0, 100000, by=1000))) %>%
  mutate(gas_price = 4.50) %>%
  mutate(estGasCost = (miles/awd_mpg_comb)*gas_price) %>%
  mutate(vehicle = paste(make, model, trim)) %>%
  filter(trim %in% c("EX-L", "Hybrid XLE")) %>%
  ggplot(., aes(x=miles, y=estGasCost, color=vehicle)) +
    geom_point() + 
    theme_bw()

pricediff = unique(carinfo$price[carinfo$trim=="Hybrid XLE"] - carinfo$price[carinfo$trim=="EX-L"])

carinfo %>%
  merge(., data.frame(miles = seq(0, 100000, by=1000))) %>%
  mutate(gas_price = 4.50) %>%
  mutate(estGasCost = (miles/awd_mpg_comb)*gas_price) %>%
  mutate(vehicle = paste(make, model, trim)) %>%
  filter(trim %in% c("EX-L", "Hybrid XLE")) %>%
  select(miles, vehicle, estGasCost) %>%
  pivot_wider(names_from=vehicle, values_from=estGasCost) %>%
  mutate(gasSavings = `Honda Pilot EX-L` - `Toyota Grand Highlander Hybrid Hybrid XLE`) %>%
  ggplot(., aes(x=miles, y=gasSavings)) +
    geom_point() + 
    theme_bw() + 
    geom_hline(yintercept = pricediff, color="red")



carinfo %>%
  merge(., data.frame(miles = seq(0, 100000, by=1000))) %>%
  mutate(gas_price = 4.50) %>%
  mutate(estGasCost = (miles/awd_mpg_comb)*gas_price) %>%
  mutate(vehicle = paste(make, model, trim)) %>%
  filter(trim %in% c("EX-L", "Hybrid XLE")) %>%
  select(miles, vehicle, estGasCost) %>%
  pivot_wider(names_from=vehicle, values_from=estGasCost) %>%
  mutate(gasSavings = `Honda Pilot EX-L` - `Toyota Grand Highlander Hybrid Hybrid XLE`) %>%
  filter(gasSavings > 3823)


