# weights as XTS
w <- all_weights %>% 
  filter(method == "factor1") %>% 
  select(-method) %>% 
  pivot_wider(names_from = name, values_from = value)

w_xts <- xts(w[,-1], order.by = w$date)

# returns as XTS

r <- ff100_data$monthly %>% 
  filter(Date >= "1973-06-01" & Date <= "2017-12-01")

r_xts <- xts(r[,-1], r$Date)

out <- Return.portfolio(R = r_xts, weights = w_xts, verbose = TRUE)

beginWeights <- out$BOP.Weight
endWeights <- out$EOP.Weight
txns <- beginWeights - lag(endWeights)
monthlyTO <- xts(rowSums(abs(txns)), order.by=index(txns))
plot(monthlyTO)
yearlyTO <- runSum(monthlyTO, 2)
plot(yearlyTO, main = "running one year turnover")

test_GE <- all_weights %>% 
  filter(method == "sample") %>% 
  group_by(date) %>% 
  dplyr::summarise(SumAbsWeights = sum(abs(value))) %>% 
  ungroup %>% 
  dplyr::summarise(mean = mean(SumAbsWeights))

test_TO <- all_weights %>% 
  # filter(method == "sample") %>% 
  dplyr::group_by(method,name) %>% 
  dplyr::summarise(date = all_weights$date %>% unique %>% .[-1], 
                   diff = diff(value)) %>% 
  ungroup %>% 
  group_by(method, date) %>% 
  dplyr::summarise(sum = sum(abs(diff))) %>% 
  ungroup %>% 
  group_by(method) %>% 
  dplyr::summarise(mean = mean(sum)) %>% 
  ungroup %>% 
  suppressWarnings()

test_TO$sum %>% mean   
