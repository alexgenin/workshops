hourly_delay <- flights %>% 
 filter(!is.na(dep_delay)) %>% 
 group_by(date, hour) %>% 
 summarise( 
   delay = mean(dep_delay), 
   n = n() ) %>% 
 filter(n > 10)