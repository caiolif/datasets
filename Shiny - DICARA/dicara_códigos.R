#-----------------------------------
# Formulário de aplicação familiar
#-----------------------------------

base1 <- read.table(text = getURL('https://raw.githubusercontent.com/caiolif/datasets/master/dados_aplicacaofamiliar.txt', 
                                  .encoding = 'UTF-8'),
                    header = TRUE, sep = "\t")



# plot municípios
base1 %>% 
  as_data_frame() %>% 
  select('Município') %>% 
  group_by(Município) %>% 
  count() %>%
  ungroup() %>% 
  mutate(prop = round(n*100/sum(n), 0), y.breaks = cumsum(prop) - prop/2) %>% 
  ggplot(aes(x = Município, y = prop)) +
  geom_bar(stat = 'identity', fill = 'darkgreen') + 
  geom_text(aes(label = paste0('(', n, ')')), colour = 'white',
            position = position_stack(vjust = 0.5)) +
  labs(x = '', y = 'Nº de entrevistados (%)') +
  theme_minimal(base_size = 14) 


# plot regional
base1 %>% 
  as_data_frame() %>% 
  select('Município', 'Regional') %>%
  group_by(Município) %>% 
  count(Regional) %>%
  ungroup() %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = Município, y = prop, fill = Regional, group = Regional)) +
  geom_bar(stat = 'identity') +
  scale_fill_d3(name = 'Região:') + 
  geom_text(aes(label = paste0(n)),
            position = position_stack(vjust = 0.5), size = 4.5) +
  scale_y_discrete(limits = seq(0, 35, by = 5)) + 
  labs(y = 'Região (%)') + 
  theme_minimal(base_size = 14) + 
  theme(legend.position = 'top')


# plot UC
base1 %>% 
  as_data_frame() %>% 
  select('UC') %>% 
  group_by(UC) %>% 
  count() %>%
  ungroup() %>%
  mutate(prop = round(n*100/sum(n), 0), y.breaks = cumsum(prop) - prop/2) %>% 
  ggplot(aes(x = UC, y = prop)) +
  geom_bar(fill = 'darkgreen', stat = 'identity') + 
  geom_text(aes(label = paste0(n)), colour = 'white',
            position = position_stack(vjust = 0.5), size = 4.5) +
  labs(x = "", y = "UC (%)") + 
  theme_minimal(base_size = 14) +
  coord_flip()


# plot Local de Aplicação
base1 %>% as_data_frame() %>% 
  select('Município', 'Local.de.aplicação') %>% 
  group_by(Município) %>% 
  count(Local.de.aplicação) %>%
  mutate(prop = n*100/sum(n)) %>% 
  ungroup() %>% 
  ggplot(aes(x = Município, y = prop, fill = Local.de.aplicação, group = Local.de.aplicação)) +
  geom_bar(stat = 'identity') + 
  geom_text(aes(label = paste0('(', n, ')')), color = 'white',
            position = position_stack(vjust = 0.5), size = 4.5) +
  labs(y = 'Local de Aplicação (%)') +
  scale_fill_d3(name = 'Local de Aplicação:') +
  theme_minimal()


# plot nº de moradores na mesma família
base1 %>% as_data_frame() %>% 
  select('Município', 'No..de.moradores.na.casa..mesma.família.') %>% 
  filter(No..de.moradores.na.casa..mesma.família. != 196) %>% 
  group_by(Município) %>% 
  count(No..de.moradores.na.casa..mesma.família.) %>% 
  ggplot(aes(x = No..de.moradores.na.casa..mesma.família., y = n, color = Município)) +
  geom_line(size = 1.1) + geom_point(size = 2) + 
  scale_color_d3(name = 'Município:') +
  scale_x_discrete(limits = seq(0, 30, by = 3)) +
  labs(y = 'n', x = 'Nº de moradores na casa') +
  theme_minimal() + theme(legend.position = 'top')
  
  
base1 %>% as_data_frame() %>% 
  select('Município', 'No..de.moradores.na.casa..mesma.família.') %>% 
  filter(No..de.moradores.na.casa..mesma.família. != 196) %>% 
  group_by(Município) %>% 
  count(No..de.moradores.na.casa..mesma.família.) %>% 
  ggplot(aes(x = No..de.moradores.na.casa..mesma.família., y = n, fill = Município)) +
  geom_bar(stat = 'identity') +
  theme_minimal(base_size = 14) + theme(legend.position = 'top')
  
  
