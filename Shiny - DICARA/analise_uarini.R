#-------------------------
# Contagem das comunidades
#-------------------------

base1 %>% 
  as_data_frame() %>% 
  filter(Município == 'Uarini') %>% 
  count(Comunidade) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = reorder(Comunidade, n), y = prop)) +
  geom_bar(stat = 'identity', fill = 'forestgreen') + 
  coord_flip() +
  geom_text(aes(label = paste0(n)), colour = 'white',
            position = position_stack(vjust = 0.5),
            size = 4.5) +
  labs(x = 'Comunidade', y = '(Nº de famílias entrevistadas) %') +
  scale_y_discrete(limits = seq(0, 18, 3)) +
  theme_minimal(base_size = 13)


#-------------------------
# Nº de pessoas na família
#-------------------------
base1 %>% 
  as_data_frame() %>% 
  filter(Município == 'Uarini') %>% 
  select(Nº.de.moradores.na.casa..mesma.família.) %>%
  group_by(Nº.de.moradores.na.casa..mesma.família.) %>% 
  count() %>% 
  ggplot(aes(x = Nº.de.moradores.na.casa..mesma.família., y = n)) +
  geom_point(colour = 'forestgreen') + 
  geom_line(colour = 'forestgreen', size = 1) +
  labs(x = 'Nº de familiares na casa', y = 'Frequência') +
  scale_x_discrete(limits = seq(2, 18, 2)) +
  scale_y_discrete(limits = seq(0, 45, 5)) +
  theme_minimal(base_size = 13) 

#-------------------------
# crianças/adol na cidade
#-------------------------
base1 %>% 
  as_data_frame() %>% 
  filter(Município == 'Uarini') %>% 
  select(Comunidade, Filhos..crianças.adolescentes..morando.na.cidade.) %>% 
  group_by(Comunidade) %>% 
  count(Filhos..crianças.adolescentes..morando.na.cidade.) %>% 
  mutate(prop = n*100/sum(n)) %>%
  ggplot(aes(x = Comunidade, y = prop, fill = Filhos..crianças.adolescentes..morando.na.cidade.)) +
  geom_bar(stat = 'identity') +
  coord_flip() + 
  labs(y = '(Nº de famílias) %') +
  geom_text(aes(label = paste0(n)), 
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = 'Greens', 
                    name = 'Crianças/Adolescentes morando na cidade:') +
  theme_minimal()  +
  theme(legend.position = 'top')


base1 %>% 
  as_data_frame() %>% 
  filter(Município == 'Uarini', Filhos..crianças.adolescentes..morando.na.cidade. == 'Sim') %>% 
  select(Comunidade, Quantos.filhos.) %>% 
  group_by(Comunidade) %>% 
  ggplot(aes(x = Comunidade, y = Quantos.filhos.)) +
  #geom_point(size = 2, alpha = 0.4) +
  geom_boxplot(fill = 'forestgreen') +
  labs(y = 'Quantidade de filhos') +
  coord_flip() +
  theme_minimal() 
  
  
#---------------------------
# Forma de renda
#---------------------------

base1 %>% 
  as_data_frame() %>% 
  filter(Município == 'Uarini') %>% 
  select(Pesca, Agricultura, Funcionário.Público, Comércio, Extrativismo, Outra.forma.de.renda) %>% 
  melt() %>% 
  group_by(variable) %>% 
  filter(value == 1) %>% 
  count(value) %>% 
  mutate(prop = n*100/212) %>% 
  ggplot(aes(x = variable, y = prop)) +
  geom_bar(stat = 'identity', fill = 'forestgreen') +
  geom_text(aes(label = paste0(n)), color = 'white',
            position = position_stack(vjust = 0.5), size = 5) +
  labs(x = 'Forma de Renda', y = 'Frequência (%)') +
  scale_x_discrete(labels = c('Pesca', 'Agricultura', 'Func. Público', 'Comércio', 'Outros')) +
  theme_minimal(base_size = 14)
  

base1 %>% 
  as_data_frame() %>% 
  filter(Município == 'Uarini') %>% 
  select(Comunidade, Pesca, Agricultura, Funcionário.Público, Comércio, Extrativismo) %>% 
  melt() %>% 
  group_by(Comunidade, variable) %>% 
  count(value) %>% 
  ungroup() %>% 
  group_by(Comunidade) %>% 
  filter(value == 1) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = Comunidade, y = prop, fill = variable)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = paste0(n)), colour = 'white',
            position = position_stack(vjust = 0.5)) +
  labs(y = 'Frequência (%)') +
  scale_fill_jama(name = 'Forma de Renda:') +
  coord_flip() +
  theme_minimal(base_size = 12) + theme(legend.position = 'top')


#---------------------------
#  Renda mensal
#---------------------------
base1 %>% 
  as_data_frame() %>% 
  filter(Município == 'Uarini') %>% 
  select(Comunidade, Renda..R...média.mensal.da.família) %>% 
  ggplot(aes(x = Comunidade, y = Renda..R...média.mensal.da.família)) +
  geom_boxplot(fill = 'forestgreen') + 
  labs(y = 'Renda mensal (R$)') +
  coord_flip() +
  theme_minimal()

#---------------------------
#  Benefícios
#---------------------------

base1 %>% 
  as_data_frame() %>% 
  filter(Município == 'Uarini') %>% 
  select(Bolsa.Família, Bolsa.Floresta, Aposentadoria, Outros.benefícios) %>% 
  melt() %>% 
  group_by(variable) %>% 
  filter(value == 1) %>% 
  count(value) %>% 
  mutate(prop = n*100/212) %>% 
  ggplot(aes(x = variable, y = prop)) +
  geom_bar(stat = 'identity', fill = 'forestgreen') +
  geom_text(aes(label = paste0(n)), color = 'white',
            position = position_stack(vjust = 0.5), size = 5) +
  labs(x = 'Tipo de Benefício', y = 'Frequência (%)') +
  scale_x_discrete(labels = c('Bolsa Família', 'Bolsa Floresta', 'Aposentadoria')) +
  theme_minimal(base_size = 14)

  
base1 %>% 
  as_data_frame() %>% 
  filter(Município == 'Uarini') %>% 
  select(Comunidade, Bolsa.Família, Bolsa.Floresta, Aposentadoria, Outros.benefícios) %>% 
  melt() %>% 
  group_by(Comunidade, variable) %>% 
  count(value) %>% 
  ungroup() %>% 
  group_by(Comunidade) %>% 
  filter(value == 1) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = Comunidade, y = prop, fill = variable)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = paste0(n)), colour = 'white',
            position = position_stack(vjust = 0.5)) +
  labs(y = 'Frequência (%)') +
  scale_fill_jama(name = 'Forma de Renda:') +
  coord_flip() +
  theme_minimal(base_size = 12) + theme(legend.position = 'top')

#---------------------------
#  Religião da família
#---------------------------
base1 %>% 
  as_data_frame() %>% 
  filter(Município == 'Uarini') %>% 
  select(Religião.da.família) %>% 
  group_by(Religião.da.família) %>% 
  count() %>%  
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = Religião.da.família, y = n)) +
  geom_bar(stat = 'identity', fill = 'forestgreen') +
  geom_text(aes(label = paste0(n)), color = 'white',
            position = position_stack(vjust = 0.5), size = 5) +
  labs(x = 'Religião da Família', y = 'Frequência (%)') +
  scale_x_discrete(labels = c('Apenas crê em Deus', 'Católica', 'Evangélica')) +
  theme_minimal(base_size = 14)



base1 %>% 
  as_data_frame() %>% 
  filter(Município == 'Uarini') %>% 
  select(Comunidade, Religião.da.família) %>% 
  group_by(Comunidade) %>% 
  count(Religião.da.família) %>%
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = Comunidade, y = prop, fill = Religião.da.família)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = paste0(n)), color = 'white',
            position = position_stack(vjust = 0.5)) +
  labs(y = 'Religião (%)') +
  coord_flip() +
  scale_fill_jama(name = 'Religião da família:') +
  theme_minimal() +
  theme(legend.position = 'top')

#-----------------------------------
#  óbito de crianças/ad na família
#-----------------------------------
base1 %>% 
  as_data_frame() %>% 
  filter(Município == 'Uarini') %>% 
  select(Comunidade, Já.houve.alguma.perda.fatal.de.criança.adolescente.na.família.) %>% 
  group_by(Comunidade) %>% 
  count(Já.houve.alguma.perda.fatal.de.criança.adolescente.na.família.) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = Comunidade, y = prop, fill = Já.houve.alguma.perda.fatal.de.criança.adolescente.na.família.)) +
  geom_bar(stat = 'identity') +
  scale_fill_d3(name = 'Perda de criança/adol.') +
  geom_text(aes(label = paste0(n)), color = 'white',
            position = position_stack(vjust = 0.5)) +
  labs(y = '%') +
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(legend.position = 'top')

#-----------------------------------
#  Moradia
#-----------------------------------
base1 %>% 
  as_data_frame() %>% 
  filter(Município == 'Uarini') %>% 
  select(Comunidade, Moradia) %>% 
  group_by(Comunidade) %>% 
  count(Moradia) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = Comunidade, y = prop, fill = Moradia)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = paste0(n)),  color = 'white',
            position = position_stack(vjust = 0.5)) +
  labs(y = '%') +
  scale_fill_d3() +
  coord_flip() +
  theme_minimal()

#-----------------------------------
#  energia
#-----------------------------------
base1 %>% 
  as_data_frame() %>% 
  filter(Município == 'Uarini') %>% 
  select(Comunidade, Energia.na.casa) %>% 
  group_by(Comunidade) %>% 
  count(Energia.na.casa) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = Comunidade, y = prop, fill = Energia.na.casa)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = paste0(n)),  color = 'white',
          position = position_stack(vjust = 0.5)) +
  labs(y = '%') +
  scale_fill_d3() +
  coord_flip() +
  theme_minimal()
