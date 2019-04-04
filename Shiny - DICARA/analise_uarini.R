caio_theme <- theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 12)
  )

blank_theme <- theme_minimal(base_size = 16) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 12)
  )


#-------------------------
# Contagem das comunidades
#-------------------------

base1 %>% 
  as_data_frame() %>% 
  filter(Município == 'Uarini') %>% 
  count(Comunidade) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = reorder(Comunidade, n), y = prop)) +
  geom_bar(stat = 'identity', fill = 'olivedrab3') + 
  coord_flip() +
  geom_text(aes(label = paste0(n)),
            position = position_stack(vjust = 0.5),
            size = 4.5) +
  labs(x = '', y = '%') +
  scale_y_discrete(limits = seq(0, 18, 3)) +
  caio_theme

#-------------------------
# Nº de pessoas na família
#-------------------------
base1 %>% 
  as_data_frame() %>% 
  filter(Município == 'Uarini') %>% 
  select(Comunidade, Nº.de.moradores.na.casa..mesma.família.) %>%
  group_by(Comunidade) %>% 
  summarise(média = round(mean(Nº.de.moradores.na.casa..mesma.família.), 0)) %>% 
  ggplot(aes(x = reorder(Comunidade, média), y = média)) +
  geom_bar(stat = 'identity', fill = 'olivedrab3') +
  labs(x = "", y = '') +
  geom_text(aes(label = paste0(média)), 
            position = position_stack(vjust = 1.05)) +
  scale_y_discrete(limits = "") +
  coord_flip() +
  blank_theme

#-------------------------
# crianças/adol na cidade
#-------------------------
base1 %>% 
  as_data_frame() %>% 
  filter(Município == 'Uarini') %>% 
  select(Comunidade, Filhos..crianças.adolescentes..morando.na.cidade.) %>% 
  count(Filhos..crianças.adolescentes..morando.na.cidade.) %>% 
  mutate(prop = n*100/sum(n)) %>%
  ggplot(aes(x = "", y = prop, fill = Filhos..crianças.adolescentes..morando.na.cidade.)) +
  geom_bar(stat = 'identity') + coord_polar('y') +
  geom_text(aes(label = paste0(round(prop, 2), "%")), size = 6,
            position = position_stack(vjust = 0.5)) +
  scale_fill_jama(name = '') +
  blank_theme +
  theme(axis.text.x = element_blank())
  
  
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
  ungroup() %>% 
  mutate(prop = n*100/218) %>% 
  ggplot(aes(x = reorder(variable, -prop), y = prop)) +
  geom_bar(stat = 'identity', fill = 'olivedrab3') +
  geom_text(aes(label = paste0(round(prop, 1), '%')), vjust = 0, size = 5) +
  labs(x = '', y = '%') +
  scale_x_discrete(labels = c('Pesca', 'Agricultura', 'Func. Público', 'Comércio', 'Outros')) +
  blank_theme +
  theme(axis.text.y = element_blank())

  

#---------------------------
#  Renda mensal
#---------------------------
base1 %>% 
  as_data_frame() %>% 
  filter(Município == 'Uarini') %>% 
  select(Comunidade, Renda..R...média.mensal.da.família) %>% 
  group_by(Comunidade) %>% 
  summarise(média = mean(Renda..R...média.mensal.da.família, na.rm = TRUE)) %>% 
  ggplot(aes(x = reorder(Comunidade, média), y = média)) +
  geom_bar(stat = 'identity', fill = 'olivedrab3') + 
  geom_text(aes(label = paste0(round(média, 0))),
            position = position_stack(vjust = 0.5)) +
  labs(x = '', y = '') +
  coord_flip() +
  blank_theme +
  theme(axis.text.x = element_blank())


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
  ungroup() %>% 
  mutate(prop = n*100/218) %>% 
  ggplot(aes(x = variable, y = prop)) +
  geom_bar(stat = 'identity', fill = 'olivedrab3')  +
  geom_text(aes(label = paste0(round(prop, 1), '%')), vjust = 0, size = 5) +
  scale_x_discrete(labels = c('Bolsa Família', 'Bolsa Floresta', "Aposentadoria")) +
  blank_theme + theme(axis.text.y = element_blank())


#---------------------------
#  Religião da família
#---------------------------
base1 %>% 
  as_data_frame() %>% 
  filter(Município == 'Uarini') %>% 
  select(Religião.da.família) %>% 
  group_by(Religião.da.família) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = "", y = prop, fill = Religião.da.família)) +
  geom_bar(stat = 'identity') + coord_polar('y') +
  geom_text(aes(label = paste0(round(prop, 1), '%')), 
            position = position_stack(vjust = 0.5), size = 5) +
  scale_fill_jama(name = '') +
  blank_theme + theme(axis.text.x = element_blank())


#-----------------------------------
#  óbito de crianças/ad na família
#-----------------------------------
base1 %>% 
  as_data_frame() %>% 
  filter(Município == 'Uarini', 
         Já.houve.alguma.perda.fatal.de.criança.adolescente.na.família. != "") %>% 
  select(Já.houve.alguma.perda.fatal.de.criança.adolescente.na.família.) %>% 
  count(Já.houve.alguma.perda.fatal.de.criança.adolescente.na.família.) %>% 
  mutate(prop = n*100/sum(n)) %>%
  ggplot(aes(x = "", y = prop, fill = Já.houve.alguma.perda.fatal.de.criança.adolescente.na.família.)) +
  geom_bar(stat = 'identity') + coord_polar('y') +
  geom_text(aes(label = paste0(round(prop, 2), "%")), size = 6,
            position = position_stack(vjust = 0.5)) +
  scale_fill_jama(name = '') +
  blank_theme +
  theme(axis.text.x=element_blank())

#-----------------------------------
#  Moradia
#-----------------------------------
base1 %>% 
  as_data_frame() %>% 
  filter(Município == 'Uarini') %>% 
  select(Moradia) %>% 
  count(Moradia) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = reorder(Moradia, -prop), y = prop)) +
  geom_bar(stat = 'identity', fill = 'chocolate') +
  geom_text(aes(label = paste0(round(prop, 1))), vjust = 0, size = 5)+
  labs(x = '', y = '%') +
  scale_fill_jama(name = 'Tipo de moradia:') +
  caio_theme 


#-----------------------------------
#  energia
#-----------------------------------
base1 %>% 
  as_data_frame() %>% 
  filter(Município == 'Uarini') %>% 
  select(Energia.na.casa) %>% 
  count(Energia.na.casa) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = reorder(Energia.na.casa, prop), y = prop)) +
  geom_bar(stat = 'identity', fill = 'gold3') +
  geom_text(aes(label = paste0(round(prop, 1))), vjust = 0, size = 5) +
  labs(x = '', y = '%') +
  scale_fill_jama(name = 'Energia na casa:') +
  coord_flip() +
  caio_theme
  
#-----------------------------------
#  abastecimento de água
#-----------------------------------
base1 %>% 
  as_data_frame() %>% 
  filter(Município == 'Uarini') %>% 
  select(Abastecimento.de.água) %>% 
  count(Abastecimento.de.água) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = reorder(Abastecimento.de.água, prop), y = prop)) +
  geom_bar(stat = 'identity', fill = 'aquamarine3') +
  geom_text(aes(label = paste0(round(prop, 1))), vjust = 0, size = 5) +
  labs(x = '', y = '%') +
  scale_fill_jama(name = 'Abastecimento de água:') +
  coord_flip() +
  caio_theme

#-----------------------------------
#  tratamento da água
#-----------------------------------
base1 %>% 
  as_data_frame() %>% 
  filter(Município == 'Uarini') %>% 
  select(Tratamento.da.água.para.consumo) %>% 
  count(Tratamento.da.água.para.consumo) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = reorder(Tratamento.da.água.para.consumo, -prop), y = prop)) +
  geom_bar(stat = 'identity', fill = 'aquamarine3') +
  geom_text(aes(label = paste0(round(prop, 1))), vjust = 0, size = 5) +
  labs(x = '', y = '%') +
  scale_fill_jama(name = 'Tratamento da água:') +
  caio_theme

#-----------------------------------
#  tipo de esgoto
#-----------------------------------
base1 %>% 
  as_data_frame() %>% 
  filter(Município == 'Uarini') %>% 
  select(Tipo.de.esgoto) %>% 
  count(Tipo.de.esgoto) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = reorder(Tipo.de.esgoto, prop), y = prop)) +
  geom_bar(stat = 'identity', fill = 'tan1') +
  geom_text(aes(label = paste0(round(prop, 1))), vjust = 0, size = 5) +
  labs(x = '', y = '%') +
  scale_fill_jama(name = 'Tipo de esgoto:') +
  coord_flip() +
  caio_theme


#-----------------------------------
#  destino do lixo
#-----------------------------------
base1 %>% 
  as_data_frame() %>% 
  filter(Município == 'Uarini') %>% 
  select(Destino.do.lixo) %>% 
  count(Destino.do.lixo) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = reorder(Destino.do.lixo, prop), y = prop)) +
  geom_bar(stat = 'identity', fill = 'olivedrab3') +
  geom_text(aes(label = paste0(round(prop, 1))), vjust = 0, size = 5) +
  labs(x = '', y = '%') +
  scale_fill_jama(name = 'Destino do lixo:') +
  coord_flip() +
  caio_theme

#-----------------------------------
#  serviço de saúde
#-----------------------------------
base1 %>% 
  as_data_frame() %>% 
  filter(Município == 'Uarini') %>% 
  select(Serviço.de.saúde.utilizado) %>% 
  count(Serviço.de.saúde.utilizado) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = Serviço.de.saúde.utilizado, y = prop)) +
  geom_bar(stat = 'identity', fill = 'brown3') +
  geom_text(aes(label = paste0(round(prop, 1))), vjust = 0, size = 5) +
  labs(x = '', y = '%') +
  scale_fill_jama(name = 'Destino do lixo:') +
  caio_theme

#-----------------------------------
#  tempo de locomoção à cidade
#-----------------------------------
base1 %>% 
  as_data_frame() %>% 
  filter(Município == 'Uarini') %>% 
  select(Tempo.de.locomoção.até.a.cidade) %>% 
  filter(Tempo.de.locomoção.até.a.cidade != "") %>% 
  count(Tempo.de.locomoção.até.a.cidade) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = Tempo.de.locomoção.até.a.cidade, y = prop)) +
  geom_bar(stat = 'identity', fill = 'snow4') +
  geom_text(aes(label = paste0(round(prop, 1))), vjust = 0, size = 5) +
  labs(x = '', y = '%') +
  scale_fill_jama(name = 'Tempo de locomoção à cidade:') +
  caio_theme

#-----------------------------------
#  meio de transporte utilizado
#-----------------------------------
base1 %>% 
  as_data_frame() %>% 
  filter(Município == 'Uarini') %>% 
  select(Meio.de.transporte.utilizado.Lancha,
         Meio.de.transporte.utilizado.Barco,
         Meio.de.transporte.utilizado.Bicicleta,
         Meio.de.transporte.utilizado.Carro,
         Meio.de.transporte.utilizado.Moto,
         Meio.de.transporte.utilizado.Outro,
         Meio.de.transporte.utilizado.Canoa.motor,
         Meio.de.transporte.utilizado.Canoa.remo) %>% 
  melt() %>% 
  group_by(variable) %>% 
  filter(value == 1) %>% 
  count(value) %>% 
  mutate(prop = n*100/212) %>% 
  ggplot(aes(x = reorder(variable, -prop), y = prop)) +
  geom_bar(stat = 'identity', fill = 'snow4') +
  geom_text(aes(label = paste0(round(prop, 1))), vjust = 0, size = 5) +
  labs(x = '', y = '%') +
  scale_x_discrete(labels = c('Canoa/motor', 
                              'Lancha', 
                              'Moto',
                              'Canoa/remo',
                              'Outro')) +
  caio_theme

#-----------------------------------
#  hp do motor
#-----------------------------------
base1 %>% 
  as_data_frame() %>% 
  filter(Município == 'Uarini', Hp.do.motor != "") %>% 
  select(Comunidade, Hp.do.motor) %>% 
  group_by(Comunidade) %>% 
  summarise(média = mean(Hp.do.motor, na.rm = TRUE)) %>% 
  ggplot(aes(x = reorder(Comunidade, média), y = média)) +
  geom_bar(stat = 'identity', fill = 'snow4') + 
  geom_text(aes(label = paste0(round(média, 0))),
            position = position_stack(vjust = 0.5), color = 'white') +
  labs(x = '', y = 'Força do motor (hp)') +
  coord_flip() +
  caio_theme

#****************************************************************
# Grupo familiar
#****************************************************************

base2 <- read.table('clipboard', blank.lines.skip = FALSE, header = TRUE, sep = '\t', quote = '')

base2 %>% 
  as_data_frame() %>%
  mutate(groupage = factor(findInterval(base2$idade, c(7, 14, 18)),
                           labels = c('0 a 6', '7 a 13', '14 a 17', '+18'))) %>% 
  filter(X_parent_index >= 1409) %>% 
  group_by(groupage) %>% 
  count(groupage) %>% 
  ungroup() %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = groupage, y = prop)) +
  geom_bar(stat = 'identity')



cmds <- NULL

for(i in 1:nrow(base2)){
  if(base2$X_parent_index[i] == base1$X_index[i])
  cmds[i] <- 
}

base1 %>% 
  filter(Município == 'Uarini') %>% 
  select(X_index, Comunidade)





