library(ggplot2)
library(magrittr)
library(dplyr)
library(ggsci)
library(tidyr)
library(RColorBrewer)
library(reshape2)
library(ggthemes)
library(ggpol)
library(ggpubr)
library(ggrepel)
library(wesanderson)

caio_theme <- theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_text(face = "bold", size = 17, color = 'black'),
    axis.text.y = element_text(face = "bold", size = 17, color = 'black'),
    panel.border = element_blank()
  )

blank_theme <- theme_minimal(base_size = 16) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(face = "bold", size = 17, color = 'black'),
    axis.text.y = element_text(face = "bold", size = 17, color = 'black')
  )

#-----------------------------------
# leitura e manipulação dos dados
#-----------------------------------

# leitura
base1 <- read.table('base1.txt', header = TRUE, sep = "\t", encoding = 'UTF-8') %>% 
  as_data_frame() 

base1 <- base1 %>%  rename(Local.de.aplicação = X.U.FEFF.Local.de.aplicação)

base2 <- read.table('base2.txt', header = TRUE, blank.lines.skip = FALSE, sep = "\t", quote = "") %>% 
  as_data_frame()

base2 <- base2 %>% 
  mutate(faixa_etária = factor(findInterval(base2$idade, c(7, 14, 18)),
                               labels = c('0 a 6', '7 a 13', '14 a 17', '+18')))


#**********************************************************
#   Formulário de aplicação familiar
#**********************************************************

unique(filter(base1, Município == 'Maraã') %>% select(Comunidade)) %>% 
  count()

# nº de famílias entrevistadas
base1 %>% 
  filter(Município %in% c('Maraã')) %>% 
  count(Município)

base2 %>% 
  filter(Município %in% c('Maraã'),
         idade < 18) %>% 
  count(Município) 


# nº de famílais por comunidade
base1 %>% 
  filter(Município == 'Carauari') %>% 
  count(Comunidade) %>% 
  ggplot(aes(x = reorder(Comunidade, n), y = n, label = n)) +
  geom_bar(stat = 'identity', position = 'dodge', fill = 'tomato', width = 0.8) + 
  coord_flip(ylim = c(1, 130)) +
  geom_text(size = 5,  position = position_dodge(width = 0.8), hjust = -0.1) +
  labs(y = 'Nº de famílias') +
  caio_theme +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        line = element_blank()) 


base1 %>% 
  filter(Município == 'Carauari') %>% 
  count(Comunidade) %>% 
  grid.arrange(nrow = 1) %>% 
  tableGrob()

mytheme <- gridExtra::ttheme_default(
  core = list(fg_params = list(cex = 0.8)),
  colhead = list(fg_params = list(cex = 1.0)))

tableGrob(com_cara[16:30, ], theme = mytheme, rows = NULL) %>% 
  grid::grid.draw()

# nº de famílias por zona
base1 %>% 
  filter(Município == 'Maraã') %>% 
  count(Local.de.aplicação)


# Nº de pessoas na família
base1 %>% 
  filter(Município == 'Maraã') %>% 
  group_by(Comunidade) %>% 
  summarise(moda = which.max(tabulate(Nº.de.moradores.na.casa..mesma.família.))) %>%
  count(moda) %>% 
  ggplot(aes(x = reorder(moda, -n), y = n, label = n)) +
  geom_bar(stat = 'identity', fill = 'black') + 
  geom_text(size = 7, position = position_stack(vjust = 0.5), color = 'white') +
  labs(x = 'Nº de moradores na casa') +
  caio_theme +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        line = element_blank())



#-------------------------
# crianças/adol na cidade
#-------------------------
base1 %>% 
  filter(Município == 'Maraã',
         Filhos..crianças.adolescentes..morando.na.cidade. != '') %>% 
  select(Comunidade, Filhos..crianças.adolescentes..morando.na.cidade.) %>% 
  count(Filhos..crianças.adolescentes..morando.na.cidade.) %>% 
  mutate(prop = n*100/sum(n)) %>%
  ggplot(aes(x = "", y = prop, fill = Filhos..crianças.adolescentes..morando.na.cidade.)) +
  geom_bar(stat = 'identity') + coord_polar('y') +
  geom_label_repel(aes(label = paste0(round(prop, 2), "%")), size = 6,
            position = position_stack(vjust = 0.5), color = 'white', show.legend = FALSE) +
  scale_fill_manual(name = '', values = c('grey10', 'grey60')) +
  blank_theme +
  theme(axis.text.x = element_blank(),
        legend.text = element_text(size = 20))
  
  
#-----------------------------------
#  óbito de crianças/ad na família
#-----------------------------------
base1 %>% 
  filter(Município == 'Maraã', 
         Já.houve.alguma.perda.fatal.de.criança.adolescente.na.família. != "") %>% 
  count(Já.houve.alguma.perda.fatal.de.criança.adolescente.na.família.) %>% 
  mutate(prop = n*100/sum(n)) %>%
  ggplot(aes(x = "", y = prop, fill = Já.houve.alguma.perda.fatal.de.criança.adolescente.na.família.)) +
  geom_bar(stat = 'identity') + coord_polar('y') +
  geom_text(aes(label = paste0(round(prop, 2), "%")), size = 7,
            position = position_stack(vjust = 0.5),
            color = 'white') +
  scale_fill_manual(name = 'Óbitos de crianças ou adolescentes?', values = c('firebrick1', 'firebrick4')) +
  blank_theme +
  theme(axis.text.x = element_blank(),
        legend.position = 'right',
        legend.title = element_text(size = 16, color = 'white'),
        legend.text = element_text(size = 17, color = 'white'),
        plot.background = element_rect(fill = 'grey12'))


base1 %>% 
  as_data_frame() %>% 
  filter(Município == 'Maraã', 
         Já.houve.alguma.perda.fatal.de.criança.adolescente.na.família. == 'Sim',
         Causa.da.perda.2 != '') %>% 
  count(Causa.da.perda.2) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = reorder(Causa.da.perda.2, n), y = n, label = n)) +
  geom_bar(stat = 'identity', fill = 'firebrick3') +
  geom_text(position = position_stack(vjust = 0.5), size = 7, color = 'white') +
  caio_theme + coord_flip() +
  labs(x = '', y = '') +
  theme(axis.text.x = element_blank(),
        legend.position = 'right',
        axis.text.y = element_text(color = 'white'),
        legend.title = element_text(size = 16, color = 'white'),
        legend.text = element_text(size = 17, color = 'white'),
        plot.background = element_rect(fill = 'grey12'), 
        line = element_blank())

#---------------------------
#  Renda mensal
#---------------------------
base1 %>% 
  filter(Renda..R...média.mensal.da.família != '',
         Município == 'Maraã') %>% 
  group_by(Comunidade) %>% 
  summarise(média = mean(Renda..R...média.mensal.da.família)) %>% 
  ggplot(aes(x = reorder(Comunidade, média), y = média)) +
  geom_bar(stat = 'identity', fill = 'goldenrod1', color = 'darkgreen', size = 1, position = 'dodge', width = 0.8) +
  geom_text(aes(label = round(média, 0), hjust = 1), size = 5, color = 'darkgreen',
            position = position_dodge(width = 0.8), hjust = -0.1) +
  labs(x = '', y = '') +
  caio_theme + coord_flip(ylim = c(0, 3000)) +
  theme(legend.position = 'top',
        plot.background = element_rect(fill = 'darkseagreen3'),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = 'darkgreen', size = 14,
        line = element_blank()))


 #---------------------------
# Forma de renda
#---------------------------

base1 %>% 
  filter(Município == 'Carauari') %>% 
  select(Pesca, Agricultura, Funcionário.Público, Comércio, Extrativismo, Outra.forma.de.renda) %>% 
  melt() %>% 
  group_by(variable) %>% 
  count(value) %>% 
  mutate(prop = round(n*100/sum(n), 1)) %>% 
  filter(value == 1) %>% 
  ggplot(aes(x = reorder(variable, -prop), y = prop)) +
  geom_bar(stat = 'identity', fill = c('olivedrab3', 'lightcyan3', 'salmon',
                                       'steelblue1', 'plum2', 'gold3')) +
  geom_text(aes(label = paste0(prop, '%')), size = 7,
            position = position_stack(vjust = 0.5)) +
  labs(x = '', y = 'Nº de famílias') +
  scale_x_discrete(labels = c('Agricultura', 'Outra forma', 'Extrativismo',
                             'Pesca', 'Func. Público', 'Comércio')) +
  blank_theme + 
  theme(legend.position = 'top',
        axis.text.y = element_blank()) 



#---------------------------
#  Benefícios
#---------------------------

base1 %>% 
  filter(Município == 'Carauari') %>% 
  select(Local.de.aplicação, Bolsa.Família, Bolsa.Floresta, Aposentadoria, Outros.benefícios) %>% 
  melt() %>% 
  group_by(Local.de.aplicação, variable) %>%
  count(value) %>%
  mutate(prop = round(n*100/sum(n), 1)) %>% 
  filter(value == 1) %>% 
  ggplot(aes(x = variable, y = prop, fill = Local.de.aplicação)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.9)  +
  geom_text(aes(label = paste0(prop, '%')), vjust = -0.2,
            position = position_dodge(width = 0.8), size = 7) +
  scale_x_discrete(labels = c('Bolsa Família', 'Bolsa Floresta', "Aposentadoria", 'Outros benefícios')) +
  blank_theme +
  scale_fill_jco(name = '') +
  theme(axis.text.y = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size = 17)) +
  coord_cartesian(ylim = c(1, 90))


#---------------------------
#  Religião da família
#---------------------------
base1 %>% 
  filter(Município == 'Carauari', Religião.da.família != '') %>% 
  count(Religião.da.família) %>% 
  ungroup() %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = reorder(Religião.da.família, -prop), y = prop)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.7, fill = 'royalblue4') +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5), size = 7, color = 'white') +
  blank_theme + 
  scale_x_discrete(labels = c('Evangélica', 'Católica', 'Acredita em Deus,\nmas não possui religião', 'Outra', 'Espírita')) +
  theme(legend.position = 'none',
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = 'wheat3'),
        axis.text.x = element_text(size = 18),
        panel.border = element_blank()
  ) +
  coord_cartesian(ylim = c(1, 70)) 


#-----------------------------------
#  Moradia
#-----------------------------------
base1 %>% 
  filter(Município == 'Carauari',
         Moradia != '') %>%
  count(Local.de.aplicação, Moradia) %>% 
  mutate(prop = round(n*100/sum(n), 1)) %>% 
  ggplot(aes(x = reorder(Moradia, -prop), y = prop, fill = Local.de.aplicação)) +
  geom_bar(stat = 'identity', fill = 'tan3') + 
  geom_text(aes(label = paste0(round(prop, 1), "%")), size = 7,
            position = position_stack(vjust = 0.5),
            color = 'black') +
  # stat_summary(fun.y = sum, 
  #              aes(label = paste(..y.., '%'), group = Moradia), 
  #              geom = 'text',
  #              size = 7, vjust = -0.5) +
  blank_theme +
  #scale_fill_brewer(name = '', palette = 'Set2') +
  theme(axis.text.y = element_blank(),
        legend.position = 'top',
        legend.text = element_text(size = 17)) +
  coord_cartesian(ylim = c(1, 80))

#-----------------------------------
#  meio de transporte utilizado
#-----------------------------------
base1 %>% 
  filter(Município == 'Carauari') %>% 
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
  count(value) %>% 
  mutate(prop = round(n*100/sum(n), 1)) %>% 
  filter(value == 1) %>% 
  ggplot(aes(x = reorder(variable, -n), y = n)) +
  geom_bar(stat = 'identity', fill = 'tomato', width = 0.9) +
  geom_text(aes(label = n, vjust = -.5), 
            position = position_stack(vjust = 0.5), size = 7) +
  labs(x = '', y = '') +
  blank_theme + 
  scale_x_discrete(labels = c('Canoa/motor', 'Canoa/remo', 'Barco', 'Lancha', 'Moto', 'Bicicleta')) +
  theme(legend.position = 'top',
        axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(1, 600))


#-----------------------------------
#  serviço de saúde
#-----------------------------------
base1 %>% 
  filter(Município == 'Maraã',
         Serviço.de.saúde.utilizado != '') %>%
  count(Local.de.aplicação, Serviço.de.saúde.utilizado) %>% 
  mutate(prop = round(n*100/sum(n), 1)) %>% 
  ggplot(aes(x = reorder(Serviço.de.saúde.utilizado, -prop), y = prop, fill = Local.de.aplicação)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = ifelse(prop > 0.5, paste0(prop, '%'), '')), 
            position = position_stack(vjust = 0.5), size = 7) +
  labs(x = '', y = '%') + 
  blank_theme + 
  stat_summary(fun.y = sum, 
               aes(label = paste(..y.., '%'), group = Serviço.de.saúde.utilizado), 
               geom = 'text',
               size = 7, vjust = -0.5) +
  scale_fill_npg(name = '') +
  theme(legend.position = 'top',
        legend.text = element_text(size = 18),
        axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(1, 60))

#-----------------------------------
#  energia
#-----------------------------------
base1 %>% 
  filter(Município == 'Maraã',
         Energia.na.casa != '',) %>%
  count(Local.de.aplicação, Energia.na.casa) %>% 
  mutate(prop = round(n*100/sum(n), 1)) %>% 
  ggplot(aes(x = reorder(Energia.na.casa, -prop), y = prop, fill = Local.de.aplicação)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = paste0(round(prop, 1), '%')), 
            position = position_stack(vjust = 0.5), size = 7) +
  labs(x = '', y = '%') +
  stat_summary(fun.y = sum, 
               aes(label = paste(..y.., '%'), group = Energia.na.casa), 
               geom = 'text',
               size = 7, vjust = -0.5) +
  scale_fill_manual(name = '', values = c('gold', 'goldenrod3')) +
  blank_theme + 
  theme(axis.text.y = element_blank(),
        legend.position = 'top', 
        legend.text = element_text(size = 17)) +
  coord_cartesian(ylim = c(0, 100))
  
#-----------------------------------
#  abastecimento de água
#-----------------------------------
base1 %>% 
  filter(Município == 'Maraã',
         Abastecimento.de.água != '') %>%
  count(Local.de.aplicação, Abastecimento.de.água) %>% 
  mutate(prop = round(n*100/sum(n), 1)) %>%
  group_by(Abastecimento.de.água) %>% 
  mutate(soma = sum(prop)) %>% 
  ggplot(aes(x = reorder(Abastecimento.de.água, -soma), y = prop, fill = Local.de.aplicação)) +
  geom_bar(stat = 'identity', width = 0.7) +
  geom_text(aes(label = paste0(round(prop, 1), '%')), 
            position = position_stack(vjust = 0.5), size = 7, color = 'white') +
  labs(x = '', y = '%') +
  stat_summary(fun.y = sum, 
               aes(label = paste(..y.., '%'), group = Abastecimento.de.água), 
               geom = 'text',
               size = 7, vjust = -0.5) +
  scale_x_discrete(labels = c('Coletivo e\nrede de distribuição',
                              'Coleta de água da chuva','Poço comunitário'
                              ,'Direto do rio')) +
  theme_economist(base_size = 15) + 
  scale_fill_economist(name = '') +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        line = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 18, face = 'bold')) +
  coord_cartesian(ylim = c(0, 46))

#-----------------------------------
#  tratamento da água
#-----------------------------------
base1 %>% 
  filter(Município == 'Carauari', Tratamento.da.água.para.consumo != '') %>%
  count(Local.de.aplicação, Tratamento.da.água.para.consumo) %>% 
  mutate(prop = round(n*100/sum(n), 1)) %>% 
  ggplot(aes(x = reorder(Tratamento.da.água.para.consumo, -prop), y = prop, fill = Local.de.aplicação)) +
  geom_bar(stat = 'identity', fill = 'royalblue1') +
  geom_text(aes(label = paste0(prop, '%')), color = 'black',
            position = position_stack(vjust = 0.5), size = 7) +
  labs(x = '', y = '') + 
  # stat_summary(fun.y = sum, 
  #              aes(label = paste(..y.., '%'), group = Tratamento.da.água.para.consumo), 
  #              geom = 'text',
  #              size = 7, vjust = -0.5) +
  theme_economist(base_size = 17) +
  #scale_fill_economist(name = '') +
  theme(axis.text.x = element_text(size = 18, face = 'bold'),
        axis.text.y = element_blank(),
        line = element_blank()) +
  coord_cartesian(ylim = c(1, 53))

#-----------------------------------
#  tipo de esgoto
#-----------------------------------

base1 %>% 
  filter(Município == 'Maraã') %>%
  count(Local.de.aplicação, Tipo.de.esgoto) %>% 
  mutate(prop = round(n*100/sum(n), 1)) %>% 
  ggplot(aes(x = reorder(Tipo.de.esgoto, -prop), y = prop, fill = Local.de.aplicação)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = ifelse(prop > 0.9, paste0(prop, '%'), '')), 
            position = position_stack(vjust = 0.5), size = 7, color = 'black') +
  labs(x = '', y = '%') +
  stat_summary(fun.y = sum, 
               aes(label = paste(..y.., '%'), group = Tipo.de.esgoto), 
               geom = 'text',
               size = 7, vjust = -0.5) +
  blank_theme + 
  scale_fill_brewer(name = '', palette = 'Set2') +
  scale_x_discrete(labels = c('Fossa séptica', 'Pau da gata', 'Pedra sanitária\nfossa negra', 'Rio ou igarapé', 'Fossa aberta')) +
  theme(axis.text.x = element_text(color = 'black', size = 18),
        axis.text.y = element_blank(),
        legend.position = 'top',
        legend.text = element_text(size = 18, face = 'bold'),
        plot.background = element_rect(fill = 'bisque4')) +
  coord_cartesian(ylim = c(0, 50))


#-----------------------------------
#  destino do lixo
#-----------------------------------
base1 %>% 
  filter(Município == 'Itapiranga',
         Destino.do.lixo != '') %>%
  count(Local.de.aplicação, Destino.do.lixo) %>% 
  mutate(prop = round(n*100/sum(n), 1)) %>% 
  group_by(Destino.do.lixo) %>% 
  mutate(soma = sum(prop)) %>% 
  ggplot(aes(x = reorder(Destino.do.lixo, -prop), y = prop, fill = Local.de.aplicação)) +
  geom_bar(stat = 'identity') +
  stat_summary(fun.y = sum, 
               aes(label = paste(..y.., '%'), group = Destino.do.lixo), 
               geom = 'text',
               size = 7, vjust = -0.5) +
  geom_text(aes(label = ifelse(prop > 1, paste0(prop, '%'), '')), 
            position = position_stack(vjust = 0.5), size = 6.5) +
  labs(x = '', y = '%') + 
  blank_theme + 
  scale_x_discrete(labels = c('Coleta pública', 'Queima', 'Separa o lixo orgânico\ndo resto', 'Joga no mato', 'Enterra')) +
  scale_fill_brewer(name = '', palette = 'Set2') +
  theme(legend.position = 'top',
        legend.text = element_text(size = 20),
        axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 60))




