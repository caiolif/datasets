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

#dev.new(width = 8, height = 4, unit = "in")

caio_theme <- theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(face = "bold", size = 16, color = 'black'),
    axis.text.y = element_text(face = "bold", size = 16, color = 'black')
  )

blank_theme <- theme_minimal(base_size = 16) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(face = "bold", size = 14, color = 'black'),
    axis.text.y = element_text(face = "bold", size = 14, color = 'black')
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


#-----------------------------------
# Informações dos municípios
#-----------------------------------

# comunidades por municípios
base1 %>% 
  group_by(Município) %>% 
  select(Comunidade) %>%
  unique() %>% 
  ungroup() %>% 
  count(Município) %>% 
  mutate(n = c(n[1:2], 21, n[4:7])) %>% 
  ggplot(aes(x = reorder(Município, n), y = n, fill = Município)) +
  geom_bar(stat = 'identity', color = 'black') + 
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),
            size = 7) + coord_flip() +
  labs(x = '', y = '') +
  scale_fill_simpsons(name = '') + 
  caio_theme +
  theme(legend.position = 'none',
        axis.text.x = element_blank(),
        line = element_blank(),
        axis.text.y = element_text(color = 'black', face = 'bold', size = 18)) 

# nº de famílias por municípios
base1 %>% 
  count(Município) %>% 
  ggplot(aes(x = reorder(Município, n), y = n)) +
  geom_bar(stat = 'identity', fill = 'steelblue4') +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),
            size = 7, color = 'white') +
  labs(x = '', y = '%') +
  blank_theme +  coord_flip() +
  theme(legend.position = 'none',
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 17, color = 'black', face = 'bold')) 


# nº de famílias por zona
base1 %>% 
  count(Município, Local.de.aplicação) %>% 
  group_by(Município) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = reorder(Município, n), y = prop, fill = Local.de.aplicação)) +
  geom_bar(stat = 'identity', color = 'white') +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),
            size = 7, color = 'white') +
  labs(x = '', y = '%') +
  blank_theme + coord_flip() +
  scale_fill_manual(name = '', values = c('forestgreen', 'grey35')) +
  theme(legend.position = 'top',
        legend.text = element_text(size = 18),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18))



#-----------------------------------
# Informações das famílias
#-----------------------------------


# moda de moradores por família
base1 %>% 
  filter(Nº.de.moradores.na.casa..mesma.família. != '') %>% 
  group_by(Município) %>% 
  count(Nº.de.moradores.na.casa..mesma.família.) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  #summarise(moda = which.max(tabulate(Nº.de.moradores.na.casa..mesma.família.))) %>% 
  ggplot(aes(x = reorder(Município, -moda), y = moda)) +
  geom_line(aes(group = 1), size = 1) +
  geom_point(size = 2) +
  geom_text(aes(label = paste0(moda)),
            vjust = -1, size = 6) +
  labs(x = '', y = 'Nº de moradores') +
  scale_y_discrete(limits = seq(1, 6, 1)) +
  caio_theme +
  theme(axis.title.y = element_text(face = 'bold')) 


base1 %>% 
  count(Nº.de.moradores.na.casa..mesma.família.) %>% 
  mutate(prop = n*100/sum(n))
  

# nº de famílias com crianças e adolescentes na cidade
base1 %>% 
  filter(Filhos..crianças.adolescentes..morando.na.cidade. != '') %>% 
  group_by(Município) %>% 
  count(Filhos..crianças.adolescentes..morando.na.cidade.) %>% 
  mutate(prop = n*100/sum(n)) %>%
  filter(Filhos..crianças.adolescentes..morando.na.cidade. == 'Sim') %>% 
  ggplot(aes(x = reorder(Município, prop), y = prop)) +
  geom_bar(stat = 'identity', fill = 'grey20') + 
  geom_text(aes(label = paste0(round(prop, 1), '%')),
            position = position_stack(vjust = 0.5),
            size = 7, color = 'white') +
  labs(x = '', y = 'Nº de famílias (%)') +
  blank_theme + coord_flip() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18))


# renda média mensal
base1 %>% 
  group_by(Município) %>% 
  filter(Renda..R...média.mensal.da.família != '') %>% 
  summarise(média = mean(Renda..R...média.mensal.da.família)) %>% 
  ggplot(aes(x = reorder(Município, média), y = média)) +
  geom_bar(stat = 'identity', fill = 'goldenrod1', color = 'darkgreen', size = 1) +
  geom_text(aes(label = round(média, 1), hjust = 1), size = 7, color = 'darkgreen') +
  labs(x = '', y = '') +
  caio_theme + coord_flip() +
  theme(legend.position = 'top',
        plot.background = element_rect(fill = 'darkseagreen3'),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = 'darkgreen', size = 18),
        line = element_blank())

# formas de renda
base1 %>% 
  group_by(Município) %>% 
  select(Pesca, Agricultura, Funcionário.Público, Comércio, Extrativismo, Outra.forma.de.renda) %>% 
  melt() %>% 
  group_by(Município, variable) %>% 
  count(value) %>%
  mutate(prop = round(n*100/sum(n), 1),
         sum(n)) %>%
  filter(value == 1) %>% 
  ggplot(aes(x = Município, y = variable, size = prop, color = variable)) +
  geom_point(alpha = 0.7) +
  caio_theme +
  geom_text(aes(label = ifelse(prop > 5, paste0(prop, '%'), ''), vjust = 0.5), size = 5, color = 'black') +
  labs(x = '', y = '') +
  scale_y_discrete(labels = c('Pesca', 'Agricultura', 'Func. Público', 
                              'Comércio', 'Extrativismo', 'Outra forma')) +
  scale_size_continuous(range = c(1, 40), guide = FALSE) +
  scale_x_discrete(labels = c('Carauari', 'Eirunepé', 'Marãa', 'N. Aripuanã', 'Tefé', 'Uarini')) +
  theme(legend.position = 'none') +
  scale_color_simpsons()



  # base1 %>%  
  # select(Município, Pesca, Agricultura, Funcionário.Público, Comércio, Extrativismo, Outra.forma.de.renda) %>% 
  # group_by(Município) %>%
  # summarise(Pesca = sum(Pesca, na.rm = TRUE),
  #           Agricultura = sum(Agricultura, na.rm = TRUE),
  #           Func.Público = sum(Funcionário.Público, na.rm = TRUE),
  #           Comércio = sum(Comércio, na.rm = TRUE),
  #           Extrativismo = sum(Extrativismo, na.rm = TRUE),
  #           Outra.forma = sum(Outra.forma.de.renda, na.rm = TRUE)) 


# benefícios
base1 %>% 
  group_by(Município) %>% 
  select(Bolsa.Família, Bolsa.Floresta, Aposentadoria, Outros.benefícios) %>% 
  melt() %>% 
  group_by(variable, Município) %>%
  count(value) %>% 
  filter(value != '') %>% 
  mutate(prop = round(n*100/sum(n), 0)) %>% 
  filter(value == 1) %>% 
  ggplot(aes(x = Município, y = prop, fill = variable)) +
  geom_bar(stat = 'identity', color = 'black', position = 'dodge', width = 0.8) +
  geom_text(aes(label = ifelse(prop > 0, paste0(prop, '%'), '')), 
            position = position_dodge(width = 0.8), size = 5.5,
            hjust = -0.1) +
  scale_fill_manual(name = '', values = c('yellow1', 'olivedrab3', 'grey', 'tomato'),
                    labels = c('Bolsa Família', 'Bolsa Floresta', 'Aposentadoria', 'Outros benefícios')) +
  caio_theme + coord_flip(ylim = c(0, 100)) +
  theme(legend.position = 'top',
        legend.text = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        line = element_blank(),
        axis.text.y = element_text(size = 17)) 


# religião
base1 %>% 
  group_by(Município) %>% 
  filter(Religião.da.família %in% c('Católica', 'Evangélica', 'Acredita em Deus, mas não possuem uma religião')) %>% 
  count(Religião.da.família) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = reorder(Município, prop), y = prop, fill = Religião.da.família)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = paste0(round(prop, 1), '%')), size = 6,
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(name = '', palette = 'PuBu',
                 labels = c('Apenas acredita em Deus', 'Católica', 'Evangélica')) +
  labs(x = '', y = 'Nº de famílias (%)') +
  blank_theme + coord_flip() +
  theme(legend.position = 'top',
        legend.text = element_text(size = 17),
        axis.text.x = element_blank(),
        plot.background = element_rect(fill = 'wheat3'),
        axis.text.y = element_text(size = 18)
        )


# perdas fatais
base1 %>% 
  filter(Já.houve.alguma.perda.fatal.de.criança.adolescente.na.família. != '') %>% 
  group_by(Município) %>% 
  count(Já.houve.alguma.perda.fatal.de.criança.adolescente.na.família.) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  filter(Já.houve.alguma.perda.fatal.de.criança.adolescente.na.família. == 'Sim') %>% 
  ggplot(aes(x = reorder(Município, prop), y = prop)) +
  geom_bar(stat = 'identity', fill = 'firebrick4') + 
  geom_text(aes(label = paste0(round(prop, 1), '%')),
            position = position_stack(vjust = 0.5),
            size = 7, color = 'white') +
  labs(x = '', y = 'Nº de famílias (%)') +
  caio_theme + coord_flip() +
  theme(plot.background = element_rect(fill = 'grey12'),
        line = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = 'white', size = 18))


base1 %>% 
  filter(Já.houve.alguma.perda.fatal.de.criança.adolescente.na.família. == 'Sim',
         Causa.da.perda.2 != '') %>% 
  group_by(Município) %>% 
  count(Causa.da.perda.2) %>%
  mutate(prop = round(n*100/sum(n), 1)) %>%
  filter(prop > 3) %>% 
  ggplot(aes(x = Município, y = Causa.da.perda.2, size = prop, color = 'firebrick4')) +
  geom_point(alpha = 0.8) +
  caio_theme +
  geom_text(aes(label = ifelse(prop > 3, paste0(prop, '%'), ''), vjust = 0.5), size = 6, color = 'white') +
  labs(x = '', y = '')  +
  scale_size_continuous(range = c(1, 40), guide = FALSE) +
  theme(legend.position = 'none',
        plot.background = element_rect(fill = 'grey12'),
        axis.text.x = element_text(color = 'white'),
        axis.text.y = element_text(color = 'white'),
        line = element_blank())

# serviço de saúde
base1 %>% 
  filter(Serviço.de.saúde.utilizado != '') %>% 
  group_by(Município) %>% 
  count(Serviço.de.saúde.utilizado) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  filter(Serviço.de.saúde.utilizado == 'Não tem') %>% 
  ggplot(aes(x = reorder(Município, prop), y = prop)) +
  geom_bar(stat = 'identity', fill = 'firebrick3') +
  geom_text(aes(label = paste0(round(prop, 1), '%')), size = 8, position = position_stack(vjust = 0.5), color = 'lightskyblue2') +
  theme_minimal(base_size = 35) + coord_flip() +
  labs(x = '', y = '(Nº de adolescentes) %') +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 25, color = 'firebrick3'),
        axis.title.x = element_blank(),
        line = element_blank(),
        plot.background = element_rect(fill = 'lightskyblue2'))


# tipo de transporte
base1 %>% 
  group_by(Município) %>% 
  mutate(Lancha = Meio.de.transporte.utilizado.Lancha,
         Barco = Meio.de.transporte.utilizado.Barco,
         Bicicleta = Meio.de.transporte.utilizado.Bicicleta,
         Carro = Meio.de.transporte.utilizado.Carro,
         Moto = Meio.de.transporte.utilizado.Moto,
         Outro = Meio.de.transporte.utilizado.Outro,
         Canoa.motor = Meio.de.transporte.utilizado.Canoa.motor,
         Canoa.remo = Meio.de.transporte.utilizado.Canoa.remo,
         Caminhando = Meio.de.transporte.utilizado.Caminhando) %>% 
  select(Lancha, Barco, Bicicleta, Carro, Moto, Canoa.motor, Canoa.remo, Caminhando, Outro) %>% 
  melt() %>%
  group_by(Município, variable) %>% 
  count(value) %>% 
  filter(value != '') %>% 
  mutate(prop = round(n*100/sum(n),1)) %>%
  filter(value == 1) %>% 
  ggplot(aes(x = Município, y = variable, size = prop, color = variable)) +
  geom_point(alpha = 0.7) +
  caio_theme +
  geom_text(aes(label = ifelse(prop > 10, paste0(prop, '%'), ''), vjust = 0.5), size = 5, color = 'black') +
  labs(x = '', y = '') +
  scale_colour_d3() +
  scale_size_continuous(range = c(1, 30), guide = FALSE) +
  theme(legend.position = 'none')


# tempo de locomoção
base1 %>% 
  group_by(Município) %>% 
  filter(Tempo.de.locomoção.até.a.cidade != '') %>% 
  count(Tempo.de.locomoção.até.a.cidade) %>% 
  mutate(prop = n*100/sum(n),
         Tempo.de.locomoção.até.a.cidade = factor(Tempo.de.locomoção.até.a.cidade,
                                                  levels = c("Até 30min", "De 30min à 1h", "De 1h à 2h", "De 2h à 3h", "Mais de 3h"),
                                                  labels = c("Até 30min", "30min - 1h", "1h - 2h", "2h - 3h", "Mais que 3h"))) %>% 
  ggplot(aes(x = Município, y = prop, fill = Tempo.de.locomoção.até.a.cidade)) +
  geom_bar(stat = 'identity', color = 'brown') +
  geom_label_repel(aes(label = paste0(round(prop, 1), '%'), hjust = 0),
                   position = position_stack(vjust = 0.5), size = 5,
                   show.legend = FALSE) +
  caio_theme + coord_flip() +
  scale_fill_manual(name = '', values = c('#FFFFCC', '#FFCC99', '#FF9933', '#FF6633', '#CC3300')) +
  theme(legend.position = 'top',
        legend.text = element_text(face = 'bold'),
        axis.text.x = element_blank()) +
  labs(x = '', y = '')

#-----------------------------------
#   Informações das moradias
#-----------------------------------

# tipo de moradia
base1 %>% 
  group_by(Município) %>% 
  filter(Moradia != '') %>% 
  count(Moradia, Local.de.aplicação) %>% 
  mutate(prop = n*100/sum(n),
         soma = sum(n)) %>% 
  filter(Moradia == 'Madeira') %>% 
  ggplot(aes(x = reorder(Município, -prop), y = prop, fill = Local.de.aplicação)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = ifelse(prop > 3, paste0(round(prop, 1), '%'), '')), 
            color = 'white', size = 6,
            position = position_stack(vjust = 0.5)) +
  stat_summary(fun.y = sum, aes(label = paste0(round(..y.., 1), '%'), group = Município), geom = 'text', vjust = -0.1, size = 6) +
  scale_fill_manual(name = '', values = c('salmon4', 'lightsalmon3')) +
  labs(x = '', y = '') +
  caio_theme +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(color = 'black', size = 18),
        axis.title.y = element_blank(),
        line = element_blank(),
        legend.position = 'none',
        legend.text = element_text(size = 16)) +
  coord_cartesian(ylim = c(1, 110))


# energia na casa
base1 %>% 
  group_by(Município) %>% 
  filter(Energia.na.casa != '') %>% 
  count(Energia.na.casa) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  filter(Energia.na.casa %in% c('Gerador comunitário', 'Gerador individual', 'Rede pública')) %>% 
  ggplot(aes(x = Município, y = prop, fill = Energia.na.casa)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = ifelse(prop > 4, paste0(round(prop, 1), '%'), '')), 
            color = 'black', size = 6,
            position = position_stack(vjust = 0.5)) + 
  stat_summary(fun.y = sum, aes(label = paste(round(..y.., 1), '%'), 
                                group = Município), geom = 'text', size = 6, vjust = -0.1) +
  labs(x = '', y = '') +
  scale_fill_jco(name = '') +
  blank_theme + 
  theme(legend.position = 'top',
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 18),
        axis.text.x = element_text(size = 17))


# abastecimento de água
base1 %>% 
  group_by(Município) %>% 
  filter(Abastecimento.de.água != '') %>% 
  count(Abastecimento.de.água) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  filter(Abastecimento.de.água %in% c('Coleta de água da chuva',
                                       'Coletivo e rede de distribuição',
                                       'Direto do igarapé',
                                       'Direto do rio',
                                      'Poço comunitário')) %>% 
  ggplot(aes(x = Município, y = prop, fill = Abastecimento.de.água)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = ifelse(prop > 5, paste0(round(prop, 0), '%'), '')), 
                position = position_stack(vjust = 0.5), size = 6, color = 'white') +
  stat_summary(fun.y = sum, aes(label = paste0(round(..y..,0), '%'), group = Município), geom = 'text', vjust = -0.1, size = 6) +
  labs(x = '', y = '')  + 
  theme_economist(base_size = 15) +
  scale_fill_economist(name = '') +
  scale_y_discrete(labels = c('Coleta \nágua da chuva',
                              'Coletivo e\nrede de distribuição',
                              'Direto do igarapé',
                              'Direto do rio',
                            'Poço comunitário')) +
  theme(legend.position = 'top',
        axis.text.x = element_text(face = 'bold'),
        legend.text = element_text(size = 15),
        line = element_blank()) +
  coord_cartesian(ylim = c(0, 110)) +
  guides(fill = guide_legend(ncol = 3, nrow = 2, byrow = TRUE))


# tratamento da água
base1 %>% 
  group_by(Município) %>% 
  count(Tratamento.da.água.para.consumo) %>% 
  mutate(prop = n*100/sum(n))  %>% 
  filter(Tratamento.da.água.para.consumo %in% c('Cloro', 'Coado', 'Sachê', 'Sem tratamento')) %>% 
  ggplot(aes(x = Município, y = prop, fill = Tratamento.da.água.para.consumo)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = ifelse(prop > 3, paste0(round(prop, 0), '%'), '')), 
            color = 'white', size = 6,
            position = position_stack(vjust = 0.5)) +
  stat_summary(fun.y = sum, aes(label = paste(round(..y.., 1), '%'), 
                                group = Município), geom = 'text', size = 6, vjust = -0.5) +
  labs(x = '', y = '') +
  scale_fill_economist(name = '') + 
  theme_economist(base_size = 16) + 
  theme(legend.position = 'top',
    axis.text.x = element_text(face = "bold", size = 18),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    line = element_blank()
  ) +
  coord_cartesian(ylim = c(0, 110))
  

# tipo de esgoto
base1 %>% 
  filter(Tipo.de.esgoto != '') %>% 
  group_by(Município) %>% 
  count(Tipo.de.esgoto) %>% 
  mutate(prop = round(n*100/sum(n), 1)) %>% 
  ggplot(aes(x = Município, y = prop, fill = Tipo.de.esgoto)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = ifelse(prop > 3, paste0(prop, '%'), '')), size = 6, position = position_stack(vjust = 0.5)) +
  caio_theme +
  scale_fill_simpsons(name = '') +
  theme(legend.position = 'top',
        legend.text = element_text(size = 16),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 18))



# destino do lixo
base1 %>% 
  filter(Destino.do.lixo != '') %>% 
  group_by(Município) %>% 
  count(Destino.do.lixo) %>% 
  mutate(prop = n*100/sum(n))  %>% 
  ggplot(aes(x = Município, y = prop, fill = Destino.do.lixo)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.8) +
  geom_text(aes(label = n, vjust = -0.2), 
            color = 'black', size = 5,
            position = position_dodge(width = 0.8)) +
  labs(x = '', y = 'Nº de famílias (%)') +
  theme(legend.position = 'top',
        axis.text.x = element_text(face = "bold", size = 14),
        axis.text.y = element_text(face = "bold", size = 14)) +
  scale_fill_jco(name = '') +
  caio_theme +
  theme(legend.position = 'top')


# hp do motor
base1 %>% 
  group_by(Município) %>% 
  filter(Hp.do.motor != '') %>% 
  summarise(me_dp = paste(round(mean(Hp.do.motor), 2), 
                          round(sd(Hp.do.motor), 1), sep = ' ± '))


base1 %>% 
  group_by(Município) %>% 
  summarise(moda = which.max(tabulate(Hp.do.motor)))


#---------------------------------------
# descrição das crianças e adolescentes
#---------------------------------------

# qtd de crianças
base2 %>% 
  filter(idade < 18) %>% 
  count(Município, Local.de.Aplicação) %>% 
  group_by(Município) %>% 
  mutate(soma = sum(n)) %>% 
  ggplot(aes(x = reorder(Município, -soma), y = n, label = n, fill = Local.de.Aplicação)) +
  geom_bar(stat = 'identity') +
  geom_text(position = position_stack(vjust = 0.8), size = 7, color = 'white') +
  stat_summary(fun.y = sum, aes(label = ..y.., group = Município), geom = 'text', vjust = -0.1, size = 7) +
  caio_theme +
  scale_fill_manual(name = '', values = c('forestgreen', 'grey35')) +
  scale_x_discrete(labels = c('Maraã', 'Itapiranga', 'Uarini', 'Eirunepé', 'Tefé', 'N. Aripuanã', 'Carauari')) +
  theme(legend.position = 'top',
        legend.text = element_text(size = 17),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        line = element_blank(),
        axis.text.x = element_text(size = 18))

# crianças com deficiência
base2 %>% 
  group_by(Município) %>% 
  filter(Possui.alguma.deficiência. != '',
         idade < 18) %>% 
  count(Município, Possui.alguma.deficiência.) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  filter(Possui.alguma.deficiência. == 1) %>% 
  ggplot(aes(x = reorder(Município, prop), y = prop)) +
  geom_bar(stat = 'identity', fill = 'royalblue3') +
  geom_text(aes(label = paste0(round(prop, 1), '%')),
            position = position_stack(vjust = 0.5),
            size = 7, color = 'white') +
  labs(x = '', y = '%') +
  blank_theme + coord_flip() +
  scale_fill_wsj(name = '') +
  theme(legend.position = 'top',
        legend.text = element_text(size = 18),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18))

# sexo
base2 %>% 
  filter(idade < 18) %>% 
  group_by(Município) %>% 
  count(Gênero) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = Município, y = prop, fill = Gênero)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = paste0(round(prop,1), '%')), position = position_stack(vjust = 0.5), size = 7) +
  labs(x = '', y = 'Nº de crianças e adol. (%)') +
  coord_flip() +
  caio_theme +
  theme(legend.position = 'none',
        axis.text.y = element_text(size = 18),
        axis.text.x = element_blank(),
        axis.title.x = element_blank())


base2 %>% 
  filter(idade < 18) %>% 
  group_by(Município, Gênero) %>% 
  count(faixa_etária) %>%
  ungroup() %>% 
  group_by(faixa_etária, Município) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = Município, y = prop, fill = Gênero)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = paste0(round(prop,1), '%')), position = position_stack(vjust = 0.5), size = 6) +
  facet_grid(aes(cols = faixa_etária)) + 
  labs(x = '', y = '') +
  coord_flip() +
  caio_theme +
  theme(legend.position = 'none',
        axis.text.y = element_text(size = 18),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_rect(fill = 'grey'),
        strip.text.x = element_text(face = 'bold', size = 16), 
        line = element_blank())


# faixa etária
base2 %>% 
  filter(idade < 18) %>% 
  group_by(Município) %>% 
  count(faixa_etária) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = Município, y = prop, fill = faixa_etária, label = n)) +
  geom_bar(stat = 'identity', color = 'grey') +
  geom_text(position = position_stack(vjust = 0.5), size = 6) +
  caio_theme + coord_flip() +
  labs(x = '', y = 'Nº de crianças e adolescentes (%)') +
  scale_fill_brewer(name = '', palette = 'Greens') +
  theme(legend.position = 'top',
        legend.text = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(hjust = 1.1),
        line = element_blank())


# atualmente estuda
base2 %>% 
  filter(idade < 18, Atualmente.estuda. != '') %>%
  group_by(Município) %>% 
  count(Atualmente.estuda.) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  filter(Atualmente.estuda. == 'Não') %>% 
  ggplot(aes(x = reorder(Município, prop), y = prop, label = paste(round(prop, 1), '%'))) +
  geom_bar(stat = 'identity', fill = 'darkred') +
  geom_text(position = position_stack(vjust = 0.5), color = 'white', size = 8) + 
  coord_flip() +
  labs(x = '', y = 'Nº de crianças e adolescentes (%)') +
  blank_theme +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18, colour = 'black'))


base2 %>% 
  filter(idade < 18, Atualmente.estuda. == 'Não') %>%
  group_by(Município) %>% 
  count(Por.que.não.estuda.) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  filter(Por.que.não.estuda. == 'Não está na idade') %>% 
  ggplot(aes(x = reorder(Município, prop), y = prop, label = paste0(round(prop,0), '%'))) +
  geom_bar(stat= 'identity', fill = 'firebrick4') +
  geom_text(position = position_stack(vjust = 0.5), size = 7, color = 'white') +
  blank_theme + coord_flip() +
  labs(x = '', y = '') +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(color = 'black', size = 18))


# escolaridade
base2 %>% 
  filter(idade < 18,
         Escolaridade != '') %>%
  group_by(Município) %>% 
  count(Escolaridade) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  filter(Escolaridade %in% c('Pré-escolar',
                             'Ensino Fundamental Incompleto',
                             'Ensino Médio Incompleto')) %>% 
  mutate(tot.prop = sum(prop)) %>% 
  ggplot(aes(x = Município, y = prop, fill = Escolaridade)) +
  geom_bar(stat = 'identity') +
  labs(x = '', y = 'Nº de crianças e adolescentes (%)') +
  geom_text(aes(label = paste0(round(prop, 1), '%')), 
            position = position_stack(vjust = 0.5), size = 7) +
  caio_theme  +
  stat_summary(fun.y = sum, aes(label = paste(round(..y.., 1), '%'), 
                                group = Município), geom = 'text', size = 7, vjust = -0.2) +
  theme(legend.position = 'none',
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.title.x = element_blank(),
        line = element_blank()
        ) +
  scale_fill_jco(name = '', labels = c('Fundamental Incompleto', 
                            'Médio Incompleto', 
                            'Pré-escolar')) +
  coord_cartesian(ylim = c(1, 100))


#  crianças e adolescentes que repetiram de ano
base2 %>% 
  group_by(Município) %>% 
  filter(idade < 18,
         Já.repetiu.de.ano. != '') %>%
  count(Já.repetiu.de.ano.) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = Município, y = prop, fill = Já.repetiu.de.ano., label = ifelse(prop > 3, paste0(round(prop, 1), '%'), ''))) +
  geom_bar(stat= 'identity') +
  geom_text(position = position_stack(vjust = 0.5), size = 6) +
  caio_theme +
  theme(legend.position = 'top',
        legend.text = element_text(size = 16),
        axis.text.y = element_blank()) +
  scale_fill_simpsons(name = '') +
  labs(x = '', y = '') 


# crianças e adolescentes com merenda na escola
base2 %>% 
  filter(idade < 18,
         Merenda.na.escola != '') %>% 
  group_by(Município) %>% 
  count(Merenda.na.escola) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  filter(Merenda.na.escola == 'Sim') %>% 
  ggplot(aes(x = reorder(Município, prop), y = prop)) +
  geom_bar(stat = 'identity', fill = 'mediumseagreen') +
  geom_text(aes(label = paste0(round(prop,1), '%')), size = 6, position = position_stack(vjust = 0.5)) +
  caio_theme + coord_flip() +
  scale_y_discrete(limits = seq(0, 100, 20)) +
  labs(x = '', y = '') +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        line = element_blank())


# crianças que faltam aula para trabalhar
base2 %>% 
  filter(idade < 18,
         Falta.aula.para.trabalhar. != '') %>% 
  group_by(Município) %>%
  count(Falta.aula.para.trabalhar.) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  filter(Falta.aula.para.trabalhar. == 1) %>% 
  ggplot(aes(x = reorder(Município, prop), y = prop)) +
  geom_bar(stat = 'identity', fill = 'plum3') +
  geom_text(aes(label = paste0(round(prop, 1), '%')), size = 6, position = position_stack(vjust = 0.5)) +
  caio_theme + coord_flip() +
  labs(x = '', y = '') +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        line = element_blank())
  

# documentação 
base2 %>% 
  group_by(Município) %>% 
  filter(idade < 18) %>%
  select(Certidão.de.casamento, 
         Certidão.de.nascimento,
         RG, CPF, Título.de.eleitor,
         Carteira.de.trabalho) %>% 
  melt() %>% 
  group_by(Município, variable) %>% 
  count(value) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  filter(value == 1) %>% 
  ggplot(aes(x = Município, y = variable, size = prop, color = variable)) +
  geom_point(alpha = 0.8) +
  caio_theme +
  geom_text(aes(label = ifelse(prop > 10, paste0(round(prop, 1), '%'), ''), vjust = 0.5), size = 5, color = 'black') +
  labs(x = '', y = '') +
  scale_y_discrete(labels = c('Cert. de Casamento',
                              'Cert. de Nascimento',
                              'RG', 'CPF',
                              'Título de Eleitor',
                              'Carteira de Trabalho')) +
  scale_size_continuous(range = c(1, 40), guide = FALSE) +
  theme(legend.position = 'none',
        axis.text.x = element_text(face = 'bold'),
        axis.text.y = element_text(face = 'bold'))  +
  scale_color_simpsons()


# ajuda os pais
base2 %>% 
  filter(idade < 18) %>%
  group_by(Município) %>% 
  select(Roça, Pesca, Caça, 
         Fazendo.farinha, Comércio, Ajudando.em.casa, 
         Trabalhando.na.casa.de.outra.pessoa,
         Cuida.dos.irmãos) %>% 
  mutate_if(is.factor, as.numeric) %>% 
  melt() %>% 
  group_by(Município, variable) %>% 
  count(value) %>% 
  mutate(prop = n*100/sum(n)) %>%
  filter(value == 1) %>% 
  ggplot(aes(x = Município, y = prop, fill = variable, label = round(prop,1), hjust = 0.6, vjust = -0.2)) +
  geom_bar(stat= 'identity', position = 'dodge', width = 0.6) +
  geom_text(position = position_dodge(width = 0.8), size = 5) +
  caio_theme +
  theme(legend.position = 'top') +
  scale_fill_jco(name = '',
                 labels = c('Roça', 'Pesca', 'Caça', 'Faz farinha',
                            'Ajuda em casa', 'Cuida dos irmãos')) +
  labs(x = '', y = 'Nº de crianças e adolescentes (%)')


# crianças que pensaram em se mudar
base2 %>% 
  filter(idade < 18) %>% 
  group_by(Município) %>%
  count(Você.já.pensou.em.morar.em.outro.lugar.) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  filter(Você.já.pensou.em.morar.em.outro.lugar. == 'Sim')


# adolescentes grávidas
base2 %>% 
  filter(idade < 18,
         Está.grávida. != '') %>% 
  group_by(Município) %>% 
  count(Está.grávida., Local.de.Aplicação) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  filter(Está.grávida. == 'Sim') %>% 
  group_by(Município) %>% 
  mutate(soma = sum(n)) %>% 
  ggplot(aes(x = reorder(Município, soma), y = n, fill = Local.de.Aplicação)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = n), size = 9, position = position_stack(vjust = 0.5), color = 'lightpink2') +
  #stat_summary(fun.y = sum, aes(label = ..y.., group = Município), geom = 'text', size = 9, hjust = -0.1) +
  theme_minimal(base_size = 25) + coord_flip() +
  labs(x = '', y = '') +
  scale_fill_manual(name = '', values = c('black', 'grey30')) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 25, color = 'black'),
        plot.background = element_rect(fill = 'lightpink2'),
        line = element_blank(),
        legend.position = 'none')


# já possui filho?
base2 %>% 
  filter(idade < 18, 
         Já.possui.filho. != '') %>% 
  group_by(Município) %>% 
  count(Já.possui.filho., Local.de.Aplicação) %>%
  mutate(prop = n*100/sum(n)) %>% 
  filter(Já.possui.filho. == 'Sim') %>% 
  mutate(soma = sum(n)) %>% 
  ggplot(aes(x = reorder(Município, soma), y = n, fill = Local.de.Aplicação)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = n), size = 8, color = 'lightpink2', position = position_stack(vjust = 0.5)) +
  #stat_summary(fun.y = sum, aes(label = ..y.., group = Município), geom = 'text', size = 9, hjust = -0.1) +
  theme_minimal(base_size = 20) + 
  labs(x = '', y = '') + coord_flip() +
  scale_fill_manual(name = '', values = c('black', 'grey30')) +
  #scale_x_discrete(labels = c('Tefé', 'Novo\nAripuanã', 'Itapiranga', 'Uarini', 'Eirunepé', 'Maraã', 'Carauari')) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 25, color = 'black'),
        plot.background = element_rect(fill = 'lightpink2'),
        line = element_blank(),
        legend.position = 'none'
  )


base2 %>% 
  filter(idade < 18, Já.possui.filho. == 'Sim') %>% 
  count(Quantos.)

# status de relacionamento
base2 %>% 
  group_by(Município) %>% 
  filter(idade < 18,
         Status.de.relacionamento != '') %>%
  count(Status.de.relacionamento) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  filter(Status.de.relacionamento %in% c('Amigado', 'Namorando', 'Solteiro')) %>% 
  ggplot(aes(x = Município, y = prop, fill = Status.de.relacionamento, 
             label = ifelse(prop > 3 , paste0(round(prop, 0), '%'), ''))) +
  geom_bar(stat= 'identity', color = 'grey20') +
  geom_text(position = position_stack(vjust = 0.5), size = 6) +
  caio_theme + coord_flip() +
  theme(legend.position = 'top',
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.text = element_text(size = 15),
        axis.text.y = element_text(size = 18)) +
  scale_fill_brewer(name = '', palette = 'Reds') +
  labs(x = '', y = '')


# consumo
base2 %>% 
  filter(idade < 18,
         Consomem. != '') %>% 
  group_by(Município) %>% 
  count(Consomem., Local.de.Aplicação) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  filter(Consomem. == 'Cigarro') %>% 
  ggplot(aes(x = reorder(Município, n), y = n, fill = Local.de.Aplicação)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = n), size = 9, position = position_stack(vjust = 0.5), color = 'darkorange4') +
  theme_minimal(base_size = 25) + 
  scale_fill_manual(name = '', values = c('white', 'grey60')) +
  labs(x = '', y = '') + coord_flip() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 25, color = 'white'),
        line = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(color = 'white'),
        plot.background = element_rect(fill = 'darkorange4'))


base2 %>% 
  filter(idade < 18,
         Consomem. != '') %>% 
  group_by(Município) %>% 
  count(Consomem., Local.de.Aplicação) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  filter(Consomem. == 'Bebidas alcóolicas') %>% 
  mutate(soma = sum(n)) %>% 
  ggplot(aes(x = reorder(Município, soma), y = n, fill = Local.de.Aplicação)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = n), size = 8, position = position_stack(vjust = 0.5), color = 'gray22') +
  theme_minimal(base_size = 25) + coord_flip() +
  scale_fill_manual(name = '', values = c('darkgoldenrod2', 'darkgoldenrod4')) +
  labs(x = '', y = '') +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 25, color = 'darkgoldenrod2'),
        line = element_blank(),
        plot.background = element_rect(fill = 'gray22'),
        legend.position = 'bottom',
        legend.text = element_text(color = 'darkgoldenrod2'))

# pensou morar em outro lugar
base2 %>% 
  filter(idade < 18, Você.já.pensou.em.morar.em.outro.lugar. != '') %>%
  group_by(Município) %>% 
  count(Você.já.pensou.em.morar.em.outro.lugar.) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  filter(Você.já.pensou.em.morar.em.outro.lugar. == 'Sim') %>% 
  ggplot(aes(x = reorder(Município, prop), y = prop)) +
  geom_bar(stat = 'identity', fill = 'dodgerblue1') +
  geom_text(aes(label = paste0(round(prop, 1), '%')), size = 7, 
            position = position_stack(vjust = 0.5)) +
  blank_theme + coord_flip() +
  theme(axis.text.x = element_blank())

# para onde?
base2 %>% 
  filter(idade < 18, Você.já.pensou.em.morar.em.outro.lugar. == 'Sim') %>%
  group_by(Município) %>% 
  count(Para.onde.você.mudaria.) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = reorder(Município, -prop), y = prop, fill = Para.onde.você.mudaria.)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = ifelse(prop > 3, paste0(round(prop, 1), '%'), '')), size = 6,
            position = position_stack(vjust = 0.5)) +
  blank_theme + 
  scale_fill_d3(name = '') +
  theme(legend.position = 'top',
        legend.text = element_text(size = 18),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 18))

# voltaria?
base2 %>% 
  filter(idade < 18, Depois.de.sair.da.comunidade..você.voltaria. != '') %>%
  group_by(Município) %>% 
  count(Depois.de.sair.da.comunidade..você.voltaria.) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  #filter(Depois.de.sair.da.comunidade..você.voltaria. == 'Sim') %>% 
  ggplot(aes(x = reorder(Município, -prop), y = prop, fill = Depois.de.sair.da.comunidade..você.voltaria.)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = paste0(round(prop, 1), '%')), size = 6,
            position = position_stack(vjust = 0.5)) +
  blank_theme + coord_flip() +
  scale_fill_d3(name = 'Você voltaria?') +
  theme(legend.position = 'top',
        legend.text = element_text(size = 18),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18)) +
  guides(fill = guide_legend(ncol = 2, nrow = 3, byrow = TRUE))


# o que lá tem que não tem aqui
library(wordcloud2)

base2 %>% 
  filter(idade < 18,
         O.que.tem.lá.que.0.tem.na.sua.comunidade. != '',
         Município == 'Uarini') %>%
  count(O.que.tem.lá.que.0.tem.na.sua.comunidade.) %>% 
  wordcloud2(size = 1)


base2 %>% 
  filter(idade < 18, 
         O.que.tem.na.sua.comunidade.que.0.tem.em.outros.lugares. != '',
         Município == 'Uarini') %>% 
  count(O.que.tem.na.sua.comunidade.que.0.tem.em.outros.lugares.) %>% 
  wordcloud2(size = 0.6)


# cursos
base2 %>% 
  filter(idade < 18,
         Participa.de.algum.curso..Artesanato != '',
         Participa.de.algum.curso..Educação.Ambiental != '',
         Participa.de.algum.curso..Música != '',
         Participa.de.algum.curso..Esportes != '',
         Participa.de.algum.curso..Outros != '',
         Participa.de.algum.curso..Informática != '') %>%
  group_by(Município) %>% 
  select(Participa.de.algum.curso..Artesanato,
         Participa.de.algum.curso..Educação.Ambiental,
         Participa.de.algum.curso..Música,
         Participa.de.algum.curso..Esportes,
         Participa.de.algum.curso..Outros,
         Participa.de.algum.curso..Informática) %>% 
  melt() %>% 
  group_by(variable, Município) %>% 
  count(value) %>% 
  mutate(prop = n*100/sum(n)) %>%
  filter(value == 1) %>% 
  ggplot(aes(x = Município, y = variable, size = prop, color = variable)) +
  geom_point(alpha = 0.8) +
  caio_theme +
  geom_text(aes(label = ifelse(prop > 10, paste0(round(prop, 1), '%'), ''), vjust = 0.5), size = 5, color = 'black') +
  labs(x = '', y = '')  +
  scale_size_continuous(range = c(1, 40), guide = FALSE) +
  theme(legend.position = 'none',
        axis.text.x = element_text(face = 'bold'),
        axis.text.y = element_text(face = 'bold'))  +
  scale_color_simpsons() +
  scale_y_discrete(labels = c('Artesanato',
                              'Educação Ambiental',
                              'Música',
                              'Esportes',
                              'Outros',
                              'Informática')) 


#----------------------------
# zona rural vs zonar urbana
#----------------------------

# gravidez na adolescência
base2 %>% 
  filter(idade < 18, Está.grávida. == 'Sim') %>% 
  count(Está.grávida., Local.de.Aplicação) 

base2 %>% 
  filter(idade < 18, Já.possui.filho. == 'Sim') %>% 
  count(Já.possui.filho., Local.de.Aplicação) 


base2 %>% 
  filter(idade < 18, Atualmente.estuda. != '') %>% 
  count(Falta.aula.para.trabalhar., Gênero) %>% 
  mutate(prop = n*100/sum(n))

