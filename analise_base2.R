#****************************************************************
#                     Grupo familiar
#****************************************************************

base2 <- read.table('base2.txt', header = TRUE, blank.lines.skip = FALSE, sep = "\t", quote = "") %>% 
  as_data_frame()

base2 <- base2 %>% 
  mutate(faixa_etária = factor(findInterval(base2$idade, c(7, 14, 18)),
                               labels = c('0 a 6', '7 a 13', '14 a 17', '+18')))


# faixa etária
base2 %>% 
  filter(Município == 'Maraã', idade < 18) %>% 
  count(faixa_etária, Gênero) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = faixa_etária, y = n, fill = Gênero)) +
  geom_bar(stat = 'identity')  +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5), size = 7) +
  stat_summary(fun.y = sum, aes(label = ..y.., group = faixa_etária), geom = 'text', size = 7, vjust = -0.2) +
  labs(x = 'Faixa Etária', y = '') +
  scale_fill_manual(name = '', values = c('hotpink2', 'royalblue2')) +
  blank_theme +
  theme(legend.position = 'top',
        legend.text = element_text(size = 18),
        axis.title.x = element_text(),
        axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(1, 580))


base2 %>% 
  filter(Município == 'Uarini', idade < 18) %>% 
  group_by(Local.de.Aplicação) %>% 
  count(Gênero) %>% ungroup() %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = Local.de.Aplicação, y = n, fill = Gênero)) + 
  geom_bar(stat = 'identity', position = 'dodge', width = 0.9) +
  geom_text(aes(label = paste0(n), vjust = -0.2), 
            position = position_dodge(width = 0.8),
            size = 5) +
  blank_theme +
  scale_fill_discrete(name = '') +
  theme(axis.text.y = element_blank(), legend.position = 'top')


# portadores de deficiência
base2 %>% 
  filter(Município == 'Maraã', idade < 18, 
         Possui.alguma.deficiência. == 1) %>% 
  count(Recebe.algum.benefício.por.causa.da.deficiência.) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = '', y = prop, fill = as.factor(Recebe.algum.benefício.por.causa.da.deficiência.))) +
  geom_bar(stat = 'identity') + coord_polar('y') +
  geom_text(aes(label = paste0(round(prop, 1), '%', '(', n, ')')),
            position = position_stack(vjust = 0.5),
            size = 9, color = 'white') +
  blank_theme +
  scale_fill_manual(name = 'Recebe algum benefício por causa da deficiência?',
                    labels = c('Não', 'Sim'),
                    values = c('dodgerblue', 'dodgerblue4')) +
  theme(axis.text.x = element_blank(),
        legend.position = 'top',
        legend.text = element_text(size = 18)) 



#------------------------
# atualmente estuda?
#------------------------

base2 %>% 
  filter(Município == 'Maraã', idade < 18) %>% 
  count(Local.de.Aplicação, faixa_etária, Atualmente.estuda.) %>% 
  mutate(prop = round(n*100/sum(n), 1)) %>%
  filter(Atualmente.estuda. == 'Não') %>% 
  group_by(faixa_etária) %>% 
  mutate(soma = sum(n)) %>% 
  ggplot(aes(x = reorder(faixa_etária, soma), y = n, fill = Local.de.Aplicação)) +
  geom_bar(stat = 'identity', color = 'black') + #coord_polar('y') +
  geom_text(aes(label = ifelse(n > 2, n, '')),
            position = position_stack(vjust = 0.5),
            size = 7, color = 'black') +
  caio_theme +
  labs(y = '', x = 'Faixa etária') +
  stat_summary(fun.y = sum, aes(label = ..y.., group = faixa_etária),
               geom = 'text', hjust = -0.2, size = 7) +
  scale_fill_brewer(name = '', palette = 'Set2') +
  theme(axis.text.x = element_blank(),
        legend.position = 'top',
        axis.title.y = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        line = element_blank()) +
  coord_flip(ylim = c(0, 220))


base2 %>% 
  filter(Município == 'Maraã', idade < 18) %>% 
  filter(Atualmente.estuda. == 'Não') %>% 
  count(Por.que.não.estuda.) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = Por.que.não.estuda., y = prop)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.8) +
  geom_text(aes(label = paste0(n), hjust = -0.2),
            position = position_dodge(width = 0.8),
            size = 6) +
  blank_theme + coord_flip() +
  labs(x = '', y = '') +
  scale_fill_d3(name = '') +
  theme(axis.text.x = element_blank(), 
        legend.position = 'top')

#---------------------
# documentos
#---------------------
base2 %>% 
  filter(Município == 'Maraã', idade < 18) %>% 
  select(Local.de.Aplicação, CPF, RG, Certidão.de.nascimento, Certidão.de.casamento, Título.de.eleitor, Carteira.de.trabalho) %>% 
  group_by(Local.de.Aplicação) %>% 
  melt() %>%
  group_by(Local.de.Aplicação, variable) %>% 
  count(value) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  filter(value == 1) %>% 
  group_by(variable) %>% 
  mutate(soma = sum(n)) %>% 
  ggplot(aes(x = reorder(variable, -soma), y = n, fill = Local.de.Aplicação)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.8) +
  geom_text(aes(label = n),
            position = position_dodge(width = 0.8),
            size = 6, vjust = -0.1) +
  blank_theme + 
  scale_x_discrete(labels = c('Certidão\nde nascimento', 'CPF', 'RG', 'Título de eleitor',
                              'Carteira\nde trabalho', 'Certidão\nde casamento')) +
  labs(x = '', y = '') +
  scale_fill_d3(name = '') +
  theme(axis.text.y = element_blank(), 
        legend.text = element_text(size = 18),
        legend.position = 'top') +
  coord_cartesian(ylim = c(1, 480))

#------------------------- 
# escolaridade
#-------------------------
base2 %>% 
  filter(Município == 'Maraã', idade < 18, Escolaridade != '') %>% 
  count(Escolaridade) %>%
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = reorder(Escolaridade, n), y = n)) +
  geom_bar(stat = 'identity', position = 'dodge', fill = 'black', width = 0.9) +
  geom_text(aes(label = n), hjust = -0.1,
            position = position_dodge(width = 0.9),
            size = 7) +
  blank_theme +
  scale_x_discrete(labels = c('Fundamental completo', 'Médio completo', 'EJA', 'Alfabetizado', 'Médio incompleto', 'Pré-escolar',
                              'Não alfabetizado', 'Fundamental incompleto')) +
  labs(x = '', y = '') +
  coord_flip(ylim = c(0, 900)) + 
  theme(legend.position = 'top',
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 20)) 

#-------------------------
# merenda
#-------------------------

base2 %>% 
  filter(Município == 'Maraã', idade < 18, Merenda.na.escola != '') %>% 
  count(Merenda.na.escola) %>%
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = reorder(Merenda.na.escola, -n), y = n)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.8, fill = 'gold2') +
  geom_text(aes(label = paste0(n), vjust = -0.2),
            position = position_dodge(width = 0.8),
            size = 7) +
  blank_theme +
  labs(x = '', y = '') +
  theme(legend.position = 'top',
        axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(1, 600))


#-------------------------
# repetiu de ano
#-------------------------

base2 %>% 
  filter(Município == 'Maraã', idade < 18, Já.repetiu.de.ano. != '') %>% 
  count(Já.repetiu.de.ano.) %>%
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = reorder(Já.repetiu.de.ano., -n), y = n)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.8, fill = 'firebrick1') +
  geom_text(aes(label = paste0(n), vjust = -0.2),
            position = position_dodge(width = 0.6),
            size = 7) +
  blank_theme +
  labs(x = '', y = '') +
  theme(legend.position = 'top',
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 19)) +
  coord_cartesian(ylim = c(1, 700))


#-------------------------
# ajuda os pais
#-------------------------

base2 %>% 
  filter(Município == 'Maraã') %>% 
  select(Roça, Pesca, Caça, 
         Fazendo.farinha, Comércio, Ajudando.em.casa, 
         Trabalhando.na.casa.de.outra.pessoa,
         Cuida.dos.irmãos) %>%
  melt() %>% 
  group_by(variable) %>% 
  count(value) %>% 
  mutate(prop = n*100/sum(n)) %>%
  filter(value == 1) %>% 
  ggplot(aes(x = reorder(variable, n), y = prop)) +
  geom_bar(stat = 'identity', fill = 'seagreen2') +
  geom_text(aes(label = paste0(round(prop, 1), '%')),
            position = position_stack(vjust = 0.5),
            size = 7) +
  scale_x_discrete(labels = c('Trabalha na casa de outra pessoa',
                              'Caça', 'Comércio', 'Fazendo farinha',
                              'Pesca', 'Roça', 'Cuida dos irmãos', 
                              'Ajudando em casa')) +
  blank_theme +
  coord_flip() +
  theme(axis.text.x = element_blank())

#-------------------------
# falta aula pra trabalhar
#-------------------------

base2 %>% 
  filter(Município == 'Maraã', idade < 18, 
         Falta.aula.para.trabalhar. != '') %>% 
  count(Falta.aula.para.trabalhar.) %>% 
  mutate(prop = round(n*100/sum(n), 1)) %>% 
  ggplot(aes(x = '', y = prop, fill = as.factor(Falta.aula.para.trabalhar.))) +
  geom_bar(stat = 'identity', color = 'black') + coord_polar('y') +
  geom_label(aes(label = paste0(round(prop, 1), '%')),
            position = position_stack(vjust = 0.5),
            size = 7, color = 'black', show.legend = FALSE) +
  blank_theme +
  scale_fill_brewer(name = 'Falta aula para trabalhar?',
                    labels = c('Não', 'Sim'),
                    palette = 'PRGn') +
  theme(axis.text.x = element_blank(),
        legend.position = 'top',
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18))

#-------------------------
# já pensou em se mudar?
#-------------------------

base2 %>% as_data_frame() %>% 
  filter(X_parent_index >= 1409, idade < 18,
         Você.já.pensou.em.morar.em.outro.lugar. != '') %>% 
  select(Você.já.pensou.em.morar.em.outro.lugar.) %>% 
  count(Você.já.pensou.em.morar.em.outro.lugar.) %>% 
  mutate(prop = n*100/sum(n),
         Você.já.pensou.em.morar.em.outro.lugar. = as.factor(Você.já.pensou.em.morar.em.outro.lugar.)) %>% 
  ggplot(aes(x = '', y = prop, fill = Você.já.pensou.em.morar.em.outro.lugar.)) +
  geom_bar(stat = 'identity') + coord_polar('y') +
  geom_text(aes(label = paste0(round(prop, 1), '%')),
            position = position_stack(vjust = 0.5),
            size = 7) +
  blank_theme +
  scale_fill_simpsons(name = '',
                      labels = c('Não',
                                 'Sim')) +
  theme(axis.text.x = element_blank(),
        legend.position = 'top',
        legend.text = element_text(size = 18)) 



base2 %>% 
  filter(X_parent_index >= 1409, idade < 18) %>% 
  select(Para.onde.você.mudaria.) %>% 
  filter(Para.onde.você.mudaria. != '') %>% 
  count(Para.onde.você.mudaria.) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = reorder(Para.onde.você.mudaria., n), y = prop)) +
  geom_bar(stat = 'identity', fill = 'cornflowerblue') +
  geom_text(aes(label = paste0(round(prop, 1), '%')),
            position = position_stack(vjust = 0.5),
            size = 6) +
  caio_theme + coord_flip() +
  labs(x = '', y = '') +
  theme(axis.text.x = element_blank())




base2 %>% as_data_frame() %>% 
  filter(X_parent_index >= 1409, idade < 18,
         Você.já.pensou.em.morar.em.outro.lugar. != '',
         Você.já.pensou.em.morar.em.outro.lugar. == 'Sim') %>% 
  count(Depois.de.sair.da.comunidade..você.voltaria.) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = reorder(Depois.de.sair.da.comunidade..você.voltaria., n), y = prop)) +
  geom_bar(stat = 'identity', fill = 'cornflowerblue') +
  geom_text(aes(label = paste0(n)),
            position = position_stack(vjust = 0.5),
            size = 7) +
  caio_theme + coord_flip() +
  scale_fill_brewer(name = '') +
  labs(x = '', y = '%') +
  scale_x_discrete(labels = c('Talvez', 
                              'Provavelmente sim',
                              'Provavelmente não',
                              'De jeito nenhum',
                              'Com certeza')) +
  theme(legend.position = 'none') 


#_-----------------------------
# o que lá tem que não tem aqui
#------------------------------
library(wordcloud2)

base2 %>% 
  filter(X_parent_index >= 1409, idade < 18,
         O.que.tem.lá.que.não.tem.na.sua.comunidade. != '') %>% 
  count(O.que.tem.lá.que.não.tem.na.sua.comunidade.) %>% 
  wordcloud2()


base2 %>% 
  filter(X_parent_index >= 1409, idade < 18, 
         O.que.tem.na.sua.comunidade.que.não.tem.em.outros.lugares. != '') %>% 
  count(O.que.tem.na.sua.comunidade.que.não.tem.em.outros.lugares.) %>% 
  wordcloud2(size = 0.6)

base2 %>% 
  filter(X_parent_index >= 1409, idade < 18, 
         O.que.faz.os.jovens.querer.sair.da.comunidade. != '') %>% 
  count(O.que.faz.os.jovens.querer.sair.da.comunidade.) %>%
  wordcloud2()


#-------------------------
# documentação 
#-------------------------
base2 %>% 
  filter(Município == 'Carauari', idade < 18) %>%
  group_by(Local.de.Aplicação) %>% 
  select(Certidão.de.casamento, 
         Certidão.de.nascimento,
         RG, CPF, Título.de.eleitor,
         Carteira.de.trabalho) %>% 
  melt() %>% 
  group_by(variable, Local.de.Aplicação) %>% 
  count(value) %>% 
  filter(value == 1) %>% 
  ggplot(aes(x = reorder(variable, n), y = n, fill = Local.de.Aplicação)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.9) +
  geom_text(aes(label = paste0(n), hjust = -0.2),
            position = position_dodge(width = 0.8),
            size = 7) +
  scale_x_discrete(labels = c('Certidão de casamento',
                              'Carteira de trabalho',
                              'Título de eleitor',
                              'RG',
                              'CPF',
                              'Certidão de nascimento')) +
  scale_fill_startrek(name = '') +
  blank_theme +
  coord_flip(ylim = c(0, 500)) +
  theme(axis.text.x = element_blank(),
        legend.position = 'top') 

#-------------------------
# status relacionamento
#-------------------------
base2 %>% 
  filter(Município == 'Maraã', idade < 18,
         Status.de.relacionamento %in% c('Amigado', 'Solteiro', 'Namorando')) %>% 
  count(Status.de.relacionamento) %>% 
  mutate(prop = round(n*100/sum(n)), 1) %>% 
  ggplot(aes(x = '', y = prop, fill = Status.de.relacionamento)) +
  geom_bar(stat = 'identity', color = 'black') +
  coord_polar('y') +
  geom_label_repel(aes(label = paste0(prop, '%')),
            position = position_stack(vjust = 0.5),
            size = 8, show.legend = FALSE) +
  labs(x = '', y = '') +
  scale_fill_brewer(name = '', palette =  'OrRd') +
  blank_theme +
  theme(legend.position = 'top',
        legend.text = element_text(size = 17),
        axis.text.y = element_blank(),
        axis.text.x = element_blank())


 # adolescentes grávidas
base2 %>% 
  filter(idade < 18,
         Está.grávida. != '',
         Município == 'Itapiranga') %>% 
  count(Local.de.Aplicação, Está.grávida.) %>% 
  mutate(prop = round(n*100/sum(n), 1)) %>% 
  filter(Está.grávida. == 'Sim') %>% 
  ggplot(aes(x = Local.de.Aplicação, y = n, fill = Local.de.Aplicação)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.78) + 
  geom_label(aes(label = n), size = 14,
             position = position_dodge(width = 0.9), 
             color = 'lightpink2', show.legend = FALSE) +
  labs(x = '', y = '') +
  caio_theme +
  scale_fill_manual(name = '', values = c('black', 'grey30')) +
  theme(legend.text = element_text(size = 18),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold", size = 25, color = 'black'),
        plot.background = element_rect(fill = 'lightpink2'),
        line = element_blank(),
        legend.position = 'none') +
  coord_cartesian(ylim = c(0, 5))


# já possui filho?
base2 %>% 
  filter(idade < 18,
         Está.grávida. != '',
         Município == 'Itapiranga') %>% 
  count(Local.de.Aplicação, Já.possui.filho.) %>% 
  mutate(prop = round(n*100/sum(n),1)) %>% 
  filter(Já.possui.filho. == 'Sim') %>% 
  ggplot(aes(x = Local.de.Aplicação, y = n, fill = Local.de.Aplicação)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.8) + 
  geom_label(aes(label = n), size = 14, 
             position = position_dodge(width = 0.9), 
             color = 'lightpink2', show.legend = FALSE) +
  labs(x = '', y = '') +
  caio_theme +
  scale_fill_manual(name = '', values = c('black', 'grey30')) +
  theme(axis.text.y = element_blank(),
        legend.text = element_text(size = 18),
        axis.text.x = element_text(face = "bold", size = 25, color = 'black'),
        plot.background = element_rect(fill = 'lightpink2'),
        line = element_blank(),
        legend.position = 'none') +
  coord_cartesian(ylim = c(0, 11))



#-----------------------------
#   consumo
#-----------------------------

base2 %>% 
   filter(idade < 18,
         Consomem. != '',
         Município == 'Carauari') %>% 
  count(Consomem., Local.de.Aplicação) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  filter(Consomem. == 'Cigarro') %>% 
  ggplot(aes(x = Local.de.Aplicação, y = n, fill = Local.de.Aplicação)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.9) +
  geom_label(aes(label = n), size = 14, 
            position = position_dodge(width = 0.8), color = 'darkorange4') +
  theme_minimal(base_size = 25) + 
  scale_fill_manual(name = '', values = c('white', 'snow3')) +
  labs(x = '', y = '') + 
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold", size = 25, color = 'white'),
        line = element_blank(),
        legend.position = 'none',
        legend.text = element_text(color = 'white'),
        plot.background = element_rect(fill = 'darkorange4')) +
  coord_cartesian(ylim = c(0, 4))


base2 %>% 
  filter(idade < 18,
         Consomem. != '',
         Município == 'Carauari') %>% 
  count(Consomem., faixa_etária) %>% 
  mutate(prop = n*100/sum(n)) %>% 
  filter(Consomem. == 'Bebidas alcóolicas') %>% 
  ggplot(aes(x = faixa_etária, y = n, fill = faixa_etária)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.9) +
  geom_label(aes(label = n), size = 14, position = position_dodge(width = 0.8), color = 'gray22') +
  theme_minimal(base_size = 25) + 
  scale_fill_manual(name = '', values = c('darkgoldenrod1', 'darkgoldenrod3', 'gold')) +
  labs(x = 'Faixa etária', y = '') +
  theme(axis.text.y = element_blank(),
        axis.title.x = element_text(face = "bold", size = 25, color = 'darkgoldenrod2'),
        axis.text.x = element_text(face = "bold", size = 25, color = 'darkgoldenrod2'),
        line = element_blank(),
        plot.background = element_rect(fill = 'gray22'),
        legend.position = 'none',
        legend.text = element_text(color = 'darkgoldenrod2')) +
  coord_cartesian(ylim = c(0, 10))

#-------------------
# dicara
#------------------

base2 %>% 
  filter(Município == 'Maraã', idade < 18,
         Participa.de.algum.curso..Artesanato != '',
         Participa.de.algum.curso..Educação.Ambiental != '',
         Participa.de.algum.curso..Música != '',
         Participa.de.algum.curso..Esportes != '',
         Participa.de.algum.curso..Outros != '',
         Participa.de.algum.curso..Informática != '') %>%
  select(Participa.de.algum.curso..Artesanato,
         Participa.de.algum.curso..Educação.Ambiental,
         Participa.de.algum.curso..Música,
         Participa.de.algum.curso..Esportes,
         Participa.de.algum.curso..Outros,
         Participa.de.algum.curso..Informática) %>% 
  melt() %>% 
  group_by(variable) %>% 
  count(value) %>% 
  mutate(prop = n*100/sum(n)) %>%
  filter(value == 1) %>% 
  ggplot(aes(x = reorder(variable, -n), y = n)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.8) +
  geom_text(aes(label = paste0(n), vjust = -0.2),
            position = position_dodge(width = 0.8),
            size = 8) +
  scale_x_discrete(labels = c('Informática', 'Música', 'Esportes', 'Artesanato' , 'Educação \nAmbiental','Outros')) +
  blank_theme +
  labs(x = '', y = '') + 
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 20)) +
  coord_cartesian(ylim = c(1, 200))




# crianças deficientes
base2 %>% 
  filter(Município == 'Maraã',
         Possui.alguma.deficiência. != '',
         idade < 18) %>% 
  count(Possui.alguma.deficiência.) %>%
  mutate(prop = n*100/sum(n)) 


base2 %>% 
  filter(Município == 'Maraã',
         Possui.alguma.deficiência. == 1,
         Recebe.algum.benefício.por.causa.da.deficiência. != '') %>% 
  count(Recebe.algum.benefício.por.causa.da.deficiência.) %>%
  mutate(prop = n*100/sum(n)) %>% 
  ggplot(aes(x = '', y = n, fill = as.factor(Recebe.algum.benefício.por.causa.da.deficiência.))) +
  geom_bar(stat = 'identity') + coord_polar('y') +
  geom_text(aes(label = paste0(round(prop, 2), "%")), size = 10,
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(name = 'Recebe algum benefício?', palette = 'Paired',
                    labels = c('Não', 'Sim')) +
  blank_theme +
  theme(axis.text.x = element_blank(),
        legend.position = 'top',
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 17),
        axis.text.y = element_blank())
