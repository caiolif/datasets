library(RCurl)

dadosPIR <- read.table(text = getURL('https://raw.githubusercontent.com/caiolif/datasets/master/dadosPIR.txt', .encoding = 'UTF-8'),
                    header = TRUE, sep = '\t') %>% as_data_frame()

dadosPIR <- read.table('clipboard', header = TRUE, sep = '\t')
dadosPIR$Sexo[c(2814, 2858)] <- rep('Feminino', 2)

names(dadosPIR)

# município
dadosPIR %>% 
  count(Município) %>% 
  ggplot(aes(x = Município, y = n)) +
  geom_bar(stat = 'identity')


# comunidades
dadosPIR %>% 
  group_by(Município) %>% 
  count(Comunidade.residência) %>% 
  count(Município)


# gênero
dadosPIR %>% 
  group_by(Município) %>% 
  mutate(Sexo = factor(Sexo, levels = c('F', 'Feminino', 'M', 'Masculino'),
                       labels = c('Feminino', 'Feminino', 'Masculino', 'Masculino'))) %>% 
  count(Sexo) %>% 
  mutate(prop = n*100/sum(n))


# nº de matriculados por ano
dadosPIR %>% 
  count(Ano)

# curso
dadosPIR %>% 
  group_by(Município) %>% 
  count(Curso) %>% 
  mutate(prop = n*100/sum(n))

# idade
dadosPIR %>% 
  group_by(Município) %>% 
  summarise(média = mean(Idade, na.rm = TRUE))
