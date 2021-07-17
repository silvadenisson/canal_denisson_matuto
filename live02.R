
# verificando dados de vacinação
# fontes de erros em possiveis análises

# caso de exemplo vacinas aplicada em alagoas
# fonte de dados opendataus
# https://opendatasus.saude.gov.br/dataset/covid-19-vacinacao/resource/ef3bd0b8-b605-474b-9ae5-c97390c197a8

# limpando a área
rm(list = ls())

# carregando pacotes
library(data.table)
library(ggplot2)
library(stringr)

# carregando dados 
vacinas <- fread("vacinas.csv", encoding = "Latin-1")

str(vacinas)

summary(vacinas$paciente_idade)

View(vacinas[paciente_idade > 100, ])

# "b5cfdb3a5deddf8921d8db85abc4e5c1b7c6e6a324b67675b8f5456298a78118"

View(vacinas[paciente_idade < 18, ])
# 9fd62e48d5eb3d9000a2fd6c2faf8204474925e47141ff18fd9fef8302002a7f


#vericando casos de vacinacao duplicada e outros casos estranhos
vac <- dcast(vacinas, paciente_id ~ vacina_descricao_dose)

# selcionando casos onde a idade do paciente é maior qur 17 anos e menor que 110 amos
vacinas <- vacinas[paciente_idade > 17 & paciente_idade < 110, ]

# criando uma nova variavel que indica se vacinação completa ou parcial
vacinas$vacinacao_completa <- ifelse(str_detect(vacinas$vacina_descricao_dose, "1"), 
                                     "parcial",  "completa" )

# separando as variaveis de interesse pelo id da coluna
vacinas <- unique(vacinas[, c(2, 19, 28, 35)])

# agregando (somando/contando) a quantidade de vacinas aplicadas por dia e pelos status de vacinação parcial ou completa
vac_al <- vacinas[, .N, by = list(vacina_dataaplicacao, vacinacao_completa)]

# ordenando a base de dados pela data da vacina
vac_al <- vac_al[order(vacina_dataaplicacao), ]

# criando uma variavel com a soma acumulada dia a dia, pelo status
vac_al <- vac_al[, acumulada := cumsum(N), by = vacinacao_completa ]

# retirando a notação cientifica da exibição do resultados
options(scipen = 9999)

# criando um grafico da evolução da vacinacao em alagoas comparando pelo status de completa ou parcial
ggplot(vac_al, aes(vacina_dataaplicacao, acumulada, color = vacinacao_completa)) + geom_line() +
  theme_classic() +
  scale_color_manual(name = "", values = c( "blue", "red")) +
  labs(x = "", y = "N acumulado") +
  scale_y_continuous(limits = c(0, 3000000)) +
  geom_hline(yintercept = 2100000, linetype = 2)

