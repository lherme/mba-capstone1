library(dplyr)
library(tidyverse)
library(tidyr)
library(stringr)


load("cpopg.RData")

# função para classificar a decisão

class_decisao <- function(x) {
  dct <- stringr::str_detect
  desfavoravel <- "nego prov|improced|indef[ei]r\\w+|deneg" %>%
    stringr::str_c("nega-se|não prov|negado prov", sep = "|") %>%
    stringr::str_c("nega prov", sep = "|") %>%
    stringr::regex(ignore_case = TRUE)
  parcial <- stringr::regex("parcial|em parte", ignore_case = TRUE)
  favoravel <- stringr::regex("def[ei]r|acolh|provido|dar prov|conced\\w+|imediata [transfer\\w+|disponibiliza\\w+|interna\\w+]|mantenho", ignore_case = TRUE)
  prejudicado <- stringr::regex("julg+ prejud", ignore_case = TRUE)
  desistencia <- stringr::regex("desist|renún|desnecessidade", ignore_case = TRUE)
  arquiva <- stringr::regex("arquivem\\-se|arquiva\\w+|ao arquiv\\w+", ignore_case = TRUE)
  extincao <- stringr::regex("extin|prescri", ignore_case = TRUE)
  cumprimento <- stringr::regex("cumpra\\-se|cumprimento", ignore_case = TRUE)
  dplyr::case_when(
    dct(x, parcial) ~ "Decisão Parcialmente Favorável",
    dct(x, desfavoravel) ~ "Decisão Desfavorável",
    dct(x, favoravel) ~ "Decisão Favorável",
    dct(x, prejudicado) ~ "Prejudicado",
    dct(x, desistencia) ~ "Desistência",
    dct(x, arquiva) ~ "Arquivamento",
    dct(x, extincao) ~ "Extinção",
    dct(x, cumprimento) ~ "Pede cumprimento",
    TRUE ~ "outro"
  )
}

# função para o glossário da decisão

glossario_decisao <- function(x){
  dct <- stringr::str_detect
  reg <- stringr::regex
  dplyr::case_when(
    dct(x, reg("parcialmente", ignore_case = TRUE)) ~ "Significa que o juiz ou a juíza entendeu que apenas uma parte do pedido feito pelo autor do processo é procedente. Ou seja, a pessoa que entrou com o processo ganhou a causa, mas não ganhou exatamente tudo o que pediu.",
    dct(x, reg("desfavorável", ignore_case = TRUE)) ~ "Significa que o juiz ou a juíza entendeu que o pedido feito pelo autor do processo não é procedente. Ou seja, a pessoa que entrou com o processo perdeu a causa.",
    dct(x, reg("favorável", ignore_case = TRUE)) ~ "Significa que o juiz ou a juíza entendeu que o pedido feito pelo autor do processo é procedente. Ou seja, a pessoa que entrou com o processo ganhou a causa.",
    dct(x, reg("prejudicado", ignore_case = TRUE)) ~ "Significa que o juiz ou a juíza entendeu que o pedido feito pelo autor do processo está prejudicado e não poderá ser julgado.",
    dct(x, reg("desistência", ignore_case = TRUE)) ~ "Significa que o juiz ou a juíza determinou o encerramento do processo porque o autor ou a autora desistiu da ação.",
    dct(x, reg("arquivamento", ignore_case = TRUE)) ~ "Significa que o juiz ou a juíza determinou que o processo seja arquivado. Isso ocorre nas situações em que não há expectativa de prosseguimento do processo.",
    dct(x, reg("extinção", ignore_case = TRUE)) ~ "Significa que o juiz ou a juíza determinou a extinção do processo, porque ele não possui alguns requisitos previstos em lei. Não significa que o autor tenha ganhado ou perdido a causa, mas, apenas, que o processo não poderá prosseguir.",
    dct(x, reg("cumprimento", ignore_case = TRUE)) ~ "Significa que o juiz ou a juíza determinou o cumprimento de uma decisão tomada anteriormente no processo. Nesse caso, verifique as decisões anteriores.",
    TRUE ~ "outro"
  )
}

mov_cpopg <- cpopg %>% 
  select(id_processo, movimentacoes) %>% 
  unnest(movimentacoes) %>% 
  mutate(classificacao_decisao = case_when(
    str_detect(movimento,"^Decisão$") ~ class_decisao(descricao),
    TRUE ~ NA_character_
  )) %>% 
  mutate(glossario = case_when(
    !is.na(classificacao_decisao) ~ glossario_decisao(classificacao_decisao),
    TRUE ~ NA_character_
  ))

