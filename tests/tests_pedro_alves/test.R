

# Exemplo 00: Gráfico   -------------
remotes::install_github("ipeadata-lab/ipea_templates")


#library(ipeaplot)
library(ggplot2)
library(dplyr)
data("mtcars")

mtcars <- mtcars %>%
  mutate(am = ifelse(am == 1,"Manual","Automático"))


f <- function(k) {
  step <- k
  function(y) seq(floor(min(y)), ceiling(max(y)), by = step)
}



# continuous
ggplot() +
  geom_point(data=mtcars, aes(x=mpg , y=drat, color=drat)) +
  scale_color_ipea(discrete = F, palette = 'Reds') +
  facet_wrap(.~am) +
  labs(x="Weather    stations",
     y="Accumulated Rainfall [mm]",
     title="GRÁFICO 7",
     subtitle="Forma de organização do horário de trabalho",
     caption = 'Fonte: ipea') +
  theme_ipea(legend.position = 'right')


# discrete
ggplot(data=mtcars, aes(x=mpg , y=cyl, color=carb)) +
  geom_point() +
  scale_colour_discrete_ipea()


# Exemplo 01: Gráfico de pontos  -------------

cons = abjData::consumo

# Gráfico simples
ggplot(data=cons, aes(x=dec_date, y=tempo, color=assunto, group = assunto)) +
  geom_point() +
  xlab('Ano') +
  ylab('Tempo') +
  ggtitle(label = "Tempo", subtitle = "Brasil (%)") +
  labs(color = "Assunto") +
  ipea_style(legend.position = "right") +
  scale_color_discrete_ipea()




# Create a continuous scatter plot with 'mpg' on the x-axis, 'wt' on the y-axis, and color-coded by 'wt'
# Use the 'scale_ipea()' function to apply the IPEA continuous color scale
ggplot(data = mtcars, aes(x = mpg, y = wt, color = wt)) +
  geom_point() +
  theme_ipea(type = "continuous", style = "color")


# Create a continuous scatter plot with 'mpg' on the x-axis, 'wt' on the y-axis, and filled by 'wt'
# Use the 'scale_ipea()' function to apply the IPEA continuous fill scale
ggplot(data = mtcars, aes(x = mpg, y = wt, fill = wt)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_ipea(type = "continuous", style = "fill")


# Create a discrete scatter plot with 'mpg' on the x-axis, 'wt' on the y-axis, and color-coded by 'quantile'
# Use the 'scale_ipea()' function to apply the IPEA discrete color scale
ggplot(data = mtcars, aes(x = mpg, y = wt, color = quantile)) +
  geom_point() +
  theme_ipea(type = "discrete", style = "color")



# Gráfico simples (sem legenda)
ggplot(data=cons, aes(x=dec_date, y=tempo, color=assunto, group = assunto)) +
  geom_point() +
  xlab('Ano') +
  ylab('Tempo') +
  ggtitle(label = "Tempo", subtitle = "Brasil (%)")+
  ipea_style(legend.position = "none") +
  scale_color_discrete_ipea()

# Exemplo 02: Gráfico de linha -------------
graph <- abjData::pnud_muni %>%
  group_by(ano) %>%
  dplyr::summarise(espvida = mean(espvida),
                   rdpc = mean(rdpc))

ggplot(data=graph, aes(x=as.character(ano), y=espvida, color = espvida, group = 1)) +
  geom_line() +
  geom_point() +
  xlab('Ano') +
  ylab('Expectativa de vida') +
  theme_ipea(type = 'continuous', style = 'color',
             label = 'espvida',
             legend.position = 'none') +
  ggtitle(label = "Expectativa de vida", subtitle = "Brasil (%)")

# Exemplo 03: Gráfico de barras  -------------

# Exemplo 3.1 - Gráfico padrão  -------------
graph <- abjData::pnud_muni %>%
  group_by(ano) %>%
  dplyr::summarise(espvida = mean(espvida),
                   rdpc = mean(rdpc))

ggplot(data=graph, aes(x=as.character(ano), y=espvida, fill=espvida)) +
  geom_bar(stat="identity") +
  labs(x="Ano",
       y="Expectativa de vida",
       fill = "Expectativa de vida",
       title="Gráfico 1",
       subtitle="Expectativa de vida",
       caption = 'Fonte: ipea') +
  insert_text(label = "espvida", decimals = 0) +
  theme_ipea() +
  scale_fill_ipea()

ggplot() +
  geom_point(data=mtcars, aes(x=mpg , y=cyl, color=carb)) +
  scale_color_ipea(discrete = F, palette = 'ipeatd') +
  facet_wrap(.~am) +

  theme_ipea(legend.position = 'right')

# Exemplo 3.1 - Gráfico para cada UF  -------------
graph <- abjData::pnud_muni %>%
  mutate(regn = substr(uf,1,1),
         regn = case_when(regn == "1"~"Norte",
                          regn == "2"~"Nordeste",
                          regn == "3"~"Sudeste",
                          regn == "4"~"Sul",
                          regn == "5"~"Centro Oeste", TRUE ~ regn)) %>%
  group_by(regn,ano) %>%
  dplyr::summarise(espvida = mean(espvida),
                   rdpc = mean(rdpc))

# Opção 1 de cor do título
ggplot(data=graph, aes(x=as.character(ano), y=espvida, fill=espvida)) +
  geom_bar(stat="identity") +
  xlab('Ano') +
  ylab('Expectativa de vida') +
  insert_text(label = "espvida", decimals = 0) +
  facet_wrap(.~regn) +
  scale_y_continuous(labels = scales::label_comma(decimal.mark = ",", big.mark = ".")) +
  ggtitle(label = "Expectativa de vida", subtitle = "Brasil (%)")+
  ipea_style(box_option = 0) +
  scale_fill_continuous_ipea() +
  coord_cartesian(ylim = c(min(graph$espvida),max(graph$espvida)+10))

# Opção 2 de cor do título
ggplot(data=graph, aes(x=as.character(ano), y=espvida, fill=espvida)) +
  geom_bar(stat="identity") +
  xlab('Ano') +
  ylab('Expectativa de vida') +
  insert_text(label = "espvida", decimals = 0) +
  facet_wrap(.~regn) +
  scale_y_continuous(labels = scales::label_comma(decimal.mark = ",", big.mark = ".")) +
  ggtitle(label = "Expectativa de vida", subtitle = "Brasil (%)")+
  ipea_style() +
  scale_fill_continuous_ipea() +
  coord_cartesian(ylim = c(min(graph$espvida),max(graph$espvida)+10))


# Exemplo 4 - Mapa do Brasil  -------------
mun = geobr::read_municipality()
uf  = geobr::read_state()
pnud = abjData::pnud_muni %>%
  subset(ano == 2010) %>%
  select(ano,code_muni = codmun7,e_anosestudo,rdpc)
df = left_join(mun, pnud)
a = subset(df,abbrev_state == "PB")

graf2 <- ggplot(data = a, aes(fill = e_anosestudo)) +
  geom_sf() +
  #geom_sf(data = uf, color = "black", fill = NA) +
  ggtitle("Anos de estudo", "Em anos") +
  xlab("")+ylab("") +
  ipea_style(axis = "full", text = F) +
  scale_fill_continuous_ipea( options = c("ipea1"))

graf2

# Caso deseja mudar a variação da escala
graf2 <- ggplot(data = df, aes(fill = e_anosestudo)) +
  geom_sf(color = NA) +
  geom_sf(data = uf, color = "black", fill = NA) +
  ggtitle("Anos de estudo", "Em anos") +
  xlab("")+ylab("") +
  theme_ipea(axis = "none", text = F,
             show.limits = T,name = "Anos de estudo",
             type = 'continuous', style = 'fill')
graf2
