## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = identical(tolower(Sys.getenv("NOT_CRAN")), "true"),
  out.width = "100%"
)

# Function that aligns color palettes
pal <- function(col, border = "light gray") {
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE,
       xlab = "", ylab = "")
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}

## ----eval=FALSE, message=FALSE, warning=FALSE---------------------------------
#  # Development version
#  utils::remove.packages('ipeaplot')
#  remotes::install_github("ipeadata-lab/ipeaplot")

## ----eval=TRUE, message=FALSE, warning=FALSE, results='hide'------------------
# Load packages
library(ipeaplot)
library(ggplot2)
library(dplyr)
library(abjData)
library(geobr)
library(patchwork)

# Load mtcars dataset
data(mtcars)

## ---- message=FALSE, warning=FALSE, fig.align="center", out.width = "100%"----
#  fig_raw <- ggplot() +
#                geom_point(data = mtcars, aes(x = hp , y = mpg, color = cyl)) +
#                labs(y='Consumo de Combustível (milhas por galão)',
#                     x ='Potência (Número de cavalos)',
#                     color='Cilindradas')
#  
#  fig_base <- fig_raw +
#                scale_color_ipea() +
#                theme_ipea()
#  
#  fig_base
#  

## ----eval=TRUE, echo=FALSE, fig.dim = c(5, 4),fig.align = "center", out.width = "50%"----
# Salvar as configurações originais
old.par <- par(no.readonly = TRUE)

# Modificar as configurações conforme necessário
par(mfrow = c(3, 1), mar = c(1, 0, 2, 0))
pal(ipea_pal(palette = "Blue")(9)); mtext("Blue")
pal(ipea_pal(palette = "Green")(9)); mtext("Green")
pal(ipea_pal(palette = "Orange")(9)); mtext("Orange")

# Restaurar as configurações originais
par(old.par)

## ----eval=TRUE, echo=FALSE, fig.dim = c(5, 4),fig.align = "center", out.width = "50%"----
# Salvar as configurações originais
old.par <- par(no.readonly = TRUE)

# Modificar as configurações conforme necessário
par(mfrow = c(4, 1), mar = c(1, 0, 2, 0))
pal(ipea_pal(palette = "Red-Blue")(9)); mtext("Red-Blue")
pal(ipea_pal(palette = "Orange-Blue")(9)); mtext("Orange-Blue")
pal(ipea_pal(palette = "Green-Blue")(9)); mtext("Green-Blue")

# Restaurar as configurações originais
par(old.par)

## ---- message=FALSE, warning=FALSE, fig.align="center", out.width = "100%"----
#  # paleta sequencial verde
#  fig_base + scale_color_ipea(palette = "Green")

## ---- message=FALSE, warning=FALSE, fig.align="center", out.width = "100%"----
#  # paleta divergente de laranja a azul
#  fig_base + scale_color_ipea(palette = "Orange-Blue")

## ----eval=TRUE----------------------------------------------------------------
df <- abjData::pnud_muni

## ---- message=FALSE, warning=FALSE, fig.align="center", out.width = "100%"----
#  # cria variavel identificando a regiao de cada municipio
#  df <- df |>
#        mutate(regiao = substring(uf, 1, 1),
#               regiao = case_when(regiao == 1 ~ 'Norte',
#                                  regiao == 2 ~ 'Nordeste',
#                                  regiao == 3 ~ 'Sudeste',
#                                  regiao == 4 ~ 'Sul',
#                                  regiao == 5 ~ 'Centro Oeste'))
#  
#  # calcula media de colega de esgoto por ano e regiao
#  df_fig1 <- df |>
#             mutate(regiao = substring(uf, 1, 1)) |>
#             group_by(ano, regiao) |>
#             summarise( t_lixo = weighted.mean(x=t_lixo, w = pop)) |>
#             collect()
#  
#  # plot
#  ggplot() +
#    geom_line(data = df_fig1, aes(x=ano, y=t_lixo, color= regiao)) +
#    scale_color_ipea(palette = 'Orange') +
#    labs(title = 'Proporção da população com coleta de lixo', color='Região') +
#    ylab('Proporção em %') +
#    xlab('Ano') +
#    theme_ipea()
#  

## ---- message=FALSE, warning=FALSE, fig.align="center", out.width = "100%"----
#  ggplot() +
#    geom_col(data = df_fig1, aes(x=ano, y=t_lixo, fill= factor(ano))) +
#    scale_fill_ipea(palette = 'Green') +
#    labs(title = 'Proporção da população com coleta de lixo', fill='Ano') +
#    ylab('Proporção em %') +
#    xlab('Ano') +
#    facet_wrap(. ~ regiao) +
#    theme_ipea(x_breaks = 3)
#  

## ----eval=TRUE, message=FALSE, warning=FALSE, results='hide'------------------
# Load municipality and state spatial data
mun <- geobr::read_municipality(year = 2010)
uf  <- geobr::read_state(year = 2010)

## ----eval=TRUE, message=FALSE, warning=FALSE, results='hide'------------------
# Load municipality and state spatial data
mun = read_municipality()
uf  = read_state()

# Subset and select specific columns from the 'pnud_muni' dataset
df_escola <- df |>
             subset(ano == 2010) %>%
             select(ano, code_muni = codmun7, e_anosestudo)

# Perform a left join between the 'mun' and 'pnud' data frames
df3 <- dplyr::left_join(mun, df_escola, by = 'code_muni')


## ---- message=FALSE, warning=FALSE, fig.align="center", out.width = "100%"----
#  ggplot() +
#    geom_sf(data = df3, aes(fill = e_anosestudo), color = NA) +
#    geom_sf(data = uf, color = "black", fill = NA) +
#    ggtitle("Média de anos de estudo") +
#    scale_fill_ipea(palette = 'Orange-Blue',
#                    name='Anos de\nestudo') +
#    theme_ipea(axis_lines = 'none', include_ticks = F, axis_values = F)

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  save_eps(fig2,
#           file.name = "figura_2.eps")
#  save_pdf(fig2,
#           file.name = "figura_2.pdf")
#  

