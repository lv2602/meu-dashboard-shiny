# Imagem base com shiny-server + R 4.4.1
FROM rocker/shiny:4.4.1

# Evita prompts interativos
ENV DEBIAN_FRONTEND=noninteractive

# Sistemas para {sf} e afins
RUN apt-get update && apt-get install -y --no-install-recommends \
    gdal-bin \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    libssl-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
 && rm -rf /var/lib/apt/lists/*

# Pacotes R do app
RUN R -e "install.packages(c( \
    'sf','dplyr','rlang','tidyr','magrittr','ggplot2','scales', \
    'reactable','plotly','readxl','stringi','htmlwidgets','tibble' \
  ), repos='https://cran.rstudio.com/')"

# Copia o app e os dados para o diretório padrão do shiny-server
# (Se preferir, troque por COPY ./ /srv/shiny-server/)
COPY app.R /srv/shiny-server/
COPY indicadores_rj_certo.xlsx /srv/shiny-server/
COPY Limite_de_Bairros.shp /srv/shiny-server/
COPY Limite_de_Bairros.dbf /srv/shiny-server/
COPY Limite_de_Bairros.shx /srv/shiny-server/
COPY Limite_de_Bairros.prj /srv/shiny-server/

# (Opcional) defina paths via env vars que seu app.R já entende
ENV EDU_DATA_PATH=/srv/shiny-server/indicadores_rj_certo.xlsx
ENV EDU_SHP_PATH=/srv/shiny-server/Limite_de_Bairros.shp

# Permissões para o usuário 'shiny'
RUN chown -R shiny:shiny /srv/shiny-server \
 && mkdir -p /var/lib/shiny-server/bookmarks/shiny \
 && chown -R shiny:shiny /var/lib/shiny-server

EXPOSE 3838

USER shiny
CMD ["/usr/bin/shiny-server"]
