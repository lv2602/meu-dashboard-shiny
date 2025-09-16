FROM rocker/shiny:4.4.1

# Instale pacotes R necessários (adicione mais se faltar)
RUN apt-get update && apt-get install -y \
    libgdal-dev \
    libproj-dev \
    libssl-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    && R -e "install.packages(c('sf', 'dplyr', 'rlang', 'tidyr', 'magrittr', 'ggplot2', 'scales', 'reactable', 'plotly', 'readxl', 'stringi', 'htmlwidgets', 'tibble'), repos='https://cran.rstudio.com/')"

# Copie o app e dados direto pra raiz
COPY app.R /srv/shiny-server/
COPY indicadores_rj_certo.xlsx /srv/shiny-server/
COPY Limite_de_Bairros.shp /srv/shiny-server/
COPY Limite_de_Bairros.dbf /srv/shiny-server/
COPY Limite_de_Bairros.shx /srv/shiny-server/
# Adicione mais COPY se tiver outros arquivos de shapefile, ex:
# COPY Limite_de_Bairros.prj /srv/shiny-server/
# COPY Limite_de_Bairros.cpg /srv/shiny-server/

# Exponha a porta
EXPOSE 3838

# Rode o Shiny Server
USER shiny
CMD ["/usr/bin/shiny-server"]
