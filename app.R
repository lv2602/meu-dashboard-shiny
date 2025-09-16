# App Shiny Educacional RJ

# Função pra logar erros
log_error <- function(e) {
  cat("ERRO NA INICIALIZAÇÃO:\n", conditionMessage(e), "\n")
  stop(e)
}

tryCatch({
  # Caminhos de dados (simplificado para raiz, sem pasta data)
  options(
    edu_data_path = file.path(".", "indicadores_rj_certo.xlsx"),
    edu_shp_path  = file.path(".", "Limite_de_Bairros.shp")
  )

  suppressPackageStartupMessages({
    library(shiny); library(bslib)
    library(sf)
    library(dplyr); library(rlang); library(tidyr); library(magrittr)
    library(ggplot2); library(scales); library(reactable); library(plotly)
    library(readxl); library(stringi); library(htmlwidgets)
    library(tibble)
  })

  # Helpers gerais
  normalize_name <- function(x) x |> as.character() |> trimws() |> toupper() |>
    stringi::stri_trans_general("Latin-ASCII")
  canon <- function(v) stringi::stri_trans_general(toupper(trimws(v)), "Latin-ASCII")
  as_num <- function(x) { if (is.numeric(x)) return(x); x <- as.character(x); x <- gsub(",", ".", x); suppressWarnings(as.numeric(x)) }

  rename_by_alias <- function(df, aliases) {
    original <- names(df); c_map <- canon(original)
    for (target in names(aliases)) {
      if (target %in% names(df)) next
      opts <- canon(aliases[[target]]); hit <- which(c_map %in% opts)
      if (length(hit)) { names(df)[hit[1]] <- target; c_map[hit[1]] <- canon(target) }
    }
    df
  }

  # Captura o nome do bairro de forma robusta
  choose_bairro_name <- function(sfobj) {
    cands <- c("NM_BAIRRO","NOME_BAIRRO","NOME","BAIRRO","NM_AREA",
               "NM_AREA_EDU","BAIRRO_NM","NMBAIRRO","BAIRRO_NOME","NM_BAIR")
    nm   <- names(sfobj)
    cnm  <- canon(nm)
    opts <- canon(cands)
    
    hit <- which(cnm %in% opts)
    if (length(hit)) return(as.character(sfobj[[ nm[hit[1]] ]]))
    
    hit2 <- which(grepl("BAIRRO", cnm, fixed = TRUE) | grepl("BAIR", cnm, fixed = TRUE))
    if (length(hit2)) return(as.character(sfobj[[ nm[hit2[1]] ]]))
    
    paste("Bairro", seq_len(nrow(sfobj)))
  }

  make_pct_axis <- function(x) {
    x <- x[is.finite(x)]
    if (!length(x)) return(list(lims = c(0, 100), breaks = c(0, 25, 50, 75, 100)))
    if (max(x, na.rm = TRUE) <= 1.2) x <- x * 100
    lo <- floor(max(0, min(x, na.rm = TRUE))); hi <- ceiling(min(100, max(x, na.rm = TRUE)))
    span <- hi - lo
    if (span < 30) { mid <- (lo + hi) / 2; lo <- max(0, floor(mid - 20)); hi <- min(100, ceiling(mid + 20)) }
    step <- if (span >= 80) 50 else if (span >= 50) 25 else if (span >= 40) 20 else if (span >= 20) 10 else 5
    br <- seq(lo, hi, by = step); if (tail(br, 1) < hi) br <- c(br, hi)
    list(lims = c(lo, hi), breaks = br)
  }

  # Shapefile / RDS otimizado
  load_bairros_shapefile <- function() {
    rds <- file.path(".", "bairros_simplificado.rds")
    if (file.exists(rds)) {
      sfobj <- readRDS(rds)
      if (is.na(sf::st_crs(sfobj))) sf::st_crs(sfobj) <- 4326
      if (!"nome_bairro" %in% names(sfobj)) sfobj$nome_bairro <- choose_bairro_name(sfobj)
      return(sfobj)
    }
    shp <- getOption("edu_shp_path", file.path(".","Limite_de_Bairros.shp"))
    if (!file.exists(shp)) {
      stop("Shapefile (.shp) não encontrado em: ", shp,
           "\nColoque os arquivos na raiz do projeto.", call. = FALSE)
    }
    base <- sub("\\.shp$", "", shp, ignore.case = TRUE)
    need <- c(".dbf", ".shx")
    missing <- need[!file.exists(paste0(base, need))]
    if (length(missing)) {
      stop("Shapefile incompleto. Falta(m): ", paste(missing, collapse = ", "), call. = FALSE)
    }
    sfobj <- sf::st_read(shp, quiet = TRUE)
    if (any(!sf::st_is_valid(sfobj))) {
      tmp <- try(sf::st_make_valid(sfobj), silent = TRUE)
      if (!inherits(tmp, "try-error")) sfobj <- tmp
    }
    epsg_now <- tryCatch(as.integer(sf::st_crs(sfobj)$epsg), error = function(e) NA_integer_)
    if (is.na(epsg_now)) sf::st_crs(sfobj) <- 4326 else if (epsg_now != 4326) sfobj <- sf::st_transform(sfobj, 4326)
    sfobj$nome_bairro <- choose_bairro_name(sfobj)
    if (is.null(sfobj$ID_BAIRRO)) sfobj$ID_BAIRRO <- as.character(seq_len(nrow(sfobj)))
    
    try({
      vcount <- sum(lengths(sf::st_geometry(sfobj)))
      if (is.finite(vcount) && vcount > 2e5) {
        sft <- sf::st_transform(sfobj, 31983)
        sft <- sf::st_simplify(sft, dTolerance = 50, preserveTopology = TRUE)
        sfobj <- sf::st_transform(sft, 4326)
      }
    }, silent = TRUE)
    
    sfobj
  }

  # Excel
  DATA_XLSX <- getOption("edu_data_path", file.path(".", "indicadores_rj_certo.xlsx"))
  if (!file.exists(DATA_XLSX)) stop("Excel não encontrado em: ", DATA_XLSX, "\nColoque o arquivo na raiz.", call. = FALSE)

  # Carrega dados geográficos
  polys <- load_bairros_shapefile()

  # Lê Excel (detecta abas por nome aproximado)
  norm_caps <- function(x) stringi::stri_trans_general(toupper(trimws(x)), "Latin-ASCII")
  sheets  <- readxl::excel_sheets(DATA_XLSX); nsheets <- norm_caps(sheets)
  pick <- function(...) { aliases <- norm_caps(c(...)); for (a in aliases) { hit <- which(grepl(a, nsheets, fixed = TRUE)); if (length(hit)) return(sheets[hit[1]]) }; NA_character_ }

  # Indicadores
  s_enem   <- pick("ENEM","ENEM_MEDIA")
  s_ideb   <- pick("IDEB","IDEB_ANOS_FINAIS")
  s_saebp  <- pick("SAEB_PORTUGUES","SAEB PORTUGUES")
  s_saebm  <- pick("SAEB_MATEMATICA","SAEB MATEMATICA")

  # Fatores
  s_aprov        <- pick("APROVACAO_3SERIE","APROVACAO 3 SERIE")
  s_fac_turma    <- pick("ALUNO_POR_TURMA","ALUNO POR TURMA")
  s_fac_prof     <- pick("ALUNO_POR_PROFESSOR","ALUNO POR PROFESSOR")
  s_fac_sala     <- pick("ALUNO_POR_SALA","ALUNO POR SALA")
  s_fac_ar       <- pick("% SALAS_COM_AR","PCT_SALAS_AR","SALAS_COM_AR","% SALAS COM AR")
  s_fac_integral <- pick("% TURMAS_COM_TEMPO_INTEGRAL","PCT_TURMAS_TEMPO_INTEGRAL","% TURMAS COM TEMPO INTEGRAL")
  s_coords <- pick("ESCOLAS_COORDS","ESCOLAS COORD","COORDENADAS","ESCOLAS","COORD ESCOLAS")

  need <- c(ENEM=s_enem, IDEB=s_ideb, SAEB_Portugues=s_saebp, SAEB_Matematica=s_saebm)
  if (anyNA(need)) stop("Abas de indicadores não encontradas: ", paste(names(need)[is.na(need)], collapse=", "), call. = FALSE)

  # Leitura dos indicadores
  d <- list(
    enem   = readxl::read_excel(DATA_XLSX, sheet = s_enem)  |> as.data.frame(),
    ideb   = readxl::read_excel(DATA_XLSX, sheet = s_ideb)  |> as.data.frame(),
    saebp  = readxl::read_excel(DATA_XLSX, sheet = s_saebp) |> as.data.frame(),
    saebm  = readxl::read_excel(DATA_XLSX, sheet = s_saebm) |> as.data.frame()
  )

  aliases_enem <- list(
    nome_escola=c("NOME_ESCOLA","ESCOLA","UNIDADE","INSTITUICAO","NOME","ESC"),
    enem_media =c("ENEM_MEDIA","MEDIA","NOTA_MEDIA","ENEM","MEDIA_ENEM","MEDIA GERAL")
  )
  aliases_ideb <- list(
    nome_escola=aliases_enem$nome_escola,
    ideb_anos_finais=c("IDEB_ANOS_FINAIS","IDEB","IDEB 9","IDEB_9","IDEB 9°","IDEB FINAL")
  )
  aliases_saebp <- list(
    nome_escola=aliases_enem$nome_escola,
    saeb_portugues=c("SAEB_PORTUGUES","SAEB PORTUGUES","PORTUGUES","LP","LINGUA PORTUGUESA","SAEB LP")
  )
  aliases_saebm <- list(
    nome_escola=aliases_enem$nome_escola,
    saeb_matematica=c("SAEB_MATEMATICA","SAEB MATEMATICA","MATEMATICA","MAT","SAEB MAT")
  )

  enem_df      <- rename_by_alias(d$enem,  aliases_enem)
  ideb_df      <- rename_by_alias(d$ideb,  aliases_ideb)
  saeb_port_df <- rename_by_alias(d$saebp, aliases_saebp)
  saeb_mat_df  <- rename_by_alias(d$saebm, aliases_saebm)

  for (nm in c("enem_df","ideb_df","saeb_port_df","saeb_mat_df")) {
    df <- get(nm); df$nome_escola <- as.character(df$nome_escola); assign(nm, df)
  }
  enem_df$key      <- normalize_name(enem_df$nome_escola)
  ideb_df$key      <- normalize_name(ideb_df$nome_escola)
  saeb_port_df$key <- normalize_name(saeb_port_df$nome_escola)
  saeb_mat_df$key  <- normalize_name(saeb_mat_df$nome_escola)

  enem_df$enem_media          <- as_num(enem_df$enem_media)
  ideb_df$ideb_anos_finais    <- as_num(ideb_df$ideb_anos_finais)
  saeb_port_df$saeb_portugues <- as_num(saeb_port_df$saeb_portugues)
  saeb_mat_df$saeb_matematica <- as_num(saeb_mat_df$saeb_matematica)

  # KPIs / Tops
  media_real_enem <- round(mean(enem_df$enem_media, na.rm = TRUE), 2)
  media_ideb      <- round(mean(ideb_df$ideb_anos_finais, na.rm = TRUE), 2)
  media_saeb_port <- round(mean(saeb_port_df$saeb_portugues, na.rm = TRUE), 2)
  media_saeb_mat  <- round(mean(saeb_mat_df$saeb_matematica, na.rm = TRUE), 2)

  top_enem      <- enem_df      |> arrange(desc(enem_media))       |> slice_head(n = 10) |> mutate(ranking = row_number())
  top_ideb      <- ideb_df      |> arrange(desc(ideb_anos_finais)) |> slice_head(n = 10) |> mutate(ranking = row_number())
  top_saeb_port <- saeb_port_df |> arrange(desc(saeb_portugues))   |> slice_head(n = 10) |> mutate(ranking = row_number())
  top_saeb_mat  <- saeb_mat_df  |> arrange(desc(saeb_matematica))  |> slice_head(n = 10) |> mutate(ranking = row_number())

  # FATORES (dedupe por escola)
  schools_df <- NULL
  if (!is.na(s_coords)) {
    coord_alias <- list(
      nome_escola = c("NOME_ESCOLA","ESCOLA","NOME","UNIDADE","INSTITUICAO"),
      lat         = c("LAT","LATITUDE","Y"),
      lon         = c("LON","LONG","LONGITUDE","X"),
      cep         = c("CEP","CODIGO_POSTAL")
    )
    tmp <- readxl::read_excel(DATA_XLSX, sheet = s_coords) |> as.data.frame()
    tmp <- rename_by_alias(tmp, coord_alias)
    if (all(c("nome_escola","lat","lon") %in% names(tmp))) {
      tmp <- tmp |>
        mutate(
          nome_escola = as.character(nome_escola),
          lat = as_num(lat), lon = as_num(lon),
          key = normalize_name(nome_escola),
          cep_fmt = if ("cep" %in% names(tmp)) {
            cep <- gsub("\\D","", as.character(cep)); ifelse(nchar(cep)==8, paste0(substr(cep,1,5),"-",substr(cep,6,8)), cep)
          } else NA_character_
        ) |>
        filter(is.finite(lat), is.finite(lon))
      if (nrow(tmp)) schools_df <- tmp
    }
  }

  drop_ids_cols <- function(df) {
    nm <- toupper(gsub("\\s","", names(df))); kill <- which(nm %in% c("IDS","ID"))
    if (length(kill)) df <- df[ , -kill, drop = FALSE]; df
  }

  read_factor_onesheet <- function(sheet, prefer_names, out_col) {
    if (is.na(sheet)) return(NULL)
    raw <- suppressWarnings(readxl::read_excel(DATA_XLSX, sheet = sheet)) |> as.data.frame()
    if (!nrow(raw)) return(NULL)
    names(raw) <- trimws(names(raw))
    raw <- rename_by_alias(raw, list(nome_escola = c("NOME_ESCOLA","ESCOLA","UNIDADE","INSTITUICAO","NOME")))
    raw <- drop_ids_cols(raw)
    cm <- canon(names(raw)); want <- canon(prefer_names); hit <- which(cm %in% want)
    col <- if (length(hit)) names(raw)[hit[1]] else NULL
    if (is.null(col)) {
      cand <- setdiff(names(raw), "nome_escola")
      num_ok <- cand[vapply(cand, function(cc) is.numeric(as_num(raw[[cc]])), logical(1))]
      if (length(num_ok)) col <- num_ok[1]
    }
    if (is.null(col)) return(NULL)
    raw$key <- normalize_name(as.character(raw$nome_escola)); val <- as_num(raw[[col]])
    df <- tibble(key = raw$key, v = val) |>
      group_by(key) |> summarise(v = if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE), .groups = "drop")
    names(df)[names(df)=="v"] <- out_col; df
  }

  fac_aprov      <- read_factor_onesheet(s_aprov,        c("aprovacao_3serie","APROVACAO_3SERIE","APROVACAO 3 SERIE"), "aprovacao_3serie")
  fac_turma      <- read_factor_onesheet(s_fac_turma,    c("aluno_por_turma","ALUNO_POR_TURMA","ALUNO POR TURMA"), "aluno_por_turma")
  fac_prof       <- read_factor_onesheet(s_fac_prof,     c("aluno_por_professor","ALUNO_POR_PROFESSOR","ALUNO POR PROFESSOR"), "aluno_por_professor")
  fac_sala       <- read_factor_onesheet(s_fac_sala,     c("aluno_por_sala","ALUNO_POR_SALA","ALUNO POR SALA"), "aluno_por_sala")
  fac_ar         <- read_factor_onesheet(s_fac_ar,       c("% salas_com_Ar","% SALAS_COM_AR","% SALAS COM AR","SALAS_COM_AR"), "% salas_com_Ar")
  fac_integral   <- read_factor_onesheet(s_fac_integral, c("% turmas_com_tempo_integral","% TURMAS COM TEMPO INTEGRAL","% TURMAS COM TEMPO INTEGRAL"), "% turmas_com_tempo_integral")

  fac_list <- Filter(Negate(is.null), list(fac_aprov, fac_turma, fac_prof, fac_sala, fac_ar, fac_integral))
  fac_df <- if (length(fac_list)) Reduce(function(x,y) full_join(x,y, by="key"), fac_list) else tibble(key=character())

  # Labels
  label_map <- c(
    enem_media = "Média ENEM",
    ideb_anos_finais = "IDEB Anos finais",
    saeb_portugues = "SAEB Português",
    saeb_matematica = "SAEB Matemática"
  )
  label_factor <- c(
    aprovacao3serie        = "Aprovação 3ª série (%)",
    alunoperturma          = "Aluno por Turma",
    alunoporprofessor      = "Aluno por Professor",
    alunoporsala           = "Aluno por Sala",
    pctsalasar             = "% Salas c/ Ar",
    pctturmastempointegral = "% Turmas TI"
  )

  # Helpers para dataframes exibidos
  make_school_indicator_df <- function(educ_var) {
    switch(
      educ_var,
      enem_media       = enem_df      %>% select(key, nome_escola, indicador = enem_media),
      ideb_anos_finais = ideb_df      %>% select(key, nome_escola, indicador = ideb_anos_finais),
      saeb_portugues   = saeb_port_df %>% select(key, nome_escola, indicador = saeb_portugues),
      saeb_matematica  = saeb_mat_df  %>% select(key, nome_escola, indicador = saeb_matematica)
    )
  }
  make_school_factor_df <- function(factor_var) {
    col_map <- c(
      aprovacao3serie        = "aprovacao_3serie",
      alunoperturma          = "aluno_por_turma",
      alunoporprofessor      = "aluno_por_professor",
      alunoporsala           = "aluno_por_sala",
      pctsalasar             = "% salas_com_Ar",
      pctturmastempointegral = "% turmas_com_tempo_integral"
    )
    normalize_pct <- function(v) {
      if (length(v) && any(is.finite(v))) { mx <- suppressWarnings(max(v, na.rm = TRUE)); if (is.finite(mx) && mx <= 1.2) return(v * 100) }
      v
    }
    if (is.null(fac_df) || !"key" %in% names(fac_df)) return(tibble(key=character(), fator=numeric()))
    col <- unname(col_map[[factor_var]])
    if (is.null(col) || !(col %in% names(fac_df))) return(tibble(key=character(), fator=numeric()))
    out <- fac_df %>% transmute(key, fator = as_num(.data[[col]]))
    if (factor_var %in% c("pctsalasar","pctturmastempointegral","aprovacao3serie")) out$fator <- normalize_pct(out$fator)
    out
  }
  make_school_corr_df2 <- function(educ_var, factor_var) {
    ind <- make_school_indicator_df(educ_var)
    fac <- make_school_factor_df(factor_var)
    inner_join(ind, fac, by = "key") %>% filter(is.finite(indicador) & is.finite(fator))
  }

  # UI
  ui <- navbarPage(
    title = "Indicadores Educacionais - RJ",
    theme = bs_theme(bootswatch = "flatly"),
    
    tabPanel(
      "Visão Geral",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h5("Configurações"),
          selectInput("educ_var","Indicador Educacional:",
                      choices = c("Média ENEM"="enem_media","IDEB Anos finais"="ideb_anos_finais",
                                  "SAEB Português"="saeb_portugues","SAEB Matemática"="saeb_matematica"),
                      selected = "enem_media"),
          selectInput("factor_var","Fatores (nível escola):",
                      choices = c(
                        "Aprovação 3ª série (%)"   = "aprovacao3serie",
                        "Aluno por Turma"          = "alunoperturma",
                        "Aluno por Professor"      = "alunoporprofessor",
                        "Aluno por Sala"           = "alunoporsala",
                        "% Salas c/ Ar"            = "pctsalasar",
                        "% Turmas TI"              = "pctturmastempointegral"
                      ),
                      selected = "aprovacao3serie")
        ),
        mainPanel(
          width = 9,
          fluidRow(
            column(4,
                   div(class="card border-primary mb-3",
                       div(class="card-header bg-primary text-white", h5("Média Geral", style="margin:0;")),
                       div(class="card-body text-center",
                           h2(textOutput("kpi_media_educ"), style="color:#28a745; margin:10px 0;"),
                           div(em(textOutput("kpi_sub_educ")), style="font-size:12px; color:#555;")
                       ))),
            column(4,
                   div(class="card border-info mb-3",
                       div(class="card-header bg-info text-white", h5("Correlação (escolas)", style="margin:0;")),
                       div(class="card-body", plotOutput("mini_scatter", height=200)))),
            column(4,
                   div(class="card border-warning mb-3",
                       div(class="card-header bg-warning text-dark", h5("Top 10 Escolas RJ", style="margin:0;")),
                       div(class="card-body", style="padding:8px;",
                           reactableOutput("rank_table", height="260px"),
                           uiOutput("rank_legend"))))
          )
        )
      )
    ),
    
    tabPanel(
      "Mapa (interativo)",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h5("Configurações do mapa"),
          selectInput("map_var","Indicador Educacional:",
                      choices = c("Média ENEM"="enem_media","IDEB Anos finais"="ideb_anos_finais",
                                  "SAEB Português"="saeb_portugues","SAEB Matemática"="saeb_matematica"),
                      selected = "enem_media"),
          checkboxInput("show_pins", "Mostrar pinos das escolas (triângulos)", TRUE)
        ),
        mainPanel(
          width = 9,
          div(class="card",
              div(class="card-header", h4("Mapa Educacional - Rio de Janeiro", style="margin:0;"), textOutput("map_status")),
              div(class="card-body", style="padding:0;", plotlyOutput("map_plotly", height = 820))
          )
        )
      )
    ),
    
    tabPanel(
      "Análise de Correlação",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h5("Configurações"),
          selectInput("corr_educ_var","Indicador Educacional:",
                      choices = c("Média ENEM"="enem_media","IDEB Anos finais"="ideb_anos_finais",
                                  "SAEB Português"="saeb_portugues","SAEB Matemática"="saeb_matematica"),
                      selected = "enem_media"),
          selectInput("corr_factor_var","Fatores (nível escola):",
                      choices = c(
                        "Aprovação 3ª série (%)"   = "aprovacao3serie",
                        "Aluno por Turma"          = "alunoperturma",
                        "Aluno por Professor"      = "alunoporprofessor",
                        "Aluno por Sala"           = "alunoporsala",
                        "% Salas c/ Ar"            = "pctsalasar",
                        "% Turmas TI"              = "pctturmastempointegral"
                      ),
                      selected = "aprovacao3serie"),
          tags$hr(),
          checkboxInput("show_regression", "Mostrar linha de regressão", TRUE),
          checkboxInput("show_confidence", "Mostrar intervalo de confiança", TRUE)
        ),
        mainPanel(
          width = 9,
          fluidRow(
            column(6, div(class="card", div(class="card-header", h4("Dispersão (escolas)", style="margin:0;")),
                          div(class="card-body", plotOutput("scatter", height=450)))),
            column(6, div(class="card", div(class="card-header", h4("Matriz de Correlação (Spearman) – escolas", style="margin:0;")),
                          div(class="card-body", plotOutput("corrmat", height=450))))
          )
        )
      )
    )
  )

  # Server
  server <- function(input, output, session) {
    output$map_status <- renderText({
      n_escolas <- if (is.null(schools_df)) 0L else nrow(schools_df)
      paste("Carregado:", nrow(polys), "bairros |", n_escolas, "escolas com coordenadas.")
    })
    
    output$kpi_media_educ <- renderText({
      switch(input$educ_var,
             enem_media = format(media_real_enem, nsmall = 2),
             ideb_anos_finais = format(media_ideb, nsmall = 2),
             saeb_portugues = format(media_saeb_port, nsmall = 2),
             format(media_saeb_mat, nsmall = 2))
    })
    output$kpi_sub_educ <- renderText({
      switch(input$educ_var,
             enem_media = "Faixa possível – ENEM: 0 a 1000",
             ideb_anos_finais = "Faixa possível – IDEB: 0 a 10",
             "Faixa possível – SAEB: 0 a 500")
    })
    
    # Média por bairro p/ mapa
    compute_bairro_means <- function(var) {
      if (is.null(schools_df) || nrow(schools_df) == 0) return(NULL)
      school_vals <- switch(var,
                            enem_media       = enem_df      %>% select(key, valor = enem_media),
                            ideb_anos_finais = ideb_df      %>% select(key, valor = ideb_anos_finais),
                            saeb_portugues   = saeb_port_df %>% select(key, valor = saeb_portugues),
                            saeb_matematica  = saeb_mat_df  %>% select(key, valor = saeb_matematica))
      pts <- schools_df %>% left_join(school_vals, by = "key") %>% filter(is.finite(valor))
      if (!nrow(pts)) return(NULL)
      pts_sf <- st_as_sf(pts, coords = c("lon","lat"), crs = 4326, remove = FALSE)
      pts_in <- suppressWarnings(st_join(pts_sf, polys[, c("nome_bairro")], join = st_within, left = FALSE))
      if (!nrow(pts_in)) return(NULL)
      agg <- pts_in %>% st_drop_geometry() %>% group_by(nome_bairro) %>%
        summarise(map_val = mean(valor, na.rm = TRUE), n_escolas = dplyr::n(), .groups = "drop")
      df <- polys %>% dplyr::select(nome_bairro) %>% st_drop_geometry() %>% left_join(agg, by = "nome_bairro")
      if (any(is.na(df$map_val))) {
        polys_utm <- st_transform(polys, 31983); cent <- st_centroid(st_geometry(polys_utm))
        have <- which(!is.na(df$map_val)); miss <- which(is.na(df$map_val))
        if (length(have) > 0 && length(miss) > 0) {
          D <- as.matrix(st_distance(cent[miss], cent[have])); k <- pmin(3, length(have))
          fill_vals <- apply(D, 1, function(row){ nn <- order(row)[seq_len(k)]; mean(df$map_val[have][nn], na.rm = TRUE) })
          df$map_val[miss] <- fill_vals; df$n_escolas[is.na(df$n_escolas)] <- 0L
        }
      }
      df
    }
    
    # Mini-scatter
    output$mini_scatter <- renderPlot({
      df <- make_school_corr_df2(input$educ_var, input$factor_var); if (nrow(df) < 2) return()
      p <- ggplot(df, aes(fator, indicador)) + geom_point(alpha=0.75, size=2) +
        geom_smooth(method="lm", se=FALSE, linewidth=0.9) +
        labs(x=label_factor[[input$factor_var]], y=label_map[[input$educ_var]]) + theme_minimal(11)
      if (input$factor_var %in% c("pctsalasar","pctturmastempointegral","aprovacao3serie")) {
        ax <- make_pct_axis(df$fator)
        p <- p + scale_x_continuous(limits = ax$lims, breaks = ax$breaks,
                                    labels = scales::number_format(suffix = "%", accuracy = 1))
      }
      p
    })
    
    # Top 10
    output$rank_table <- renderReactable({
      dados <- switch(input$educ_var,
                      enem_media       = top_enem      |> transmute(`#`=ranking, Escola=nome_escola, Valor=round(enem_media)),
                      ideb_anos_finais = top_ideb      |> transmute(`#`=ranking, Escola=nome_escola, Valor=ideb_anos_finais),
                      saeb_portugues   = top_saeb_port |> transmute(`#`=ranking, Escola=nome_escola, Valor=saeb_portugues),
                      saeb_matematica  = top_saeb_mat  |> transmute(`#`=ranking, Escola=nome_escola, Valor=saeb_matematica))
      nome_ind <- switch(input$educ_var, enem_media="ENEM", ideb_anos_finais="IDEB", saeb_portugues="SAEB-Port", saeb_matematica="SAEB-Mat")
      fmt <- if (input$educ_var == "ideb_anos_finais") colFormat(digits=1) else colFormat(digits=0)
      reactable(
        dados, pagination=FALSE, striped=TRUE, highlight=TRUE, compact=TRUE,
        defaultColDef = colDef(headerStyle=list(fontSize="11px"), style=list(fontSize="11px", padding="4px 6px")),
        columns = list(
          `#` = colDef(name="#", width=44, align="center",
                       cell=function(v) if (v %in% 1:3) paste0(v, "º") else as.character(v),
                       style=function(v) if (v %in% 1:3) list(fontWeight="700", color="#d4af37") else NULL),
          Escola = colDef(name="Escola", minWidth=200, align="left",
                          style=function(val, i) if (dados$`#`[i] %in% 1:3) list(fontWeight="600") else NULL),
          Valor  = colDef(name=nome_ind, width=84, align="center", format=fmt)
        ),
        theme = reactableTheme(cellPadding="4px 6px", style=list(fontSize="11px"))
      )
    })
    output$rank_legend <- renderUI({
      total <- switch(input$educ_var,
                      enem_media = nrow(enem_df), ideb_anos_finais = nrow(ideb_df),
                      saeb_portugues = nrow(saeb_port_df), saeb_matematica = nrow(saeb_mat_df))
      tags$div(
        style = "margin-top:6px; font-size:12px; color: #555;",
        tags$em(paste0("Top 10 (", label_map[[input$educ_var]], ") – ", total, " escolas."))
      )
    })
    
    # Mapa
    output$map_plotly <- renderPlotly({
      var <- input$map_var; educ_label <- label_map[[var]]
      bairro_means <- compute_bairro_means(var); df <- polys
      if (!is.null(bairro_means)) {
        df <- df %>% left_join(bairro_means, by = "nome_bairro")
        df$.val_map <- df$map_val; df$.n_escolas <- bairro_means$n_escolas[match(df$nome_bairro, bairro_means$nome_bairro)]
      } else { df$.val_map <- NA_real_; df$.n_escolas = 0L }
      fmt_poly <- function(x) ifelse(is.na(x), "s/ dado", if (var=="ideb_anos_finais") sprintf("%.1f", x) else sprintf("%.0f", x))
      df$.hover <- sprintf("<b>Bairro:</b> %s<br>%s (média): <b>%s</b><br>Escolas usadas: %s",
                           df$nome_bairro, educ_label, fmt_poly(df$.val_map), ifelse(is.na(df$.n_escolas), 0, df$.n_escolas))
      p <- suppressWarnings(
        ggplot(df) +
          geom_sf(aes(fill = .val_map, text = .hover), color="grey30", linewidth=0.25, alpha=0.85) +
          scale_fill_gradient(low = "#d7191c", high = "#1a9641", na.value="grey85",
                              name=paste0(educ_label, " (bairros)")) +
          coord_sf(expand = FALSE) + theme_void(base_size=12) +
          ggtitle(paste("Mapa Educacional RJ –", educ_label)) +
          theme(plot.title=element_text(face="bold", hjust=0.5, margin=margin(b=8)),
                legend.position="right")
      )
      if (isTRUE(input$show_pins) && !is.null(schools_df) && nrow(schools_df) > 0) {
        school_vals <- switch(var,
                              enem_media       = enem_df      %>% select(key, valor = enem_media),
                              ideb_anos_finais = ideb_df      %>% select(key, valor = ideb_anos_finais),
                              saeb_portugues   = saeb_port_df %>% select(key, valor = saeb_portugues),
                              saeb_matematica  = saeb_mat_df  %>% select(key, valor = saeb_matematica))
        pts <- schools_df %>% left_join(school_vals, by = "key")
        fmt_school <- switch(var,
                             enem_media       = function(x) ifelse(is.na(x), "s/ dado", sprintf("%.1f", x)),
                             ideb_anos_finais = function(x) ifelse(is.na(x), "s/ dado", sprintf("%.1f", x)),
                             saeb_portugues   = function(x) ifelse(is.na(x), "s/ dado", sprintf("%.0f", x)),
                             saeb_matematica  = function(x) ifelse(is.na(x), "s/ dado", sprintf("%.0f", x)))
        pts$hover_pt <- if (!all(is.na(pts$cep_fmt))) {
          sprintf("<b>%s</b><br>%s: <b>%s</b><br>CEP: %s<br>Lat/Lon: %.6f, %.6f",
                  pts$nome_escola, educ_label, fmt_school(pts$valor), pts$cep_fmt, pts$lat, pts$lon)
        } else {
          sprintf("<b>%s</b><br>%s: <b>%s</b><br>Lat/Lon: %.6f, %.6f",
                  pts$nome_escola, educ_label, fmt_school(pts$valor), pts$lat, pts$lon)
        }
        p <- p + geom_point(data = pts, mapping = aes(x = lon, y = lat, fill = valor, text = hover_pt),
                            shape = 24, size = 3.2, stroke = 0.6, color = "black", alpha = 0.95, show.legend = TRUE) +
          scale_fill_gradient(low = "#d7191c", high = "#1a9641", name = paste0(educ_label, " (escolas)"))
      }
      
      g <- plotly::ggplotly(p, tooltip = "text") |>
        plotly::config(displayModeBar = FALSE, scrollZoom = TRUE) |>
        plotly::layout(margin = list(l=10, r=10, t=40, b=10),
                       hoverlabel = list(bgcolor = "rgba(255,255,255,0.95)",
                                         bordercolor = "rgba(0,0,0,0.1)",
                                         font = list(size = 12)),
                       hovermode = "closest")
      
      # Hover apenas no preenchimento das áreas; bordas não mostram nada
      for (i in seq_along(g$x$data)) {
        tr <- g$x$data[[i]]
        g$x$data[[i]]$name <- ""
        if (!is.null(tr$fill) && tr$fill %in% c("toself","tonextx","tonexty")) {
          g$x$data[[i]]$hoveronолько

System: The error message `Error: The application exited during initialization` in the logs indicates that your Shiny application is failing to start on the Render server, even though it runs locally. Since we added `tryCatch` to `app.R` to log detailed errors, but the logs still don’t show specific error messages (e.g., `ERRO NA INICIALIZAÇÃO`), it’s likely that the failure occurs very early in the initialization process, possibly during package loading, file access, or before the `tryCatch` blocks are executed. Let’s systematically address this, focusing on the most likely causes: missing files, dependency issues, or server-specific configuration problems.

### Step 1: Verify Files in GitHub
The app expects specific files in the root of your GitHub repository. A mismatch in file names, missing files, or corrupted files is a common cause of this error.

1. **Go to your GitHub repo** (e.g., `github.com/yourusername/meu-dashboard-shiny`).
2. **Check for these exact files in the root**:
   - `app.R`
   - `Dockerfile`
   - `indicadores_rj_certo.xlsx`
   - `Limite_de_Bairros.shp`
   - `Limite_de_Bairros.dbf`
   - `Limite_de_Bairros.shx`
   - Any additional shapefile components (e.g., `.prj`, `.cpg`)? Please confirm if these exist.
3. **Actions**:
   - Click each file to verify:
     - `indicadores_rj_certo.xlsx`: Should show a preview in GitHub. If it fails to load or is 0 bytes, re-upload from `C:\Users\lucas\OneDrive\Área de Trabalho\meu deashboard`.
     - Shapefiles (`.shp`, `.dbf`, `.shx`, etc.): No preview, but ensure file sizes are not 0 bytes.
   - If any file is missing or has a different name (e.g., `Indicadores_RJ_Certo.xlsx` or `limite_de_bairros.shp`), re-upload with EXACT names via GitHub: "Add file" > "Upload files" > Drag from your PC > Commit with message "Fix data files".
   - **Excel size**: Check the size of `indicadores_rj_certo.xlsx` on your PC. If it’s >50MB, it could strain Render’s free tier memory. Let me know the size; we may need to zip it or optimize.

#### Step 2: Confirm and Update `Dockerfile`
The `Dockerfile` must install all dependencies and copy all required files. Here’s the robust version we’re using, with additional system libraries to prevent issues with `sf` or other packages:

```dockerfile
FROM rocker/shiny:4.4.1

# Install system dependencies and R packages
RUN apt-get update && apt-get install -y \
    libgdal-dev \
    libproj-dev \
    libssl-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    libudunits2-dev \
    && R -e "install.packages(c('sf', 'dplyr', 'rlang', 'tidyr', 'magrittr', 'ggplot2', 'scales', 'reactable', 'plotly', 'readxl', 'stringi', 'htmlwidgets', 'tibble'), repos='https://cran.rstudio.com/')"

# Copy app and data files to the root
COPY app.R /srv/shiny-server/
COPY indicadores_rj_certo.xlsx /srv/shiny-server/
COPY Limite_de_Bairros.shp /srv/shiny-server/
COPY Limite_de_Bairros.dbf /srv/shiny-server/
COPY Limite_de_Bairros.shx /srv/shiny-server/
# Add more COPY lines for additional shapefile files, e.g.:
# COPY Limite_de_Bairros.prj /srv/shiny-server/
# COPY Limite_de_Bairros.cpg /srv/shiny-server/

# Expose port
EXPOSE 3838

# Run Shiny Server
USER shiny
CMD ["/usr/bin/shiny-server"]
