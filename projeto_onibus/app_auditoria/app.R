############################################################
# app.R — auditoria visual das viagens e saídas (Shiny)
#
# Rodar:  Rscript app_auditoria/rodar.R      (abre em http://127.0.0.1:8766)
#
# Fluxo: escolha linha e dia → selecione um ou mais horários na grade →
# o mapa e o diagrama espaço-tempo se ajustam à janela desses horários →
# use o controle de tempo (▶) para ver os ônibus se movendo e confira a
# classificação de cada saída no painel à direita.
############################################################
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(leaflet); library(reactable)
  library(data.table); library(ggplot2)
})
APP_DIR <- if (exists("APP_DIR")) APP_DIR else normalizePath(".")
PROJ_DIR <- dirname(APP_DIR)
source(file.path(PROJ_DIR, "R", "00_config.R"), local = TRUE)
source(file.path(PROJ_DIR, "R", "11_relatorio_linha.R"), local = TRUE)
source(file.path(PROJ_DIR, "R", "12_frota.R"), local = TRUE)
source(file.path(APP_DIR, "dados.R"), local = TRUE)

COR_SENT <- c(`0` = "#2a78d6", `1` = "#eb6834")           # traçado / ônibus por sentido inferido
COR_DESTAQUE <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#7d4fd6", "#d6457a", "#4f8a10", "#00838f")
fmt_h <- function(x) ifelse(is.na(x), "", format(x, "%H:%M:%S", tz = TZ))

idx0 <- indice_particoes()
linhas0 <- sort(unique(idx0$linha))

# ------------------------------------------------------------------ UI
ui <- page_sidebar(
  title = "Auditoria Olho Vivo — viagens e saídas programadas",
  theme = bs_theme(version = 5, preset = "cosmo", font_scale = 0.9),
  fillable = FALSE,
  sidebar = sidebar(width = 300,
    selectInput("linha", "Linha", linhas0, selected = linhas0[1]),
    selectizeInput("datas", "Dias de serviço (um ou mais)", choices = NULL, multiple = TRUE,
                   options = list(plugins = list("remove_button"), placeholder = "escolha os dias")),
    selectInput("foco", "Dia em foco (mapa, diagrama, veículos)", choices = NULL),
    uiOutput("resumo_dia"),
    hr(),
    radioButtons("filtro_veic", "Ônibus no mapa",
                 c("Todos da linha" = "todos", "Só os das saídas/viagens selecionadas" = "sel"), "todos"),
    sliderInput("rastro", "Rastro (min)", min = 0, max = 30, value = 8, step = 1),
    selectInput("passo", "Passo do controle de tempo",
                c("15 s" = 15, "30 s" = 30, "1 min" = 60, "2 min" = 120), selected = 30),
    checkboxInput("mostra_invalidos", "Mostrar registros com flag de QC / descartados", TRUE),
    actionButton("limpar", "Limpar seleção", class = "btn-sm btn-outline-secondary"),
    hr(),
    helpText("Selecione horários na grade (aba 'Grade horária') ou viagens (aba 'Viagens').",
             "Clique no diagrama espaço-tempo para saltar para um instante.")
  ),
  layout_columns(col_widths = c(8, 4),
    card(full_screen = TRUE,
      card_header(div(style = "display:flex;justify-content:space-between;align-items:center",
                      span("Mapa"), textOutput("relogio", inline = TRUE))),
      leafletOutput("mapa", height = 520),
      div(style = "padding:0 12px", uiOutput("ui_tempo"))
    ),
    card(full_screen = TRUE, card_header("Classificação das saídas selecionadas"),
         div(style = "max-height:640px;overflow-y:auto", uiOutput("painel_classif")))
  ),
  card(full_screen = TRUE,
    card_header("Diagrama espaço-tempo (posição ao longo do itinerário × hora)"),
    plotOutput("marey", height = 360, click = "marey_click")
  ),
  navset_card_tab(
    nav_panel("Grade horária", reactableOutput("grade")),
    nav_panel("Viagens reconstruídas", reactableOutput("viagens")),
    nav_panel("Veículos do dia em foco",
              helpText("Linha do tempo de cada ônibus no dia: viagens reconstruídas (cor = status), lacunas de GPS > 10 min",
                       "(faixa vermelha) e saídas programadas sem partida observada (▼). Clique para mover o mapa."),
              plotOutput("gantt", height = 420, click = "gantt_click"),
              reactableOutput("veiculos")),
    nav_panel("Registros GPS (ônibus destacados, na janela)", reactableOutput("gps"))
  )
)

# ------------------------------------------------------------------ servidor
server <- function(input, output, session) {

  # datas disponíveis da linha
  datas_disp <- reactive(datas_linha(req(input$linha)))
  rot_data <- function(d) sprintf("%s %s", format(d, "%d/%m/%Y"),
                                  c("dom", "seg", "ter", "qua", "qui", "sex", "sáb")[as.POSIXlt(d)$wday + 1])
  observeEvent(input$linha, {
    ds_ <- datas_disp()
    if (!length(ds_)) return()
    ch <- setNames(as.character(ds_), rot_data(ds_))
    sel <- intersect(input$datas, ch)
    if (!length(sel)) sel <- ch[min(2L, length(ch))]
    updateSelectizeInput(session, "datas", choices = ch, selected = sel)
  })
  datas_sel <- reactive({
    d <- as.Date(req(input$datas))
    sort(d[d %in% datas_disp()])
  })
  observeEvent(datas_sel(), {
    d <- datas_sel()
    f <- if (!is.null(input$foco) && input$foco %in% as.character(d)) input$foco else as.character(d[1])
    updateSelectInput(session, "foco", choices = setNames(as.character(d), rot_data(d)), selected = f)
  })
  # todos os dias escolhidos (grade e viagens) e o dia em foco (mapa, diagrama, veículos)
  dias <- reactive({
    d <- req(datas_sel())
    withProgress(message = "Lendo dados...", lapply(d, function(x) carregar_dia_cache(input$linha, x)))
  })
  dia <- reactive({
    req(input$foco, input$foco %in% as.character(datas_sel()))
    carregar_dia_cache(input$linha, as.Date(input$foco))
  })

  output$resumo_dia <- renderUI({
    D <- dia(); au <- D$au
    if (!nrow(au)) return(helpText("Sem programação para este dia."))
    tb <- au[, .N, by = .(rotulo, cor)][STATUS[, .(rotulo)], on = "rotulo", nomatch = 0]
    tags$div(style = "font-size:.85rem",
      tags$b(sprintf("Dia em foco: %s · %s (%s)", D$linha, format(D$data, "%d/%m/%Y"), au$tipo_dia[1])),
      tags$div(sprintf("%d saídas programadas · %d viagens · %d veículos", nrow(au), nrow(D$vg),
                       uniqueN(D$pos$vehicle_id))),
      lapply(seq_len(nrow(tb)), function(i) tags$div(
        tags$span(style = sprintf("display:inline-block;width:10px;height:10px;border-radius:2px;background:%s;margin-right:6px", tb$cor[i])),
        sprintf("%s: %d", tb$rotulo[i], tb$N[i]))))
  })

  # ---------------- seleção (grade de saídas e viagens)
  au_todos <- reactive(rbindlist(lapply(dias(), `[[`, "au"), fill = TRUE))
  vg_todos <- reactive({
    v <- rbindlist(lapply(dias(), `[[`, "vg"), fill = TRUE)
    if (nrow(v)) v[, chave_v := paste(service_date, vid)]
    v
  })
  # seleções em todos os dias (painel) e só no dia em foco (mapa, diagrama, janela)
  sel_au_todos <- reactive({
    i <- getReactableState("grade", "selected")
    if (is.null(i) || !length(i)) return(au_todos()[0])
    au_todos()[chave %in% grade_tab()[i, chave]]
  })
  sel_vg_todos <- reactive({
    i <- getReactableState("viagens", "selected")
    if (is.null(i) || !length(i)) return(vg_todos()[0])
    vg_todos()[chave_v %in% viagens_tab()[i, chave_v]]
  })
  sel_au <- reactive(sel_au_todos()[service_date == dia()$data])
  sel_vg <- reactive(sel_vg_todos()[service_date == dia()$data])
  # se tudo o que foi selecionado é de um único dia, ele vira o dia em foco
  observeEvent(list(sel_au_todos(), sel_vg_todos()), {
    d <- unique(c(as.character(sel_au_todos()$service_date), as.character(sel_vg_todos()$service_date)))
    if (length(d) == 1 && !identical(d, input$foco)) updateSelectInput(session, "foco", selected = d)
  }, ignoreInit = TRUE)
  # trocar de linha ou de conjunto de dias zera as seleções (os índices da tabela antiga não valem mais)
  observeEvent(list(input$linha, input$datas), {
    updateReactable("grade", selected = NA)
    updateReactable("viagens", selected = NA)
  }, priority = 10)
  observeEvent(input$limpar, {
    updateReactable("grade", selected = NA)
    updateReactable("viagens", selected = NA)
  })

  # veículos destacados: os das viagens associadas às saídas selecionadas + viagens selecionadas
  destaque <- reactive({
    v <- unique(na.omit(c(sel_au()$observed_vehicle_id, sel_vg()$vehicle_id)))
    setNames(COR_DESTAQUE[(seq_along(v) - 1) %% length(COR_DESTAQUE) + 1], v)
  })

  # janela de tempo: em torno das seleções, ou o dia inteiro
  janela <- reactive({
    D <- dia(); sa <- sel_au(); sv <- sel_vg()
    if (nrow(sa) || nrow(sv)) {
      ids <- c(sa$observed_trip_id, sv$trip_id)
      vv <- D$vg[trip_id %in% ids]
      ini <- min(c(sa$t_prog, sa$t_obs, vv$t_ini), na.rm = TRUE) - 20 * 60
      fim <- max(c(sa$t_prog + 60 * 60, vv$t_fim), na.rm = TRUE) + 10 * 60
    } else if (nrow(D$pos)) {
      ini <- min(D$pos$t); fim <- max(D$pos$t)
    } else {
      ini <- as.POSIXct(paste(D$data, "04:00"), tz = TZ); fim <- ini + 20 * 3600
    }
    c(ini = as.POSIXct(floor(as.numeric(ini) / 60) * 60, origin = "1970-01-01", tz = TZ),
      fim = as.POSIXct(ceiling(as.numeric(fim) / 60) * 60, origin = "1970-01-01", tz = TZ))
  })

  output$ui_tempo <- renderUI({
    j <- janela()
    sliderInput("t", NULL, min = j[["ini"]], max = j[["fim"]], value = j[["ini"]] + 20 * 60,
                step = as.numeric(isolate(input$passo)), timeFormat = "%H:%M:%S", timezone = "-0300",
                width = "100%", animate = animationOptions(interval = 350, loop = FALSE))
  })
  observeEvent(input$passo, updateSliderInput(session, "t", step = as.numeric(input$passo)), ignoreInit = TRUE)
  observeEvent(input$marey_click, {
    x <- input$marey_click$x
    if (!is.null(x)) updateSliderInput(session, "t", value = as.POSIXct(x, origin = "1970-01-01", tz = TZ))
  })
  t_atual <- reactive({
    req(input$t)
    as.POSIXct(input$t, tz = TZ)
  })
  output$relogio <- renderText({
    tt <- t_atual()
    sprintf("%s  %s", format(tt, "%d/%m"), format(tt, "%H:%M:%S"))
  })

  # ---------------- mapa base (muda só com linha × dia)
  output$mapa <- renderLeaflet({
    D <- dia()
    m <- leaflet() |>
      addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}",
               group = "Mapa claro", attribution = "Tiles &copy; Esri", options = tileOptions(maxZoom = 16)) |>
      addTiles(group = "OpenStreetMap")
    if (nrow(D$tra)) {
      for (d in sort(unique(D$tra$direction_id))) {
        tr <- D$tra[direction_id == d][order(seq)]
        hs <- D$sen[direction_id == d, headsign]
        m <- m |> addPolylines(tr$lon, tr$lat, color = unname(COR_SENT[as.character(d)]), weight = 4, opacity = 0.45,
                               group = "Itinerário", label = sprintf("sentido %d → %s", d, hs))
      }
    }
    if (nrow(D$par)) {
      ter <- D$par[, .SD[c(1, .N)], by = direction_id]
      m <- m |>
        addCircleMarkers(D$par$stop_lon, D$par$stop_lat, radius = 3, weight = 1, color = "#5f5e57",
                         fillColor = "#ffffff", fillOpacity = 1, opacity = 0.8, group = "Paradas",
                         label = sprintf("%s · seq %d · sentido %d · %s", D$par$stop_id, D$par$stop_sequence,
                                         D$par$direction_id, D$par$stop_name)) |>
        addCircleMarkers(ter$stop_lon, ter$stop_lat, radius = 7, weight = 2, color = "#0b0b0b",
                         fillColor = "#ffffff", fillOpacity = 1, group = "Paradas",
                         label = sprintf("Terminal: %s (sentido %d)", ter$stop_name, ter$direction_id))
    }
    m |> addLayersControl(baseGroups = c("Mapa claro", "OpenStreetMap"),
                          overlayGroups = c("Itinerário", "Paradas", "Ônibus", "Rastro"),
                          options = layersControlOptions(collapsed = TRUE)) |>
      addLegend("bottomleft", colors = unname(COR_SENT), opacity = 0.9,
                labels = sprintf("sentido inferido %s", names(COR_SENT)), title = NULL)
  })

  # posições visíveis (filtro de veículos e de QC)
  pos_vis <- reactive({
    D <- dia(); p <- D$pos
    if (!nrow(p)) return(p)
    if (!isTRUE(input$mostra_invalidos)) p <- p[valido == TRUE & quebra != 4L]
    if (input$filtro_veic == "sel") p <- p[vehicle_id %in% names(destaque())]
    p
  })

  # ---------------- camada dinâmica: ônibus no instante t
  observe({
    tt <- t_atual(); p <- pos_vis(); dst <- destaque()
    px <- leafletProxy("mapa") |> clearGroup("Ônibus") |> clearGroup("Rastro")
    if (!nrow(p)) return()
    rastro_s <- input$rastro * 60
    w <- p[t <= tt & t >= tt - max(rastro_s, 600)]
    if (!nrow(w)) return()
    # rastro
    if (rastro_s > 0) {
      r <- w[t >= tt - rastro_s]
      # um traço por veículo × cadeia do map matching: não liga pontos separados por
      # quebra (salto implausível, lacuna longa)
      rr <- r[valido == TRUE & quebra != 4L]
      for (k in unique(paste(rr$vehicle_id, rr$chain))) {
        rv <- rr[paste(vehicle_id, chain) == k]
        if (nrow(rv) < 2) next
        v <- rv$vehicle_id[1]
        hl <- v %in% names(dst)
        px <- px |> addPolylines(rv$longitude, rv$latitude, group = "Rastro",
                                 color = if (hl) unname(dst[[v]]) else "#8a897f", weight = if (hl) 3 else 2,
                                 opacity = if (hl) 0.9 else 0.5, dashArray = if (hl) NULL else "4 4")
      }
      ruins <- r[valido == FALSE | quebra == 4L]
      if (nrow(ruins)) px <- px |> addCircleMarkers(ruins$longitude, ruins$latitude, radius = 4, group = "Rastro",
                                                    color = "#d03b3b", weight = 2, fill = FALSE,
                                                    label = sprintf("%s %s descartado: %s", ruins$vehicle_id, fmt_h(ruins$t),
                                                                    ifelse(ruins$quebra == 4L, "outlier (salto)", "QC")))
    }
    # última posição de cada veículo (até 10 min atrás)
    u <- w[valido == TRUE & quebra != 4L, .SD[.N], by = vehicle_id]
    if (!nrow(u)) return()
    u[, idade := as.numeric(tt) - as.numeric(t)]
    u[, hl := vehicle_id %in% names(dst)]
    u[, cor := ifelse(hl, unname(dst[vehicle_id]), ifelse(is.na(sentido_inferido), "#8a897f",
                                                     COR_SENT[as.character(sentido_inferido)]))]
    lab <- sprintf(paste0("<b>%s</b> · %s (há %ds)<br>sl=%s · sentido inferido=%s<br>",
                          "progresso %.2f km · dist. ao traçado %.0f m<br>%s%s"),
                   u$vehicle_id, fmt_h(u$t), round(u$idade), u$direction_sl,
                   ifelse(is.na(u$sentido_inferido), "–", u$sentido_inferido), u$s_km, u$d_rota,
                   ifelse(u$na_rota %in% TRUE, "no itinerário", "<b>fora do itinerário</b>"),
                   ifelse(u$valido, "", " · <b>registro inválido (QC)</b>"))
    u[, cor := unname(cor)]
    px <- px |> addCircleMarkers(u$longitude, u$latitude, group = "Ônibus",
                           radius = ifelse(u$hl, 9, 6), weight = ifelse(u$hl, 3, 1.5), color = "#ffffff",
                           fillColor = u$cor, fillOpacity = ifelse(u$idade > 180, 0.35, 0.95),
                           label = lapply(lab, HTML),
                           labelOptions = labelOptions(textsize = "12px"))
    if (any(u$hl)) px |> addLabelOnlyMarkers(u$longitude[u$hl], u$latitude[u$hl], label = u$vehicle_id[u$hl], group = "Ônibus",
                          labelOptions = labelOptions(noHide = TRUE, direction = "right", offset = c(10, 0),
                                                      textOnly = TRUE, style = list("font-weight" = "600")))
  })

  # ---------------- diagrama espaço-tempo
  t_lento <- throttle(t_atual, 900)
  output$marey <- renderPlot({
    D <- dia(); j <- janela(); p <- pos_vis(); dst <- destaque(); tt <- t_lento()
    p <- p[t >= j[["ini"]] & t <= j[["fim"]] & !is.na(s_km)]
    descart <- p[valido == FALSE | quebra == 4L]          # outliers/QC: pontos, não entram nas linhas
    p <- p[valido == TRUE & quebra != 4L]
    au <- D$au[t_prog >= j[["ini"]] - 600 & t_prog <= j[["fim"]]]
    s0 <- D$sen[, .(direction_id, s_ori = a_m / 1000, s_dst = b_m / 1000, headsign)]
    au <- s0[au, on = "direction_id"]
    p[, hl := vehicle_id %in% names(dst)]
    # quebra de linha quando o veículo muda de cadeia, dá a volta no ciclo ou fica 10 min sem dados
    p[, seg := cumsum(c(TRUE, diff(as.numeric(t)) > 600 | diff(chain) != 0 | abs(diff(s_km)) > 0.5 * D$L_km)),
      by = vehicle_id]
    g <- ggplot() +
      geom_hline(yintercept = unique(c(s0$s_ori, s0$s_dst)), colour = "#c3c2b7", linewidth = 0.3, linetype = 2)
    if (nrow(p[hl == FALSE]))
      g <- g + geom_path(data = p[hl == FALSE], aes(t, s_km, group = interaction(vehicle_id, seg)),
                         colour = "#b5b4ad", linewidth = 0.35)
    if (nrow(p[hl == TRUE]))
      g <- g + geom_path(data = p[hl == TRUE], aes(t, s_km, group = interaction(vehicle_id, seg), colour = vehicle_id),
                         linewidth = 0.9) +
        scale_colour_manual(values = dst, name = "veículo")
    if (nrow(descart))
      g <- g + geom_point(data = descart, aes(t, s_km), shape = 4, size = 1.6, colour = "#d03b3b", stroke = 0.6)
    if (nrow(au)) {
      sa <- sel_au()
      au[, sel := rid %in% sa$rid]
      g <- g +
        geom_segment(data = au[!is.na(t_obs)], aes(x = t_prog, xend = t_obs, y = s_ori, yend = s_ori),
                     colour = "#5f5e57", linewidth = 0.4) +
        geom_point(data = au, aes(t_prog, s_ori, fill = rotulo), shape = 25, size = 2.8, colour = "#ffffff",
                   stroke = 0.4) +
        scale_fill_manual(values = setNames(STATUS$cor, STATUS$rotulo), name = "saída programada", drop = TRUE)
      if (any(au$sel))
        g <- g + geom_point(data = au[sel == TRUE], aes(t_prog, s_ori), shape = 1, size = 6, colour = "#0b0b0b", stroke = 1) +
          geom_text(data = au[sel == TRUE], aes(t_prog, s_ori, label = horario), vjust = -1.3, size = 3.3, fontface = "bold")
    }
    lab_y <- s0[, .(y = s_ori, lab = sprintf("partida S%d", direction_id))]
    g + geom_vline(xintercept = tt, colour = "#d03b3b", linewidth = 0.6) +
      annotate("label", x = tt, y = Inf, label = format(tt, "%H:%M:%S"), vjust = 1.1, size = 3.6,
               fill = "#d03b3b", colour = "#ffffff", fontface = "bold", label.size = 0) +
      scale_x_datetime(limits = c(j[["ini"]], j[["fim"]]), date_labels = "%H:%M", timezone = TZ, expand = c(0, 0)) +
      scale_y_continuous(breaks = sort(unique(c(lab_y$y, pretty(c(0, D$L_km), 8)))),
                         labels = function(b) ifelse(b %in% lab_y$y, sprintf("%.1f  %s", b, lab_y$lab[match(b, lab_y$y)]),
                                                     sprintf("%.0f", b))) +
      labs(x = NULL, y = "km ao longo do ciclo",
           caption = "▼ = saída programada (cor = classificação; traço até a partida observada associada) · × = registro descartado (QC/outlier) · linha vermelha = instante do mapa (clique para mover)") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank(), legend.position = "right")
  }, res = 96)

  # ---------------- tabelas
  grade_tab <- reactive({
    au <- au_todos()
    if (!nrow(au)) return(data.table(chave = character()))
    au[, .(chave, dia = format(service_date, "%d/%m"), sentido = direction_id, programada = horario,
           status = rotulo, cor, partida_obs = fmt_h(t_obs), dif_min = departure_difference_minutes,
           veiculo = observed_vehicle_id, duracao_obs = round(dur_obs), duracao_est = round(dur_est),
           viagem = trip_status, confianca = confidence_level, cobertura_gps = round(100 * frac_min_dados),
           diagnostico_frota = unname(ROTULO_DIAG[diagnostico_frota]))]
  })
  output$grade <- renderReactable({
    g <- grade_tab()
    reactable(g[, !c("chave", "cor")], selection = "multiple", onClick = "select", compact = TRUE, striped = TRUE,
      filterable = TRUE, defaultPageSize = 25, highlight = TRUE,
      columns = list(
        dia = colDef(width = 65), sentido = colDef(width = 70), programada = colDef(width = 90),
        duracao_obs = colDef(name = "duração obs. (min)", width = 95),
        duracao_est = colDef(name = "duração estimada (min)", width = 105),
        diagnostico_frota = colDef(name = "diagnóstico de frota", minWidth = 200),
        status = colDef(minWidth = 150, cell = function(v, i) div(
          span(style = sprintf("display:inline-block;width:10px;height:10px;border-radius:2px;margin-right:6px;background:%s", g$cor[i])), v)),
        dif_min = colDef(name = "dif. (min)", width = 85, format = colFormat(digits = 1)),
        cobertura_gps = colDef(name = "% min c/ GPS", width = 100)),
      theme = reactableTheme(rowSelectedStyle = list(backgroundColor = "#e4eefb")))
  })

  viagens_tab <- reactive({
    vg <- vg_todos()
    if (!nrow(vg)) return(data.table(chave_v = character()))
    vg[, .(chave_v, dia = format(service_date, "%d/%m"), veiculo = vehicle_id, sentido = direction_id, inicio = fmt_h(t_ini), partida = fmt_h(t_dep),
           chegada = fmt_h(t_arr), duracao_min = round(duration_minutes), status = trajectory_status,
           confianca = confidence_level, cobertura = round(100 * observation_coverage),
           evid_origem = origin_evidence, evid_destino = destination_evidence, saida_associada,
           status_saida, flags = quality_flags)]
  })
  output$viagens <- renderReactable({
    reactable(viagens_tab()[, !"chave_v"], selection = "multiple", onClick = "select", compact = TRUE, striped = TRUE,
              filterable = TRUE, defaultPageSize = 25, highlight = TRUE,
              columns = list(cobertura = colDef(name = "cobertura %", width = 95),
                             flags = colDef(minWidth = 220)),
              theme = reactableTheme(rowSelectedStyle = list(backgroundColor = "#e4eefb")))
  })

  output$gps <- renderReactable({
    j <- janela(); dst <- destaque(); p <- dia()$pos
    p <- p[vehicle_id %in% names(dst) & t >= j[["ini"]] & t <= j[["fim"]]]
    if (!nrow(p)) return(reactable(data.frame(aviso = "Selecione uma saída com viagem associada, ou uma viagem.")))
    flags <- grep("^qc_", names(p), value = TRUE)
    p[, flags_qc := apply(as.matrix(.SD), 1, function(r) paste(sub("^qc_", "", flags)[r %in% TRUE], collapse = ";")), .SDcols = flags]
    reactable(p[, .(veiculo = vehicle_id, hora = fmt_h(t), sl = direction_sl, sentido_inferido, progresso_km = round(s_km, 3),
                    dist_tracado_m = round(d_rota), na_rota, cadeia = chain,
                    quebra = c("", "novo veículo", "lacuna longa", "transição implausível", "outlier")[quebra + 1L],
                    valido, flags_qc, vel_kmh = round(vel_kmh, 1), lat = latitude, lon = longitude)],
              compact = TRUE, striped = TRUE, filterable = TRUE, defaultPageSize = 30,
              rowStyle = function(i) if (!p$valido[i] || p$quebra[i] == 4L) list(color = "#b02a2a"))
  })

  # ---------------- painel de classificação
  output$painel_classif <- renderUI({
    a <- sel_au_todos(); VG <- vg_todos()
    if (!nrow(a)) return(helpText("Nenhuma saída selecionada. Escolha um ou mais horários na grade abaixo."))
    nd <- uniqueN(a$service_date)
    tagList(
    if (nd > 1) div(class = "alert alert-secondary py-1 px-2", style = "font-size:.8rem",
                    sprintf("Saídas de %d dias selecionadas. Mapa e diagrama mostram o dia em foco (%s).",
                            nd, format(dia()$data, "%d/%m"))),
    lapply(seq_len(nrow(a)), function(i) {
      r <- a[i]
      v <- if (!is.na(r$observed_trip_id)) VG[trip_id == r$observed_trip_id] else NULL
      linha_kv <- function(k, val) {
        ok <- inherits(val, c("shiny.tag", "shiny.tag.list")) ||
          (length(val) == 1 && !is.na(val) && nzchar(as.character(val)))
        if (ok) tags$tr(tags$td(style = "color:#5f5e57;padding-right:8px;vertical-align:top", k), tags$td(val))
      }
      card(class = "mb-2", style = sprintf("border-left:5px solid %s", r$cor),
        card_body(padding = 10,
          tags$div(tags$b(sprintf("%s · S%d %s", format(r$service_date, "%d/%m"), r$direction_id, r$horario)), " — ",
                   tags$span(style = "font-weight:600", r$rotulo),
                   if (!is.na(r$confidence_level)) tags$span(style = "color:#5f5e57", sprintf(" (confiança %s)", r$confidence_level))),
          tags$p(style = "font-size:.85rem;margin:4px 0 6px", EXPLICA[[r$audit_status]]),
          tags$table(style = "font-size:.82rem",
            linha_kv("partida observada", if (!is.na(r$t_obs)) sprintf("%s (%+.1f min)", fmt_h(r$t_obs), r$departure_difference_minutes)),
            linha_kv("veículo", r$observed_vehicle_id),
            linha_kv("status da viagem", r$trip_status),
            linha_kv("traçado percorrido", if (!is.null(r$pct_tracado) && !is.na(r$pct_tracado)) sprintf("%.0f%% (cobertura GPS %.0f%%)", r$pct_tracado, r$pct_cobertura)),
            linha_kv("ônibus sem GPS", r$veiculos_sem_gps),
            linha_kv("partida descartada", r$partida_descartada),
            linha_kv("GPS na janela", sprintf("%.0f%% dos minutos (%s)", 100 * r$frac_min_dados, r$data_coverage_status)),
            if (!is.null(v) && nrow(v)) tagList(
              linha_kv("viagem", v$trip_id),
              linha_kv("início/fim obs.", sprintf("%s → %s (%d min)", fmt_h(v$t_ini), fmt_h(v$t_fim), round(v$duration_minutes))),
              linha_kv("chegada", fmt_h(v$t_arr)),
              linha_kv("evidência partida/chegada", sprintf("%s / %s", v$origin_evidence, v$destination_evidence)),
              linha_kv("cobertura do percurso", sprintf("%.0f%% (maior lacuna %.0f s)", 100 * v$observation_coverage, v$lacuna_max_s)),
              linha_kv("vel. média", sprintf("%.1f km/h", v$vel_media_kmh)),
              linha_kv("flags", v$quality_flags)),
            linha_kv("duração", sprintf("%s (estimada pelas viagens próximas: %s)",
                                        if (is.na(r$dur_obs)) "não observada" else sprintf("%.0f min observados", r$dur_obs),
                                        if (is.na(r$dur_est)) "–" else sprintf("%.0f min", r$dur_est))),
            linha_kv("evidência", r$evidence_summary),
            if (!is.na(r$diagnostico_frota)) tagList(
              linha_kv("diagnóstico de frota", tags$b(ROTULO_DIAG[[r$diagnostico_frota]])),
              linha_kv("", r$detalhe_frota)),
            linha_kv("programação", sprintf("%s (%s)", r$schedule_source, r$schedule_quality_flag))
          )))
    }))
  })

  # ---------------- veículos do dia em foco: linha do tempo e tabela
  output$gantt <- renderPlot({
    D <- dia(); tt <- t_lento(); dst <- destaque()
    vg <- D$vg; lac <- D$lac
    req(nrow(vg) || nrow(D$pos))
    ord <- sort(unique(c(vg$vehicle_id, D$pos$vehicle_id)))
    vg <- copy(vg)[, y := match(vehicle_id, ord)]
    ex <- D$pos[valido == TRUE, .(ini = min(t), fim = max(t)), by = vehicle_id][, y := match(vehicle_id, ord)]
    st_cor <- c(viagem_confirmada = "#0ca30c", viagem_provavel = "#7cc97c", trajetoria_parcial = "#8f2a2a",
                observacao_inconclusiva = "#9a998f")
    g <- ggplot() +
      geom_segment(data = ex, aes(x = ini, xend = fim, y = y, yend = y), colour = "#e1e0d9", linewidth = 6) +
      geom_segment(data = vg, aes(x = t_ini, xend = t_fim, y = y, yend = y, colour = trajectory_status), linewidth = 4.5)
    if (nrow(lac)) {
      lac <- copy(lac)[, y := match(vehicle_id, ord)]
      if (!"aberta" %in% names(lac)) lac[, aberta := FALSE]
      fe <- lac[aberta == FALSE]; ab <- lac[aberta == TRUE & n_saidas > 0]
      if (nrow(fe)) g <- g + geom_rect(data = fe, aes(xmin = t0, xmax = t1, ymin = y - 0.38, ymax = y + 0.38),
                                       fill = "#d03b3b", alpha = 0.35, colour = "#d03b3b", linewidth = 0.3)
      # parou de transmitir e não voltou: faixa tracejada só quando há saídas que ele poderia ter feito
      if (nrow(ab)) g <- g + geom_rect(data = ab, aes(xmin = t0, xmax = t1, ymin = y - 0.3, ymax = y + 0.3),
                                       fill = NA, colour = "#d03b3b", linetype = "22", linewidth = 0.4) +
        geom_text(data = ab, aes(x = t0, y = y + 0.42, label = "sem sinal →"), hjust = 0, size = 2.6, colour = "#b02a2a")
    }
    sem <- D$au[audit_status %in% c("inconclusiva", "possivelmente_nao_realizada")]
    if (nrow(sem)) g <- g + geom_point(data = sem, aes(t_prog, length(ord) + 0.9, fill = rotulo), shape = 25, size = 3,
                                       colour = "#ffffff") +
      scale_fill_manual(values = setNames(STATUS$cor, STATUS$rotulo), name = "saída sem partida")
    g + geom_vline(xintercept = tt, colour = "#d03b3b", linewidth = 0.5) +
      annotate("label", x = tt, y = Inf, label = format(tt, "%H:%M"), vjust = 1.1, size = 3.2,
               fill = "#d03b3b", colour = "#ffffff", label.size = 0) +
      scale_colour_manual(values = st_cor, name = "viagem", drop = TRUE) +
      scale_y_continuous(breaks = seq_along(ord), labels = ifelse(ord %in% names(dst), paste0("▶ ", ord), ord),
                         expand = expansion(add = c(0.6, 1.3))) +
      scale_x_datetime(date_labels = "%H:%M", timezone = TZ, date_breaks = "1 hour") +
      labs(x = NULL, y = NULL, caption = "cinza = período com registros · vermelho = lacuna de GPS > 10 min (veículo em serviço) · tracejado = parou de transmitir e não voltou (com saídas que poderia ter feito)") +
      theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank(), legend.position = "top")
  }, res = 96)
  observeEvent(input$gantt_click, {
    x <- input$gantt_click$x
    if (is.null(x)) return()
    j <- janela(); x <- as.POSIXct(x, origin = "1970-01-01", tz = TZ)
    if (x >= j[["ini"]] && x <= j[["fim"]]) updateSliderInput(session, "t", value = x)
  })
  output$veiculos <- renderReactable({
    D <- dia(); v <- D$vdf
    if (!nrow(v)) return(reactable(data.frame(aviso = "Sem diagnóstico de frota para este dia (rode o pipeline).")))
    lac <- D$lac
    if (nrow(lac) && "aberta" %in% names(lac)) lac <- lac[aberta == FALSE | n_saidas > 0]
    det <- if (nrow(lac)) lac[, .(lacunas_detalhe = paste(sprintf("%s–%s (%s, %s%s)", substr(inicio_local, 1, 5),
                                                              ifelse(is.na(fim_local), "não voltou", substr(fim_local, 1, 5)),
                                                              ifelse(is.na(s1_km), sprintf("sumiu no km %.1f", s0_km),
                                                                     sprintf("%.0f min, km %.1f→%.1f", dur_min, s0_km, s1_km)),
                                                              gsub("_", " ", sub("_sem_sinal", " sem sinal", tipo)),
                                                              ifelse(is.na(saidas), "", paste0("; disputada por ", saidas))),
                                                      collapse = " | ")), by = vehicle_id] else data.table(vehicle_id = character())
    v <- det[v, on = "vehicle_id"]
    reactable(v[order(-saidas_explicaveis, -min_sem_gps), .(veiculo = vehicle_id, viagens, completas, parciais,
                                                             lacunas = lacunas_10min, min_sem_gps, cabem_viagem = lacunas_que_cabem_viagem,
                                                             parado_sem_sinal = lacunas_parado, saidas_explicaveis, padrao,
                                                             lacunas_detalhe)],
              compact = TRUE, striped = TRUE, defaultPageSize = 30,
              columns = list(lacunas_detalhe = colDef(name = "lacunas (sumiu–voltou, km, tipo)", minWidth = 420),
                             min_sem_gps = colDef(name = "min sem GPS"), cabem_viagem = colDef(name = "lacunas c/ viagem"),
                             saidas_explicaveis = colDef(name = "saídas explicáveis")))
  })
}

shinyApp(ui, server)
