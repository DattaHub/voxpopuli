## ============================================================
##  Wisdom of the Crowd — Shiny App  (single-file app.R)
##
##  USAGE:
##    shiny::runApp("path/to/folder/containing/this/app.R")
##    -- or open in RStudio and click Run App --
##
##  To share on a local network:
##    shiny::runApp(host = "0.0.0.0", port = 3838)
##
##  INSTRUCTOR SETTINGS: edit the four lines below only.
## ============================================================

library(shiny)
library(ggplot2)
library(scales)

## ── INSTRUCTOR SETTINGS ──────────────────────────────────────
TRUE_ANSWER  <- 147     # the real count
LOWER_BOUND  <- 50      # hint shown to students
UPPER_BOUND  <- 400     # hint shown to students
OBJECT_NAME  <- "gumballs"
## ─────────────────────────────────────────────────────────────

pal <- list(
  bg      = "#0f1117",
  panel   = "#1a1d27",
  accent1 = "#f5c518",
  accent2 = "#e05a5a",
  dot     = "#7eb8f7",
  text    = "#e8eaf0",
  subtext = "#8b90a0",
  border  = "#2d3145"
)

css <- sprintf("
* { box-sizing: border-box; margin: 0; padding: 0; }
body {
  background-color: %s; color: %s;
  font-family: 'DM Sans', 'Helvetica Neue', Arial, sans-serif;
  min-height: 100vh;
}
@import url('https://fonts.googleapis.com/css2?family=Syne:wght@700;800&family=DM+Sans:wght@300;400;500&display=swap');

.app-header {
  background: linear-gradient(135deg, #1a1d27 0%%, #0f1117 100%%);
  border-bottom: 2px solid %s;
  padding: 24px 36px 18px;
}
.app-title {
  font-family: 'Syne', sans-serif; font-size: 2.2rem; font-weight: 800;
  color: %s; letter-spacing: -0.5px; line-height: 1;
}
.app-subtitle { color: %s; font-size: 0.92rem; margin-top: 6px; }

.content-wrap { display: flex; min-height: calc(100vh - 100px); }

.left-panel {
  width: 300px; min-width: 250px;
  background: %s; border-right: 1px solid %s;
  padding: 26px 20px; display: flex; flex-direction: column; gap: 22px;
}

.hint-box {
  background: rgba(245,197,24,0.07);
  border: 1px solid rgba(245,197,24,0.3);
  border-radius: 10px; padding: 16px; text-align: center;
}
.hint-label {
  font-size: 0.70rem; letter-spacing: 2px; text-transform: uppercase;
  color: %s; margin-bottom: 8px;
}
.hint-range { font-family: 'Syne',sans-serif; font-size: 1.6rem; font-weight: 700; color: %s; }
.hint-sub   { font-size: 0.80rem; color: %s; margin-top: 4px; }

.input-section label {
  font-size: 0.76rem; letter-spacing: 1.5px; text-transform: uppercase;
  color: %s; display: block; margin-bottom: 7px;
}
.form-control, input[type=number] {
  width: 100%%; background: #0f1117 !important; border: 1px solid %s !important;
  border-radius: 8px !important; color: %s !important;
  font-size: 1.25rem !important; padding: 11px 13px !important;
  -moz-appearance: textfield;
}
.form-control:focus, input[type=number]:focus {
  border-color: %s !important; outline: none !important;
  box-shadow: 0 0 0 3px rgba(245,197,24,0.15) !important;
}

.btn-submit {
  width: 100%%; background: %s !important; color: #0f1117 !important;
  font-family: 'Syne',sans-serif !important; font-size: 0.95rem !important;
  font-weight: 700 !important; letter-spacing: 1px !important;
  border: none !important; border-radius: 8px !important;
  padding: 12px !important; cursor: pointer !important;
}
.btn-submit:hover { opacity: 0.88 !important; }

.btn-reveal {
  width: 100%%; background: transparent !important; color: %s !important;
  font-family: 'Syne',sans-serif !important; font-size: 0.88rem !important;
  font-weight: 700 !important; letter-spacing: 1px !important;
  border: 1px solid %s !important; border-radius: 8px !important;
  padding: 10px !important; cursor: pointer !important; margin-top: 4px;
}
.btn-reveal:hover { background: rgba(224,90,90,0.1) !important; }

.btn-reset {
  width: 100%%; background: transparent !important; color: %s !important;
  font-size: 0.80rem !important; border: 1px solid %s !important;
  border-radius: 8px !important; padding: 9px !important; cursor: pointer !important;
}

.stats-strip { display: flex; gap: 1px; background: %s; }
.stat-card   { flex: 1; background: %s; padding: 16px 14px; text-align: center; }
.stat-value  { font-family: 'Syne',sans-serif; font-size: 1.9rem; font-weight: 800; line-height: 1; }
.stat-label  { font-size: 0.70rem; letter-spacing: 1.5px; text-transform: uppercase; color: %s; margin-top: 5px; }
.stat-card.yellow .stat-value { color: %s; }
.stat-card.blue   .stat-value { color: %s; }
.stat-card.white  .stat-value { color: %s; }
.stat-card.red    .stat-value { color: %s; }

.main-panel  { flex: 1; display: flex; flex-direction: column; }
.plot-wrap   { flex: 1; padding: 18px 24px 12px; }

.reveal-banner {
  background: linear-gradient(90deg, rgba(224,90,90,0.15), rgba(224,90,90,0.03));
  border-top: 1px solid rgba(224,90,90,0.4);
  padding: 13px 24px; text-align: center;
  font-family: 'Syne',sans-serif; font-size: 1rem; font-weight: 700; color: %s;
}
",
  pal$bg, pal$text,           # body
  pal$border,                 # header border
  pal$accent1,                # title
  pal$subtext,                # subtitle
  pal$panel, pal$border,      # left panel
  pal$subtext, pal$accent1, pal$subtext,   # hint box
  pal$subtext,                # input label
  pal$border, pal$text, pal$accent1,       # input
  pal$accent1,                # btn-submit bg
  pal$accent2, pal$accent2,   # btn-reveal
  pal$subtext, pal$border,    # btn-reset
  pal$border, pal$panel,      # stats strip
  pal$subtext,                # stat-label
  pal$accent1, pal$dot, pal$text, pal$accent2,  # stat values
  pal$accent2                 # reveal banner
)

# ── UI ────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Syne:wght@700;800&family=DM+Sans:wght@300;400;500&display=swap"),
    tags$style(HTML(css))
  ),

  div(class = "app-header",
    div(class = "app-title", "Wisdom of the Crowd"),
    div(class = "app-subtitle",
        paste0("How many ", OBJECT_NAME, " are in the jar? ",
               "Submit your guess \u2014 no peeking at others'!"))
  ),

  div(class = "content-wrap",

    # ── Left panel ────────────────────────────────────────────
    div(class = "left-panel",

      div(class = "hint-box",
        div(class = "hint-label", "Your hint"),
        div(class = "hint-range",
            paste0(comma(LOWER_BOUND), " \u2013 ", comma(UPPER_BOUND))),
        div(class = "hint-sub",
            paste0("The count is between ", comma(LOWER_BOUND),
                   " and ", comma(UPPER_BOUND), "."))
      ),

      div(class = "input-section",
        numericInput("guess",
                     label = paste0("Estimate of ", OBJECT_NAME),
                     value = NA, min = 0, max = UPPER_BOUND * 3, step = 1),
        tags$br(),
        actionButton("submit", "Submit Guess", class = "btn-submit"),
        tags$br(), tags$br(),
        uiOutput("flash_msg")
      ),

      div(style = "margin-top: auto;",
        actionButton("reveal", "\u2691  Reveal True Answer", class = "btn-reveal"),
        tags$br(), tags$br(),
        actionButton("reset",  "\u21ba  Reset All Guesses",  class = "btn-reset")
      )
    ),

    # ── Main panel ────────────────────────────────────────────
    div(class = "main-panel",

      div(class = "stats-strip",
        div(class = "stat-card yellow",
            div(class = "stat-value", textOutput("stat_avg")),
            div(class = "stat-label", "crowd average")),
        div(class = "stat-card blue",
            div(class = "stat-value", textOutput("stat_n")),
            div(class = "stat-label", "guesses so far")),
        div(class = "stat-card white",
            div(class = "stat-value", textOutput("stat_med")),
            div(class = "stat-label", "crowd median")),
        div(class = "stat-card red",
            div(class = "stat-value", textOutput("stat_sd")),
            div(class = "stat-label", "std deviation"))
      ),

      div(class = "plot-wrap",
          plotOutput("main_plot", height = "430px")),

      uiOutput("reveal_banner")
    )
  )
)

# ── Server ────────────────────────────────────────────────────
server <- function(input, output, session) {

  guesses  <- reactiveVal(numeric(0))
  revealed <- reactiveVal(FALSE)
  flash    <- reactiveVal(NULL)

  observeEvent(input$submit, {
    req(input$guess)
    g <- as.numeric(input$guess)
    if (is.na(g) || g <= 0) {
      flash(list(msg = "\u26a0 Please enter a positive number.", ok = FALSE))
      return()
    }
    guesses(c(guesses(), g))
    flash(list(msg = paste0("\u2713  ", comma(round(g)), " recorded!"), ok = TRUE))
    updateNumericInput(session, "guess", value = NA)
  })

  observeEvent(input$reveal, { revealed(TRUE) })

  observeEvent(input$reset, {
    guesses(numeric(0))
    revealed(FALSE)
    flash(NULL)
  })

  # ── Stats ──────────────────────────────────────────────────
  output$stat_n   <- renderText({
    n <- length(guesses()); if (n == 0) "\u2014" else as.character(n)
  })
  output$stat_avg <- renderText({
    g <- guesses(); if (length(g) == 0) "\u2014" else comma(round(mean(g), 1))
  })
  output$stat_med <- renderText({
    g <- guesses(); if (length(g) == 0) "\u2014" else comma(round(median(g), 1))
  })
  output$stat_sd  <- renderText({
    g <- guesses(); if (length(g) < 2) "\u2014" else comma(round(sd(g), 1))
  })

  # ── Flash message ─────────────────────────────────────────
  output$flash_msg <- renderUI({
    f <- flash(); if (is.null(f)) return(NULL)
    col <- if (isTRUE(f$ok)) "#6fcf97" else "#e05a5a"
    div(style = paste0("color:", col,
                       ";font-size:0.87rem;text-align:center;padding:6px 0;"),
        f$msg)
  })

  # ── Reveal banner ─────────────────────────────────────────
  output$reveal_banner <- renderUI({
    if (!revealed() || length(guesses()) == 0) return(NULL)
    err <- abs(mean(guesses()) - TRUE_ANSWER)
    div(class = "reveal-banner",
        paste0("True answer: ", comma(TRUE_ANSWER), " ", OBJECT_NAME,
               "  \u00b7  Crowd average: ", comma(round(mean(guesses()), 1)),
               "  \u00b7  Error: ", round(err, 1)))
  })

  # ── Main plot ─────────────────────────────────────────────
  output$main_plot <- renderPlot({

    g   <- guesses()
    rev <- revealed()

    base_theme <- theme_void() +
      theme(
        plot.background  = element_rect(fill = pal$bg,    colour = NA),
        panel.background = element_rect(fill = pal$panel, colour = NA),
        panel.border     = element_rect(fill = NA, colour = pal$border, linewidth = 0.5),
        plot.margin      = margin(14, 18, 10, 14)
      )

    # Empty state
    if (length(g) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "Waiting for the first guess\u2026",
                   colour = pal$subtext, size = 5.5, fontface = "italic") +
          xlim(0, 1) + ylim(0, 1) + base_theme
      )
    }

    n      <- length(g)
    idx    <- seq_len(n)
    cum_mn <- cumsum(g) / idx

    # Safe axis range (no inline if inside min/max)
    g_min  <- min(g)
    g_max  <- max(g)
    x_lo   <- min(LOWER_BOUND * 0.80, g_min * 0.90)
    x_hi   <- max(UPPER_BOUND * 1.15, g_max * 1.10)
    x_rng  <- x_hi - x_lo

    # Running SD
    cum_sd <- vapply(idx, function(k) {
      if (k < 2) NA_real_ else sd(g[seq_len(k)])
    }, numeric(1))

    # Layout: two virtual strips on a single y axis
    # [strip_bot, strip_top] = dot strip; [conv_bot, conv_top] = convergence panel
    strip_bot <- 0.42; strip_top <- 1.00
    conv_bot  <- -0.80; conv_top <- 0.30
    conv_rng  <- conv_top - conv_bot

    # Map value -> convergence-panel y
    y_lo_val <- min(cum_mn, g, LOWER_BOUND, na.rm = TRUE)
    y_hi_val <- max(cum_mn, g, UPPER_BOUND, na.rm = TRUE)
    val_rng  <- max(y_hi_val - y_lo_val, 1)
    map_y    <- function(v) conv_bot + (v - y_lo_val) / val_rng * conv_rng

    # Map index -> x coordinate (reuses x axis = guess value in dot strip)
    map_x <- function(i) x_lo + (i - 1) / max(n - 1, 1) * x_rng

    set.seed(42)
    jit  <- runif(n, 0.3, 0.7) * (strip_top - strip_bot) + strip_bot
    df   <- data.frame(i = idx, guess = g, cum_mean = cum_mn,
                       cum_sd = cum_sd, y_jit = jit,
                       x_conv = map_x(idx), y_conv = map_y(cum_mn))

    p <- ggplot() +
      # ── panel labels
      annotate("text", x = x_lo, y = strip_top + 0.09,
               label = "INDIVIDUAL GUESSES", hjust = 0,
               colour = pal$subtext, size = 2.9) +
      annotate("text", x = x_lo, y = conv_top + 0.06,
               label = "RUNNING AVERAGE", hjust = 0,
               colour = pal$subtext, size = 2.9) +
      # ── divider
      annotate("segment", x = x_lo, xend = x_hi,
               y = strip_bot - 0.04, yend = strip_bot - 0.04,
               colour = pal$border, linewidth = 0.4) +
      # ── bound guides (dot strip)
      annotate("segment", x = LOWER_BOUND, xend = LOWER_BOUND,
               y = strip_bot, yend = strip_top,
               colour = pal$border, linewidth = 0.5, linetype = "dashed") +
      annotate("segment", x = UPPER_BOUND, xend = UPPER_BOUND,
               y = strip_bot, yend = strip_top,
               colour = pal$border, linewidth = 0.5, linetype = "dashed") +
      annotate("text", x = LOWER_BOUND, y = strip_bot - 0.07,
               label = comma(LOWER_BOUND), colour = pal$subtext, size = 2.8, hjust = 0.5) +
      annotate("text", x = UPPER_BOUND, y = strip_bot - 0.07,
               label = comma(UPPER_BOUND), colour = pal$subtext, size = 2.8, hjust = 0.5) +
      # ── dot strip: individual guesses
      geom_point(data = df, aes(x = guess, y = y_jit),
                 colour = pal$dot, size = 3.5, alpha = 0.80, shape = 16)

    # ── running average vline in dot strip
    cur_mean <- mean(g)
    p <- p +
      annotate("segment", x = cur_mean, xend = cur_mean,
               y = strip_bot, yend = strip_top,
               colour = pal$accent1, linewidth = 1.3) +
      annotate("text", x = cur_mean, y = strip_top + 0.09,
               label = paste0("avg: ", comma(round(cur_mean, 1))),
               colour = pal$accent1, size = 3.0, hjust = 0.5, fontface = "bold")

    # ── true answer in dot strip (if revealed)
    if (rev) {
      p <- p +
        annotate("segment", x = TRUE_ANSWER, xend = TRUE_ANSWER,
                 y = strip_bot, yend = strip_top,
                 colour = pal$accent2, linewidth = 1.2) +
        annotate("text", x = TRUE_ANSWER, y = strip_bot - 0.15,
                 label = paste0("true: ", comma(TRUE_ANSWER)),
                 colour = pal$accent2, size = 2.9, hjust = 0.5, fontface = "bold")
    }

    # ── convergence ribbon (n >= 2)
    df_rb <- df[!is.na(df$cum_sd), ]
    if (nrow(df_rb) >= 2) {
      df_rb$y_lo_r <- map_y(df_rb$cum_mean - df_rb$cum_sd)
      df_rb$y_hi_r <- map_y(df_rb$cum_mean + df_rb$cum_sd)
      p <- p +
        geom_ribbon(data = df_rb,
                    aes(x = x_conv, ymin = y_lo_r, ymax = y_hi_r),
                    fill = pal$accent1, alpha = 0.10)
    }

    # ── convergence line + points
    p <- p +
      geom_line(data = df, aes(x = x_conv, y = y_conv),
                colour = pal$accent1, linewidth = 1.3) +
      geom_point(data = df, aes(x = x_conv, y = y_conv),
                 colour = pal$accent1, size = 2.3, shape = 16)

    # ── true answer line in convergence panel (if revealed)
    if (rev) {
      y_true <- map_y(TRUE_ANSWER)
      p <- p +
        annotate("segment",
                 x = map_x(1), xend = map_x(n),
                 y = y_true, yend = y_true,
                 colour = pal$accent2, linewidth = 1.0, linetype = "dashed") +
        annotate("text", x = map_x(n), y = y_true,
                 label = paste0("  true = ", comma(TRUE_ANSWER)),
                 colour = pal$accent2, size = 2.9, hjust = 0, fontface = "bold")
    }

    # ── y-axis tick labels for convergence panel
    ref_vals <- pretty(c(y_lo_val, y_hi_val), n = 5)
    ref_vals <- ref_vals[ref_vals >= y_lo_val & ref_vals <= y_hi_val]
    for (rv in ref_vals) {
      p <- p +
        annotate("text", x = x_lo - x_rng * 0.01, y = map_y(rv),
                 label = comma(rv), colour = pal$subtext, size = 2.7, hjust = 1)
    }

    # ── x-axis label for convergence panel
    p <- p +
      annotate("text",
               x    = (map_x(1) + map_x(n)) / 2,
               y    = conv_bot - 0.15,
               label = "Guess number (chronological order)",
               colour = pal$subtext, size = 2.9, hjust = 0.5)

    p +
      xlim(x_lo - x_rng * 0.06, x_hi + x_rng * 0.07) +
      ylim(conv_bot - 0.28, strip_top + 0.22) +
      base_theme

  }, bg = pal$bg)
}

shinyApp(ui, server)
