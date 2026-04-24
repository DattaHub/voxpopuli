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
##  All instructor settings (true answer, bounds, object name,
##  group size) are now set INSIDE the app on the Setup tab.
##  No need to edit any code between sessions.
## ============================================================

library(shiny)
library(ggplot2)
library(scales)

## ── Default values (can be overridden in the app) ────────────
DEFAULT_TRUE    <- 150
DEFAULT_LOWER   <- 100
DEFAULT_UPPER   <- 200
DEFAULT_OBJECT  <- "gumballs"
DEFAULT_NGROUPS <- 6
DEFAULT_GSIZE   <- 5

## ── Colorblind-safe palette (Paul Tol "bright", light bg) ────
pal <- list(
  bg       = "#FAFAFA",
  panel    = "#FFFFFF",
  header   = "#1A3A5C",
  border   = "#C8D0DC",
  text     = "#1A1A2E",
  subtext  = "#4A5568",
  accent1  = "#0077BB",   # Tol blue  — running average
  accent2  = "#EE3377",   # Tol pink  — true answer
  dot      = "#009988",   # Tol teal  — individual guesses
  positive = "#228833",   # Tol green — success
  warning  = "#CC3311"    # Tol red   — error
)

## ── Group colours (Paul Tol bright, up to 6 groups) ──────────
GROUP_COLS <- c("#4477AA","#EE6677","#228833","#CCBB44","#66CCEE","#AA3377")

css <- "
@import url('https://fonts.googleapis.com/css2?family=Syne:wght@700;800&family=DM+Sans:wght@300;400;500&display=swap');
* { box-sizing: border-box; margin: 0; padding: 0; }
body {
  background-color: #FAFAFA; color: #1A1A2E;
  font-family: 'DM Sans', 'Helvetica Neue', Arial, sans-serif;
  min-height: 100vh;
}
.app-header {
  background: #1A3A5C; padding: 20px 36px 16px;
  border-bottom: 3px solid #0077BB;
}
.app-title {
  font-family: 'Syne', sans-serif; font-size: 2.3rem; font-weight: 800;
  color: #FFFFFF; letter-spacing: -0.4px; line-height: 1;
}
.app-subtitle { color: #A8C4DC; font-size: 1.05rem; margin-top: 6px; }

.nav-tabs { border-bottom: 2px solid #C8D0DC !important; margin-bottom: 0 !important; }
.nav-tabs > li > a {
  font-family: 'Syne', sans-serif !important; font-size: 0.95rem !important;
  font-weight: 700 !important; letter-spacing: 1px !important;
  text-transform: uppercase !important; color: #4A5568 !important;
  border: none !important; border-bottom: 3px solid transparent !important;
  border-radius: 0 !important; padding: 12px 20px !important;
  background: transparent !important;
}
.nav-tabs > li.active > a {
  color: #0077BB !important; border-bottom: 3px solid #0077BB !important;
  background: transparent !important;
}
.tab-content { padding: 0 !important; }

/* ── Setup tab ── */
.setup-wrap { max-width: 580px; margin: 36px auto; padding: 0 20px; }
.setup-card {
  background: #FFFFFF; border: 1px solid #C8D0DC;
  border-radius: 12px; padding: 26px 26px 20px;
  box-shadow: 0 2px 8px rgba(0,0,0,0.06); margin-bottom: 18px;
}
.setup-card h4 {
  font-family: 'Syne', sans-serif; font-size: 1.10rem; font-weight: 700;
  color: #1A3A5C; margin-bottom: 16px; padding-bottom: 8px;
  border-bottom: 1px solid #E2E8F0;
}
.setup-card label {
  font-size: 0.88rem; letter-spacing: 1.0px; text-transform: uppercase;
  color: #4A5568; font-weight: 500;
}
.setup-card .form-control, .setup-card input[type=number], .setup-card input[type=text] {
  background: #F7FAFC !important; border: 1px solid #C8D0DC !important;
  border-radius: 6px !important; color: #1A1A2E !important;
  font-size: 1.10rem !important; padding: 9px 12px !important;
}
.setup-card .form-control:focus, .setup-card input:focus {
  border-color: #0077BB !important;
  box-shadow: 0 0 0 3px rgba(0,119,187,0.12) !important; outline: none !important;
}
.btn-configure {
  background: #0077BB !important; color: #FFFFFF !important;
  font-family: 'Syne', sans-serif !important; font-size: 1.02rem !important;
  font-weight: 700 !important; letter-spacing: 1px !important;
  border: none !important; border-radius: 8px !important;
  padding: 12px 28px !important; cursor: pointer !important;
}
.btn-configure:hover { background: #005f99 !important; }
.config-status {
  background: #EBF8F2; border: 1px solid #228833;
  border-radius: 8px; padding: 11px 14px;
  color: #1A5C2E; font-size: 0.97rem; font-weight: 500; margin-top: 12px;
}

/* ── Live tab ── */
.content-wrap { display: flex; min-height: calc(100vh - 140px); }
.left-panel {
  width: 295px; min-width: 255px;
  background: #FFFFFF; border-right: 1px solid #C8D0DC;
  padding: 20px 16px; display: flex; flex-direction: column; gap: 16px;
}
.hint-box {
  background: rgba(0,119,187,0.06); border: 1px solid rgba(0,119,187,0.28);
  border-radius: 10px; padding: 13px; text-align: center;
}
.hint-label {
  font-size: 0.78rem; letter-spacing: 2px; text-transform: uppercase;
  color: #4A5568; margin-bottom: 6px;
}
.hint-range { font-family: 'Syne',sans-serif; font-size: 1.65rem; font-weight: 800; color: #0077BB; }
.hint-sub   { font-size: 0.90rem; color: #4A5568; margin-top: 3px; }
.group-badge {
  background: #EBF4FF; border: 1px solid rgba(0,119,187,0.22);
  border-radius: 8px; padding: 10px 13px; text-align: center;
}
.group-badge-title {
  font-size: 0.78rem; letter-spacing: 2px; text-transform: uppercase;
  color: #4A5568; margin-bottom: 3px;
}
.group-badge-val { font-family: 'Syne',sans-serif; font-size: 1.40rem; font-weight: 800; color: #1A3A5C; }
.input-section label {
  font-size: 0.85rem; letter-spacing: 1.0px; text-transform: uppercase;
  color: #4A5568; display: block; margin-bottom: 6px; font-weight: 500;
}
.input-section .form-control, .input-section input[type=number] {
  width: 100% !important; background: #F7FAFC !important;
  border: 2px solid #C8D0DC !important; border-radius: 8px !important;
  color: #1A1A2E !important; font-size: 1.50rem !important;
  padding: 10px 12px !important; -moz-appearance: textfield;
}
.input-section .form-control:focus, .input-section input[type=number]:focus {
  border-color: #0077BB !important;
  box-shadow: 0 0 0 3px rgba(0,119,187,0.12) !important; outline: none !important;
}
.btn-submit {
  width: 100% !important; background: #0077BB !important; color: #FFFFFF !important;
  font-family: 'Syne',sans-serif !important; font-size: 1.05rem !important;
  font-weight: 700 !important; letter-spacing: 1px !important;
  border: none !important; border-radius: 8px !important; padding: 12px !important;
  cursor: pointer !important; box-shadow: 0 2px 6px rgba(0,119,187,0.22) !important;
}
.btn-submit:hover { background: #005f99 !important; }
.btn-reveal {
  width: 100% !important; background: transparent !important; color: #EE3377 !important;
  font-family: 'Syne',sans-serif !important; font-size: 0.97rem !important;
  font-weight: 700 !important; letter-spacing: 1px !important;
  border: 2px solid #EE3377 !important; border-radius: 8px !important;
  padding: 10px !important; cursor: pointer !important;
}
.btn-reveal:hover { background: rgba(238,51,119,0.07) !important; }
.btn-next-group {
  width: 100% !important; background: #228833 !important; color: #FFFFFF !important;
  font-family: 'Syne',sans-serif !important; font-size: 0.97rem !important;
  font-weight: 700 !important; border: none !important; border-radius: 8px !important;
  padding: 10px !important; cursor: pointer !important;
}
.btn-next-group:hover { background: #1a6626 !important; }
.btn-reset {
  width: 100% !important; background: transparent !important; color: #4A5568 !important;
  font-size: 0.90rem !important; border: 1px solid #C8D0DC !important;
  border-radius: 8px !important; padding: 9px !important; cursor: pointer !important;
}
.btn-reset:hover { background: #F7FAFC !important; }

.stats-strip { display: flex; gap: 1px; background: #C8D0DC; border-bottom: 1px solid #C8D0DC; }
.stat-card   { flex: 1; background: #FFFFFF; padding: 14px 8px; text-align: center; }
.stat-value  { font-family: 'Syne',sans-serif; font-size: 2.0rem; font-weight: 800; line-height: 1; }
.stat-label  { font-size: 0.80rem; letter-spacing: 1.2px; text-transform: uppercase; color: #4A5568; margin-top: 4px; }
.stat-card.s-avg .stat-value { color: #0077BB; }
.stat-card.s-n   .stat-value { color: #009988; }
.stat-card.s-med .stat-value { color: #1A3A5C; }
.stat-card.s-sd  .stat-value { color: #EE3377; }
.main-panel { flex: 1; display: flex; flex-direction: column; }
.plot-wrap  { flex: 1; padding: 16px 20px 10px; }
.reveal-banner {
  background: linear-gradient(90deg, rgba(238,51,119,0.09), rgba(238,51,119,0.01));
  border-top: 2px solid rgba(238,51,119,0.38);
  padding: 13px 22px; text-align: center;
  font-family: 'Syne',sans-serif; font-size: 1.10rem;
  font-weight: 700; color: #B01050;
}

/* ── Pitch tab ── */
.pitch-wrap { max-width: 700px; margin: 34px auto; padding: 0 22px; }
.pitch-card {
  background: #FFFFFF; border: 1px solid #C8D0DC; border-radius: 12px;
  padding: 26px 28px; box-shadow: 0 2px 8px rgba(0,0,0,0.06); margin-bottom: 16px;
}
.pitch-card h4 {
  font-family: 'Syne',sans-serif; font-size: 1.10rem; font-weight: 700;
  color: #1A3A5C; margin-bottom: 12px; padding-bottom: 8px;
  border-bottom: 1px solid #E2E8F0;
}
.pitch-card p  { font-size: 1.02rem; color: #2D3748; line-height: 1.72; margin-bottom: 9px; }
.pitch-card ul { padding-left: 18px; }
.pitch-card li { font-size: 1.00rem; color: #2D3748; line-height: 1.80; }
.stage-label {
  display: inline-block; background: #0077BB; color: #fff;
  font-size: 0.80rem; font-weight: 700; letter-spacing: 1.5px;
  text-transform: uppercase; border-radius: 4px; padding: 3px 9px; margin-bottom: 7px;
}
"

# ── UI ────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Syne:wght@700;800&family=DM+Sans:wght@300;400;500&display=swap"),
    tags$style(HTML(css))
  ),
  
  div(class = "app-header",
      div(class = "app-title",    "Wisdom of the Crowd"),
      div(class = "app-subtitle", "A live classroom experiment in collective estimation")
  ),
  
  tabsetPanel(id = "main_tabs",
              
              # ══ TAB 1: SETUP ══════════════════════════════════════════
              tabPanel("Setup",
                       div(class = "setup-wrap",
                           
                           div(class = "setup-card",
                               tags$h4("\u2699\ufe0f  Experiment Settings"),
                               fluidRow(
                                 column(6, numericInput("cfg_lower",  "Lower bound (hint shown to students)",
                                                        value = DEFAULT_LOWER,  min = 0,   step = 1)),
                                 column(6, numericInput("cfg_upper",  "Upper bound (hint shown to students)",
                                                        value = DEFAULT_UPPER,  min = 1,   step = 1))
                               ),
                               fluidRow(
                                 column(6, numericInput("cfg_true",   "True answer (kept secret until reveal)",
                                                        value = DEFAULT_TRUE,   min = 1,   step = 1)),
                                 column(6, textInput(   "cfg_object", "Object name (e.g. gumballs)",
                                                        value = DEFAULT_OBJECT))
                               )
                           ),
                           
                           div(class = "setup-card",
                               tags$h4("\U1F465  Group Settings"),
                               fluidRow(
                                 column(6, numericInput("cfg_ngroups", "Number of groups",
                                                        value = DEFAULT_NGROUPS, min = 1, max = 20, step = 1)),
                                 column(6, numericInput("cfg_gsize",   "Students per group",
                                                        value = DEFAULT_GSIZE,   min = 1, max = 30, step = 1))
                               )
                           ),
                           
                           actionButton("btn_configure", "\u2713  Save & Go to Live Session",
                                        class = "btn-configure"),
                           uiOutput("config_status")
                       )
              ),
              
              # ══ TAB 2: LIVE SESSION ═══════════════════════════════════
              tabPanel("Live Session",
                       div(class = "content-wrap",
                           
                           div(class = "left-panel",
                               uiOutput("hint_box"),
                               uiOutput("group_badge"),
                               
                               div(class = "input-section",
                                   uiOutput("guess_input_ui"),
                                   tags$br(),
                                   actionButton("submit", "Submit Guess", class = "btn-submit"),
                                   tags$br(), tags$br(),
                                   uiOutput("flash_msg")
                               ),
                               
                               div(style = "margin-top: auto; display: flex; flex-direction: column; gap: 7px;",
                                   actionButton("next_group", "\u25b6  Next Group",       class = "btn-next-group"),
                                   actionButton("reveal",     "\u2691  Reveal True Answer", class = "btn-reveal"),
                                   actionButton("reset",      "\u21ba  Reset Everything",   class = "btn-reset")
                               )
                           ),
                           
                           div(class = "main-panel",
                               div(class = "stats-strip",
                                   div(class = "stat-card s-avg",
                                       div(class = "stat-value", textOutput("stat_avg")),
                                       div(class = "stat-label", "crowd average")),
                                   div(class = "stat-card s-n",
                                       div(class = "stat-value", textOutput("stat_n")),
                                       div(class = "stat-label", "guesses so far")),
                                   div(class = "stat-card s-med",
                                       div(class = "stat-value", textOutput("stat_med")),
                                       div(class = "stat-label", "crowd median")),
                                   div(class = "stat-card s-sd",
                                       div(class = "stat-value", textOutput("stat_sd")),
                                       div(class = "stat-label", "std deviation"))
                               ),
                               div(class = "plot-wrap",
                                   plotOutput("main_plot", height = "430px")),
                               uiOutput("reveal_banner")
                           )
                       )
              ),
              
              # ══ TAB 3: INSTRUCTOR SCRIPT ══════════════════════════════
              tabPanel("Instructor Script",
                       div(class = "pitch-wrap",
                           
                           div(class = "pitch-card",
                               tags$h4("\U1F4CB  What This Experiment Demonstrates"),
                               tags$p("This replicates Francis Galton's 1907 'vox populi' experiment.
                  Galton found that the median of ~800 crowd estimates of an ox's weight
                  was within 1% of the true value. The theoretical underpinning is
                  Condorcet's Jury Theorem extended to estimation: independent, unbiased
                  estimators average out their errors, and the crowd mean converges to
                  the truth."),
                               tags$p("With 5 students per group you won't see dramatic convergence, but the
                  running average will typically land closer to the true value than most
                  individual guesses \u2014 and the discussion about why is the real lesson.")
                           ),
                           
                           div(class = "pitch-card",
                               tags$h4("\U1F3AC  Before the Group Arrives"),
                               tags$ul(
                                 tags$li("Go to the Setup tab. Enter the true count, bounds, object name, and
                     group size. Click Save."),
                                 tags$li("Place the jar where students can see it but cannot easily count."),
                                 tags$li("Write the hint bounds on the board: 'The answer is between [lower] and [upper].'"),
                                 tags$li("Turn the laptop screen away from students while they submit \u2014
                     this prevents anchoring on the running average.")
                               )
                           ),
                           
                           div(class = "pitch-card",
                               tags$h4("\U1F5E3\ufe0f  Script: What to Say"),
                               div(class = "stage-label", "Introduction (~ 1 min)"),
                               tags$p(HTML("<em>\"I have a jar of objects in front of you. Your only job is
                  to estimate how many are inside. The number is somewhere between
                  [LOWER] and [UPPER] \u2014 that's your only hint. Take a moment to
                  look, then come up one at a time and type your guess.
                  Your guess is private; no one else will see it until the end.\"</em>")),
                               div(class = "stage-label", "Collecting guesses (~ 2 min)"),
                               tags$p(HTML("<em>\"Each of you, when it's your turn: type your number and
                  press Submit. Please don't share your guess with others yet.\"</em>")),
                               div(class = "stage-label", "Reveal and discussion (~ 3 min)"),
                               tags$p(HTML("<em>\"Here are your five guesses. The blue line is your group's
                  running average. Now \u2014 let me reveal the true answer.\"</em>
                  [Click Reveal True Answer]")),
                               tags$p(HTML("<em>\"How many of you individually beat the group average?
                  Usually very few \u2014 why? What happens to individual errors
                  when we add independent estimates together?\"</em>")),
                               div(class = "stage-label", "Moving on"),
                               tags$p("Click 'Next Group' to archive this group's guesses and reset the
                  input for the next group. The convergence panel accumulates across
                  all groups, so you can show how the overall average tightens as
                  more students contribute.")
                           ),
                           
                           div(class = "pitch-card",
                               tags$h4("\U1F4A1  Discussion Prompts (after all groups)"),
                               tags$ul(
                                 tags$li("Did the crowd average improve as more groups contributed?
                     Look at the convergence panel."),
                                 tags$li("What would happen if students could see each other's guesses
                     before submitting? (Information cascades break independence.)"),
                                 tags$li("Galton originally used the median, not the mean. Does it matter
                     here? When is the median more robust?"),
                                 tags$li(HTML("If each person is on the correct side of the truth with
                     probability p > 1/2, Cram&eacute;r's theorem gives
                     exponential decay of the error probability in n \u2014
                     much faster than the CLT's 1/&radic;n rate. Why?"))
                               )
                           )
                       )
              )
  )
)

# ── Server ────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── Config reactive values ─────────────────────────────────
  cfg <- reactiveValues(
    lower   = DEFAULT_LOWER,
    upper   = DEFAULT_UPPER,
    true    = DEFAULT_TRUE,
    object  = DEFAULT_OBJECT,
    ngroups = DEFAULT_NGROUPS,
    gsize   = DEFAULT_GSIZE
  )
  
  observeEvent(input$btn_configure, {
    lo <- input$cfg_lower;  hi <- input$cfg_upper
    tr <- input$cfg_true;   ob <- trimws(input$cfg_object)
    ng <- input$cfg_ngroups; gs <- input$cfg_gsize
    
    err_msg <- NULL
    if (any(is.na(c(lo, hi, tr, ng, gs)))) err_msg <- "All fields must be filled in."
    else if (lo >= hi)  err_msg <- "Lower bound must be less than upper bound."
    else if (tr <= 0)   err_msg <- "True answer must be a positive number."
    else if (ob == "")  err_msg <- "Object name cannot be empty."
    
    if (!is.null(err_msg)) {
      output$config_status <- renderUI(
        div(class = "config-status",
            style = "background:#FFF0F0;border-color:#CC3311;color:#7A1010;",
            paste0("\u26a0 ", err_msg))
      )
      return()
    }
    
    cfg$lower <- lo; cfg$upper <- hi; cfg$true <- tr
    cfg$object <- ob; cfg$ngroups <- ng; cfg$gsize <- gs
    
    output$config_status <- renderUI(
      div(class = "config-status",
          sprintf("\u2713 Saved!  True = %s  |  Bounds: %s \u2013 %s  |  %d groups \u00d7 %d students",
                  comma(tr), comma(lo), comma(hi), ng, gs))
    )
    updateTabsetPanel(session, "main_tabs", selected = "Live Session")
  })
  
  # ── Session state ──────────────────────────────────────────
  guesses     <- reactiveVal(numeric(0))
  group_ids   <- reactiveVal(integer(0))
  current_grp <- reactiveVal(1L)
  revealed    <- reactiveVal(FALSE)
  flash       <- reactiveVal(NULL)
  
  # ── Reactive UI elements ───────────────────────────────────
  output$guess_input_ui <- renderUI({
    numericInput("guess",
                 label = paste0("Your estimate of ", cfg$object),
                 value = NA, min = cfg$lower, max = cfg$upper, step = 1)
  })
  
  output$hint_box <- renderUI({
    div(class = "hint-box",
        div(class = "hint-label", "Your hint"),
        div(class = "hint-range",
            paste0(comma(cfg$lower), " \u2013 ", comma(cfg$upper))),
        div(class = "hint-sub",
            paste0("Enter a number between ",
                   comma(cfg$lower), " and ", comma(cfg$upper), "."))
    )
  })
  
  output$group_badge <- renderUI({
    grp  <- current_grp()
    done <- sum(group_ids() == grp)
    div(class = "group-badge",
        div(class = "group-badge-title", "Current group"),
        div(class = "group-badge-val",
            sprintf("Group %d / %d", grp, cfg$ngroups)),
        div(style = "font-size:0.90rem;color:#4A5568;margin-top:3px;",
            sprintf("%d / %d guesses in", done, cfg$gsize))
    )
  })
  
  # ── Submit ─────────────────────────────────────────────────
  observeEvent(input$submit, {
    req(input$guess)
    g   <- as.numeric(input$guess)
    lo  <- cfg$lower; hi <- cfg$upper
    grp <- current_grp()
    
    # Validation: within bounds
    if (is.na(g) || g < lo || g > hi) {
      flash(list(
        msg = sprintf("\u26a0 Please enter a number between %s and %s.",
                      comma(lo), comma(hi)),
        ok  = FALSE))
      return()
    }
    
    # Validation: group not already full
    done <- sum(group_ids() == grp)
    if (done >= cfg$gsize) {
      flash(list(
        msg = sprintf("\u26a0 Group %d already has %d guesses. Click Next Group first.",
                      grp, cfg$gsize),
        ok  = FALSE))
      return()
    }
    
    guesses(c(guesses(), g))
    group_ids(c(group_ids(), grp))
    flash(list(msg = paste0("\u2713  ", comma(round(g)), " recorded!"), ok = TRUE))
    updateNumericInput(session, "guess", value = NA)
  })
  
  # ── Next Group ─────────────────────────────────────────────
  observeEvent(input$next_group, {
    grp <- current_grp()
    if (grp < cfg$ngroups) {
      current_grp(grp + 1L)
      revealed(FALSE)
      flash(list(msg = paste0("\u25b6 Starting Group ", grp + 1L, "."), ok = TRUE))
    } else {
      flash(list(msg = "\u2713 All groups complete! Use Reset to start over.", ok = TRUE))
    }
  })
  
  observeEvent(input$reveal, { revealed(TRUE) })
  
  observeEvent(input$reset, {
    guesses(numeric(0)); group_ids(integer(0))
    current_grp(1L); revealed(FALSE); flash(NULL)
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
  
  # ── Flash message ──────────────────────────────────────────
  output$flash_msg <- renderUI({
    f <- flash(); if (is.null(f)) return(NULL)
    col <- if (isTRUE(f$ok)) "#228833" else "#CC3311"
    div(style = paste0("color:", col,
                       ";font-size:0.97rem;text-align:center;",
                       "padding:5px 0;font-weight:500;"),
        f$msg)
  })
  
  # ── Reveal banner ──────────────────────────────────────────
  output$reveal_banner <- renderUI({
    g <- guesses()
    if (!revealed() || length(g) == 0) return(NULL)
    err <- abs(mean(g) - cfg$true)
    div(class = "reveal-banner",
        paste0("True answer: ", comma(cfg$true), " ", cfg$object,
               "  \u00b7  Crowd average: ", comma(round(mean(g), 1)),
               "  \u00b7  Error: ", round(err, 1)))
  })
  
  # ── Main plot ──────────────────────────────────────────────
  output$main_plot <- renderPlot({
    
    g    <- guesses()
    gids <- group_ids()
    rev  <- revealed()
    lo   <- cfg$lower; hi <- cfg$upper; tr <- cfg$true
    
    base_theme <- theme_void() +
      theme(
        plot.background  = element_rect(fill = pal$bg,    colour = NA),
        panel.background = element_rect(fill = pal$panel, colour = NA),
        panel.border     = element_rect(fill = NA, colour = pal$border, linewidth = 0.6),
        plot.margin      = margin(14, 18, 10, 14)
      )
    
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
    
    g_min  <- min(g)
    g_max  <- max(g)
    x_lo   <- min(lo * 0.85, g_min * 0.90)
    x_hi   <- max(hi * 1.12, g_max * 1.08)
    x_rng  <- x_hi - x_lo
    
    cum_sd <- vapply(idx, function(k) {
      if (k < 2) NA_real_ else sd(g[seq_len(k)])
    }, numeric(1))
    
    strip_bot <- 0.42; strip_top <- 1.00
    conv_bot  <- -0.80; conv_top <- 0.30
    conv_rng  <- conv_top - conv_bot
    
    y_lo_val <- min(cum_mn, g, lo, na.rm = TRUE)
    y_hi_val <- max(cum_mn, g, hi, na.rm = TRUE)
    val_rng  <- max(y_hi_val - y_lo_val, 1)
    map_y    <- function(v) conv_bot + (v - y_lo_val) / val_rng * conv_rng
    map_x    <- function(i) x_lo + (i - 1) / max(n - 1, 1) * x_rng
    
    set.seed(42)
    jit <- runif(n, 0.3, 0.7) * (strip_top - strip_bot) + strip_bot
    df  <- data.frame(
      i        = idx,
      guess    = g,
      group    = factor(gids),
      cum_mean = cum_mn,
      cum_sd   = cum_sd,
      y_jit    = jit,
      x_conv   = map_x(idx),
      y_conv   = map_y(cum_mn)
    )
    
    ngrp  <- length(unique(gids))
    gcols <- setNames(GROUP_COLS[seq_len(ngrp)], as.character(sort(unique(gids))))
    
    p <- ggplot() +
      annotate("text", x = x_lo, y = strip_top + 0.09,
               label = "INDIVIDUAL GUESSES (by group)", hjust = 0,
               colour = pal$subtext, size = 3.6) +
      annotate("text", x = x_lo, y = conv_top + 0.06,
               label = "RUNNING AVERAGE (all groups)", hjust = 0,
               colour = pal$subtext, size = 3.6) +
      annotate("segment", x = x_lo, xend = x_hi,
               y = strip_bot - 0.04, yend = strip_bot - 0.04,
               colour = pal$border, linewidth = 0.5) +
      annotate("segment", x = lo, xend = lo,
               y = strip_bot, yend = strip_top,
               colour = "#888888", linewidth = 0.6, linetype = "dashed") +
      annotate("segment", x = hi, xend = hi,
               y = strip_bot, yend = strip_top,
               colour = "#888888", linewidth = 0.6, linetype = "dashed") +
      annotate("text", x = lo, y = strip_bot - 0.07,
               label = comma(lo), colour = pal$subtext, size = 3.5, hjust = 0.5) +
      annotate("text", x = hi, y = strip_bot - 0.07,
               label = comma(hi), colour = pal$subtext, size = 3.5, hjust = 0.5) +
      geom_point(data = df, aes(x = guess, y = y_jit, colour = group),
                 size = 4.5, alpha = 0.85, shape = 16) +
      scale_colour_manual(values = gcols, name = "Group",
                          guide  = guide_legend(override.aes = list(size = 4)))
    
    # Running average vline in dot strip
    cur_mean <- mean(g)
    p <- p +
      annotate("segment", x = cur_mean, xend = cur_mean,
               y = strip_bot, yend = strip_top,
               colour = pal$accent1, linewidth = 1.6) +
      annotate("text", x = cur_mean, y = strip_top + 0.09,
               label = paste0("avg: ", comma(round(cur_mean, 1))),
               colour = pal$accent1, size = 3.8, hjust = 0.5, fontface = "bold")
    
    # True answer vline in dot strip
    if (rev) {
      p <- p +
        annotate("segment", x = tr, xend = tr,
                 y = strip_bot, yend = strip_top,
                 colour = pal$accent2, linewidth = 1.5) +
        annotate("text", x = tr, y = strip_bot - 0.16,
                 label = paste0("true: ", comma(tr)),
                 colour = pal$accent2, size = 3.6, hjust = 0.5, fontface = "bold")
    }
    
    # Convergence ribbon
    df_rb <- df[!is.na(df$cum_sd), ]
    if (nrow(df_rb) >= 2) {
      df_rb$y_lo_r <- map_y(df_rb$cum_mean - df_rb$cum_sd)
      df_rb$y_hi_r <- map_y(df_rb$cum_mean + df_rb$cum_sd)
      p <- p +
        geom_ribbon(data = df_rb,
                    aes(x = x_conv, ymin = y_lo_r, ymax = y_hi_r),
                    fill = pal$accent1, alpha = 0.13, inherit.aes = FALSE)
    }
    
    # Convergence line + dots
    p <- p +
      geom_line(data  = df, aes(x = x_conv, y = y_conv),
                colour = pal$accent1, linewidth = 1.4, inherit.aes = FALSE) +
      geom_point(data = df, aes(x = x_conv, y = y_conv),
                 colour = pal$accent1, size = 2.5, shape = 16, inherit.aes = FALSE)
    
    # True answer in convergence panel
    if (rev) {
      y_true <- map_y(tr)
      p <- p +
        annotate("segment",
                 x = map_x(1), xend = map_x(n),
                 y = y_true, yend = y_true,
                 colour = pal$accent2, linewidth = 1.1, linetype = "dashed") +
        annotate("text", x = map_x(n), y = y_true,
                 label = paste0("  true = ", comma(tr)),
                 colour = pal$accent2, size = 3.6, hjust = 0, fontface = "bold")
    }
    
    # Y-axis ticks
    ref_vals <- pretty(c(y_lo_val, y_hi_val), n = 5)
    ref_vals <- ref_vals[ref_vals >= y_lo_val & ref_vals <= y_hi_val]
    for (rv in ref_vals) {
      p <- p +
        annotate("text", x = x_lo - x_rng * 0.01, y = map_y(rv),
                 label = comma(rv), colour = pal$subtext, size = 3.4, hjust = 1)
    }
    
    # X-axis label
    p <- p +
      annotate("text",
               x = (map_x(1) + map_x(n)) / 2,
               y = conv_bot - 0.15,
               label = "Guess number (order submitted)",
               colour = pal$subtext, size = 3.5, hjust = 0.5)
    
    p +
      xlim(x_lo - x_rng * 0.07, x_hi + x_rng * 0.08) +
      ylim(conv_bot - 0.28, strip_top + 0.22) +
      base_theme +
      theme(
        legend.position   = "right",
        legend.title      = element_text(size = 11, colour = pal$subtext),
        legend.text       = element_text(size = 11, colour = pal$text),
        legend.key        = element_rect(fill = NA, colour = NA),
        legend.background = element_rect(fill = pal$bg, colour = NA)
      )
    
  }, bg = pal$bg)
}

shinyApp(ui, server)