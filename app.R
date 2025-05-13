library(shiny)
library(tidyverse)
library(gt)
library(gtExtras)
library(readxl)
library(glue)
library(shinyBS)
library(RSQLite)

# Conexión a la base de datos ---------------------------------------------

con <- dbConnect(RSQLite::SQLite(), 
                 "data/indicadores_informe_local_voluntario_quito.db")

# Tablas ------------------------------------------------------------------
etiquetas_poa_pai <- dbGetQuery(con, "select * from etiquetas_poa_pai")

etiquetas_sectores <- dbGetQuery(con, "select * from etiquetas_sectores_ejes")


# Cargar los datos (asegúrate de que la ruta sea correcta)
datos <- dbGetQuery(con, "select * from tabla_aporte_calculado") 

metas <- read_excel("data/Global Indicator Framework after refinement.Spanish.xlsx",range = "B3:B272")

metas <- metas %>% filter(!is.na(`Objetivos y metas (de la Agenda 2030 para el Desarrollo Sostenible)`))


ods <- metas %>% 
  filter(str_detect(`Objetivos y metas (de la Agenda 2030 para el Desarrollo Sostenible)`, "^Objetivo"))


ods <- ods %>% mutate(ODS = `Objetivos y metas (de la Agenda 2030 para el Desarrollo Sostenible)` %>% 
                        str_extract("^Objetivo \\d+\\.") %>% 
                        str_remove("Objetivo") %>%
                        str_remove("\\.") %>%
                        str_trim()) %>% 
  rename(ODS_Descripcion = `Objetivos y metas (de la Agenda 2030 para el Desarrollo Sostenible)`)


metas_lab <- metas %>% 
  filter(str_detect(`Objetivos y metas (de la Agenda 2030 para el Desarrollo Sostenible)`, "^Objetivo",negate = T)) %>% 
  mutate(META_temp = str_extract(`Objetivos y metas (de la Agenda 2030 para el Desarrollo Sostenible)`, "^\\d+\\.[a-zA-Z0-9]+") ) %>% 
  separate(META_temp, into = c("ODS","META"), sep = "\\.") %>% 
  filter(!is.na(ODS)) %>% 
  rename(META_Descripcion = `Objetivos y metas (de la Agenda 2030 para el Desarrollo Sostenible)`) 




metas_lab <- metas_lab %>% 
  left_join(ods, by = "ODS") 

colores_ods <- c(
  "#E5243B", # 1
  "#DDA63A", # 2
  "#4C9F38", # 3
  "#C5192D", # 4
  "#FF3A21", # 5
  "#26BDE2", # 6
  "#FCC30B", # 7
  "#A21942", # 8
  "#FD6925", # 9
  "#DD1367", # 10
  "#FD9D24", # 11
  "#BF8B2E", # 12
  "#3F7E44", # 13
  "#0A97D9", # 14
  "#56C02B", # 15
  "#00689D", # 16
  "#19486A"  # 17
)




crear_tabla_ods <- function(datos, num_ods, descripcion_ods) {
  # Definir colores para cada ODS (puedes personalizarlos)

  
  # Seleccionar color según número de ODS
  color_ods <- colores_ods[num_ods]
  
  # Crear tabla gt
  tabla_gt <- datos %>%
    mutate(
      META = str_extract(META_Descripcion, "^\\d+\\.[a-zA-Z0-9]+"),
      META_Descripcion = str_remove(META_Descripcion, "^\\d+\\.[a-zA-Z0-9]+")
    ) %>%
    arrange(desc(aporte)) %>% # Ordenar por aporte en orden descendente
    select(META, META_Descripcion, aporte, proyectos, programas) %>%
    gt() %>%
    tab_header(
      title = md(glue("**<span style='color:white'>Aporte al {descripcion_ods}</span>**"))
    ) %>%
    cols_label(
      META = "Meta",
      META_Descripcion = "",
      aporte = "% Aporte"
    ) %>%
    fmt_percent(
      columns = aporte,
      decimals = 1
    ) %>%
    data_color(
      columns = c(aporte, proyectos, programas), # Aplicar gradiente a estas columnas
      colors = scales::col_numeric(
        palette = c("white", color_ods), # Gradiente de blanco al color del ODS
        domain = NULL
      )
    ) %>%
    tab_style(
      style = cell_fill(color = color_ods),
      locations = cells_title(groups = "title")
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = color_ods),
        cell_text(color = "white")
      ),
      locations = cells_column_labels()
    ) %>%
    tab_source_note(source_note = "Elaboración: Alex Bajaña; Revisión: Axel Guanoluisa") %>%
    cols_width(
      META ~ pct(5),
      META_Descripcion ~ pct(68),
      aporte ~ pct(9),
      proyectos ~ pct(9),
      programas ~ pct(9)
    ) %>%
    gtExtras::gt_theme_538() 
  
  return(tabla_gt)
}


crear_tabla_ods_agrupada <- function(datos, num_ods, descripcion_ods) {

  color_ods <- colores_ods[num_ods]
  
  # browser()
  
  # Crear tabla gt con formato definitivo
  tabla_gt <- datos  %>% mutate(
    POA_PAI_label = case_when(
      POA_PAI == "POA" ~ "Plan Operativo Anual",
      POA_PAI == "PAI" ~ "Plan Anual de Inversiones",
      TRUE ~ as.character(POA_PAI)
    )
  ) %>% 
    gt(rowname_col = "POA_PAI_label") %>%
    tab_header(
      title = md(glue("**<span style='color:white'>Resumen de planificación: {descripcion_ods}</span>**"))
    ) %>%
    fmt_percent(
      columns = cumplimiento,
      decimals = 1
    ) %>%
    fmt_percent(
      columns = aporte,
      decimals = 1
    ) %>%
    fmt_number(
      columns = c(proyectos, metas, programas, proyectos),
      decimals = 0
    ) %>%
    data_color(
      columns = c(proyectos, metas, programas, proyectos),
      colors = scales::col_numeric(
        palette = c("white", color_ods),
        domain = NULL
      )
    ) %>%
    tab_style(
      style = cell_fill(color = color_ods),
      locations = cells_title(groups = "title")
    ) %>%
    tab_style(
      style = list(cell_fill(color = color_ods), cell_text(color = "white")),
      locations = cells_column_labels()
    ) %>% 
    tab_source_note(source_note = "Elaboración: Alex Bajaña; Revisión: Axel Guanoluisa") %>%
    gtExtras::gt_theme_538()
  
  return(tabla_gt)
}


crear_tabla_ods_sinergia <- function(datos, num_ods, descripcion_ods) {
  
  color_ods <- colores_ods[num_ods]
  
  # browser()
  
  # Crear tabla gt con formato definitivo
  tabla_gt <- datos  %>% 
  # %>% mutate(
  #   POA_PAI_label = case_when(
  #     POA_PAI == "POA" ~ "Plan Operativo Anual",
  #     POA_PAI == "PAI" ~ "Plan Anual de Inversiones",
  #     TRUE ~ as.character(POA_PAI)
  #   )
  # ) %>% 
    gt() %>%
    tab_header(
      title = md(glue("**<span style='color:white'>Sinergias del {descripcion_ods}</span>**"))
    ) %>%
    fmt_percent(
      columns = cumplimiento,
      decimals = 1
    ) %>%
    fmt_percent(
      columns = aporte,
      decimals = 1
    ) %>%
    fmt_number(
      columns = c(proyectos,
                  metas),
      decimals = 0
    ) %>%
    data_color(
      columns = c(proyectos,
                  metas),
      colors = scales::col_numeric(
        palette = c("white", color_ods),
        domain = NULL
      )
    ) %>%
    tab_style(
      style = cell_fill(color = color_ods),
      locations = cells_title(groups = "title")
    ) %>%
    tab_style(
      style = list(cell_fill(color = color_ods), cell_text(color = "white")),
      locations = cells_column_labels()
    ) %>% 
    tab_source_note(source_note = "Elaboración: Alex Bajaña; Revisión: Axel Guanoluisa") %>%
    gtExtras::gt_theme_538()
  
  return(tabla_gt)
}


crear_tabla_ods_sinergia <- function(datos, num_ods, descripcion_ods) {
  
  color_ods <- colores_ods[num_ods]
  
  # browser()
  
  # Crear tabla gt con formato definitivo
  tabla_gt <- datos  %>% 
    # %>% mutate(
    #   POA_PAI_label = case_when(
    #     POA_PAI == "POA" ~ "Plan Operativo Anual",
    #     POA_PAI == "PAI" ~ "Plan Anual de Inversiones",
    #     TRUE ~ as.character(POA_PAI)
    #   )
    # ) %>% 
    gt() %>%
    tab_header(
      title = md(glue("**<span style='color:white'>Sinergias del {descripcion_ods}</span>**"))
    ) %>%
    fmt_percent(
      columns = cumplimiento,
      decimals = 1
    ) %>%
    fmt_percent(
      columns = aporte,
      decimals = 1
    ) %>%
    fmt_number(
      columns = c(proyectos,
                  metas),
      decimals = 0
    ) %>%
    data_color(
      columns = c(proyectos,
                  metas),
      colors = scales::col_numeric(
        palette = c("white", color_ods),
        domain = NULL
      )
    ) %>%
    tab_style(
      style = cell_fill(color = color_ods),
      locations = cells_title(groups = "title")
    ) %>%
    tab_style(
      style = list(cell_fill(color = color_ods), cell_text(color = "white")),
      locations = cells_column_labels()
    ) %>% 
    tab_source_note(source_note = "Elaboración: Alex Bajaña; Revisión: Axel Guanoluisa") %>%
    gtExtras::gt_theme_538()
  
  return(tabla_gt)
}

crear_tabla_ods_sectores <- function(datos, num_ods, descripcion_ods) {
  
  color_ods <- colores_ods[num_ods]
  
  # browser()
  
  # Crear tabla gt con formato definitivo
  tabla_gt <- datos %>% 
    gt() %>%
    tab_header(
      title = md(glue("**<span style='color:white'>Ejecutores del {descripcion_ods}</span>**"))
    ) %>%
    fmt_percent(
      columns = cumplimiento,
      decimals = 1
    ) %>%
    fmt_percent(
      columns = aporte,
      decimals = 1
    ) %>%
    fmt_number(
      columns = c(proyectos,
                  programas,
                  metas),
      decimals = 0
    ) %>%
    data_color(
      columns = c(proyectos,
                  metas, programas),
      colors = scales::col_numeric(
        palette = c("white", color_ods),
        domain = NULL
      )
    ) %>%
    tab_style(
      style = cell_fill(color = color_ods),
      locations = cells_title(groups = "title")
    ) %>%
    tab_style(
      style = list(cell_fill(color = color_ods), cell_text(color = "white")),
      locations = cells_column_labels()
    ) %>% 
    tab_source_note(source_note = "Elaboración: Alex Bajaña; Revisión: Axel Guanoluisa") %>%
    gtExtras::gt_theme_538()
  
  return(tabla_gt)
}


crear_tabla_ods_objetivo_pmdot <- function(datos, num_ods, descripcion_ods) {
  
  color_ods <- colores_ods[num_ods]
  
  # Crear tabla gt con formato definitivo
  tabla_gt <- datos %>% 
  left_join(etiquetas_sectores) %>% 
  as_tibble() %>% 
  filter(ODS == num_ods) %>% 
  group_by(OBJETIVO_DESARROLLO_PMDOT) %>% 
  summarise(
    proyectos = n_distinct(ID_PROYECTO, na.rm = T),
    metas = n_distinct(ID_META_PROYECTO, na.rm = T),
    programas = n_distinct(ID_PROGRAMA, na.rm = T),
    cumplimiento = mean(CUMPLIMIENTO, na.rm = T),
    aporte = mean(aporte, na.rm = T)
  ) %>% 
  gt() %>% 
  tab_header(
    title = md(glue("**<span style='color:white'>Objetivos de Desarrollo PMDOT: {descripcion_ods}</span>**"))
  ) %>% 
  fmt_percent(
    columns = c(cumplimiento, aporte),
    decimals = 1
  ) %>% 
  fmt_number(
    columns = c(proyectos, metas, programas),
    decimals = 0
  ) %>% 
  data_color(
    columns = c(proyectos, metas, programas, cumplimiento, aporte),
    colors = scales::col_numeric(
    palette = c("white", color_ods),
    domain = NULL
    )
  ) %>% 
  tab_style(
    style = cell_fill(color = color_ods),
    locations = cells_title(groups = "title")
  ) %>% 
  tab_style(
    style = list(cell_fill(color = color_ods), cell_text(color = "white")),
    locations = cells_column_labels()
  ) %>% 
  tab_source_note(source_note = "Elaboración: Alex Bajaña; Revisión: Axel Guanoluisa") %>% 
  gtExtras::gt_theme_538()
  
  return(tabla_gt)
}


crear_tabla_ods_ejes_estrategicos <- function(datos, num_ods, descripcion_ods) {
  
  color_ods <- colores_ods[num_ods]
  
  # Crear tabla gt con formato definitivo
  tabla_gt <- datos %>% 
  left_join(etiquetas_sectores) %>% 
  as_tibble() %>% 
  filter(ODS == num_ods) %>% 
  group_by(EJES_ESTRATEGICOS_ADM_METROPOLITANA) %>% 
  summarise(
    proyectos = n_distinct(ID_PROYECTO, na.rm = T),
    metas = n_distinct(ID_META_PROYECTO, na.rm = T),
    programas = n_distinct(ID_PROGRAMA, na.rm = T),
    cumplimiento = mean(CUMPLIMIENTO, na.rm = T),
    aporte = mean(aporte, na.rm = T)
  ) %>% 
  gt() %>% 
  tab_header(
    title = md(glue("**<span style='color:white'>Ejes Estratégicos de la Administración Metropolitana: {descripcion_ods}</span>**"))
  ) %>% 
  fmt_percent(
    columns = c(cumplimiento, aporte),
    decimals = 1
  ) %>% 
  fmt_number(
    columns = c(proyectos, metas, programas),
    decimals = 0
  ) %>% 
  data_color(
    columns = c(proyectos, metas, programas, cumplimiento, aporte),
    colors = scales::col_numeric(
    palette = c("white", color_ods),
    domain = NULL
    )
  ) %>% 
  tab_style(
    style = cell_fill(color = color_ods),
    locations = cells_title(groups = "title")
  ) %>% 
  tab_style(
    style = list(cell_fill(color = color_ods), cell_text(color = "white")),
    locations = cells_column_labels()
  ) %>% 
  tab_source_note(source_note = "Elaboración: Alex Bajaña; Revisión: Axel Guanoluisa") %>% 
  gtExtras::gt_theme_538()
  
  return(tabla_gt)
}


crear_tabla_ods_ejes_pmdot <- function(datos, num_ods, descripcion_ods) {
  
  color_ods <- colores_ods[num_ods]
  
  # Crear tabla gt con formato definitivo
  tabla_gt <- datos %>% 
  left_join(etiquetas_sectores) %>% 
  as_tibble() %>% 
  filter(ODS == num_ods) %>% 
  group_by(EJES_ESTRATEGICOS_PMDOT) %>% 
  summarise(
    proyectos = n_distinct(ID_PROYECTO, na.rm = T),
    metas = n_distinct(ID_META_PROYECTO, na.rm = T),
    programas = n_distinct(ID_PROGRAMA, na.rm = T),
    cumplimiento = mean(CUMPLIMIENTO, na.rm = T),
    aporte = mean(aporte, na.rm = T)
  ) %>% 
  gt() %>% 
  tab_header(
    title = md(glue("**<span style='color:white'>Ejes Estratégicos PMDOT: {descripcion_ods}</span>**"))
  ) %>% 
  fmt_percent(
    columns = c(cumplimiento, aporte),
    decimals = 1
  ) %>% 
  fmt_number(
    columns = c(proyectos, metas, programas),
    decimals = 0
  ) %>% 
  data_color(
    columns = c(proyectos, metas, programas, cumplimiento, aporte),
    colors = scales::col_numeric(
    palette = c("white", color_ods),
    domain = NULL
    )
  ) %>% 
  tab_style(
    style = cell_fill(color = color_ods),
    locations = cells_title(groups = "title")
  ) %>% 
  tab_style(
    style = list(cell_fill(color = color_ods), cell_text(color = "white")),
    locations = cells_column_labels()
  ) %>% 
  tab_source_note(source_note = "Elaboración: Alex Bajaña; Revisión: Axel Guanoluisa") %>% 
  gtExtras::gt_theme_538()
  
  return(tabla_gt)
}


crear_tabla_ods_ejes_plan_gobierno <- function(datos, num_ods, descripcion_ods) {
  
  color_ods <- colores_ods[num_ods]
  
  # Crear tabla gt con formato definitivo
  tabla_gt <- datos %>% 
  left_join(etiquetas_sectores) %>% 
  as_tibble() %>% 
  filter(ODS == num_ods) %>% 
  group_by(EJE_PLAN_GOBIERNO_DE_ALCALDIA) %>% 
  summarise(
    proyectos = n_distinct(ID_PROYECTO, na.rm = T),
    metas = n_distinct(ID_META_PROYECTO, na.rm = T),
    programas = n_distinct(ID_PROGRAMA, na.rm = T),
    cumplimiento = mean(CUMPLIMIENTO, na.rm = T),
    aporte = mean(aporte, na.rm = T)
  ) %>% 
  gt() %>% 
  tab_header(
    title = md(glue("**<span style='color:white'>Ejes del Plan de Gobierno de Alcaldía: {descripcion_ods}</span>**"))
  ) %>% 
  fmt_percent(
    columns = c(cumplimiento, aporte),
    decimals = 1
  ) %>% 
  fmt_number(
    columns = c(proyectos, metas, programas),
    decimals = 0
  ) %>% 
  data_color(
    columns = c(proyectos, metas, programas, cumplimiento, aporte),
    colors = scales::col_numeric(
    palette = c("white", color_ods),
    domain = NULL
    )
  ) %>% 
  tab_style(
    style = cell_fill(color = color_ods),
    locations = cells_title(groups = "title")
  ) %>% 
  tab_style(
    style = list(cell_fill(color = color_ods), cell_text(color = "white")),
    locations = cells_column_labels()
  ) %>% 
  tab_source_note(source_note = "Elaboración: Alex Bajaña; Revisión: Axel Guanoluisa") %>% 
  gtExtras::gt_theme_538()
  
  return(tabla_gt)
}


# Crear UI
ui <- fluidPage(
  tags$style(HTML("
    body {
      background-color: #f4f4f9; /* Cambia este color según tus necesidades */
    }
  ")),
  
  # Fila con los logos
  fluidRow(
    column(
      width = 4,
      align = "center",
      img(
        src = "https://www.aeropuertoquito.gob.ec/wp-content/uploads/2023/06/logo-alcaldia-horizontal.png", # URL o ruta del logo 1
        height = "100px",
        style = "margin: 10px;"
      )
    ),
    column(
      width = 4,
      align = "center",
      img(
        src = "https://gobiernoabierto.quito.gob.ec/wp-content/uploads/2023/08/Sec_planificacioncolor.svg", # URL o ruta del logo 2
        height = "100px",
        style = "margin: 10px;"
      )
    ),
    column(
      width = 4,
      align = "center",
      img(
        src = "https://www.un.org/sustainabledevelopment/es/wp-content/uploads/sites/3/2019/09/S_SDG_logo_UN_emblem_square_trans_WEB-1024x813.png", # URL o ruta del logo 3
        height = "100px",
        style = "margin: 10px;"
      )
    )
  ),
  h3(
    "Da click en un ODS",
    style = "text-align: left; margin-top: 20px; color: #33d1ff"
  ),
  fluidRow( 
    lapply(c(1:6, 8:13,15:17), function(i) {
      column(
        width = 2,
        align = "center",
        tags$a(
          href = NULL,
          tags$button(
            id = paste0("ods_", i),
            class = "btn action-button",
            style = "background-color:transparent; border:none;",
            img(
              # si i tiene 2 digitos que le quite el 0
              # src = sprintf("https://sdgs.un.org/sites/default/files/goals/E_SDG_Icons-0%d.jpg", i),
              src = ifelse(i < 10,
                sprintf("https://sdgs.un.org/sites/default/files/goals/E_SDG_Icons-0%d.jpg", i),
                sprintf("https://sdgs.un.org/sites/default/files/goals/E_SDG_Icons-%d.jpg", i)
              ),
              height = "100px",
              style = "border-radius: 0px; padding: 5px;"
            )
          )
        )
      )
    })
  ),
  hr(),
  actionButton("open_modal", "Revisa los resultados Generales del aporte del GDMQ", class = "btn-primary"),
  hr(),
  fluidRow(
    # titlo h1 del color ods con un render ui
    column(
      width = 12,
      align = "center",
      uiOutput("titulo_ods")
    )
    
  ),
  fluidRow(
    column(3,uiOutput("box_ods")),
    column(3, uiOutput("box_proyectos")),
    column(3, uiOutput("box_programas")),
    column(3, uiOutput("box_metas"))
  ),
 
  fluidRow(
    column(
      width = 12,
      align = "center",
      HTML("<h2 style='margin-top: 20px; font-size: 24px; line-height: 1.6;'>¿Qué son los ODS?</h2>")
    )),
  fluidRow(
    column(
      width = 12,
      align = "center",
      HTML("
<p style='margin-top: 20px; font-size: 16px; line-height: 1.6;'>
  Los Objetivos de Desarrollo Sostenible (ODS) son una agenda global adoptada por las Naciones Unidas para erradicar la pobreza, proteger el planeta y asegurar el bienestar de todas las personas al 2030. 
  Si bien son metas globales, su cumplimiento depende en gran parte de acciones locales concretas. 
  En este sentido, el Municipio del Distrito Metropolitano de Quito juega un papel clave, ya que tiene la capacidad de planificar, ejecutar y evaluar políticas públicas que impactan directamente en áreas como salud, educación, igualdad, servicios básicos y sostenibilidad ambiental. 
  La localización de los ODS permite adaptar los compromisos globales a las realidades territoriales y responder de manera efectiva a las necesidades de la ciudadanía. 
  Para conocer más sobre el rol de los gobiernos locales en esta agenda, puedes consultar el documento oficial de la CEPAL 
  <a href='https://repositorio.cepal.org/bitstream/handle/11362/40155/24/S1801141_es.pdf' target='_blank'>aquí</a>.
</p>
")
  )
    
  ),
 fluidRow(
  column(
    width = 6,
    align = "center",
    gt_output("tabla_objetivo_pmdot") # Tabla de objetivos PMDOT
  ),
  column(
    width = 6,
    align = "center",
    gt_output("tabla_ejes_estrategicos") # Tabla de ejes estratégicos
  )
),
fluidRow(
  column(
    width = 6,
    align = "center",
    gt_output("tabla_ejes_pmdot") # Tabla de ejes PMDOT
  ),
  column(
    width = 6,
    align = "center",
    gt_output("tabla_ejes_plan_gobierno") # Tabla de ejes del plan de gobierno
  )
),

  bsCollapse(
    id = "collapse_sections", open = "Análisis de metas",
    bsCollapsePanel(title = "Análisis de metas",
                    fluidRow(
                      column(
                        width = 12,
                        align = "center",
                        gt_output("tabla_ods")
                      )          
    )),
    bsCollapsePanel(title = "Análisis POA PAI",
                    fluidRow(
                      column(
                        width = 12,
                        align = "center",
                        gt_output("tabla_ods_agrupada")
                      )          
                    )),
    bsCollapsePanel(title = "Análisis de sinergías",
                    fluidRow(
                      column(
                        width = 12,
                        align = "center",
                        gt_output("tabla_ods_sinergias")
                      )          
                    )),
    bsCollapsePanel(title = "Análisis por sectores",
                    fluidRow(
                      column(
                        width = 12,
                        align = "center",
                        gt_output("tabla_ods_sectores")
                      )          
                    ))
    
 
  )
  
)

server <- function(input, output, session) {
  
  observeEvent(input$open_modal, {
    showModal(
      modalDialog(
        title = NULL,
        easyClose = TRUE,
        footer = NULL,
        size = "l",
        tags$iframe(
          src = "https://alexvbr.quarto.pub/ilv-resultado-general/",
          style = "width: 100%; height: 80vh; border: none;"
        )
      )
    )
  })
  
  tablas <- reactiveValues(
    ods = 1,
    resumen_metas = NULL,
    resumen_poa_pai = NULL,
    resumen_sinergias = NULL,
    resumen_sectores = NULL,
    aporte_global = NULL,
    proyectos = 1,
    programas = 2,
    metas = 3
  )
  
  
  observe({
    walk(c(1:6, 8:13,15:17), ~observeEvent(input[[paste0("ods_", .x)]], {
      tablas$ods <- .x
    }))
  })
  
  observeEvent(tablas$ods, {
    
    # tablas$resumen_metas <- 
    
    
    resumen_aporte <- datos %>%
      group_by(ODS, META) %>%
      summarise(aporte = mean(aporte, na.rm = T),
                programas = n_distinct(ID_PROGRAMA, na.rm = T),
                proyectos = n_distinct(ID_PROYECTO, na.rm = T)) %>%
      ungroup()
    
    resumen_aporte <- resumen_aporte %>%
      mutate(ODS = as.character(ODS)) %>%
      left_join(metas_lab, by = c("ODS","META")) %>% 
      filter(ODS == tablas$ods) 
    
    
    resumen_aporte_agrupado <- datos %>%
      group_by(ODS, POA_PAI) %>%
      summarise(aporte = mean(aporte, na.rm = T),
                programas = n_distinct(ID_PROGRAMA, na.rm = T),
                proyectos = n_distinct(ID_PROYECTO, na.rm = T),
                cumplimiento = mean(CUMPLIMIENTO, na.rm = T),
                metas = n_distinct(ID_META_PROYECTO)) %>%
      ungroup()
    
    resumen_aporte_agrupado <- resumen_aporte_agrupado %>%
      mutate(ODS = as.character(ODS)) %>%
      left_join(metas_lab %>% distinct(ODS, ODS_Descripcion), by = c("ODS")) %>%
      filter(ODS == tablas$ods)
    
    temp_sinergias <- datos %>%
      group_by(ID_PROGRAMA) %>%
      distinct(ODS)
    
    temp_sinergias <- temp_sinergias %>% 
      mutate(include = any(ODS == tablas$ods)) %>% 
      filter(include) %>%
      pull(ID_PROGRAMA) %>% unique()
    
    resumen_sinergias <- datos %>%
      filter(ID_PROGRAMA %in% temp_sinergias) %>%
      group_by(ODS,ID_PROGRAMA) %>%
      summarise(aporte = mean(aporte, na.rm = T),
                proyectos = n_distinct(ID_PROYECTO, na.rm = T),
                cumplimiento = mean(CUMPLIMIENTO, na.rm = T),
                metas = n_distinct(ID_META_PROYECTO)) %>%
      ungroup() %>% 
      left_join(etiquetas_poa_pai %>% distinct(ID_PROGRAMA, NOMBRE_PROGRAMA), by = "ID_PROGRAMA") %>% 
      ungroup() %>% 
      mutate(ODS = as.character(ODS)) %>% 
      left_join(ods) %>% 
      select(-ID_PROGRAMA, -ODS)
    
    resumen_sectores <- datos %>% 
      left_join(etiquetas_sectores) %>% 
      filter(ODS == tablas$ods) %>% 
      group_by(EJECUTOR, SECTOR_EJECUTOR) %>%
      summarise(proyectos = n_distinct(ID_PROYECTO),
                programas = n_distinct(ID_PROGRAMA),
                metas = n_distinct(ID_META_PROYECTO),
                cumplimiento = mean(CUMPLIMIENTO, na.rm = T),
                aporte = mean(aporte, na.rm = T)) %>% 
      ungroup() %>%
      group_by(SECTOR_EJECUTOR) 
    
    tablas$resumen_sectores <- resumen_sectores
    
    tablas$resumen_sinergias <- resumen_sinergias
    
    tablas$resumen_poa_pai <- resumen_aporte_agrupado
    
    tablas$resumen_metas <- resumen_aporte
  })
  
  # 
  
  output$tabla_ods <- render_gt({
    crear_tabla_ods(tablas$resumen_metas, tablas$ods, tablas$resumen_metas$ODS_Descripcion[1])
    
  })
  
  # Resumen de metas agrupadas por POA_PAI
  output$tabla_ods_agrupada <- render_gt(
    crear_tabla_ods_agrupada(tablas$resumen_poa_pai, tablas$ods, tablas$resumen_metas$ODS_Descripcion[1])
  )
  # Resumen de metas agrupadas por sinergias
  
  output$tabla_ods_sinergias <- render_gt(
    crear_tabla_ods_sinergia(tablas$resumen_sinergias %>% group_by(NOMBRE_PROGRAMA), tablas$ods, tablas$resumen_metas$ODS_Descripcion[1])
  )
  
  # Resumen de metas agrupadas por sectores
  
  output$tabla_ods_sectores <- render_gt(
    crear_tabla_ods_sectores(tablas$resumen_sectores, tablas$ods, tablas$resumen_metas$ODS_Descripcion[1])
  )
  
  # Aporte total
  
  aporte_global <- reactive({
    req(tablas$resumen_metas)
    
    aporte_total <- datos %>%
      filter(ODS == tablas$ods) %>% 
      summarise(aporte = mean(aporte, na.rm = T),
                programas = n_distinct(ID_PROGRAMA, na.rm = T),
                proyectos = n_distinct(ID_PROYECTO, na.rm = T),
                metas = n_distinct(ID_META_PROYECTO)) %>%
      ungroup()
    
    return(aporte_total)
  })
  
  observeEvent(aporte_global(), {
    aporte <- aporte_global()
    
    tablas$aporte_global <- aporte$aporte
    tablas$proyectos <- aporte$proyectos
    tablas$programas <- aporte$programas
    tablas$metas <- aporte$metas
  })
  
  # Render para la tabla de objetivos PMDOT
output$tabla_objetivo_pmdot <- render_gt({
  req(tablas$resumen_metas)
  crear_tabla_ods_objetivo_pmdot(
    datos = datos,
    num_ods = tablas$ods,
    descripcion_ods = tablas$resumen_metas$ODS_Descripcion[1]
  )
})

# Render para la tabla de ejes estratégicos de la administración metropolitana
output$tabla_ejes_estrategicos <- render_gt({
  req(tablas$resumen_metas)
  crear_tabla_ods_ejes_estrategicos(
    datos = datos,
    num_ods = tablas$ods,
    descripcion_ods = tablas$resumen_metas$ODS_Descripcion[1]
  )
})

# Render para la tabla de ejes estratégicos PMDOT
output$tabla_ejes_pmdot <- render_gt({
  req(tablas$resumen_metas)
  crear_tabla_ods_ejes_pmdot(
    datos = datos,
    num_ods = tablas$ods,
    descripcion_ods = tablas$resumen_metas$ODS_Descripcion[1]
  )
})

# Render para la tabla de ejes del plan de gobierno
output$tabla_ejes_plan_gobierno <- render_gt({
  req(tablas$resumen_metas)
  crear_tabla_ods_ejes_plan_gobierno(
    datos = datos,
    num_ods = tablas$ods,
    descripcion_ods = tablas$resumen_metas$ODS_Descripcion[1]
  )
})
  
  output$box_ods <- renderUI({
    req(tablas$ods)
    
    color <- colores_ods[tablas$ods]
    aporte <- tablas$aporte_global
    
    div(
      style = "border-radius: 12px; overflow: hidden; box-shadow: 0px 0px 10px rgba(0,0,0,0.1);",
      div(
        style = glue::glue("background-color:{color}; padding: 10px;"),
        h4(glue::glue("Aporte al ODS {tablas$ods}"), style = "color: white; margin: 0px;")
      ),
      div(
        style = "background-color: #f7f7f7; padding: 20px; text-align: center;",
        h2(glue::glue("{round(aporte * 100, 1)} %"), style = "margin: 0px; color: #333;")
      )
    )
  })
  
  output$box_proyectos <- renderUI({
    color <- colores_ods[tablas$ods]
    div(
      style = "border-radius: 12px; overflow: hidden; box-shadow: 0px 0px 10px rgba(0,0,0,0.1);",
      div(
        style = glue::glue("background-color:{color}; padding: 10px;"),
        h4("Proyectos GDMQ", style = "color: white; margin: 0px;")
      ),
      div(
        style = "background-color: #f7f7f7; padding: 20px; text-align: center;",
        h2(tablas$proyectos, style = "margin: 0px; color: #333;")
      )
    )
  })
  
  output$box_programas <- renderUI({
    color <- colores_ods[tablas$ods]
    div(
      style = "border-radius: 12px; overflow: hidden; box-shadow: 0px 0px 10px rgba(0,0,0,0.1);",
      div(
        style = glue::glue("background-color:{color}; padding: 10px;"),
        h4("Programas GDMQ", style = "color: white; margin: 0px;")
      ),
      div(
        style = "background-color: #f7f7f7; padding: 20px; text-align: center;",
        h2(tablas$programas, style = "margin: 0px; color: #333;")
      )
    )
  })
  
  output$box_metas <- renderUI({
    color <- colores_ods[tablas$ods]
    div(
      style = "border-radius: 12px; overflow: hidden; box-shadow: 0px 0px 10px rgba(0,0,0,0.1);",
      div(
        style = glue::glue("background-color:{color}; padding: 10px;"),
        h4("Metas GDMQ (POA-PAI)", style = "color: white; margin: 0px;")
      ),
      div(
        style = "background-color: #f7f7f7; padding: 20px; text-align: center;",
        h2(tablas$metas, style = "margin: 0px; color: #333;")
      )
    )
  })
  
  # server para # titlo h1 del color ods con un render ui
  # column(
  #   width = 12,
  #   align = "center",
  #   uiOutput("titulo_ods")
  # )
  
  output$titulo_ods <- renderUI({
    req(tablas$ods)
    
    color <- colores_ods[tablas$ods]
    
    h1(
      glue::glue("Aporte de la gestión municipal al ODS {tablas$ods}"),
      style = glue::glue("color: {color}; font-size: 2.5em; font-weight: bold;")
    )
  })
}

shinyApp(ui = ui, server = server)