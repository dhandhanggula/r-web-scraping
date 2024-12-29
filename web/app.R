library(shiny)
library(bslib)
library(bsicons)
library(vroom)
library(shinycssloaders)
source("./scrap.R")

ui <- page_fillable(
  tags$style(HTML("
    label[for='search_member'] {
      display: none;
    }
  ")),
  card(
    card_header(
      layout_column_wrap(
        width = 1 / 2,
        class = "mb-0 align-items-center",
        card_title(
          "JKT48 Members",
          class = "m-0 fs-6 fw-bold"
        ),
        div(
          class = "d-flex flex-row gap-3 justify-content-end",
          textInput(
            "search_member",
            label = "",
            placeholder = "Cari member",
          ),
          actionButton(
            inputId = "update_member",
            label = NULL,
            icon = icon("refresh"),
            class = "btn-sm btn-primary px-3",
          )
        )
      )
    ),
    card_body(
      div(
        withSpinner(uiOutput(outputId = "member_list", class = "row"))
      )
    )
  )
)

server <- function(session, input, output) {
  load_members_data <- function() {
    members_data_path <- "../jkt48-members.csv"

    if (!file.exists(members_data_path)) {
      stop("Berkas data member tidak ditemukan")
    }

    data <- read.csv(members_data_path)

    if (nrow(data) == 0) {
      stop("Data member kosong")
    }

    return(data)
  }

  rv <- reactiveValues(data = data.frame())

  observe({
    tryCatch(
      {
        rv$data <- load_members_data()
      },
      error = function(e) {
        output$member_list <- renderUI({
          p(e$message)
        })
      }
    )
  })

  observeEvent(input$update_member, {
    withProgress(message = "Mengambil data", value = 0, {
      scrap_members_data()
      incProgress(0.5)
      tryCatch(
        {
          data <- load_members_data()
          rv$data <- data
          incProgress(1)
        },
        error = function(e) {
          output$member_list <- renderUI({
            p(e$message)
          })
        }
      )
    })
  })

  filtered_members <- reactive({
    if (is.null(input$search_member) || input$search_member == "") {
      rv$data
    } else {
      rv$data[grepl(input$search_member, rv$data$name, ignore.case = TRUE) |
        grepl(input$search_member, rv$data$nickname, ignore.case = TRUE), ]
    }
  })


  output$member_list <- renderUI({
    data <- filtered_members()

    if (nrow(data) == 0) {
      return(div("Member tidak ditemukan!", class = "text-center text-muted"))
    }

    lapply(seq_len(nrow(data)), function(i) {
      div(
        class = "col-2",
        card(
          class = "border-0",
          card_image(data$picture[i]),
          card_body(
            fill = FALSE,
            gap = 0,
            class = "p-0 py-3",
            h6(
              paste(
                data$name[i],
                paste0("(", data$nickname[i], ")")
              ),
              class = "fw-medium m-0 mb-1 lh-base"
            ),
            div(
              class = "d-flex flex-column m-0 p-0",
              div(
                class = "text-secondary",
                style = "font-size: 13px",
                p(
                  paste(data$date_of_birth[i], "/", data$horoscope[i]),
                  class = "mb-0 pb-0",
                  style = "margin-bottom: 4px"
                ),
                span(paste(data$height[i], " ─ ", data$blood_type[i]))
              )
            )
          )
        )
      )
    })
  })
}

shinyApp(ui = ui, server = server)
