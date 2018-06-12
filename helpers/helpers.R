# ddPCR R package - Dean Attali 2015
# This file contains various UI helper functions for the shiny app

# Create a little question mark link that shows a help popup on hover
helpPopup <- function(content, title = NULL) {
      a(href = "#",
        class = "popover-link",
        `data-toggle` = "popover",
        `data-title` = title,
        `data-content` = content,
        `data-html` = "true",
        `data-trigger` = "hover",
        icon("question-circle")
      )
}

# Set up a button to have an animated loading indicator and a checkmark
# for better user experience
# Need to use with the corresponding `withBusyIndicator` server function
withBusyIndicator <- function(button) {
      id <- button[['attribs']][['id']]
      tagList(
            button,
            span(
                  class = "btn-loading-container",
                  `data-for-btn` = id,
                  hidden(
                        img(src = "ajax-loader-bar.gif", class = "btn-loading-indicator"),
                        icon("check", class = "btn-done-indicator")
                  )
            )
      )
}

withBusyIndicatorServer <- function(buttonId, expr) {
      # UX stuff: show the "busy" message, hide the other messages, disable the button
      loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
      doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
      errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
      shinyjs::disable(buttonId)
      shinyjs::show(selector = loadingEl)
      shinyjs::hide(selector = doneEl)
      shinyjs::hide(selector = errEl)
      on.exit({
            shinyjs::enable(buttonId)
            shinyjs::hide(selector = loadingEl)
      })
      
      # Try to run the code when the button is clicked and show an error message if
      # an error occurs or a success message if it completes
      tryCatch({
            value <- expr
            shinyjs::show(selector = doneEl)
            shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade",
                                               time = 0.5))
            value
      }, error = function(err) { errorFunc(err, buttonId) })
}

# Some info to help with the default values of all the droplet params in plot
# plotDropsParams <- list(
#       "failed" = list(
#             name = "Droplets in<br/>failed wells",
#             type = c(plate_types$hex_positive_pnpp, plate_types$fam_positive_pnpp, "ddpcr_plate"),
#             show = TRUE,
#             col = "Default",
#             alpha = 0.2
#       ),
#       "outlier" = list(
#             name = "Outlier droplets",
#             type = c(plate_types$hex_positive_pnpp, plate_types$fam_positive_pnpp, "ddpcr_plate"),
#             show = FALSE,
#             col = "orange",
#             alpha = 1
#       ),
#       "empty" = list(
#             name = "Empty droplets",
#             type = c(plate_types$hex_positive_pnpp, plate_types$fam_positive_pnpp, plate_types$custom_thresholds, "ddpcr_plate"),
#             show = FALSE,
#             col = "Default",
#             alpha = 0.2
#       ),
#       "rain" = list(
#             name = "Rain droplets",
#             type = c(plate_types$hex_positive_pnpp, plate_types$fam_positive_pnpp),
#             show = TRUE,
#             col = "black",
#             alpha = 0.2
#       ),
#       "positive" = list(
#             name = "set value in server/tab-results.R",
#             type = c(plate_types$hex_positive_pnpp, plate_types$fam_positive_pnpp),
#             show = TRUE,
#             col = "green3",
#             alpha = 0.2
#       ),
#       "negative" = list(
#             name = "set value in server/tab-results.R",
#             type = c(plate_types$hex_positive_pnpp, plate_types$fam_positive_pnpp),
#             show = TRUE,
#             col = "purple3",
#             alpha = 0.2
#       ),
#       "x_positive" = list(
#             name = "X+Y- droplets",
#             type = c(plate_types$custom_thresholds),
#             show = TRUE,
#             col = "green3",
#             alpha = 0.2
#       ),
#       "y_positive" = list(
#             name = "X-Y+ droplets",
#             type = c(plate_types$custom_thresholds),
#             show = TRUE,
#             col = "blue",
#             alpha = 0.2
#       ),
#       "both_positive" = list(
#             name = "X+Y+ droplets",
#             type = c(plate_types$custom_thresholds),
#             show = TRUE,
#             col = "orange",
#             alpha = 0.2
#       )
# )

# Clours to let user select from in various inputs fields
allCols <- sort(c(
      "black", "blue", "green" = "green3", "purple" = "purple3", "orange", "darkgreen",
      "pink", "red", "yellow", "brown", "gold", "gray" = "gray7", "cyan", "white"
))
allColsDefault <- c("Default", allCols)