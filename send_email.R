library(emayili)
library(googlesheets4)
library(dplyr)
library(glue)

sheet_url <- "https://docs.google.com/spreadsheets/d/1CSgKpm5sZSVBukx1Qb6q1WPrht_TuYC3zQQY8X12Kq0/edit?resourcekey=&gid=1383175476#gid=1383175476"
survey_data <- read_sheet(sheet_url) 

create_newsletter_email_body_with_local_image <- function(data, 
                                                          greeting = "", 
                                                          local_image_path = NULL) {
  # Generate a Content-ID for the image
  image_cid <- if (!is.null(local_image_path)) "header-image" else NULL
  
  # Exclude unnecessary columns
  questions <- colnames(data)[!colnames(data) %in% c("Timestamp", "First name!", "Last name! (or initial)", "Email Address")]
  
  # Initialize the body with the header image if provided
  body <- if (!is.null(image_cid)) {
    glue("<img src='cid:{image_cid}' alt='Newsletter Header' style='display: block; margin: 0 auto; width:100%; max-width:600px;'><br>")
  } else {
    ""
  }
  
  # Add the greeting if provided
  if (greeting != "") {
    body <- glue("{body}<p>{greeting}</p><br>")
  }
  
  for (question in questions) {
    # Add question as a bolded header
    body <- glue("{body}<p><strong>{question}</strong></p>")
    
    question_responses <- data %>%
      select(`Email Address`, `First name!`, `Last name! (or initial)`, !!sym(question)) %>%
      filter(!is.na(!!sym(question)) & !!sym(question) != "") %>%
      mutate(
        Identifier = ifelse(!is.na(`First name!`) & `First name!` != "",
                            glue("{`First name!`} {`Last name! (or initial)`}"),
                            `Email Address`),  # Use names if available, otherwise email
        FormattedResponse = gsub("\n", "<br>", !!sym(question))  # Replace newlines with <br>
      )
    
    for (i in 1:nrow(question_responses)) {
      response <- question_responses[i, ]
      body <- glue("{body}<p><strong>{response$Identifier}</strong>: {response$FormattedResponse}</p>")
    }
    
    # Add spacing between sections
    body <- glue("{body}<br>")
  }
  
  return(list(body = body, image_cid = image_cid, local_image_path = local_image_path))
}

smtp <- server(
  host = "smtp.gmail.com",
  port = 465,
  username = Sys.getenv("EMAIL_USERNAME"),
  password = Sys.getenv("APP_PASSWORD")
)

date_start <- "2024-11-21"
date_end <- "2024-12-16"
subj <- stringr::str_glue("Letterlooper: {date_start} -> {date_end}")
survey_data <- survey_data |>
  filter(Timestamp >= date_start)

greeting_message <- "Hello friends! Thanks for continuing to indulge in my letter-looping experiment. Hope it finds you well and that the New Year is full of joy, learning, and blessings."
local_image_path <- "friends.webp"

email_content <- create_newsletter_email_body_with_local_image(
  survey_data,
  greeting = greeting_message,
  local_image_path = local_image_path
)

email <- envelope() |>
  from(Sys.getenv("EMAIL_USERNAME")) |>
  # to("nguy.philip@gmail.com") |>
  to(unique(survey_data$`Email Address`)) |>
  subject(subj) |>
  html(email_content$body)  # Correctly handle HTML content

# Attach the local image with Content-ID
if (!is.null(email_content$image_cid) && !is.null(email_content$local_image_path)) {
  email <- email |>
    attachment(
      path = email_content$local_image_path,
      cid = email_content$image_cid
    )
}

# Send the email
smtp(email)

# View email body (optional, for debugging)
cat(email_body)
