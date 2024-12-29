library(rvest)
library(stringr)
library(dplyr)

scrap_members_data <- function() {
  base_url <- "https://jkt48.com"
  page_url <- paste0(base_url, "/member/list?lang=id")
  page <- read_html(page_url) %>%
    html_element(".entry-contents__main-area") %>%
    html_elements(".entry-member")

  members <- data.frame(
    url = character(0),
    picture = character(0),
    name = character(0),
    date_of_birth = character(0),
    blood_type = character(0),
    horoscope = character(0),
    height = character(0),
    nickname = character(0)
  )

  for (member in page) {
    member_url <- member %>%
      html_element("a") %>%
      html_attr("href")

    member_profile <- member %>%
      html_element("img") %>%
      html_attr("src")

    member_detail_url <- paste0(base_url, member_url)

    member_detail <- read_html(member_detail_url) %>%
      html_elements(".entry-mypage__item") %>%
      html_element(".entry-mypage__item--content")

    data <- data.frame(
      url = member_detail_url,
      picture = paste0(base_url, member_profile),
      name = member_detail[[1]] %>% html_text2(),
      date_of_birth = member_detail[[2]] %>% html_text2(),
      blood_type = member_detail[[3]] %>% html_text2(),
      horoscope = member_detail[[4]] %>% html_text2(),
      height = member_detail[[5]] %>% html_text2(),
      nickname = member_detail[[6]] %>% html_text2()
    )

    members <- rbind(members, data)
  }

  write.csv(members, "../jkt48-members.csv", row.names = FALSE)
}
