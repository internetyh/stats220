library(tidyverse)

logged_data <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vS_p_cVWnikMDSqYOE8FdxeXxDlN5H60IvGmLGvCn5SizyMcn_qVbAlaB7jLs4fSqMFDioPpl3pvV0U/pub?output=csv')

logged_data %>% view()