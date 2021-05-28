create_log_reg_model <- function(data = dplyr::tibble(),
                                 recipe = recipes::recipe()) {
  logit_model <- parsnip::logistic_reg(mode = "classification") %>%
    parsnip::set_engine("glmnet")

  workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    workflows::add_model(logit_model)

}
