create_preprocessing_pipeline <- function(

  data = dplyr::tibble(),
  target = as.character()
) {
  recipe_specification <- recipes::recipe( ~ ., data = data) %>%
    # Make target column as outcome variable
    recipes::update_role(target, new_role = "outcome") %>%
    # Change Passenger ID from predictor to ID
    recipes::update_role("PassengerId", "Name", new_role = "ID") %>%
    # Impute missing value of passenger Age & Embarked
    recipes::step_impute_knn("Age") %>%
    recipes::step_modeimpute("Embarked") %>%
    # Remove unwanted columns
    recipes::step_rm("Ticket", "Cabin") %>%
    # Make integer variables factor
    recipes::step_mutate_at(tidyselect::any_of(c("Survived", "Pclass", "Sex", "SibSp", "Parch", "Embarked")),
                            fn = ~as.factor(.)) %>%
    # One hot encoding
    recipes::step_dummy(recipes::all_nominal(), -recipes::all_outcomes(), -"Name", -"PassengerId")
}
