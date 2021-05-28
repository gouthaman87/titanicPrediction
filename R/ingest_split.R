#' Train/Test Split
#'
#' Ingest the training dataset and holdout datasets and split the training
#' dataset into train and test.
#'
#' @param train_test_raw_path Location of the raw train & test csv file to be ingested.
#' @param holdout_raw_path Location of the raw holdout csv file to be ingested.
#' @param target Name of the target column to be predicted in the datasets.
#' @param prop The proportion of train data to model.
#'
#' @returns
#' @return list of data.
#' @return train: The tibble for training the model.
#' @return test: The tibble for testing the model.
#' @return holdout: The tibble of unseen data to predict values for.
#' @export
#'
#' @examples
#' \dontrun{
#' data_list = ingest_split(
#'      train_test_raw_path="path/to/ingest.csv",
#'      holdout_raw_path="path/to/export.csv",
#'      target="target_col",
#'      prop = 0.8
#' )
#' }

ingest_split <- function(
  train_test_raw_path = as.character(),
  holdout_raw_path = as.character(),
  target = as.character(),
  prop = as.numeric()
) {

  # # Log MLFlow parameters
  # mlflow::mlflow_log_param("prop", prop)

  # Import train & holdout datasets
  df_train <- readr::read_csv(train_test_raw_path, col_types = readr::cols())
  df_holdout <- readr::read_csv(holdout_raw_path, col_types = readr::cols())

  # Convert integer to floats
  df_train <- df_train %>%
    dplyr::mutate_if(is.integer, as.numeric)

  df_holdout <- df_holdout %>%
    dplyr::mutate_if(is.integer, as.numeric)

  # Train/Test split
  splits <- rsample::initial_split(df_train, prop = prop, strata = target)

  return(list(rsample::training(splits), rsample::testing(splits), df_holdout))
}
