data(Sacramento)
Sacramento

# Split the dataset, using 75% of the data for training and 25% for testing
housing_split <- Sacramento %>%
    as_tibble() %>%
    dplyr::select(price,type,sqft,beds,baths,latitude,longitude) %>%
    initial_split(prop = 0.75)

# This rsplit object tells you how many observations are used for training, how many for testing, and how many total
housing_split

# The training function can be used to extract the training data from the rsplit object
housing_training <- housing_split %>%
    training()

housing_training

# The testing function can be used to extract the testing data from the rsplit object
housing_testing <- housing_split %>%
    testing()

housing_testing

# pre-processing
housing_recipe <- housing_training %>%
    # Specify regression model formula
    recipe(price ~.)  %>%
    # step_corr removes highly correlated variables
    step_corr(all_numeric(), -all_outcomes()) %>%
    # step_center normalizes data to have a mean of 0
    step_center(all_numeric(), -all_outcomes()) %>%
    # step_scale normalizes data to have a standard deviation of 1
    step_scale(all_numeric(), -all_outcomes()) %>%
    # Create dummy variable columns for all factor columns
    step_dummy(all_predictors(),-all_numeric())
housing_recipe

housing_recipe_prepped <- housing_recipe %>%
    # prep trains the recipe using the training dataset
    prep()
housing_recipe_prepped

# Use use the juice function to apply the prepped recipe to the training dataset
housing_training_juiced <- juice(housing_recipe_prepped)
housing_training_juiced

# We use the bake function to apply the prepped recipe to the testing dataset
housing_testing_baked <- housing_recipe_prepped %>%
    bake(housing_testing) 
housing_testing_baked

# Model building
# Define and fit a linear regression model
housing_model_lm <- linear_reg() %>%
    set_engine("lm") 
housing_model_lm
housing_fit_lm <- housing_model_lm %>%
    fit(price ~ ., data = housing_training_juiced)
housing_fit_lm

# Define and fit a random forest regression model using the randomForest engine/package
housing_model_randomForest <-  
                rand_forest(trees = 100, mode = "regression") %>% set_engine("randomForest")
housing_model_randomForest
housing_fit_randomForest <- housing_model_randomForest %>%
    fit(price ~ ., data = housing_training_juiced)
housing_fit_randomForest

# Define and fit a random forest regression model using the ranger engine/package
housing_model_ranger <- rand_forest(trees = 100, mode = "regression") %>%
    set_engine("ranger")
housing_model_ranger

housing_fit_ranger <- housing_model_ranger %>%
    fit(price ~ ., data = housing_training_juiced)
housing_fit_ranger

# Generate predictions for our testing using the ranger model
predict(housing_fit_ranger, housing_testing_baked)

# Add these ranger predictions to the testing dataset
# parsnip::predict always gives you same number of rows as data, so predictions can be added using bind_cols
housing_fit_ranger %>%
    predict(housing_testing_baked) %>%
    bind_cols(housing_testing_baked)

# Save this combined dataframe for later
housing_ranger_predict <- housing_fit_ranger %>%
    predict(housing_testing_baked) %>%
    bind_cols(housing_testing_baked) %>%
    # Add a column for model name
    mutate(model_name = "ranger")

# Let's do the same thing for the linear regression model
housing_lm_predict <- housing_fit_lm %>%
    predict(housing_testing_baked) %>%
    bind_cols(housing_testing_baked) %>%
    mutate(model_name = "lm")

# Let's do the same thing for the randomForest model
housing_randomForest_predict <- housing_fit_randomForest %>%
    predict(housing_testing_baked) %>%
    bind_cols(housing_testing_baked) %>%
    mutate(model_name = "randomForest")

# Let's combine all of these datasets so we can look at them side-by-side
housing_all_predict <- bind_rows(housing_lm_predict,
                                 housing_ranger_predict,
                                 housing_randomForest_predict)

# check the predictions side by side
housing_all_predict %>%
    ggplot(aes(x = price,y=.pred,color=model_name)) +
    geom_point() +
    geom_smooth(method = "lm") +
    labs(x = "Observed price",
         y = "Predicted price",
         title = "Predictions vs observed values for 3 model types\nA simple linear regression is overlaid") +
    coord_equal()+
    theme_light()

# generate model performance metrics
housing_ranger_predict %>%
    # Here we use the metric functions and must define the truth value and the estimated prediction
    metrics(truth = price, estimate = .pred)

housing_all_predict%>%
    group_by(model_name) %>%
    metrics(truth = price, estimate = .pred)

housing_all_predict%>%
    group_by(model_name) %>%
    metrics(truth = price, estimate = .pred)%>%
    ggplot(aes(x = model_name, y = .estimate)) +
    geom_bar(stat="identity") +
    facet_wrap(~.metric,scales="free") +
    labs(x = "Model name",
         y = "Model performance metric estimate",
         title = "Model performance metrics for 3 model types")


