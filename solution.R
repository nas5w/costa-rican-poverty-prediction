library(dplyr)
library(caret)
library(lightgbm)

# Path to train and test datasets
train_path = 'train.csv'
test_path = 'test.csv'
submission_path = 'submission.csv'

# Cleans training or test data for the Costa Rica dataset
clean <- function(data) {
   
  # Convert to tibble
  data_tbl <- tbl_df(data)
  
  # We will be summarizing many variables by household. Let's create the group.
  by_household <- data_tbl %>% group_by(idhogar)
  
  # Also make a clean version that will be used in the final model. Since we only care
  # About households, the cleaned version should have one row per household
  cleaned <- by_household %>% summarize()
  
  # Get count of members per household
  # note: this is redundant with 'tamhog' variable, but tamhog reports a different value
  num_members <- by_household %>%
    summarize(num_members = n())
  cleaned <- inner_join(cleaned, num_members)
  rm(num_members)
  
  # Appears ot be a storng correlation with increased rent and increased wealth
  # When rent is high enough. Let's create categories, including rent totally missing
  rent <- by_household %>%
    summarize(rent = mean(v2a1, na.rm = TRUE)) %>%
    mutate(rent0_166 = ifelse(!is.na(rent) & rent < 166666, 1, 0),
           rent167_333 = ifelse(!is.na(rent) & rent >= 166666 & rent < 333332 , 1, 0),
           rent334_500 = ifelse(!is.na(rent) & rent >= 333332 & rent < 500000 , 1, 0),
           rent500plus = ifelse(!is.na(rent) & rent >= 500000, 1, 0),
           rent_na = ifelse(is.na(rent), 1, 0)) %>%
    select(-rent)
  
  # Add to cleaned table
  cleaned <- cleaned %>% inner_join(rent)
  rm(rent)
  
  # The next several variables seem well defined and not missing values. Let's just
  # take the mean of them by household and add to cleaned dataframe
  temp <- by_household %>% 
    summarize(overcrowding = mean(hacdor),
              num_rooms = mean(rooms),
              has_bathroom = mean(v14a),
              has_refrigerator = mean(refrig),
              has_tablet = mean(v18q),
              num_tablets = mean(v18q1),
              males_under_12 = mean(r4h1),
              males_over_12 = mean(r4h2),
              total_males = mean(r4h3),
              females_under_12 = mean(r4m1),
              females_over_12 = mean(r4m2),
              total_females = mean(r4m3))
  # Fix num_tablets variable for NA, which is clearly meant to be 0
  temp$num_tablets[is.na(temp$num_tablets)] <- 0
  
  # Join into clean data frame
  cleaned <- cleaned %>% inner_join(temp)
  rm(temp)
  
  # Total persons is redundant with total male + total female.
  # Will not include the following variables: r4t1, r4t2, r4t3
  
  # Size of household seems fine 'tamhog'
  # Num persons living in household seems fine 'tamviv'
  
  # Years of schooling (escolari) seems like potentially a more individualized 
  # metric. Let's investigate how we can aggregate by household
  schooling <- data_tbl %>%
    group_by(idhogar) %>%
    summarize(max_shooling = max(escolari),
              min_schooling = min(escolari),
              avg_shooling = mean(escolari))
  
  # Join this into the main data frame
  cleaned <- cleaned %>% inner_join(schooling)
  rm(schooling)
  
  # Same with years behind in school; seems individual so let's do some aggregate stats
  # Noticing a lot of missing values here. Need to handle that.
  
  table(data_tbl$rez_esc)
  
  school_behind <- by_household %>%
    mutate(behind0 = ifelse(!is.na(rez_esc) & rez_esc == 0, 1, 0),
           behind1 = ifelse(!is.na(rez_esc) & rez_esc == 1, 1, 0),
           behind2 = ifelse(!is.na(rez_esc) & rez_esc == 2, 1, 0),
           behind3 = ifelse(!is.na(rez_esc) & rez_esc == 3, 1, 0),
           behind4 = ifelse(!is.na(rez_esc) & rez_esc == 4, 1, 0),
           behind5 = ifelse(!is.na(rez_esc) & rez_esc == 5, 1, 0),
           behind_na = ifelse(is.na(rez_esc), 1, 0)) %>%
    select(idhogar, behind0, behind1, behind2, behind3, behind4, behind5, behind_na) %>%
    summarize(behind0 = sum(behind0), 
              behind1 = sum(behind1), 
              behind2 = sum(behind2), 
              behind3 = sum(behind3), 
              behind4 = sum(behind4), 
              behind5 = sum(behind5), 
              behind_na = sum(behind_na))
  cleaned <- cleaned %>% inner_join(school_behind)
  rm(school_behind)
  
  # tamhog and hhsize variables are exactly duplicates. Just keep one.
  temp <- by_household %>% 
    summarize(household_size = max(hhsize)) %>%
    select(idhogar, household_size)
  cleaned <- cleaned %>% inner_join(temp)
  rm(temp)
  
  # Details about house construction, water, electricity, etc.
  temp <- by_household %>% 
    summarize(wall_brick = max(paredblolad),
              wall_socket = max(paredzocalo),
              wall_cement = max(paredpreb),
              wall_waste = max(pareddes),
              wall_wood = max(paredmad),
              wall_zinc = max(paredzinc),
              wall_fibers = max(paredfibras),
              wall_other = max(paredother),
              floor_mosaic = max(pisomoscer),
              floor_cement = max(pisocemento),
              floor_other = max(pisoother),
              floor_natural = max(pisonatur),
              floor_none = max(pisonotiene),
              floor_wood = max(pisomadera),
              roof_foil = max(techozinc),
              roof_cement = max(techoentrepiso),
              roof_natural = max(techocane),
              roof_other = max(techootro),
              ceiling_none = max(cielorazo),
              water_provision_inside = max(abastaguadentro),
              water_provision_outside = max(abastaguafuera),
              water_provision = max(abastaguano),
              elec_public = max(public),
              elec_private = max(planpri),
              no_electricity = max(noelec),
              elec_coop = max(coopele),
              no_toilet = max(sanitario1),
              toilet_sewer = max(sanitario2),
              toilet_septic = max(sanitario3),
              toilet_hole = max(sanitario5),
              toilet_other = max(sanitario6),
              no_kitchen = max(energcocinar1),
              cook_elec = max(energcocinar2),
              cook_gas = max(energcocinar3),
              cook_charcoal = max(energcocinar4),
              rubbish_tanker = max(elimbasu1),
              rubbish_buried = max(elimbasu2),
              rubbish_burned = max(elimbasu3),
              rubbish_thrown = max(elimbasu4),
              rubbish_river = max(elimbasu5),
              rubbish_other = max(elimbasu6),
              wall_quality = max(epared1 * 1 + epared2 * 2 + epared3 * 3),
              roof_quality = max(etecho1 * 1 + etecho2 * 2 + etecho3 * 3),
              floor_quality = max(eviv1 * 1 + eviv2 * 2 + eviv3 * 3))
  cleaned <- cleaned %>% inner_join(temp)
  rm(temp)
  
  # Skip "male" and "female" designations, we already have this.
  
  # Details about each individual, will be summed in aggregate.
  temp <- by_household %>% 
    summarize(num_less_than_10 = sum(estadocivil1),
              num_coupled_unions = sum(estadocivil2),
              num_married = sum(estadocivil3),
              num_divorced = sum(estadocivil4),
              num_separated = sum(estadocivil5),
              num_widowed = sum(estadocivil6),
              num_single = sum(estadocivil7),
              num_spouses = sum(parentesco2),
              num_son_daughter = sum(parentesco3),
              num_stepchildren = sum(parentesco4),
              num_stepchildren_inlaw = sum(parentesco5),
              num_grandchildren = sum(parentesco6),
              num_parents = sum(parentesco7),
              num_parents_inlaw = sum(parentesco8),
              num_siblings = sum(parentesco9),
              num_siblings_inlaw = sum(parentesco10),
              num_other_family = sum(parentesco11),
              num_non_family = sum(parentesco12),
              num_children_0to9 = max(hogar_nin),
              num_adult = max(hogar_adul),
              num_over_65 = max(hogar_mayor))
  cleaned <- cleaned %>% inner_join(temp)
  rm(temp)
  
  # Education levels (need to mutate 'yes' to 1 and 'no' to 0)
  by_household$edjefe <- as.character(by_household$edjefe)
  by_household$edjefe[by_household$edjefe == 'yes'] <- '1'
  by_household$edjefe[by_household$edjefe == 'no'] <- '0'
  by_household$edjefe <- as.numeric(by_household$edjefe)
  by_household$edjefa <- as.character(by_household$edjefa)
  by_household$edjefa[by_household$edjefa == 'yes'] <- '1'
  by_household$edjefa[by_household$edjefa == 'no'] <- '0'
  by_household$edjefa <- as.numeric(by_household$edjefa)
  
  temp <- by_household %>% 
    summarize(male_head_edu_level = max(edjefe),
              female_head_edu_level = max(edjefa),
              average_adult_edu_level = max(meaneduc),
              num_no_education = sum(instlevel1),
              num_incomplete_primary = sum(instlevel2),
              num_complete_primary = sum(instlevel3),
              num_incomplete_ac_secondary = sum(instlevel4),
              num_complete_ac_secondary = sum(instlevel5),
              num_incomplete_tech_secondary = sum(instlevel6),
              num_complete_tech_secondary = sum(instlevel7),
              num_undergrad = sum(instlevel8),
              num_graduate = sum(instlevel9))
  cleaned <- cleaned %>% inner_join(temp)
  rm(temp)
  
  # Bedrooms
  temp <- by_household %>%
    summarize(bedrooms = max(bedrooms))
  cleaned <- cleaned %>% inner_join(temp)
  rm(temp)
  
  # House financing/renting, other economic indicators
  temp <- by_household %>%
    summarize(fully_own = max(tipovivi1),
              paying_off = max(tipovivi2),
              renting = max(tipovivi3),
              ownership_precarious = max(tipovivi4),
              ownership_other = max(tipovivi5),
              has_computer = max(computer),
              has_television = max(television),
              has_mobile_phone = max(mobilephone),
              num_mobile_phones = max(qmobilephone))
  cleaned <- cleaned %>% inner_join(temp)
  rm(temp)
  
  # Geography
  temp <- by_household %>%
    summarize(central_region = max(lugar1),
              chorotega_region = max(lugar2),
              pacifico_region = max(lugar3),
              brunca_region = max(lugar4),
              huetar_atlantica_region = max(lugar5),
              huetar_norte_region = max(lugar6),
              urban = max(area1),
              rural = max(area2))
  cleaned <- cleaned %>% inner_join(temp)
  rm(temp)
  
  # More specific age characteristics
  temp <- by_household %>%
    summarize(min_age = min(age),
              max_age = max(age),
              avg_age = mean(age),
              median_age = median(age),
              sd_age = sd(age))
  cleaned <- cleaned %>% inner_join(temp)
  rm(temp)
  
  # Get head-of-household targets (for training data only)
  # Target = Target - 1
  if('Target' %in% names(by_household)) {
    targets <- by_household %>%
      filter(parentesco1 == 1) %>% 
      summarize(target = max(Target) - 1)
    cleaned <- cleaned %>% inner_join(targets)
    rm(targets)    
  }

  cleaned
   
}


# Load and clean data
data <- read.csv(train_path)
cleaned_data <- clean(data)

# Split into test/train
set.seed(123)
smp_size <- floor(0.8 * nrow(cleaned_data))
train_ind <- sample(seq_len(nrow(cleaned_data)), size = smp_size)
train <- cleaned_data[train_ind,]
test <- cleaned_data[-train_ind,]

# Split train into four CV sets
flds <- createFolds(train$target, k = 4, list = TRUE, returnTrain = FALSE)

# Create empty data frame to hold CV results
cv_results = data.frame()

# Perform LightGBM model on each fold
for(fold in flds) {
  cv_train <- train[-fold,]
  cv_valid <- train[fold,]
  
  # Separate data frames for inputs and labels
  train_inputs <- as.matrix(cv_train %>% select(-c(idhogar,target)))
  train_labels <- as.matrix(cv_train %>% select(target))
  valid_inputs <- as.matrix(cv_valid %>% select(-c(idhogar,target)))
  valid_labels <- as.matrix(cv_valid %>% select(target))
  
  dtrain <- lgb.Dataset(data = train_inputs, label = train_labels)
  dtest <- lgb.Dataset.create.valid(dtrain, data = valid_inputs, label = valid_labels)
  valids <- list(test = dtest)

  params <- list(objective = 'multiclass', metric = 'multi_error', num_class = 4)
  model <- lgb.train(params,
                     dtrain,
                     10000,
                     valids,
                     min_data = 1,
                     learning_rate = 0.0001,
                     early_stopping_rounds = 10000)

  # Append CV results to overall DF
  cv_predict <- as.data.frame(predict(model, valid_inputs, reshape = TRUE))
  cv_predict$prediction <- apply(cv_predict, 1, which.max) - 1
  cv_predict$reference <- cv_valid$target
  cv_results <- rbind(cv_results, cv_predict)
}

# Cross-Validation Confusion Matrix
confusionMatrix(as.factor(cv_results$prediction), as.factor(cv_results$reference))

# Test model performance on holdout data
test_inputs <- as.matrix(test %>% select(-c(idhogar,target)))
train_labels <- as.matrix(test %>% select(target))
test_predict <- as.data.frame(predict(model, test_inputs, reshape = TRUE))
test_predict$prediction <- apply(test_predict, 1, which.max) - 1
test_predict$reference <- train_labels
confusionMatrix(as.factor(test_predict$prediction), as.factor(test_predict$reference))


# Submission dataset
submission <- read.csv(test_path)
cleaned_submission <- clean(submission)
submission_inputs <- as.matrix(cleaned_submission %>% select(-c(idhogar)))
submission_predict <- as.data.frame(predict(model, submission_inputs, reshape = TRUE))
submission_predict$prediction <- apply(submission_predict, 1, which.max)
submission_predict$idhogar <- cleaned_submission$idhogar

final_submission <- submission %>% 
  inner_join(tbl_df(submission_predict)) %>%
  select(Id = Id, Target = prediction)

write.csv(final_submission, submission_path, row.names = FALSE)
