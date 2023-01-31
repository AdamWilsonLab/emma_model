
#' @description Code to take inputs from the vb model output and predict NDVI to a wider region

#tar_load(model_output)
#tar_load(predict_data)

predict_from_model <- function(model_output = NULL,
                               predict_data = NULL){


  #Pull relevant parameters from model

  unique(model_output$variable)


      #Calculating gamma

            #gamma_mu = x*gamma_beta
            #gamma ~ normal(gamma_mu,gamma_tau)
            #gamma goes into model

        #gamma beta should be a vector of length equal to the number of pixels

          gamma_beta <-
            model_output %>%
              filter(grepl(pattern = "gamma_beta",x = variable,fixed = TRUE)) %>%
              pull(median)

        # gamma should be equal to the number of cells

          gamma <- as.vector(gamma_beta) %*% t(as.matrix(predict_data$x))

        # plot gamma

          # envdata_files  <- pb_list(repo = "AdamWilsonLab/emma_envdata")
          #
          # pb_download(file = "template.tif",
          #             tag = "raw_static",
          #             repo = "AdamWilsonLab/emma_envdata",dest = "data/")
          #
          # library(terra)
          # template <- terra::rast("data/template.tif")
          # template[1:ncell(template)] <- NA
          #
          # gamma_raster <- template
          # gamma_raster[predict_data$x_cellID] <- as.vector(gamma)
          #
          # plot(gamma_raster,main="gamma")

        #Calculating lambda

          #lambda_mu = x*lambda_beta
          #lambda ~ normal(lambda_mu,lambda_tau)
          #lambda goes into model

          #lambda beta should be a vector of length equal to the number of pixels
            lambda_beta <-
              model_output %>%
              filter(grepl(pattern = "lambda_beta",x = variable,fixed = TRUE)) %>%
              pull(median)

          # lambda should be equal to the number of cells

            lambda <- as.vector(lambda_beta) %*% t(as.matrix(predict_data$x))

          # plot lambda

            # lambda_raster <- template
            # lambda_raster[predict_data$x_cellID] <- as.vector(lambda)
            #
            # plot(lambda_raster,main="lamda")

          # Calculating alpha

            #alpha_mu
            #alpha ~ normal(alpha_mu,alpha_tau)
            #alpha goes into model

            #alpha beta should be a single value

              alpha_mu <-
                model_output %>%
                filter(grepl(pattern = "alpha_mu",x = variable,fixed = TRUE)) %>%
                pull(median)

              alpha <- alpha_mu #ignoring uncertainty for now


          # Predict from model components

            #mu[i] = exp(alpha[pid[i]])+exp(gamma[pid[i]])-exp(gamma[pid[i]])*exp(-(age[i]/exp(lambda[pid[i]])));


              model_parms <- data.frame(cellID = pid_lookup$cellID,
                                        alpha = alpha,
                                        lambda = t(lambda),
                                        gamma = t(gamma))

            #so all we need is the ages and pixelID (from which we get the gmma and lambda)

            predict_data$N #pixels x time steps
            predict_data$J #pixels
            predict_data$y_cellID

            predict_data$age
            unique(as_date(predict_data$y_date))


            #Merge the predict data with the model parms
              predict_data #age and cellID

              predict_df <- data.frame(date = predict_data$y_date,
                                       age = predict_data$age,
                                       cellID = predict_data$y_cellID,
                                       ndvi = predict_data$y_obs) %>%
                full_join(model_parms)


              predict_df %>%
                mutate(mu = exp(alpha)+exp(gamma)-exp(gamma)*exp(-(age/exp(lambda)))) -> predict_df

              if(any(predict_df$age < 0)){

                message("Impossible ages in data (negatives)")

                predict_df %>%
                  dplyr::filter(age >=0) -> predict_df

              }


              predict_df

              hist(predict_df$mu)

              min(predict_df$ndvi,na.rm = TRUE)





}
