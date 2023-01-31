#' @description Code to
#' @param predicted_data Output from the `function predict_from_model.R`
#' @param observed_data Still not sure about the format here.
#tar_load(model_output)
#tar_load(predict_data)


library(terra)
library(sf)

# should push the release as a release

# Each feature must have unique “PLOTID” and “SAMPLEID” fields.
# Estimated anomaly field: “ano_ti_est” in YYYYMMDD format #for now, use the first date of
# Anomaly score (0-1)

detect_anomalies <- function(predicted_data, temp_folder = "data/temp/anomalies",tag = "test", repo = "AdamWilsonLab/emma_model"){

  # For now, the important part is the output
  # 1) table of cellIDs, various anomaly scores
  # 2) spatial data of 1, with cells represented as polygons

  # Generate anomaly score

  predicted_data %>%
    mutate(dif_obs_v_pred = abs(mu - ndvi)) %>%
    mutate(overall_max_diff = max(dif_obs_v_pred,na.rm = TRUE))%>%
    mutate(anomaly = dif_obs_v_pred/overall_max_diff) %>%
    group_by(cellID) %>%
    mutate(max_anomaly = max(anomaly)) %>%
    ungroup()%>%
    filter(anomaly==max_anomaly) %>%
    select(date,cellID,anomaly,max_anomaly)%>%
    group_by(cellID)%>%
    filter(date == min(date)) %>%
    mutate(ano_ti_est = date) -> temp



  temp %>%
    filter(anomaly > .0000001)-> anomaly_scores


  # Write anomaly scores

  message("write code later")


  # Generate polygon output

  #need the template raster we used

  robust_pb_download(file = "template.tif",
                     dest = "data",
                     repo = "AdamWilsonLab/emma_envdata",
                     tag = "processed_static")

  outrast <- terra::rast("data/template.tif")
  values(outrast) <- NA


  # here I assign anomalous cell their own cellID.  This way they are the only ones that get converted to polygons, and can be linked to the relevant data
  outrast[unique(anomaly_scores$cellID)] <- unique(anomaly_scores$cellID)
  names(outrast) <- "cellID"

  # convert raster to polygons
  out_polys <- outrast %>%
    as.polygons() %>%
    st_as_sf()

  # Join the relevant data

  out_polys %>%
    left_join(y = anomaly_scores)%>%
    select(-date,-max_anomaly)%>%
    rename(ano_scr = anomaly)%>%
    mutate(PLOTID = row_number(),
           SAMPLEID = row_number())%>%
    st_transform(crs = "EPSG:4326")-> out_polys

  #fix the date. Desired format is YYYYMMDD

  out_polys %>%
    mutate(ano_ti_est = gsub(pattern = "-",
                             replacement = "",
                             x = as_date(ano_ti_est))) -> out_polys


  #write as shapefile

    #create folder if needed
        if(!dir.exists(temp_folder)){
          dir.create(temp_folder,recursive = TRUE)
        }

  st_write(obj = out_polys,
           dsn = file.path(temp_folder,"anomaly_polys.shp"),
           append = FALSE)


  #zip shapefile

    zip(files = list.files(path = temp_folder,
                           pattern = "anomaly_poly",
                           full.names = TRUE),
        zipfile = file.path(temp_folder,"anomaly_poly.zip"))


  #release zipfile

    released_files <- pb_list(repo = repo)

    if(!tag %in% released_files$tag){
      pb_release_create(repo = repo,tag = tag)
    }

    pb_upload(file = file.path(temp_folder,"anomaly_poly.zip"),
              repo = repo,
              tag = tag,
              overwrite = TRUE)


}


####################





