asyR <- R6::R6Class(
  "asyR",
  list(
    xml = NULL,
    file =  NULL,
    version =  NULL,
    lot =  NULL,
    type =  NULL,
    sn =  NULL,
    Inst =  NULL,
    calibration_temp = NULL,
    tick_table = NULL,
    O2_coefs = NULL,
    pH_coefs = NULL,
    calibration = NULL,
    levels = NULL,
    template =NULL,
    assay = NULL,

    initialize = function(x) {
      library(XML)
      self$xml <- XML::xmlTreeParse(
        x,
        useInternalNodes = T
      )
      self$file <- basename(xpathSApply(self$xml, "//FileName", xmlValue))
      self$version <- xpathSApply(self$xml, "//SWVersion", xmlValue)
      self$lot <- xpathSApply(self$xml, "//Cartridge//Lot", xmlValue)
      self$type <-xpathSApply(self$xml, "//Cartridge//Type", xmlValue)
      self$check_lottype()
      self$sn <- xpathSApply(self$xml, "//Cartridge//Serial", xmlValue)
      self$Inst <- xpathSApply(self$xml, "//InstrumentSerialNumber", xmlValue)
      self$calibration_temp <- as.numeric(
        xpathSApply(self$xml, "//AssayDataSet//CalibrationStartTemperature", xmlValue)
      )
      self$get_template()
      self$make_tick_table()
      self$get_O2_coefs()
      self$get_pH_coefs()
      self$get_cal_data()
      self$get_lvls()
      self$assay<-self$determine_assay(self$template,self$file,F,NULL)
    },


    make_tick_table = function() {
      start = as.numeric(xpathSApply(self$xml, "//AssayDataSet//RateSpans//StartTickIndex", xmlValue))
      end =  as.numeric(xpathSApply(self$xml, "//AssayDataSet//RateSpans//EndTickIndex", xmlValue))
      self$tick_table <- do.call('rbind',
                                 Map(
                                   function(x, y, z) {
                                     data.frame(Tick = seq(from = x,
                                                           to = y,
                                                           by = 1), Measure = z)
                                   },
                                   x = start,
                                   y = end,
                                   z = seq_along(start)
                                 ))
    },
    get_O2_coefs = function() {
      ksvs <- as.numeric(xpathSApply(self$xml, '//O2DataModifiers//Ksv', xmlValue))
      F0 <- as.numeric(XML::xpathApply(self$xml,"//AssayDataSet//O2DataModifiers//FO",XML::xmlValue)[[1]])
      coefs <- list(
        "target" = as.numeric(
          xpathSApply(
            self$xml,
            "//Item[Key='O2']//Value//AnalyteCalibration//TargetEmissionValue",
            xmlValue
          )
        ),
        "Ksv" = ksvs[1],
        "CorrectedKsv" = ksvs[2],
        "F0" = F0
      )
      self$O2_coefs <- coefs
    },
    get_pH_coefs = function() {
      coefs <- list(
        "slope" = as.numeric(
          xpathSApply(
            self$xml,
            "//Item[Key='pH']//Value//AnalyteCalibration//GainEquation//C3",
            xmlValue
          )
        ),
        "intercept" = as.numeric(
          xpathSApply(
            self$xml,
            "//Item[Key='pH']//Value//AnalyteCalibration//GainEquation//C4",
            xmlValue
          )
        ),
        "target" = as.numeric(
          xpathSApply(
            self$xml,
            "//Item[Key='pH']//Value//AnalyteCalibration//TargetEmissionValue",
            xmlValue
          )
        )
      )
      coefs$gain <- c((coefs$slope * coefs$target) + coefs$intercept)

      self$pH_coefs <- coefs
    },
    get_cal_data = function() {
      get_a_cal <- function(var, analyte) {
        xstr <- "//AssayDataSet//AnalyteCalibrationsByAnalyteName//Item[Key='"
        vars <-
          c(
            "LED" = "']//Value//AnalyteCalibration//LedValues//ArrayOfInt//int",
            "CalEmission" = "']//Value//AnalyteCalibration//CalibrationEmissionValues//ArrayOfDouble//double",
            "IntialReferenceDelta" = "']//Value//AnalyteCalibration//IntialReferenceDeltaValues//ArrayOfInt//int",
            "Status" = "']//Value//AnalyteCalibration//LedStatusValues//ArrayOfCalibrationQuality//CalibrationQuality"
          )
        type <- list(
          "LED" = as.numeric,
          "CalEmission" = as.numeric,
          "IntialReferenceDelta" = as.numeric,
          "Status" = as.character
        )
        type[[var]](xpathSApply(self$xml, paste0(xstr, analyte, vars[var]), xmlValue))
      }
      anCal <- function(xml, analyte) {
        var_names <- c("LED", "CalEmission", "IntialReferenceDelta", "Status")
        setNames(data.frame(lapply(
          var_names, get_a_cal, analyte = analyte
        )),
        paste(analyte, var_names, sep = "."))
      }
      pH <- anCal(self$xml, 'pH')
      O2 <- anCal(self$xml, 'O2')
      Well <- seq_along(pH[, 1])
      self$calibration <- cbind(Well, pH, O2)
    },
    get_lvls = function() {
      pH_CorrectedEmission = xpathApply(self$xml, "//Item[Key='pH']//Value//AnalyteDataSet//CorrectedEmissionValues", function(u) {
        as.numeric(xmlSApply(u, xmlValue))
      })
      O2_CorrectedEmission = xpathApply(self$xml, "//Item[Key='O2']//Value//AnalyteDataSet//CorrectedEmissionValues", function(u) {
        as.numeric(xmlSApply(u, xmlValue))
      })
      timestamp <-
        xpathSApply(self$xml,
                    "//Item[Key='pH']//Value//AnalyteDataSet//TimeStamp",
                    xmlValue)
      Out <- do.call(
        'rbind',
        Map(
          function(x, y, z, a) {
            Tick <- rep(z, length(x))
            data.frame(
              pH_CorrectedEmission = x,
              O2_CorrectedEmission = y,
              Tick,
              Well = seq_along(x),
              timestamp = a
            )
          },
          pH_CorrectedEmission,
          y = O2_CorrectedEmission,
          z = seq_along(pH_CorrectedEmission),
          a = timestamp
        )
      )

      minTick_Tick <- min(self$tick_table)
      minOut_Tick <- min(Out$Tick)
      #print(type)
      if (minTick_Tick == 0 && minOut_Tick == 1) {
        Out$Tick <- Out$Tick - 1
      }
      self$levels <- merge(Out, self$tick_table, by = 'Tick')
    },
    calc_gain = function() {
      tick <- self$levels$Tick
      filtered_lvls <- tick %in% (max(tick) - c(2, 1, 0))
      lvl_agg <-
        aggregate(pH_CorrectedEmission ~ Well,
                  data = self$levels[filtered_lvls, ],
                  FUN = mean)
      U <- merge(lvl_agg, self$calibration, by = "Well")
      LVL <- setNames(U, gsub("pH_CorrectedEmission", "sorpH", names(U)))
      LVL$Target <- self$pH_coefs$target
      LVL$Gain <-
        (LVL$Target / LVL$pH.CalEmission) * (1 / 800) * (LVL$pH.CalEmission - LVL$sorpH)
      LVL
    },
    calc_ksv = function() {
      spl <- lapply(split(self$levels, self$levels$Measure), function(u) {
        df <-
          u[u$Tick %in% (max(u$Tick) - c(2, 1, 0)), c('Tick', 'O2_CorrectedEmission', 'Well', 'Measure')]
        agg <- aggregate(O2_CorrectedEmission ~ Well,
                         data = df,
                         FUN = "mean")
        m <- c('Ambient', 'F0')[unique(df$Measure)]
        names(agg) <- c('Well', m)
        agg
      })
      comb <- Reduce('merge', spl)[c('Well', 'Ambient', 'F0')]
      comb$KSV <- ((comb$F0 / comb$Ambient) - 1) * 152^-1
      merge(comb, self$calibration, by = 'Well')
    },
    combo_assay = function() {
      pLVL <-
        self$levels[self$levels$Measure == 1, c("Well", "Tick", "Measure", "pH_CorrectedEmission")]
      tick <- pLVL$Tick
      filtered_lvls <- tick %in% (max(tick) - c(2, 1, 0))

      pHdf <- merge(setNames(
        aggregate(pH_CorrectedEmission ~ Well, data = pLVL[filtered_lvls, ], mean),
        c("Well", "sorpH")
      ),
      self$calibration, by = 'Well')
      pHdf[c("Target", "Gain")] <- list(
        self$pH_coefs$target,
        (self$pH_coefs$target / pHdf$pH.CalEmission) * (1 / 800) * (pHdf$pH.CalEmission -
                                                                      pHdf$sorpH)
      )
      O2df <-
        self$levels[self$levels$Measure == 1 | self$levels$Measure == 2 ,
                    c("Well", "O2_CorrectedEmission", "Tick", "Measure")]

      O2df$Measure <- c("Ambient", "F0")[O2df$Measure]
      kcalc <- lapply(split(O2df, O2df$Measure),
                      function(u) {
                        t <- u$Tick
                        out <- aggregate(O2_CorrectedEmission ~ Well,
                                         data = u[t %in% (max(t) - c(2, 1, 0)), ],
                                         FUN = "mean")
                        setNames(out, c("Well", unique(u$Measure)))
                      })

      ksv <- merge(kcalc$Ambient, kcalc$F0, by = 'Well')
      ksv$KSV <- ((ksv$F0 / ksv$Ambient) - 1) / 152


      combo <- merge(pHdf, ksv, by = 'Well')
      combo[c("Inst", "Lot", "sn")] <-
        list(self$Inst, paste0(self$type, self$lot), self$sn)
      combo
    },
    munge_pka = function() {
      pH <- c(3.8, 5, 5.8, 6.6, 7.0, 7.4, 8.15, 9.2)
      reps <- unname(c(
        "C" = 1,
        "T" = 3,
        "Q" = 3,
        "B" = 3,
        "W" = 12
      )[self$type])
      pH_df <- data.frame(pH = unlist(lapply(pH, rep, times = reps)))
      pH_df$Well <- seq_along(pH_df$pH)
      last_ticks_of_measures <-
        unlist(lapply(unique(self$levels$Measure), function(u) {
          max(self$levels$Tick[self$levels$Measure == u])
        }))
      tick_filter <-
        unlist(rev(lapply(rev(last_ticks_of_measures)[1:2], function(n) {
          c(-2, -1, 0) + n
        })))
      LVL <-
        self$levels[self$levels$Tick %in% tick_filter, c("pH_CorrectedEmission", "Tick", "Well", "Measure")]
      LVL$Tick <- as.numeric(factor(LVL$Tick))
      X <- merge(LVL, pH_df, by = 'Well')
      X$Measure <- as.numeric(factor(X$Measure))
      if (max(X$Measure == 2) & self$type != "W") {
        X$dye <- c('CL', 'PR')[X$Measure]
      } else if (self$type == "W" & max(X$Measure) == 1) {
        X$dye <- c(rep("CL", 6), rep("PR", 6))
      } else{
        X$dye <- 'CL'
      }

      setNames(aggregate(
        pH_CorrectedEmission ~ Well + pH + dye,
        data = X,
        FUN = mean
      ),c("Well", "pH", "dye", "counts"))
    },
    get_template = function() {
      usrtemplate<-xpathSApply(self$xml, path = "//IsUserTemplate", xmlValue)
      if (usrtemplate == 'false') {
        self$template <- NA
      } else{
        self$template <-
          basename(xpathSApply(self$xml, path = "//AssayTemplate", xmlValue))
      }
    },
    determine_assay=function(template,file,force=F,override=NULL){
      if(force==T){return(override)}
      sub_reg<-function(str,match){
        regmatches(str,gregexpr(match,str))
      }
      a<-unlist(sub_reg(tolower(template),"gain|ksv|barcode|wet qc xfp|wet qc|wetqc|mr|pka|outlier"))
      b<-unlist(sub_reg(tolower(file),"gain|ksv|barcode|wet qc xfp|wet qc|wetqc|mr|pka|outlier"))
      if(length(a)>0){return(a)}
      if(length(b>0)){return(b)}else{
        return(NA)
      }
    },calc_o2_lvl=function(){
      self$levels$O2<- outliers::partial_pressure_ox(cal_temp_start, ATMp) +
      self$O2_coefs$F0 * 
      (self$O2_coefs$target - self$levels$O2_CorrectedEmission) * 
      (self$levels$O2_CorrectedEmission)^-1 * 
      (self$O2_coefs$target)^-1  *
      (self$O2_coefs$Ksv)^-1 
    },
    wetqc=function(){
      if(self$type=="C" & self$assay=="wetqc"){
        return(self$combo_assay())
      }else{
        if(self$assay=="ksv"){
          return(self$calc_ksv())
        }
        if(self$assay=="gain"){
          return(self$calc_gain())
        }
        if(self$assay=="pka"){
          return(self$munge_pka())
        }
        # the
        message("you get ...nothing!")
      }
    },check_lottype=function(){
      if(self$lot!="" & self$type!=""){
        return()
      }else{
       LOT<- regmatches(self$file,gregexpr("[C|W|T|Q|B][0-9|E][0-9]{4}",self$file))[[1]]
        self$type<-substr(LOT,1,1)
        self$lot<-substr(LOT,2,6)
      }
    }

  )
)
