module Calibration exposing (..)

import Data exposing (..)
import LeastSquares exposing (..)


--- Compute Fluxes


fluxWithDefault : Gas -> Result String Fit -> Flux
fluxWithDefault gas fit =
    case fit of
        Ok fit ->
            Flux fit.slope fit.intercept fit.r2 Nothing gas

        Err message ->
            initialFlux


computeFlux : Gas -> List Point -> Flux
computeFlux gas points =
    points
        |> fitLineByLeastSquares
        |> fluxWithDefault gas


computeIncubationFluxes : Incubation -> Incubation
computeIncubationFluxes incubation =
    let
        n2o_flux =
            incubation.injections
                |> n2o_injections
                |> computeFlux N2O

        co2_flux =
            incubation.injections
                |> co2_injections
                |> computeFlux CO2

        ch4_flux =
            incubation.injections
                |> ch4_injections
                |> computeFlux CH4
    in
        { incubation
            | n2o_flux = Just n2o_flux
            , ch4_flux = Just ch4_flux
            , co2_flux = Just co2_flux
        }


updateRunStandard : Run -> (List Standard -> Point -> List Standard) -> Point -> Run
updateRunStandard run updater point =
    let
        new_point =
            { point | deleted = not point.deleted }

        new_standards =
            updater run.standards new_point

        new_run =
            { run | standards = new_standards }
    in
        new_run


computeCO2Flux : Incubation -> Incubation
computeCO2Flux incubation =
    let
        _ =
            Debug.log "computing CO2 Flux" incubation

        new_flux =
            computeFlux CO2 (co2_injections incubation.injections)
    in
        { incubation | co2_flux = Just new_flux }


computeN2OFlux : Incubation -> Incubation
computeN2OFlux incubation =
    let
        _ =
            Debug.log "computing N2O Flux" incubation

        new_flux =
            computeFlux N2O (n2o_injections incubation.injections)
    in
        { incubation | n2o_flux = Just new_flux }


computeCH4Flux : Incubation -> Incubation
computeCH4Flux incubation =
    let
        new_flux =
            computeFlux CH4 (ch4_injections incubation.injections)
    in
        { incubation | ch4_flux = Just new_flux }


computeN2OFluxes : Run -> Run
computeN2OFluxes run =
    let
        incubations =
            List.map computeN2OFlux run.incubations
    in
        { run | incubations = incubations }


computeCO2Fluxes : Run -> Run
computeCO2Fluxes run =
    let
        incubations =
            List.map computeCO2Flux run.incubations
    in
        { run | incubations = incubations }


computeCH4Fluxes : Run -> Run
computeCH4Fluxes run =
    let
        incubations =
            List.map computeCH4Flux run.incubations
    in
        { run | incubations = incubations }



--- Calibrations


calibrateInjectionN2O : Flux -> Injection -> Injection
calibrateInjectionN2O calibration injection =
    let
        n2o_ppm =
            calibration.slope * injection.n2o_mv

        -- n2o_ppm =
        --     calibration.intercept + calibration.slope * injection.n2o_mv
    in
        { injection | n2o_ppm = n2o_ppm }


calibrateInjectionCH4 : Flux -> Injection -> Injection
calibrateInjectionCH4 calibration injection =
    let
        ch4_ppm =
            calibration.intercept + calibration.slope * injection.ch4_mv
    in
        { injection | ch4_ppm = ch4_ppm }


calibrateInjectionCO2 : Flux -> Injection -> Injection
calibrateInjectionCO2 calibration injection =
    let
        co2_ppm =
            calibration.intercept + calibration.slope * injection.co2_mv
    in
        { injection | co2_ppm = co2_ppm }


calibrateIncubationN2O : Flux -> Incubation -> Incubation
calibrateIncubationN2O calibration incubation =
    let
        injections =
            incubation.injections
                |> List.map (calibrateInjectionN2O calibration)
    in
        { incubation | injections = injections }


calibrateIncubationCH4 : Flux -> Incubation -> Incubation
calibrateIncubationCH4 calibration incubation =
    let
        injections =
            incubation.injections
                |> List.map (calibrateInjectionCH4 calibration)
    in
        { incubation | injections = injections }


calibrateIncubationCO2 : Flux -> Incubation -> Incubation
calibrateIncubationCO2 calibration incubation =
    let
        injections =
            incubation.injections
                |> List.map (calibrateInjectionCO2 calibration)
    in
        { incubation | injections = injections }


calibrateRun : Run -> Run
calibrateRun run =
    let
        co2_cal =
            computeCalibrationCO2 run.standards

        ch4_cal =
            computeCalibrationCH4 run.standards

        n2o_cal =
            computeCalibrationN2O run.standards

        newRun =
            { run
                | co2_calibration = Just co2_cal
                , ch4_calibration = Just ch4_cal
                , n2o_calibration = Just n2o_cal
            }
                |> calibrateRunCO2
                |> calibrateRunCH4
                |> calibrateRunN2O
    in
        { newRun | incubations = List.map computeIncubationFluxes newRun.incubations }


calibrateRunN2O : Run -> Run
calibrateRunN2O run =
    case run.n2o_calibration of
        Just calibration ->
            let
                newIncubationList =
                    run.incubations
                        |> List.map (calibrateIncubationN2O calibration)
            in
                { run | incubations = newIncubationList }

        Nothing ->
            run


calibrateRunCH4 : Run -> Run
calibrateRunCH4 run =
    case run.ch4_calibration of
        Just calibration ->
            let
                newIncubationList =
                    run.incubations
                        |> List.map (calibrateIncubationCH4 calibration)
            in
                { run | incubations = newIncubationList }

        Nothing ->
            run


calibrateRunCO2 : Run -> Run
calibrateRunCO2 run =
    case run.co2_calibration of
        Just calibration ->
            let
                newIncubationList =
                    run.incubations
                        |> List.map (calibrateIncubationCO2 calibration)
            in
                { run | incubations = newIncubationList }

        Nothing ->
            run


computeCalibrationN2O : List Standard -> Flux
computeCalibrationN2O standards =
    averageCalibration N2O (n2o_standards standards) 0.3



-- computeFlux N2O (n2o_standards standards)


computeCalibrationCH4 : List Standard -> Flux
computeCalibrationCH4 standards =
    averageCalibration CH4 (ch4_standards standards) 4.0



-- computeFlux CH4 (ch4_standards standards)


computeCalibrationCO2 : List Standard -> Flux
computeCalibrationCO2 standards =
    averageCalibration CO2 (co2_standards standards) 1



-- computeFlux CO2 (co2_standards standards)


updateCalibrationN2O : Run -> Point -> Run
updateCalibrationN2O run point =
    let
        updatedStandardRun =
            updateRunStandard run (updateN2OStandards) point

        updatedRun =
            { updatedStandardRun
                | n2o_calibration =
                    Just
                        (computeCalibrationN2O
                            updatedStandardRun.standards
                        )
            }
    in
        updatedRun
            |> calibrateRunN2O
            |> computeN2OFluxes


updateCalibrationCH4 : Run -> Point -> Run
updateCalibrationCH4 run point =
    let
        updatedRun =
            updateRunStandard run (updateCH4Standards) point
    in
        { updatedRun | ch4_calibration = Just (computeCalibrationCH4 updatedRun.standards) }
            |> calibrateRunCH4
            |> computeCH4Fluxes


updateCalibrationCO2 : Run -> Point -> Run
updateCalibrationCO2 run point =
    let
        updatedRun =
            updateRunStandard run (updateCO2Standards) point
    in
        { updatedRun | co2_calibration = Just (computeCalibrationCO2 updatedRun.standards) }
            |> calibrateRunCO2
            |> computeCO2Fluxes


averageCalibration : Gas -> List Point -> Float -> Flux
averageCalibration gas points standard_value =
    let
        average_mv =
            points
                |> List.filter (\x -> not x.deleted)
                |> List.map (\x -> x.y)
                |> List.sum

        calibration_value =
            standard_value / average_mv
    in
        Flux calibration_value 0 0 Nothing gas
