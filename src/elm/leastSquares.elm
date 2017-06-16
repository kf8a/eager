module LeastSquares exposing (..)


type alias Point =
    { x : Float
    , y : Float
    , deleted : Bool
    , id : Int
    }


type alias Fit =
    { slope : Float, intercept : Float, r2 : Float }


fitLineByLeastSquares : List Point -> Result String Fit
fitLineByLeastSquares points =
    let
        goodPoints =
            List.filter (\x -> x.deleted == False) points

        sum_x =
            List.foldl (+) 0 (List.map .x goodPoints)

        sum_y =
            List.foldl (+) 0 (List.map .y goodPoints)

        sum_xy =
            List.foldl (\point accum -> accum + (point.x * point.y)) 0 goodPoints

        sum_xx =
            List.foldl (\point accum -> accum + (point.x * point.x)) 0 goodPoints

        sum_yy =
            List.foldl (\point accum -> accum + (point.y * point.y)) 0 goodPoints

        count =
            toFloat (List.length goodPoints)

        slope =
            (count * sum_xy - sum_x * sum_y) / (count * sum_xx - sum_x * sum_x)

        intercept =
            (sum_y / count) - (slope * sum_x) / count

        mean_y =
            sum_y / count

        correlation =
            (count * sum_xy - sum_x * sum_y) / sqrt ((count * sum_xx - sum_x * sum_x) * (count * sum_yy - sum_y * sum_y))

        r2 =
            correlation * correlation
    in
        if count >= 2 then
            Ok (Fit slope intercept r2)
        else
            Err "regression failed"
