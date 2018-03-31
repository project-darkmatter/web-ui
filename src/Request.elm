module Request
    exposing
        ( eval
        , getResult
        )

import Http
import Json.Decode as Decode


eval : String -> Int -> Http.Request EvalData
eval host p =
    Http.get "http://" ++ host ++ ":" ++ toString p decodeEvalData


type alias EvalData =
    { taskId : Int
    }


decodeEvalData : Decode.Decoder EvalData
decodeEvalData =
    Decode.map EvalData
        (Decode.field "taskId" Decode.int)


getResult : String -> Int -> Http.Request GetResultData
getResult host p =
    Http.get "https://" ++ host ++ ":" ++ toString p decodeGetResultData


type alias GetResultData =
    { status : Boolean
    , content : Maybe ResultContent
    }


type alias ResultContent =
    { context : Maybe Dictionary
    , status : ResultStatus
    , value : Any
    , output : String
    }


type ResultStatus
    = Success
    | Failure
    | Running


decodeResultStatus : Decode.Decoder ResultStatus
decodeResultStatus =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "SUCCESS" ->
                        Decode.succeed Success

                    "FAILURE" ->
                        Decode.succeed Failure

                    "RUNNING" ->
                        Decode.succeed Running

                    somethingElse ->
                        Decode.fail <| "Unknown status: " ++ somethingElse
            )


decodeGetResultData : Decode.Decoder GetResultData
decodeGetResultData =
    Decode.map2 GetResultData
        (Decode.field "status" Decode.boolean)
        (Decode.field "content" (Decode.maybe decodeResultContent))


decodeResultContent : Decode.Decoder ResultContent
decodeResultContent =
    Decode.map4 ResultContent
        (Decode.field "context" Decode.dict)
        (Decode.field "status" decodeResultStatus)
        (Decode.field "value" Decode.dict)
        (Decode.field "output" Decode.string)
