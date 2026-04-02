module GenerateProgram exposing (run)

import BackendTask
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Elm
import Generate.Program
import Json.Encode
import Pages.Script as Script exposing (Script)
import Random


type alias CliOptions =
    { seed : Int
    , count : Int
    }


run : Script
run =
    Script.withCliOptions program
        (\{ seed, count } ->
            let
                initialSeed =
                    Random.initialSeed seed

                programs =
                    generatePrograms initialSeed count []
            in
            programs
                |> List.map writeProgram
                |> BackendTask.combine
                |> BackendTask.andThen
                    (\_ ->
                        writeManifest programs
                    )
                |> BackendTask.andThen
                    (\_ ->
                        Script.log
                            ("Generated "
                                ++ String.fromInt count
                                ++ " programs with seed "
                                ++ String.fromInt seed
                            )
                    )
                |> BackendTask.allowFatal
        )


program : Program.Config CliOptions
program =
    Program.config
        |> Program.add
            (OptionsParser.build CliOptions
                |> OptionsParser.with
                    (Option.requiredKeywordArg "seed"
                        |> Option.validateMap
                            (\s ->
                                case String.toInt s of
                                    Just i ->
                                        Ok i

                                    Nothing ->
                                        Err "seed must be an integer"
                            )
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "count"
                        |> Option.withDefault "10"
                        |> Option.validateMap
                            (\s ->
                                case String.toInt s of
                                    Just i ->
                                        Ok i

                                    Nothing ->
                                        Err "count must be an integer"
                            )
                    )
            )


type GeneratedProgram
    = SingleModule { moduleName : String, file : Elm.File }
    | MultiModule { moduleName : String, helperFile : Elm.File, mainFile : Elm.File }


generatePrograms : Random.Seed -> Int -> List GeneratedProgram -> List GeneratedProgram
generatePrograms seed remaining acc =
    if remaining <= 0 then
        List.reverse acc

    else
        let
            index =
                List.length acc

            isMultiModule =
                modBy 5 index == 4
        in
        if isMultiModule then
            let
                ( ( helperFile, mainFile ), nextSeed ) =
                    Random.step (Generate.Program.multiModuleGenerator index) seed
            in
            generatePrograms nextSeed
                (remaining - 1)
                (MultiModule
                    { moduleName = "Test" ++ String.fromInt index
                    , helperFile = helperFile
                    , mainFile = mainFile
                    }
                    :: acc
                )

        else
            let
                ( file, nextSeed ) =
                    Random.step (Generate.Program.programGenerator index) seed
            in
            generatePrograms nextSeed
                (remaining - 1)
                (SingleModule
                    { moduleName = "Test" ++ String.fromInt index
                    , file = file
                    }
                    :: acc
                )


writeProgram prog =
    case prog of
        SingleModule { file } ->
            Script.writeFile
                { path = "generated/src/" ++ file.path
                , body = file.contents
                }

        MultiModule { helperFile, mainFile } ->
            Script.writeFile
                { path = "generated/src/" ++ helperFile.path
                , body = helperFile.contents
                }
                |> BackendTask.andThen
                    (\_ ->
                        Script.writeFile
                            { path = "generated/src/" ++ mainFile.path
                            , body = mainFile.contents
                            }
                    )


programModuleName : GeneratedProgram -> String
programModuleName prog =
    case prog of
        SingleModule { moduleName } ->
            moduleName

        MultiModule { moduleName } ->
            moduleName


encodeManifestEntry : GeneratedProgram -> Json.Encode.Value
encodeManifestEntry prog =
    case prog of
        SingleModule { moduleName } ->
            Json.Encode.string moduleName

        MultiModule { moduleName, helperFile } ->
            Json.Encode.object
                [ ( "main", Json.Encode.string moduleName )
                , ( "helpers"
                  , Json.Encode.list Json.Encode.string
                        [ String.replace ".elm" "" helperFile.path ]
                  )
                ]


writeManifest programs =
    Script.writeFile
        { path = "generated/manifest.json"
        , body =
            Json.Encode.encode 2
                (Json.Encode.list encodeManifestEntry programs)
        }
