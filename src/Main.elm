module Main exposing (..)

-- Render a spinning cube.
--
-- Dependencies:
--   elm install elm-explorations/linear-algebra
--   elm install elm-explorations/webgl
--

import Browser
import Browser.Events as Events
import Html exposing (Html, input, div)
import Html.Events exposing (onInput)
import Html.Attributes exposing (width, height, style, value, placeholder, type_)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Json.Decode as Decode

import WebGL
import Meshes exposing (sphere_mesh)
import Array

import Scene exposing (Scene_Objects, initialise_scene, update_scene_movement, update_object_mesh, update_scene_sphere, show_scene)

-- MAIN



main : Program() Model Msg
main =
    Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL



type alias Keys =
    { up : Bool
    , left : Bool
    , down : Bool
    , right : Bool
    , space : Bool
    , ctrl : Bool
    }

no_keys : Keys
no_keys =
    Keys False False False False False False

type alias Model =
    {   
        scene_speed : Float 
        ,camera_coordinates : Vec3
        , triangle_count : Int
        --, sphere_mesh : WebGL.Mesh Vertex 
        , scene : Scene_Objects
        , keys : Keys
    }

init : () -> (Model, Cmd Msg)
init () =
    let 
        initial_coordinate = vec3 0 1 20 
        start_scene_speed = 5
        default_triangle_count = 300
    in 
        ( {scene_speed = start_scene_speed ,camera_coordinates = initial_coordinate ,triangle_count=default_triangle_count, scene= (initialise_scene default_triangle_count) , keys = no_keys} ,  Cmd.none )
        --sphere_mesh = (sphere_mesh (vec3 0 0 0) 1 default_triangle_count)}



-- UPDATE


type Msg
    = TimeDelta Float | ChangeRotationSpeed String | ChangeTriangleCount String | KeyChanged Bool String


update_keys : Bool -> String -> Keys -> Keys
update_keys isDown key keys = 
    case key of 
        "ArrowUp" -> { keys | up = isDown } 
        "ArrowLeft" -> {keys | left = isDown}
        "ArrowRight" -> {keys | right = isDown}
        "ArrowDown" -> {keys | down = isDown}
        "w" -> { keys | up = isDown } 
        "a" -> {keys | left = isDown}
        "d" -> {keys | right = isDown}
        "s" -> {keys | down = isDown}
        "W" -> { keys | up = isDown } 
        "A" -> {keys | left = isDown}
        "D" -> {keys | right = isDown}
        "S" -> {keys | down = isDown}
        " " -> { keys | space = isDown}
        "Control" -> {keys | ctrl = isDown}
        _ -> let _ = Debug.log "key:" key in keys
 

update_coordinates : Keys -> Vec3 -> Vec3 
update_coordinates keys coordinates =
    let 
        x_diff = (if keys.left  then -1 else 0) + (if keys.right then 1 else 0)
        z_diff = (if keys.down then  -1 else 0 ) + (if keys.up then 1 else 0)
        y_diff = (if keys.ctrl then -1 else 0) + (if keys.space then 1 else 0)
        _ = Debug.log "coord: " coordinates
    in 
    vec3 ((Vec3.getX coordinates) + x_diff) ((Vec3.getY coordinates)+y_diff) ((Vec3.getZ coordinates) + z_diff)
    
    


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        TimeDelta delta ->
                let 
                    time_diff = delta * model.scene_speed 
                in 
                ({ model | scene = (update_scene_movement model.scene time_diff), camera_coordinates = (update_coordinates model.keys model.camera_coordinates)}, Cmd.none )
        ChangeRotationSpeed newSpeed ->
            if newSpeed == "" then 
                ( { model | scene_speed = 0} , Cmd.none) 
            else 
                case String.toFloat newSpeed of 
                Nothing ->
                    (model, Cmd.none)
                Just speed ->
                    ( {model | scene_speed= speed}, Cmd.none ) 
        ChangeTriangleCount newTriangles -> 
            case String.toInt newTriangles of 
                Nothing -> 
                    (model, Cmd.none) 
                Just count ->
                    let 
                        --new_sphere = { model.objects.sphere | mesh = (sphere_mesh (vec3 0 0 0) 1 count)}
                        maybe_sphere = Array.get 0 model.scene.objects 
                    in 
                    case maybe_sphere of 
                        Nothing -> 
                            (model, Cmd.none)
                        Just sphere -> 
                            let 
                                new_sphere = update_object_mesh sphere (sphere_mesh (vec3  0 0 0) 1 count)
                                scene = update_scene_sphere model.scene new_sphere
                            in

                            ( {model | triangle_count = count, scene = scene}, Cmd.none)
        KeyChanged isDown key -> 
            ({ model |  keys = update_keys isDown key model.keys} , Cmd.none)






-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch 
        [Events.onAnimationFrameDelta TimeDelta
        ,Events.onKeyUp (Decode.map (KeyChanged False) (Decode.field "key" Decode.string))
        , Events.onKeyDown (Decode.map (KeyChanged True) (Decode.field "key" Decode.string))
        ]


-- VIEW


view : Model -> Html Msg
view model =
        div [ style "background-color" "black"] 
            [
            div [style "z-index" "1", style "position" "absolute"] [
                input [ type_ "number",  placeholder "Scene speed" , value (String.fromFloat model.scene_speed), onInput ChangeRotationSpeed] []
                ,input [ type_ "number", placeholder "Triangle count" , value (String.fromInt model.triangle_count), onInput ChangeTriangleCount] []
            ]
            , WebGL.toHtml
                [ width 1920, height 1080, style "display" "table", style "width" "100%", style "height" "100%", style "background-color" "black" ,style "position" "absolute", style "top" "0"
                ]
                (show_scene model.camera_coordinates model.scene)
            ]


