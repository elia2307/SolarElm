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
import WebGL
import Meshes exposing (sphere_mesh)
import Utils exposing (update_coordinates, update_velocity)
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



type alias Model =
    {   angle : Float
        ,scene_speed : Float 
        ,coordinates : Vec3
        ,velocity : Vec3
        , triangle_count : Int
        --, sphere_mesh : WebGL.Mesh Vertex 
        , scene : Scene_Objects
    }


init : () -> (Model, Cmd Msg)
init () =
    let 
        initial_velocity = vec3 0.001 0.001 0.001
        initial_coordinate = vec3 0 0 0 
        start_scene_speed = 5
        start_angle = 0.1
        default_triangle_count = 300
    in 
        ( {scene_speed = start_scene_speed ,angle = start_angle,  coordinates = initial_coordinate , velocity= initial_velocity, triangle_count=default_triangle_count, scene= (initialise_scene default_triangle_count)} ,  Cmd.none )
        --sphere_mesh = (sphere_mesh (vec3 0 0 0) 1 default_triangle_count)}



-- UPDATE


type Msg
    = TimeDelta Float | ChangeRotationSpeed String | ChangeTriangleCount String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        TimeDelta delta ->
                let 
                    coordinates = update_coordinates model.coordinates (Vec3.scale delta model.velocity)
                    velocity = update_velocity model.velocity coordinates
                    time_diff = delta * model.scene_speed 
                in 
                ({ model | scene = (update_scene_movement model.scene time_diff),  coordinates= coordinates, velocity = velocity , angle = model.angle + delta  * (model.scene_speed / 5000) }, Cmd.none )
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






-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Events.onAnimationFrameDelta TimeDelta


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
                [ width 3000, height 2000, style "display" "table", style "width" "100%", style "height" "100%", style "background-color" "black" ,style "position" "absolute", style "top" "0"
                ]
                (show_scene model.scene)
            ]


