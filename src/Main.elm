module Main exposing (..)

import Browser
import Browser.Events as Events
import Html exposing (Html, input, div,p,text)
import Html.Events exposing (onInput,on)
import Html.Attributes exposing (id, width, height, style, value, placeholder, type_)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Json.Decode as Decode
import WebGL
import Array

import Scene exposing (Scene_Objects, initialise_scene, update_scene_movement, update_object_mesh, update_scene_sphere, show_scene, set_scene_object_coord)
import Person exposing (update_camera_coordinates, update_camera_angle, Keys, update_keys, no_keys)
import Utils exposing (vec_to_string)
import Shaders exposing (get_camera_dir)
import Meshes exposing (vertex_list_to_mesh,sphere_mesh)

-- MAIN



main : Program() Model Msg
main =
    Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



type alias Model =
    {   
        scene_speed : Float 
        ,camera_coordinates : Vec3
        , camera_pitch : Float
        , camera_yaw : Float 
        , triangle_count : Int
        , scene : Scene_Objects
        , keys : Keys
        , frame_time: Float
        , fov: Float
        , show_debug_info : Bool
    }

init : () -> (Model, Cmd Msg)
init () =
    let 
        initial_coordinate = vec3 1 1 20 
        start_scene_speed = 5
        default_triangle_count = 3000
    in 
        ( { show_debug_info = False , fov= 45, frame_time = 0.01, scene_speed = start_scene_speed ,camera_coordinates = initial_coordinate , camera_pitch =0, camera_yaw = -90, triangle_count=default_triangle_count, scene= (initialise_scene default_triangle_count) , keys = no_keys} ,  Cmd.none )


-- UPDATE


type Msg
    = TimeDelta Float | ChangeRotationSpeed String | ChangeTriangleCount String | KeyChanged Bool String | MouseMovement Point | MouseScroll Bool



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        TimeDelta delta ->
                let 
                    time_diff = delta * model.scene_speed 
                    updated_scene_movement = update_scene_movement model.scene time_diff 
                    updated_scene = set_scene_object_coord updated_scene_movement 1 (Vec3.add model.camera_coordinates (Vec3.scale 100 (get_camera_dir model.camera_pitch model.camera_yaw)))
                in 
                ({ model | frame_time = ((delta + (model.frame_time * 9)) /10) , scene = updated_scene, camera_coordinates = update_camera_coordinates model.keys model.camera_coordinates model.camera_pitch model.camera_yaw}, Cmd.none )
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
                                new_sphere = update_object_mesh sphere (vertex_list_to_mesh (sphere_mesh (vec3  0 0 0) 1 count))
                                scene = update_scene_sphere model.scene new_sphere
                            in

                            ( {model | triangle_count = count, scene = scene}, Cmd.none)
        KeyChanged isDown key -> 
            if (key == "+" || key == "=") && isDown then 
                ({ model | fov= clamp 1 179 (model.fov * 0.9)}, Cmd.none)
            else if (key == "-" || key == "_") && isDown then 
                ({ model | fov = clamp 1 179 (model.fov *1.1)}, Cmd.none)
            else if (key == "r" || key == "R" )&& isDown then 
                ({model |camera_coordinates = (vec3 1 1 20) , fov = 45, camera_yaw = -90, camera_pitch = 0} , Cmd.none)
            else if key == "`"  && isDown then 
                ({ model | show_debug_info = not model.show_debug_info} , Cmd.none)
            else 
                ({ model |  keys = update_keys isDown key model.keys} , Cmd.none)
        MouseMovement point -> 
            if not model.keys.shift then 
                ({model | camera_pitch = (update_camera_angle model.camera_pitch -point.y  180) ,camera_yaw = (update_camera_angle model.camera_yaw point.x -1)} , Cmd.none)
            else 
                ( model, Cmd.none)
        MouseScroll isUp ->
            let  
                new_fov = clamp 1 179 (model.fov  + (if isUp then -5 else 5)) 
            in 
            ({ model | fov = new_fov} , Cmd.none)






-- SUBSCRIPTIONS

type alias Point = {x: Float , y: Float}


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch 
        [Events.onAnimationFrameDelta TimeDelta
        ,Events.onKeyUp (Decode.map (KeyChanged False) (Decode.field "key" Decode.string))
        , Events.onKeyDown (Decode.map (KeyChanged True) (Decode.field "key" Decode.string))
        , Events.onMouseMove (Decode.map2 (\x y -> MouseMovement (Point x y)) (Decode.field "movementX" Decode.float) (Decode.field "movementY" Decode.float)) 
        ]


-- VIEW


view : Model -> Html Msg
view model =
        let 
            debug_info =  [input [ type_ "number",  placeholder "Scene speed" , value (String.fromFloat model.scene_speed), onInput ChangeRotationSpeed] []
                        ,input [ type_ "number", placeholder "Triangle count" , value (String.fromInt model.triangle_count), onInput ChangeTriangleCount] []
                        , p [ style "color" "white"][ text (String.concat ["fps:" ,String.fromInt (round (1000/model.frame_time)), " , frame time (ms):", String.fromInt (round (model.frame_time)) ])]
                        , p [ style "color" "white"] [ text (String.concat ["camera coords:", (vec_to_string model.camera_coordinates) ,  "camera dir:" , (vec_to_string (get_camera_dir model.camera_pitch model.camera_yaw))])]
                        ]
        in 
            
        div [  on "wheel" (Decode.map (\v -> MouseScroll ( v > 0)) (Decode.field "wheelDelta" Decode.int)) , style "background-color" "black"] 
            [
            div [style "z-index" "1", style "position" "absolute"] (if model.show_debug_info then debug_info else [])

            , WebGL.toHtml
                [ id "mainCanvas" , width 1920, height 1080, style "display" "table", style "width" "100%", style "height" "100%", style "background-color" "black" ,style "position" "absolute", style "top" "0"
                ]
                (show_scene model.camera_coordinates model.camera_pitch model.camera_yaw model.fov model.scene)
            ]


