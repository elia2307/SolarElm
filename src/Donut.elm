module Donut exposing (..)

import Browser
import Math.Vector3 exposing (vec3)

import Scene exposing (initialise_donut_scene)

import Main exposing (Model, Msg, update, subscriptions, view)
import Person exposing (no_keys)




main : Program() Model Msg
main =
    Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }





init : () -> (Model, Cmd Msg)
init () =
    let 
        initial_coordinate = vec3 1 1 20 
        start_scene_speed = 5
        default_triangle_count = 100
    in 
        ( { show_debug_info = False , fov= 45, frame_time = 0.01, scene_speed = start_scene_speed ,camera_coordinates = initial_coordinate , camera_pitch =0, camera_yaw = -90, triangle_count=default_triangle_count, scene= (initialise_donut_scene default_triangle_count) , keys = no_keys} ,  Cmd.none )







