module Sphere exposing (..)

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
import Math.Matrix4 as Mat4 exposing (Mat4)
import Array exposing (Array)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL
import Meshes exposing (sphere_mesh)
import Matrix exposing (Vertex)
import Shaders exposing (show_mesh, create_uniforms, create_global_transform_matrix)
import Utils exposing (update_coordinates, update_velocity)
import Html.Attributes exposing (coords)
import Meshes exposing (pyramid_cube_mesh)
import Array exposing (Array)
import Random

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


type alias Object_data = 
    {   mesh : WebGL.Mesh Vertex
    , coordinates : Vec3
    , rotation : Vec3
    , velocity : Vec3
    , rotation_spin_velocity: Vec3
    }

update_object_mesh : Object_data -> WebGL.Mesh Vertex -> Object_data
update_object_mesh object mesh = 
    { object | mesh = mesh}  

update_object_coordinates : Object_data -> Vec3 -> Object_data
update_object_coordinates object coords = { object | coordinates = coords}

update_object_velocity : Object_data -> Vec3 -> Object_data
update_object_velocity object vel = { object | velocity = vel}


update_object_movement : Float -> Object_data -> Object_data 
update_object_movement delta obj =  

        let 

                coords = update_coordinates obj.coordinates (Vec3.scale delta obj.velocity)
                velocity = update_velocity obj.velocity coords
                rotation = Vec3.add obj.rotation (Vec3.scale delta obj.rotation_spin_velocity) 

        in 
        { obj | coordinates = coords, velocity = velocity, rotation=rotation}  


show_object : Object_data -> WebGL.Entity 
show_object obj = 
    let 
        global_transform = create_global_transform_matrix obj.coordinates obj.rotation 
        uniforms = create_uniforms 0 global_transform
    in 
    show_mesh obj.mesh uniforms

type alias Scene_Objects = {objects : Array(Object_data) }

fmodBy : Int -> Float   -> Float
fmodBy  m x =
    let
        r = x - (toFloat (floor x))
    in 
    if (floor x) == 0 then 
        r 
    else 
        (toFloat (modBy m (floor x))) + r
    

fix_nan : Float -> Float -> Float 
fix_nan num fallback = 
    if isNaN num then 
        (fmodBy 2 fallback)
    else 
        num

generate_random_sphere : Float -> Object_data
generate_random_sphere n= 
    let 
        p = 7753757725325377
        mod = modBy p ((floor ((n+133) * 125959)) * 2245849783) 
        seed = ((-1) ^ n) * e * (sqrt (toFloat mod)) 
        --x = fmodBy (floor (logBase 2 (sqrt (e ^ (seed * seed))))) ( (logBase 150 (seed*seed ^ (1/e))) * 1.32492853491 + (seed / 23)) 
        --y = fmodBy 2 (logBase 1250 ( (e ^ seed) * 0.015)) 
        --z = fmodBy 2 ((seed * 1.213) - 12)
        x =  fix_nan ((fmodBy 17 (seed * seed * 9)) /10)  (n * 0.312)
        y = fix_nan ((fmodBy 16 (seed * seed * seed * (sqrt seed))) / 10) (n * 0.123)
        
        z = fix_nan (fmodBy 3 (logBase 10 seed)) (n * 0.345)
        coords = vec3 x y z 

        --velocity = vec3 (0.00025*z + 0.000002 * (fmodBy 100 (n+163))) (-0.00012439*x - 0.000005 * (fmodBy 100 (n+345)) + 0.000001) (0.0001 * y + 0.0000009 * (fmodBy 100 (n+136)))  
        velocity = vec3 (0.00025 * x + 0.00010 * y + 0.00009 * z + 0.00003) ( 0.00009 * x + 0.00013 * y + 0.0000913423 *z + 0.00003) (0.0001 * (x+y+z) + 0.00003)
        rotation = vec3  (0.001*x) (0.001*y) (0.001*z) 
        mesh = sphere_mesh (vec3 0 0 0) 0.1 250
        rotation_spin_velocity = vec3 (0.0001 * x) (0.0001 * y ) (0.0001 * z)
    in 
    Object_data mesh coords rotation velocity rotation_spin_velocity


generate_random_spheres : Int -> List(Object_data)
generate_random_spheres number =
    if number <= 0 then
        []
    else 
        List.append (generate_random_spheres (number - 1)) [generate_random_sphere (toFloat (number+1))]
    


initialise_scene : Int -> Scene_Objects
initialise_scene sphere_triangle_count = 
    let 
        sphere = { mesh = (sphere_mesh (vec3 0 0 0) 1 sphere_triangle_count), coordinates =  (vec3 0 0 0), rotation =  (vec3 0 0.1 0), 
            velocity = (vec3 0.0001 0.0001 0.0001), 
            rotation_spin_velocity =  (vec3 0.0 0.0005 0.00005)}
        dia = { mesh = ( pyramid_cube_mesh (vec3 0 0 0) 0.5) , coordinates = ( vec3 0 0 -2) , rotation = ( vec3 -0.1 1 0), velocity = (vec3 -0.0001 0.001 -0.001),rotation_spin_velocity = (vec3 0.0005 0 -0.0005)}
        randoms = generate_random_spheres 1000
    in 
    Scene_Objects (Array.fromList (List.concat [[sphere, dia], randoms])) 

update_scene_sphere : Scene_Objects -> Object_data -> Scene_Objects
update_scene_sphere scene sphere = { objects = (Array.set 0 sphere scene.objects)}  


update_scene_movement : Scene_Objects -> Float -> Scene_Objects 
update_scene_movement scene delta = 
    Scene_Objects (Array.map (update_object_movement delta) scene.objects)
    --Scene_Objects (update_object_movement scene.sphere delta) (update_object_movement scene.dia delta)  


show_scene  :  Scene_Objects -> List(WebGL.Entity) 
show_scene scene =Array.toList (Array.map show_object scene.objects)  


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
            input [ type_ "number",  placeholder "Scene speed" , value (String.fromFloat model.scene_speed), onInput ChangeRotationSpeed] []
            ,input [ type_ "number", placeholder "Triangle count" , value (String.fromInt model.triangle_count), onInput ChangeTriangleCount] []
            , WebGL.toHtml
                [ width 2000, height 2000, style "display" "table", style "width" "700px", style "height" "700px", style "background-color" "black"
                ]
                (show_scene model.scene)
            ]


