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
import Html.Attributes exposing (width, height, style, value, placeholder)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL
import Utils exposing (pyramid_cube_mesh, Vertex)
import Math.Matrix4 exposing (mul)


-- MAIN


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
        ,rotation_speed : Float 
        ,coordinates : Vec3
        ,velocity : Vec3
    }



init : () -> (Model, Cmd Msg)
init () =
    let 
        initial_velocity = vec3 0.001 0.001 0.001
        initial_coordinate = vec3 0 0 0 
        start_rotation_speed = 10
        start_angle = 0
    in 
        ( {rotation_speed = start_rotation_speed ,angle = start_angle,  coordinates = initial_coordinate , velocity= initial_velocity},  Cmd.none )



-- UPDATE


type Msg
    = TimeDelta Float | ChangeRotationSpeed String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        TimeDelta delta ->
                let
                    coordinates = update_coordinates model.coordinates ( Vec3.scale delta model.velocity)
                    velocity = update_velocity model.velocity coordinates
                in 
                    ({ model | angle = model.angle + delta  * (model.rotation_speed / 5000) , coordinates = coordinates , velocity = velocity }, Cmd.none )
        ChangeRotationSpeed newSpeed ->
            case String.toFloat newSpeed of 
            Nothing ->
                (model, Cmd.none)
            Just speed ->
                ( {model | rotation_speed= speed}, Cmd.none ) 



coordinate_bound : Float
coordinate_bound = 1 
update_coordinates : Vec3 -> Vec3 -> Vec3 
update_coordinates coord offset = 
    let 
        sum = Vec3.add coord offset 
    in 
        vec3 (clamp -coordinate_bound coordinate_bound (Vec3.getX sum)) ( clamp -coordinate_bound coordinate_bound (Vec3.getY sum)) (clamp -coordinate_bound coordinate_bound (Vec3.getZ sum))  

update_velocity : Vec3 -> Vec3 -> Vec3 
update_velocity velocity coord = 
    let 
        x = abs (Vec3.getX coord) ==  coordinate_bound 
        y = abs (Vec3.getY coord) ==  coordinate_bound
        z = abs (Vec3.getZ coord) ==  coordinate_bound
        mult_vector = vec3 ( if x then -1 else 1) ( if y then -1 else 1) ( if z then -1 else 1) 
    in 
        multiply_vec3_fields velocity mult_vector

        


multiply_vec3_fields : Vec3 -> Vec3 -> Vec3
multiply_vec3_fields a b =
    vec3 (( Vec3.getX a) * ( Vec3.getX b)) ((Vec3.getY a) * (Vec3.getY b)) ((Vec3.getZ a) * (Vec3.getZ b))
            



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Events.onAnimationFrameDelta TimeDelta



-- VIEW


view : Model -> Html Msg
view model =
    let 
        uniforms = create_uniforms model.angle 
    in 
        div [] 
            [WebGL.toHtml
                [ width 1080, height 720, style "display" "block", style "width" "90%", style "height" "90%"
                ]
                [ show_mesh (pyramid_cube_mesh model.coordinates 0.5) uniforms 
                ]
            , input [ placeholder "Rotation speed" , value (String.fromFloat model.rotation_speed), onInput ChangeRotationSpeed] []
            ]


show_mesh : WebGL.Mesh Vertex -> Uniforms  -> WebGL.Entity
show_mesh mesh uniforms = 
    WebGL.entity vertexShader fragmentShader mesh uniforms



type alias Uniforms =
    { rotation : Mat4
    , perspective : Mat4
    , camera : Mat4
    }


create_uniforms : Float -> Uniforms
create_uniforms angle =
    { rotation =
        Mat4.mul
        (Mat4.makeRotate (3 * angle) (vec3 0 1 0))
        (Mat4.makeRotate (2 * angle) (vec3 1 0 0))
        , perspective = Mat4.makePerspective 45 1 0.01 100
        , camera = Mat4.makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)
    }




-- SHADERS


vertexShader : WebGL.Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;
        varying vec3 vcolor;
        void main () {
            gl_Position = perspective * camera * rotation * vec4(position, 1.0);
            vcolor = color;
        }
    |]


fragmentShader : WebGL.Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = 0.8 * vec4(vcolor, 1.0);
        }
    |]
