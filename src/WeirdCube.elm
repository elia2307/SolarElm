module WeirdCube exposing (..)

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
    ( {rotation_speed = 10 ,angle = 0,  coordinates = (vec3 1 1 1) , velocity= (vec3 0.001 0.001 0.001)},  Cmd.none )



-- UPDATE


type Msg
    = TimeDelta Float | ChangeRotationSpeed String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        TimeDelta delta ->
                if not (is_cube_in_bounds model.coordinates) then 
                        ({model 
                        |angle = model.angle + delta * (model.rotation_speed / 5000)  
                        ,coordinates = (Vec3.add model.coordinates (Vec3.scale ( -1 * delta) model.velocity)) 
                        ,velocity = (Vec3.negate model.velocity)} , Cmd.none) 
                else
                     ({ model | angle = model.angle + delta  * (model.rotation_speed / 5000) , coordinates = (Vec3.add model.coordinates  ( Vec3.scale delta model.velocity )) }, Cmd.none )
        ChangeRotationSpeed newSpeed ->
            case String.toFloat newSpeed of 
            Nothing ->
                (model, Cmd.none)
            Just speed ->
                ( {model | rotation_speed= speed}, Cmd.none ) 



        
is_cube_in_bounds : Vec3 -> Bool 
is_cube_in_bounds point = 
    (abs (Vec3.getX point))  <= 1 && ( abs ( Vec3.getY point)) <= 1 && ( abs (Vec3.getZ point)) <= 1 

            
            



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
                [ width 1080, height 720, style "display" "block", style "width" "100%", style "height" "100%"
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
