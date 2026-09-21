module Shaders exposing (..)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL
import Meshes exposing (sphere_mesh, pyramid_cube_mesh)
import Matrix exposing (Vertex)



show_mesh : WebGL.Mesh Vertex -> Uniforms  -> WebGL.Entity
show_mesh mesh uniforms = 
    WebGL.entity vertexShader fragmentShader mesh uniforms



type alias Uniforms =
    { rotation : Mat4
    , perspective : Mat4
    , camera : Mat4
    , global_transform : Mat4
    }



create_3d_rotation_matrix : Float -> Float -> Float -> Mat4
create_3d_rotation_matrix roll pitch yaw= 
    let 
        rx = Mat4.makeRotate (roll) (vec3 1 0 0)
        ry = Mat4.makeRotate (pitch) (vec3 0 1 0)
        rz = Mat4.makeRotate (yaw) (vec3 0 0 1)
    in 
    Mat4.mul (Mat4.mul rz ry) rx 
        

create_global_transform_matrix : Vec3 -> Vec3  -> Mat4
create_global_transform_matrix translation rotation=
    Mat4.mul (Mat4.makeTranslate translation) (create_3d_rotation_matrix (Vec3.getX rotation) (Vec3.getY rotation) (Vec3.getZ rotation))
    

create_uniforms : Float -> Mat4  -> Uniforms
create_uniforms angle global_transform =
    { rotation =
        Mat4.mul
        (Mat4.makeRotate (3 * angle) (vec3 0 1 0))
        (Mat4.makeRotate (2 * angle) (vec3 1 0 0))
        , perspective = Mat4.makePerspective 45 1 0.01 100
        , camera = Mat4.makeLookAt (vec3 0 0 9) (vec3 0 0 0) (vec3 0 1 0)
        , global_transform = global_transform
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
        uniform mat4 global_transform;
        void main () {
            gl_Position = perspective * camera * rotation * global_transform * vec4((position), 1.0);
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
