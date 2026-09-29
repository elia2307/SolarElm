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
    {
    perspective : Mat4
    , camera : Mat4
    , global_transform : Mat4
    }



create_3d_rotation_matrix_3step : Float -> Float -> Float -> Mat4
create_3d_rotation_matrix_3step roll pitch yaw= 
    let 
        rx = Mat4.makeRotate (roll) (vec3 1 0 0)
        ry = Mat4.makeRotate (pitch) (vec3 0 1 0)
        rz = Mat4.makeRotate (yaw) (vec3 0 0 1)
    in 
    Mat4.mul (Mat4.mul rz ry) rx 

-- formula from https://en.wikipedia.org/wiki/Rotation_matrix
create_3d_rotation_matrix_1step: Float ->  Float -> Float -> Mat4 
create_3d_rotation_matrix_1step roll pitch yaw = 
    let 
        cosa = cos yaw
        sina = sin yaw
        cosb = cos pitch
        sinb = sin pitch
        cosy = cos roll
        siny = sin roll
        c1 = vec3 (cosa * cosb) 
            ((cosa * sinb * siny) - (sina * cosy)) 
            ((cosa * sinb * cosy) + (sina * siny))
        c2 = vec3 (sina * cosb) ((sina * sinb * siny) + (cosa * cosy)) ((sina * sinb * cosy) - (cosa * siny))
        c3 = vec3 -sinb (cosb * siny) (cosb * cosy)
    in 
    Mat4.makeBasis c1 c2 c3


create_global_transform_matrix : Vec3 -> Vec3  -> Mat4
create_global_transform_matrix translation rotation =
    Mat4.mul (Mat4.makeTranslate translation) (create_3d_rotation_matrix_1step (Vec3.getX rotation) (Vec3.getY rotation) (Vec3.getZ rotation)) 


get_camera_dir : Float -> Float -> Vec3 
get_camera_dir pitch yaw = 
    vec3 (( cos yaw) * (cos pitch)) (sin pitch) ((sin yaw) * (cos pitch))



create_camera_uniform : Float -> Float -> Vec3 -> Mat4 
create_camera_uniform pitch yaw camera_coordinates= 
    let 
        camera_dir = vec3 ((Basics.cos yaw) * (Basics.cos pitch)) (Basics.sin pitch) ((Basics.sin yaw) * (Basics.cos pitch))
        looking_at = Vec3.add camera_coordinates camera_dir
    in 
    Mat4.makeLookAt camera_coordinates looking_at Vec3.j

create_perspective_matrix : Float -> Mat4 
-- fov, aspect ratio , znear , zfar was (1 0.01 100) 
create_perspective_matrix fov = Mat4.makePerspective fov (16/9) 0.01 100 


create_uniforms : Mat4  -> Mat4 -> Mat4 -> Uniforms
create_uniforms global_transform camera_uniform perspective_uniform=
    { 
        perspective = perspective_uniform
        , camera = camera_uniform
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
        varying vec3 vcolor;
        uniform mat4 global_transform;
        void main () {
            gl_Position = perspective * camera * global_transform * vec4((position), 1.0);
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
