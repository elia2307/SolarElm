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



create_3d_rotation_matrix_3steps : Float -> Float -> Float -> Mat4
create_3d_rotation_matrix_3steps roll pitch yaw= 
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
        c1 = vec3 (cosa * cosb) (sina * cosb) (-sinb)
        c2 = vec3 ((cosa * sinb * siny) - (sina * cosy)) ((sina * sinb * siny) + (cosa * cosy)) (cosb * sinb)
        c3 = vec3 ((cosa * sinb * cosy) + (sina * siny)) ((sina * sinb * cosy) - (cosa * siny)) (cosb * cosy)
        --_ = Debug.log "(makeBasis,3step)" ( (Mat4.makeBasis c1 c2 c3), (create_3d_rotation_matrix_3steps roll pitch yaw))
    in 
    Mat4.makeBasis c1 c2 c3
    --create_3d_rotation_matrix_3steps roll pitch yaw 


create_global_transform_matrix : Vec3 -> Vec3  -> Mat4
create_global_transform_matrix translation rotation=
    Mat4.mul (Mat4.makeTranslate translation) (create_3d_rotation_matrix_1step (Vec3.getX rotation) (Vec3.getY rotation) (Vec3.getZ rotation))
    


create_camera_uniform : Float -> Float -> Vec3 -> Mat4 
create_camera_uniform pitch yaw camera_coordinates = 
    let 
        yaw_d = degrees yaw
        pitch_d = degrees pitch
        --_ = Debug.log "coordinate,yaw,pitch:" (camera_coordinates,pitch,yaw)
        rotation_vec = vec3 ((Basics.cos yaw_d) * (Basics.cos pitch_d)) (Basics.sin pitch_d) ((Basics.sin yaw_d) * (Basics.cos pitch_d))
        looking_at = Vec3.add camera_coordinates rotation_vec
    in 
    Mat4.makeLookAt camera_coordinates looking_at (vec3 0 1 0)

create_uniforms : Mat4  -> Mat4 -> Uniforms
create_uniforms global_transform camera_uniform=
    { 
        perspective = Mat4.makePerspective 45 1 0.01 100
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
