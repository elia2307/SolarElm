module Person exposing (..)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)

import Dict exposing (keys)
import Shaders exposing (get_camera_dir)


type alias Keys =
    { up : Bool
    , left : Bool
    , down : Bool
    , right : Bool
    , space : Bool
    , ctrl : Bool
    , shift : Bool
    }
no_keys : Keys
no_keys =
    Keys False False False False False False False 

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
        "Shift" -> {keys | shift = isDown}
        --_ -> let _ = Debug.log "key:" key in keys
        _ -> keys
 

update_camera_coordinates : Keys -> Vec3 -> Float -> Float -> Vec3 
update_camera_coordinates keys coordinates pitch yaw=
    -- make relative to camera direction 
    let 
        x_diff = (if keys.left  then -1 else 0) + (if keys.right then 1 else 0)
        z_diff = (if keys.down then  -1 else 0 ) + (if keys.up then 1 else 0)
        y_diff = (if keys.ctrl then -1 else 0) + (if keys.space then 1 else 0)
        y_vidff = vec3 0 y_diff 0
        camera_dir = get_camera_dir pitch yaw 
        camera_cross = Vec3.cross camera_dir (vec3 0 1 0)
        
        z_vdiff = if z_diff < 0 then (Vec3.scale -1 camera_dir) else if z_diff > 0 then camera_dir else (vec3 0 0 0)
        x_vdiff = if x_diff < 0 then (Vec3.scale -1 camera_cross) else if x_diff > 0 then camera_cross else (vec3 0 0 0)
        
        res = Vec3.add x_vdiff (Vec3.add y_vidff z_vdiff) 
        
        
    in 
    Vec3.add res coordinates
    
    
update_camera_angle : Float -> Float -> Float-> Float 
update_camera_angle angle movement limit = 
    let 
        angle_unit = 0.005 
        angle_diff = angle_unit * movement 
    in 
    if limit == -1 then 
        angle + angle_diff
    else 
        clamp  -limit limit (angle + angle_diff)




