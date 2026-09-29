module DebugUtils exposing (..)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)


print_vec : Vec3 -> Int 
print_vec v = 
    let 
        _ =  Debug.log "x:" (String.fromFloat (Vec3.getX v)) 
        _ = Debug.log "y:" (String.fromFloat (Vec3.getY v)) 
        _ = Debug.log "z:" (String.fromFloat (Vec3.getZ v))
    in 
        0

print_text : String -> Int 
print_text str = 
    let 
        _ = Debug.log "text:" str
    in 
    0

            


