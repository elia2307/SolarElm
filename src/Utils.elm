module Utils exposing (..)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)


average_vecs : List(Vec3) -> Vec3
average_vecs vecs = 
    if List.isEmpty vecs then 
        vec3 0 0 0
    else
        Vec3.scale (1 / (toFloat (List.length vecs))) (List.foldl Vec3.add (vec3 0 0 0) vecs)
    



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

coordinate_bound : Float
coordinate_bound = 2
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
            


