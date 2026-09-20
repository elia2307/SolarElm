module Matrix exposing (..)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (transform)



type alias Vertex =
    { color : Vec3
    , position : Vec3
    }


type alias Mat3 = { c0: Vec3 , c1:Vec3, c2:Vec3}

create_mat3 : (Vec3,Vec3,Vec3) -> Mat3 
create_mat3 vecs = 
    let 
        (a,b,c) = vecs
    in
    Mat3 a b c 


create_mat3_from_vertexes: (Vertex,Vertex,Vertex) -> Mat3
create_mat3_from_vertexes vertexes = 
    let 
        (a,b,c) = vertexes
    in 
    Mat3 a.position b.position c.position 

create_mat3_from_tuple : (Vec3,Vec3,Vec3) -> Mat3
create_mat3_from_tuple points = 
    let 
        (a,b,c) = points
    in 
    Mat3 a b c 


-- Matrix in form 
-- [ a,b,c
--  d,e,f
-- g,h,i]
getA : Mat3 -> Float
getA mat = Vec3.getX mat.c0

getB : Mat3 -> Float
getB mat = Vec3.getX mat.c1

getC : Mat3 -> Float
getC mat = Vec3.getX mat.c2

getD : Mat3 -> Float
getD mat = Vec3.getY mat.c0

getE : Mat3 -> Float
getE mat = Vec3.getY mat.c1

getF : Mat3 -> Float
getF mat = Vec3.getY mat.c2

getG : Mat3 -> Float
getG mat = Vec3.getZ mat.c0

getH: Mat3 -> Float
getH mat = Vec3.getZ mat.c1

getI : Mat3 -> Float
getI mat = Vec3.getZ mat.c2


mat3_to_string : Mat3 -> String 
mat3_to_string mat = 
    let 
        l1 = String.concat [(String.fromFloat (getA mat)) , "," , (String.fromFloat (getB mat)) , "," , (String.fromFloat (getC mat))]
        l2 = String.concat [(String.fromFloat (getD mat)) , "," , (String.fromFloat (getE mat)) , "," , (String.fromFloat (getF mat))]
        l3 = String.concat [(String.fromFloat (getG mat)) , "," , (String.fromFloat (getH mat)) , "," , (String.fromFloat (getI mat))]
    in
    String.concat [l1 ,"\n" ,  l2 , "\n" , l3 ]

mat3_apply_scalar: Float -> Mat3 -> Mat3 
mat3_apply_scalar scalar mat = Mat3 (Vec3.scale scalar mat.c0) (Vec3.scale scalar mat.c1) (Vec3.scale scalar mat.c2)

vec3_mul_mat3 : Vec3 -> Mat3 -> Vec3
vec3_mul_mat3 point transform= 
    let 
        p0 = (getA transform) * (Vec3.getX point) + (getB transform) * (Vec3.getY point) + (getC transform) * (Vec3.getZ point)
        p1 = (getD transform) * (Vec3.getX point) + (getE transform) * (Vec3.getY point) + (getF transform) * (Vec3.getZ point)
        p2 = (getG transform) * (Vec3.getX point) + (getH transform) * (Vec3.getY point) + (getI transform) * (Vec3.getZ point)
    in 
    vec3 p0 p1 p2 

mat3_mul_mat3: Mat3 -> Mat3 -> Mat3
mat3_mul_mat3 m1 m2 = 
    let 
        --_ = Debug.log "multiply 2 matrixes:" (mat3_to_string m1)

        --_ = Debug.log "and:" (mat3_to_string m2)
        transposeM1 = mat3_transpose m1 
        --transposing allows easy fetching of rows from matrix 
        --transposeM1 = m1
        row1 = vec3 (Vec3.dot transposeM1.c0 m2.c0) (Vec3.dot transposeM1.c0 m2.c1) (Vec3.dot transposeM1.c0 m2.c2)
        row2 = vec3 (Vec3.dot transposeM1.c1 m2.c0) (Vec3.dot transposeM1.c1 m2.c1) (Vec3.dot transposeM1.c1 m2.c2)
        row3 = vec3 (Vec3.dot transposeM1.c2 m2.c0) (Vec3.dot transposeM1.c2 m2.c1) (Vec3.dot transposeM1.c2 m2.c2)
        col1 = vec3 (Vec3.getX row1) (Vec3.getX row2) (Vec3.getX row3)
        col2 = vec3 (Vec3.getY row1) (Vec3.getY row2) (Vec3.getY row3)
        col3 = vec3 (Vec3.getZ row1) (Vec3.getZ row2) (Vec3.getZ row3)
        --_ = Debug.log "output:" (mat3_to_string (Mat3 col1 col2 col3)) 



    in 
    --Mat3 ( vec3_mul_mat3 transposeM1.c0 m2) (vec3_mul_mat3 transposeM1.c1 m2) (vec3_mul_mat3 transposeM1.c2 m2) 
    Mat3 col1 col2 col3


mat3_transpose : Mat3 -> Mat3 
mat3_transpose mat = 
    let 
        c1 = vec3 (getA mat) (getB mat) (getC mat)
        c2 = vec3 (getD mat) (getE mat) (getF mat)
        c3 = vec3 (getG mat) (getH mat) (getI mat)
    in 
    Mat3 c1 c2 c3 


vec3_apply_translaction : Vec3 -> Vec3 -> Vec3 
vec3_apply_translaction translation point = Vec3.add translation point

mat3_apply_translation : Vec3 -> Mat3 -> Mat3
mat3_apply_translation translaction mat = Mat3 (vec3_apply_translaction translaction mat.c0)  (vec3_apply_translaction translaction mat.c2) (vec3_apply_translaction translaction mat.c1) 


calculate_determinant3 : Mat3 -> Float 
calculate_determinant3 mat = 
    let 
        ei = (getE mat) * (getI mat)
        fh = (getF mat) * (getH mat)
        di = (getD mat) * (getI mat)
        gf = (getG mat) * (getF mat)
        dh = (getD mat) * (getH mat)
        eg = (getE mat) * (getG mat)
    in 
    ((getA mat) * (ei - fh)) - ((getB mat) * (di - gf)) + ((getC mat) * (dh - eg))


            

get_adjoint_matrix3 : Mat3 -> Mat3
get_adjoint_matrix3 mat = 
    let 
        c1 = vec3 
                ((getE mat) * (getI mat) - (getF mat) * (getH mat)) 
                ((getF mat) * (getG mat) - (getD mat) * (getI mat))
                ((getD mat) * (getH mat) - (getE mat) * (getG mat))
        c2 = vec3 
                ((getC mat) * (getH mat) - (getB mat) * (getI mat))
                ((getA mat) * (getI mat) - (getC mat) * (getG mat))
                ((getB mat) * (getG mat) - (getA mat) * (getH mat))

        c3 = vec3 
                ((getB mat) * (getF mat) - (getC mat) * (getE mat)) 
                ((getC mat) * (getD mat) - (getA mat) * (getF mat))
                ((getA mat) * (getE mat) - (getB mat) * (getD mat))

    in
        create_mat3 (c1, c2, c3) 


invert_mat3 : Mat3 ->  Maybe Mat3
invert_mat3 mat = 
    let 
        det = calculate_determinant3(mat)
    in 
    if det == 0 then 
        Nothing
    else
        Just (mat3_apply_scalar (1 / det) (get_adjoint_matrix3 mat))






find_valid_start_translation : Mat3 -> Vec3 -> Vec3 
find_valid_start_translation mat  start_transform = 
    if (calculate_determinant3 (mat3_apply_translation start_transform mat)) == 0 then 
        let 
            new_start = vec3 ( Vec3.getZ start_transform  + 1) ( Vec3.getX start_transform  + 2)  ( Vec3.getY start_transform + 3) 
        in 
        find_valid_start_translation mat new_start  
    else
        start_transform

find_transform_for_triangles : Mat3 -> Mat3 -> Result String ( Vec3,Mat3) 
find_transform_for_triangles t1 t2 = 
    let 
        start_transform = find_valid_start_translation t1 (vec3 0 0 0)
        translatedT1 = mat3_apply_translation start_transform t1
        equations = mat3_transpose translatedT1 
        inv =invert_mat3 equations
        
    in 
    case inv  of 
        Nothing -> Err "Cannot find transform to map t1 to t2"  
        Just inverse -> 
            let 
                affine = mat3_transpose (mat3_mul_mat3 inverse (mat3_transpose t2)) 
            in 
                Ok (start_transform, affine)


apply_affine_transform_to_vertex : Mat3 -> Vertex -> Vertex
apply_affine_transform_to_vertex transform vertex= 
    let 
        npos = vec3_mul_mat3 vertex.position transform 
    in 
    Vertex vertex.color npos 
    

apply_affine_transform_to_triangle_vertexes : Mat3 -> (Vertex,Vertex,Vertex) -> (Vertex,Vertex,Vertex)
apply_affine_transform_to_triangle_vertexes transform triangle= 
    let 
        (a,b,c) = triangle

    in 
    ((apply_affine_transform_to_vertex transform a), (apply_affine_transform_to_vertex transform b), (apply_affine_transform_to_vertex transform c))
    


apply_translation_to_vertex: Vec3 -> Vertex -> Vertex
apply_translation_to_vertex translation vertex = 
    let 
        npos = Vec3.add vertex.position translation
    in 
        Vertex vertex.color npos

apply_translation_to_triangle_vertexes : Vec3 -> (Vertex,Vertex,Vertex) -> (Vertex,Vertex,Vertex)
apply_translation_to_triangle_vertexes translation triangle= 
    let 
        (a,b,c ) = triangle 
    in 
    ((apply_translation_to_vertex translation a), (apply_translation_to_vertex translation b), (apply_translation_to_vertex translation c))
