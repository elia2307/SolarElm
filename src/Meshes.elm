module Meshes exposing (..)
import Utils exposing (print_text)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL
import Matrix exposing (find_transform_for_triangles, Mat3, create_mat3_from_tuple, apply_translation_to_triangle_vertexes, apply_affine_transform_to_triangle_vertexes, Vertex)
import Matrix exposing (mat3_to_string)


average_vecs : List(Vec3) -> Vec3
average_vecs vecs = 
    if List.isEmpty vecs then 
        vec3 0 0 0
    else
        Vec3.scale (1 / (toFloat (List.length vecs))) (List.foldl Vec3.add (vec3 0 0 0) vecs)
    


pyramid_mesh  : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List(Vertex,Vertex,Vertex)
--e is peak point while a,b,c,d are base points
pyramid_mesh a b c d dir= 
    let 
        colora = vec3 1 0 0
        colorb = vec3 0 1 0
        colorc = vec3 0 0 1
        colord = vec3 1 0 1
        colore = vec3 0.9 0.9 0.9
        e = Vec3.add ( average_vecs  [a,b,c,d]) dir   
        --_ = print_vec e
    in 
        [   (Vertex colora a
            ,Vertex colorb b
            ,Vertex colore e)
        ,   (Vertex colorb b
            ,Vertex colorc c
            ,Vertex colore e)
        ,   (Vertex colord d
            ,Vertex colorc c
            ,Vertex colore e)
         ,   (Vertex colord d 
            ,Vertex colora a
            ,Vertex colore e)
        ]  





-- centre x, centre y , centre z , 1/2 side length of cube 
cube_mesh: Float -> Float -> Float -> Float -> List(Vertex, Vertex, Vertex)
cube_mesh x y z  size= 
  let
    rft = vec3 (x + size) (y + size) (z + size)
    lft = vec3 (x - size) (y + size) (z + size)
    lbt = vec3 (x - size) (y - size) (z + size)
    rbt = vec3 (x + size) (y - size) (z + size)
    rbb = vec3 (x + size) (y - size) (z - size)
    rfb = vec3 (x + size) (y + size) (z - size)
    lfb = vec3 (x - size) (y + size) (z - size)
    lbb = vec3 (x - size) (y - size) (z - size)
  in
   List.concat [ face (vec3 115 210 22 ) rft rfb rbb rbt -- green
    , face (vec3 52  101 164) rft rfb lfb lft -- blue
    , face (vec3 237 212 0  ) rft lft lbt rbt -- yellow
    , face (vec3 204 0   0  ) rfb lfb lbb rbb -- red
    , face (vec3 117 80  123) lft lfb lbb lbt -- purple
    , face (vec3 245 121 0  ) rbt rbb lbb lbt -- orange
    ]






face : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Vertex, Vertex, Vertex )
face color a b c d =
    let
        vertex position =
            Vertex (Vec3.scale (1 / 255) color) position
    in
    [ ( vertex a, vertex b, vertex c )
    , ( vertex c, vertex d, vertex a )
    ]


triangles_mesh : WebGL.Mesh Vertex
triangles_mesh = 
    WebGL.triangles 
    [ ( Vertex ( vec3 0.5 0.5 0.5) (vec3 0.5 0.5 0.5)
        , Vertex (vec3 1 0.5 0.5) (vec3 -0.5 -0.5 0.5)
        , Vertex (vec3 0.5 0.5 1) (vec3 0 0 1)
        ) 
    , ( Vertex ( vec3 0.5 0.5 0.5) (vec3 0.5 0.5 -0.5)
        , Vertex (vec3 1 0.5 0.5) (vec3 -0.5 -0.5 -0.5)
        , Vertex (vec3 0.5 0.5 1) (vec3 0 0 -1)
        )
    
    ,( Vertex ( vec3 0.5 0.5 0.5) (vec3 -0.5 0.5 0.5)
        , Vertex (vec3 1 0.5 0.5) (vec3 -0.5 -0.5 -0.5)
        , Vertex (vec3 0.5 0.5 1) (vec3 -1 0 0)
        ) 
    , ( Vertex ( vec3 0.5 0.5 0.5) (vec3 0.5 0.5 0.5)
        , Vertex (vec3 1 0.5 0.5) (vec3 0.5 -0.5 -0.5)
        , Vertex (vec3 0.5 0.5 1) (vec3 1 0 0)
        ) 
    ,( Vertex ( vec3 0.5 0.5 0.5) (vec3 0.5 -0.5 0.5)
        , Vertex (vec3 1 0.5 0.5) (vec3 -0.5 -0.5 -0.5)
        , Vertex (vec3 0.5 0.5 1) (vec3 0 -1 0)
        ) 
    , ( Vertex ( vec3 0.5 0.5 0.5) (vec3 0.5 0.5 0.5)
        , Vertex (vec3 1 0.5 0.5) (vec3 -0.5 0.5 -0.5)
        , Vertex (vec3 0.5 0.5 1) (vec3 0 1 0)
        ) 
    ]


pyramid_cube_mesh: Vec3 -> Float -> WebGL.Mesh Vertex
pyramid_cube_mesh centrePoint size = 
    let 
        x = Vec3.getX centrePoint
        y = Vec3.getY centrePoint
        z = Vec3.getZ centrePoint
        rft = vec3 (x + size) (y + size) (z + size)
        lft = vec3 (x - size) (y + size) (z + size)
        lbt = vec3 (x - size) (y - size) (z + size)
        rbt = vec3 (x + size) (y - size) (z + size)
        rbb = vec3 (x + size) (y - size) (z - size)
        rfb = vec3 (x + size) (y + size) (z - size)
        lfb = vec3 (x - size) (y + size) (z - size)
        lbb = vec3 (x - size) (y - size) (z - size)
        sizeP = size * 2
    in 
        WebGL.triangles <| List.concat <|
            [
                cube_mesh x y z size
                ,pyramid_mesh rft rfb lfb lft (vec3 0 sizeP 0)
                , pyramid_mesh rft lft lbt rbt  (vec3 0 0 sizeP)
                , pyramid_mesh rfb lfb lbb rbb (vec3 0 0 -sizeP) 
                , pyramid_mesh lft lfb lbb lbt  (vec3 -sizeP 0 0)
                , pyramid_mesh  rbt rbb lbb lbt  (vec3 0 -sizeP 0) 
                , pyramid_mesh rft rfb rbb rbt  (vec3 sizeP 0 0)
                
            ]


draw_n_triangles : Float -> Float -> Float -> Float -> List(Vertex,Vertex,Vertex)
draw_n_triangles start_x start_y triangle_length end_x =
    let 
        t1 = ( Vertex (vec3 0.25 0.6 0) (vec3 start_x start_y 0) 
            , Vertex (vec3 0 0 1) (vec3 (start_x + triangle_length) start_y 0)
            ,Vertex (vec3 0 1 0) (vec3 (start_x + (triangle_length * 0.5)) (start_y + triangle_length) 0)
            )
    in
    if start_x + triangle_length >= (end_x - 0.01) then 
        [t1]
    else
        let 
            t2 = ( Vertex (vec3 0.25 0.75 1) (vec3 (start_x + (1.5 * triangle_length)) (start_y + triangle_length) 0) 
                , Vertex (vec3 0 0.5 1) (vec3 (start_x + triangle_length) start_y 0)
                ,Vertex (vec3 1 1 0) (vec3 (start_x + (triangle_length * 0.5)) (start_y + triangle_length) 0)
                )

        in 
        List.append [t1 , t2] (draw_n_triangles (start_x + triangle_length) start_y triangle_length end_x)
    

-- draws a triangle with points from (2,2,3),(3,4,3), (2,4,3)
iteratively_draw_triangles : Int -> Int -> Float -> List(Vertex,Vertex,Vertex)
iteratively_draw_triangles step max_steps triangle_length =
    if step >= max_steps then 
        []
    else
        let 
            start_x = -1 + triangle_length *  (toFloat step) / 2    
            start_y = -1  + (toFloat step) *  triangle_length
            end_x = 1 - (triangle_length * (toFloat step) ) /2 
            current_iteration = draw_n_triangles start_x start_y triangle_length end_x
        in 
        List.append current_iteration (iteratively_draw_triangles (step+1) max_steps triangle_length)
    

draw_triangles: Int -> List(Vertex,Vertex,Vertex)
draw_triangles number_triangles = 
    let 
        
        root_n =  max 1  (floor (sqrt (toFloat number_triangles)))
        triangle_length = 2 / (toFloat root_n)
    in 
        iteratively_draw_triangles 0 root_n triangle_length  
--want to translate points after this function is finished (using a map maybe so instead of (-1,-1,1), (1,-1,1) , (0, 1,1) triangle can custom set each of three points  
-- but can keep this function simple 
-- so find translation that maps custom (c1,c2,c3) to (t1,t2,t3) same translation for each of three points then map all points using that translation

normalise_point : Vec3 -> Float ->  Vec3 -> Vec3
normalise_point centre radius point = 
    let 
        dx = (Vec3.getX point) - (Vec3.getX centre)
        dy =  (Vec3.getY point) - (Vec3.getY centre)
        dz =  (Vec3.getZ point) - (Vec3.getZ centre)
        distance =sqrt (  dx * dx + dy * dy + dz * dz) 
        ratio = radius / distance
    in 
    vec3 ( (ratio*dx) + (Vec3.getX centre)) ((ratio * dy)  + (Vec3.getY centre)) ( (ratio*dz) + (Vec3.getZ centre))

normalise_vertex : Vec3 -> Float -> (Vertex, Vertex, Vertex) -> (Vertex,Vertex,Vertex)
normalise_vertex centre radius vertex = 
    let 
        (v1,v2,v3) = vertex
        newV1 = Vertex v1.color (normalise_point centre radius v1.position) 
        newV2 = Vertex v2.color (normalise_point centre radius v2.position) 
        newV3 = Vertex v3.color (normalise_point centre radius v3.position) 
        
    in 
    (newV1, newV2, newV3)






normalise_points:  List(Vertex,Vertex,Vertex) -> Vec3 -> Float -> List(Vertex,Vertex,Vertex) 
normalise_points points centre_point radius = 
        List.map (normalise_vertex centre_point radius) points 



triangle_vertexes_to_vec : (Vertex,Vertex,Vertex) -> (Vec3,Vec3,Vec3)
triangle_vertexes_to_vec tri = 
    let 
        (a,b,c) = tri
    in 
    ( a.position, b.position, c.position)


map_triangles_from_to : (Vec3,Vec3,Vec3) ->  (Vec3,Vec3,Vec3) -> List(Vertex,Vertex,Vertex)-> List(Vertex,Vertex,Vertex) 
map_triangles_from_to start_tri end_tri triangle_vertexes= 
    let 
        res = find_transform_for_triangles (create_mat3_from_tuple start_tri) (create_mat3_from_tuple end_tri)
    in 
    case res of 
        Err str -> 
            let 
                _ = print_text str
            in 
            []
        Ok transforms -> 
            let 
                (translation, affine) = transforms
                --_ = Debug.log "for triangle from" start_tri
                --_ = Debug.log "for triangle to: " end_tri
                --_ = Debug.log "affine: " (mat3_to_string affine)
                --_ = Debug.log "translation:" translation
                translated = List.map (apply_translation_to_triangle_vertexes translation) triangle_vertexes
                transformed = List.map (apply_affine_transform_to_triangle_vertexes affine) translated
                
            in 
            transformed


sphere_mesh: Vec3 -> Float -> Int -> WebGL.Mesh Vertex 
sphere_mesh centrePoint radius no_triangles = WebGL.triangles <|List.concat <|
        let 
            default_triangle = draw_triangles (no_triangles//8)
            start_triangle = (( vec3 -1 -1 0), (vec3 1 -1 0), (vec3 0 1 0)) 
            _ = Debug.log "centrePoint:" centrePoint
            _ = Debug.log "radius:" radius
            north_pole = Vec3.add (vec3 0 radius 0) centrePoint 
            south_pole = Vec3.add (vec3 0 -radius 0) centrePoint
            east_pole = Vec3.add (vec3 radius 0 0) centrePoint
            west_pole = Vec3.add (vec3 -radius 0 0) centrePoint
            front_pole = Vec3.add (vec3 0 0 radius) centrePoint
            back_pole = Vec3.add (vec3 0 0 -radius) centrePoint
            poles = [north_pole,south_pole,east_pole,west_pole,front_pole,back_pole]
            _ = Debug.log "poles:" poles

            points = List.concat
                [ 
                map_triangles_from_to start_triangle (south_pole, east_pole, front_pole) default_triangle
                ,map_triangles_from_to start_triangle (south_pole, east_pole, back_pole) default_triangle
                ,map_triangles_from_to start_triangle (south_pole, west_pole, front_pole) default_triangle
                ,map_triangles_from_to start_triangle (south_pole, west_pole, back_pole) default_triangle
                ,map_triangles_from_to start_triangle (north_pole, east_pole, front_pole) default_triangle
                ,map_triangles_from_to start_triangle (north_pole, east_pole, back_pole) default_triangle
                ,map_triangles_from_to start_triangle (north_pole, west_pole, front_pole) default_triangle
                ,map_triangles_from_to start_triangle (north_pole, west_pole, back_pole) default_triangle
                ]
        in
        [normalise_points points centrePoint radius ]
        --[points, pole_triangles]

