module Utils exposing (..)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL

type alias Vertex =
    { color : Vec3
    , position : Vec3
    }


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



