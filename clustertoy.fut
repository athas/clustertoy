let euclid_dist_2 [d] (pt1: [d]f32) (pt2: [d]f32): f32 =
  f32.sum (map (**2.0f32) (map2 (-) pt1 pt2))

let closest_point (p1: (i32,f32)) (p2: (i32,f32)): (i32,f32) =
  if p1.1 < p2.1 then p1 else p2

let find_nearest_point [k][d] (pts: [k][d]f32) (pt: [d]f32): i32 =
  let (i, _) = reduce_comm closest_point (0, euclid_dist_2 pt pts[0])
               (zip (iota k) (map (euclid_dist_2 pt) pts))
  in i

let add_centroids [d] (x: [d]f32) (y: [d]f32): *[d]f32 =
  map2 (+) x y

let centroids_of [n][d] (k: i32) (points: [n][d]f32) (membership: [n]i32): [k][d]f32 =
  let points_in_clusters =
    reduce_by_index (replicate k 0) (+) 0 membership (replicate n 1)

  let cluster_sums =
    reduce_by_index (replicate k (replicate d 0)) (map2 (+)) (replicate d 0)
                    membership
                    points

  in map2 (\point n -> map (/r32 (if n == 0 then 1 else n)) point)
          cluster_sums points_in_clusters

let kmeans_step [k][n][d]
                (cluster_centres: [k][d]f32)
                (points: [n][d]f32)
               : ([n]i32, [k][d]f32) =
  let membership = map (find_nearest_point cluster_centres) points
  let cluster_centres = centroids_of k points membership
  in (membership, cluster_centres)

import "lib/github.com/diku-dk/lys/lys"
import "lib/github.com/athas/matte/colour"

type kmeans_state [k][n] =
  { points: [n]([2]f32, i32)
  , centres: [k][2]f32
  }

type text_content = f32

let mapi_2d 'a 'b [n][m] (f: i32 -> i32 -> a -> b) (ass: [n][m]a) : [n][m]b =
  map2 (\y as -> map2 (\x a -> f y x a) (iota m) as) (iota n) ass

module lys : lys with text_content = text_content = {
  type~ state = { kmeans: kmeans_state [][]
                , pixels: [][]i32
                }

  type text_content = text_content

  let mk_new_points [k][n][l] (h: i32) (w: i32)
                              (new: [l](i32,i32))
                              (ks: kmeans_state [k][n])
                            : ([l]i32,
                               kmeans_state [k][]) =
    let new_is = map (+n) (iota l)
    let new_points = map (\(y,x) -> ([r32 (y-h/2), r32 (x-w/2)], 0)) new
    in (new_is,
        ks with points = ks.points ++ new_points)

  let mouse_at y x (s: state) : state =
    let r = 200
    let f y' x' i = (y', x', i)
    let change (y', x', i) =
      i < 0 && ((x' - x)**2 + (y' - y)**2) < r
    let to_change = mapi_2d f s.pixels |> flatten |> filter change
    let (h, w) = (length s.pixels, length s.pixels[0])
    let (new_is, ks) =
      mk_new_points h w (map (\(y',x',_) -> (y',x')) to_change) s.kmeans
    let scatter_i (y', x', _) = y' * w + x'
    let pixels_flat = scatter (copy (flatten s.pixels)) (map scatter_i to_change) new_is
    in s with pixels = unflatten h w pixels_flat
         with kmeans = ks

  let event (e: event) (s: state) : state =
    match e
    case #mouse {buttons, x, y} ->
      if buttons != 0
      then mouse_at y x s
      else s
    case #step _ ->
      let (membership, centres) =
        kmeans_step (s.kmeans.centres) (map (.0) s.kmeans.points)
      let points = zip (map (.0) s.kmeans.points) membership
      in s with kmeans = {points, centres}
    case _ -> s

  let resize h w (s: state) : state =
    let (old_h, old_w) = (length s.pixels, length s.pixels[0])
    let f i j = if i < old_h && j < old_w
                then s.pixels[i,j]
                else -1
    in s with pixels = tabulate_2d h w f

  let base_colours =
    argb.([black,
           red,
           green,
           blue,
           brown,
           yellow,
           orange,
           magenta,
           violet])
  let colours = base_colours ++ map argb.dark base_colours ++ map argb.light base_colours

  let render (s: state) : [][]argb.colour =
    let on_pixel i =
      if i >= 0
      then let c = (s.kmeans.points[i]).1
           in colours[c]
      else argb.white
    in map (map on_pixel) (s.pixels)

  let init _ (h: i32) (w: i32) : state =
    let k = length colours
    let ks = {points=[],
              centres = tabulate k (\i -> [r32 i * (r32 h/r32 k),
                                           r32 i * (r32 w/r32 k)])
             }
    in { kmeans = ks,
         pixels = tabulate_2d h w (\_ _ -> -1)
       }

  let grab_mouse = false
  let text_format () = "FPS: %f"
  let text_content fps _ = fps
  let text_colour _ = argb.blue
}
