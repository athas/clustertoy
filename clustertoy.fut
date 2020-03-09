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

let kmeans_step [n][d]
                (k: i32) (assignments: [n]([d]f32, i32))
               : [n]i32 =
  let (points, old_membership) = unzip assignments
  let cluster_centres = centroids_of k points old_membership
  let membership = map (find_nearest_point cluster_centres) points
  in membership

import "lib/github.com/diku-dk/lys/lys"
import "lib/github.com/athas/matte/colour"
import "lib/github.com/diku-dk/cpprandom/random"

module rnge = minstd_rand
type rng = rnge.rng
module dist = uniform_int_distribution i32 rnge

type text_content = (i32,i32,f32)

let initial_clusters (k: i32) (rng: rng) : (rng, [k]argb.colour) =
  let (rngs, colours) =
    rng
    |> rnge.split_rng k
    |> map (dist.rand (0, 0xFFFFFF))
    |> unzip
  in (rnge.join_rng rngs,
      colours)

module lys : lys with text_content = text_content = {
  type~ state = { points: []([2]f32, i32)
                , pixels: [][]i32
                , clusters: []argb.colour
                , rng: rng
                }

  type text_content = text_content

  let mk_new_points [n][l] (h: i32) (w: i32) (k: i32)
                           (new: [l](i32,i32))
                           (old_points: [n]([2]f32, i32))
                         : ([l]i32,
                            []([2]f32, i32)) =
    let new_is = map (+n) (iota l)
    let new_points = map2 (\(y,x) i -> ([r32 (y-h/2), r32 (x-w/2)], i % k))
                          new (iota l)
    in (new_is,
        old_points ++ new_points)

  let points_at y x (s: state) : state =
    let d = 50
    let (h, w) = (length s.pixels, length s.pixels[0])
    let f i j =
      let y' = i + y - (d/2)
      let x' = j + x - (d/2)
      let p = if y' >= 0 && y' < h && x' >= 0 && x' < w
              then unsafe s.pixels[y', x']
              else 1337
      in ((y', x'),
          p < 0 && ((x' - x)**2 + (y' - y)**2) < (d/2)**2)
    let to_add = tabulate_2d d d f |> flatten |> filter (.1) |> map (.0)
    let (new_is, points) =
      mk_new_points h w (length s.clusters) to_add s.points
    let scatter_i (y', x') = y' * w + x'
    let pixels_flat = scatter (copy (flatten s.pixels)) (map scatter_i to_add) new_is
    in s with pixels = unflatten h w pixels_flat
         with points = points

  let add_cluster (s: state) : state =
    let (rng, cluster_colour) = dist.rand (0, 0xFFFFFF) s.rng
    in s with rng = rng
         with clusters = s.clusters ++ [cluster_colour]

  let event (e: event) (s: state) : state =
    match e
    case #mouse {buttons, x, y} ->
      if buttons & 1 != 0
      then points_at y x s
      else if buttons & 4 != 0
      then add_cluster s
      else s
    case #step _ ->
      let membership = kmeans_step (length s.clusters) s.points
      let points = zip (map (.0) s.points) membership
      in s with points = points
    case _ -> s

  let resize h w (s: state) : state =
    let (old_h, old_w) = (length s.pixels, length s.pixels[0])
    let f i j = if i < old_h && j < old_w
                then s.pixels[i,j]
                else -1
    in s with pixels = tabulate_2d h w f

  let render (s: state) : [][]argb.colour =
    let on_pixel i =
      if i >= 0
      then let c = (s.points[i]).1
           in s.clusters[c]
      else argb.white
    in map (map on_pixel) (s.pixels)

  let init seed (h: i32) (w: i32) : state =
    let rng = rnge.rng_from_seed [i32.u32 seed]
    let k = 5
    let (rng, clusters) = initial_clusters k rng
    in { points = []
       , pixels = tabulate_2d h w (\_ _ -> -1)
       , rng
       , clusters
       }

  let grab_mouse = false
  let text_format () = "k: %d    n: %d    FPS: %f"
  let text_content fps (s: state) =
    (length (s.clusters),
     length s.points,
     fps)
  let text_colour _ = argb.blue
}
