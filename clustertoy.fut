let euclid_dist_2 [d] (pt1: [d]f32) (pt2: [d]f32): f32 =
  f32.sum (map (**2.0f32) (map2 (-) pt1 pt2))

let closest_point (p1: (i64,f32)) (p2: (i64,f32)): (i64,f32) =
  if p1.1 < p2.1 then p1 else p2

let find_nearest_point [k][d] (pts: [k][d]f32) (pt: [d]f32): i64 =
  let (i, _) = reduce_comm closest_point (0, euclid_dist_2 pt pts[0])
               (zip (iota k) (map (euclid_dist_2 pt) pts))
  in i

let add_centroids [d] (x: [d]f32) (y: [d]f32): *[d]f32 =
  map2 (+) x y

let centroids_of [n][d] (k: i64) (points: [n][d]f32) (membership: [n]i64): [k][d]f32 =
  let points_in_clusters =
    reduce_by_index (replicate k 0) (+) 0 membership (replicate n 1)

  let cluster_sums =
    reduce_by_index (replicate k (replicate d 0)) (map2 (+)) (replicate d 0)
                    membership
                    points

  in map2 (\point n -> map (/r32 (if n == 0 then 1 else n)) point)
          cluster_sums points_in_clusters

let kmeans_step [n][d]
                (k: i64) (assignments: [n]([d]f32, i64))
               : [n]i64 =
  let (points, old_membership) = unzip assignments
  let cluster_centres = centroids_of k points old_membership
  let membership = map (find_nearest_point cluster_centres) points
  in membership

import "lib/github.com/diku-dk/lys/lys"
import "lib/github.com/athas/matte/colour"
import "lib/github.com/diku-dk/cpprandom/random"

module rnge = minstd_rand
type rng = rnge.rng
module dist = uniform_int_distribution u32 rnge

type text_content = (i32,i32,f32)

let initial_clusters (k: i64) (rng: rng) : (rng, [k]argb.colour) =
  let (rngs, colours) =
    rng
    |> rnge.split_rng k
    |> map (dist.rand (0, 0xFFFFFF))
    |> unzip
  in (rnge.join_rng rngs,
      colours)

type mode = #del | #add

module lys : lys with text_content = text_content = {
  type~ state = { points: []([2]f32, i64)
                , pixels: [][]i64
                , clusters: []argb.colour
                , mode: mode
                , rng: rng
                }

  type text_content = text_content

  let mk_new_points [n][l] (h: i64) (w: i64) (k: i64)
                           (new: [l](i64,i64))
                           (old_points: [n]([2]f32, i64))
                         : ([l]i64,
                            []([2]f32, i64)) =
    let new_is = map (+n) (iota l)
    let new_points = map2 (\(y,x) i -> ([f32.i64 (y-h/2), f32.i64 (x-w/2)], i % k))
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
              then #[unsafe] s.pixels[y', x']
              else 1337
      in ((y', x'),
          p < 0 && ((x' - x)**2 + (y' - y)**2) < (d/2)**2)
    let to_add = tabulate_2d d d f |> flatten |> filter (.1) |> map (.0)
    let (new_is, points) =
      mk_new_points h w (length s.clusters) to_add s.points
    let scatter_i (y', x') = y' * w + x'
    let pixels_flat = scatter (copy (flatten s.pixels))
                              (map scatter_i to_add)
                              new_is
    in s with pixels = unflatten pixels_flat
         with points = points

  let add_cluster (x: i32) (y: i32) (s: state) : state =
    let (rng, cluster_colour) = dist.rand (0, 0xFFFFFF) s.rng
    let p = s.pixels[x,y]
    in if p != -1
       then s with points = (copy s.points with [p] = (s.points[p] with 1 = length s.clusters))
              with rng = rng
              with clusters = s.clusters ++ [cluster_colour]
       else s

  let del_cluster (x: i32) (y: i32) (s: state) : state =
    let p = s.pixels[x,y]
    in if p != -1 && length s.clusters > 1
       then let i = s.points[p].1
            let on_point (p, j) = (p, if j <= i then j else j-1)
            in s with clusters = take i s.clusters ++
                                 drop (i64.min (length s.clusters) (i+1)) s.clusters
                 with points = map on_point s.points
       else s

  let event (e: event) (s: state) : state =
    match e
    case #mouse {buttons, x, y} ->
      if buttons & 1 != 0
      then points_at (i64.i32 y) (i64.i32 x) s
      else if buttons & 4 != 0
      then match s.mode
           case #add -> add_cluster y x s
           case #del -> del_cluster y x s
      else s
    case #keydown {key} ->
      if key == SDLK_LSHIFT then s with mode = #del
      else s
    case #keyup {key} ->
      if key == SDLK_LSHIFT then s with mode = #add
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

  let init seed h w : state =
    let rng = rnge.rng_from_seed [i32.u32 seed]
    let k = 5
    let (rng, clusters) = initial_clusters k rng
    in { points = []
       , pixels = tabulate_2d h w (\_ _ -> -1)
       , rng
       , clusters
       , mode = #add
       }

  let grab_mouse = false
  let text_format () = "k: %d    n: %d    FPS: %f"
  let text_content fps (s: state) =
    (i32.i64 (length (s.clusters)),
     i32.i64 (length s.points),
     fps)
  let text_colour _ = argb.blue
}
