type rgb =
  { r: float
  ; g: float
  ; b: float
  }

type rgba =
  { r: float
  ; g: float
  ; b: float
  ; a: float
  }

type hsl =
  { h: float
  ; s: float
  ; l: float
  }

module Float = struct
  (** [epsilon_eq a b] returns [true] if [a] and [b] have the same exponent and
      their mantissa components are within [1e-12] *)
  let epsilon_eq a b =
    if b >= (a -. 0.000000000001) && b <= (a +. 0.000000000001)
    then true
    else false

  let min = function
    | [x] -> x
    | ls ->
      List.fold_left
        (fun acc x -> if x < acc then x else acc)
        (List.hd ls)
        (List.tl ls)

  let max = function
    | [x] -> x
    | ls ->
      List.fold_left
        (fun acc x -> if x < acc then acc else x)
        (List.hd ls)
        (List.tl ls)
end

module Rgb = struct
  type t = rgb

  let make r g b =
    { r; g; b }

  let of_hsl { h; s; l } =
    (* conversion formula provided by
     * http://www.niwa.nu/2013/05/math-behind-colorspace-conversions-rgb-hsl/
     *)
    if Float.epsilon_eq s 0.0
    then { r = l; g = l; b = l }
    else begin
      let t1 =
        if l < 0.5
        then l *. (1.0 +. s)
        else l +. s -. l *. s
      in
      let t2 = 2.0 *. l -. t1 in
      let r =
        let r' = h +. 0.333333333333 in
        if r' > 1.0
        then r' -. 1.0
        else r'
      in
      let g = h in
      let b =
        let b' = h -. 0.333333333333 in
        if b' < 0.0
        then b' +. 1.0
        else b'
      in
      let aux c =
        let c6 = 6.0 *. c in
        if c6 < 1.0
        then t2 +. (t1 -. t2) *. c6
        else begin
          let c2 = 2.0 *. c in
          if c2 < 1.0 then t1
          else begin
            let c3 = 3.0 *. c in
            if c3 < 2.0 then t2 +. (t1 -. t2) *. (0.666666666666 -. c) *. 6.0
            else t2
          end
        end
      in
      { r = aux r; g = aux g; b = aux b }
    end

  let mix ?(balance = 0.5) (t1 : rgb) (t2 : rgb) =
    assert(balance >= 0.0);
    assert(balance <= 1.0);
    let b1 = balance in
    let b2 = 1.0 -. balance in
    { r = (t1.r *. b1 +. t2.r *. b2) /. 2.0
    ; g = (t1.g *. b1 +. t2.g *. b2) /. 2.0
    ; b = (t1.b *. b1 +. t2.b *. b2) /. 2.0
    }

  module Infix : sig
    val ( *. ) : rgb -> float -> rgb
    val ( +. ) : rgb -> float -> rgb
    val ( -. ) : rgb -> float -> rgb
    val ( /. ) : rgb -> float -> rgb
  end = struct
    let mk_op op =
      let aux c x =
        let c' = op c x in
        let ci = int_of_float c' in
        c' -. (float_of_int ci)
      in
      fun ({ r; g; b } : rgb) x ->
        let r = aux r x in
        let g = aux g x in
        let b = aux b x in
        { r; g; b }

    let ( *. ) =
      mk_op ( *. )

    let ( +. ) =
      mk_op (+.)

    let ( -. ) =
      mk_op (-.)

    let ( /. ) =
      mk_op (/.)
  end
end

module Rgba = struct
  type t = rgba

  let make r g b a =
    { r; g; b; a }
end

module Hsl = struct
  type t = hsl

  let make h s l =
    { h; s; l }

  let of_rgb ({ r; g; b } : rgb) =
    (* conversion formula provided by
     * http://www.niwa.nu/2013/05/math-behind-colorspace-conversions-rgb-hsl/
     *)
    let c_max = Float.max [r; g; b] in
    let c_min = Float.min [r; g; b] in
    let l = (c_max +. c_min) /. 2.0 in
    let s =
      if c_max = c_min
      then 0.0 (* we want to avoid having to do the calculation if possible *)
      else begin
        if l >= 0.5
        then (c_max -. c_min) /. (2.0 -. c_max -. c_min)
        else (c_max -. c_min) /. (c_max +. c_min)
      end
    in
    let h =
      let theta =
        match c_max with
          | x when x = r ->
              (g -. b) /. (c_max -. c_min)
          | x when x = g ->
              2.0 +. (b -. r) /. (c_max -. c_min)
          | x when x = b ->
              4.0 +. (r -. g) /. (c_max -. c_min)
          | _ -> raise (Failure "Hsl.of_rgb seems to be broken.")
      in
      let theta = theta /. 6.0 in
      if theta < 0.0
      then theta +. 1.0
      else theta
    in
    { h; s; l }

  let adjust_hue t h =
    let h' =
      let x = t.h +. h in
      let xi = int_of_float x in
      x -. (float_of_int xi)
    in
    { t with h = h' }

  let complement t =
    adjust_hue t 0.5

  let saturate t s =
    let s' =
      let s'' = t.s +. s in
      if s'' < 0.0
      then 0.0
      else begin
        if s'' > 1.0
        then 1.0
        else s''
      end
    in
    { t with s = s' }

  let desaturate t s =
    saturate t (0.0 -. s)

  let grayscale t =
    desaturate t 1.0

  let lighten t l =
    let l' =
      let l'' = t.l +. l in
      if l'' > 1.0
      then 1.0
      else begin
        if l'' < 0.0
        then 0.0
        else l''
      end
    in
    { t with l = l' }

  let darken t l =
    lighten t (0.0 -. l)
end

type t =
  { rgb: Rgb.t
  ; hsl: Hsl.t
  }

let rgb t = t.rgb
let hsl t = t.hsl

let mk_apply_hsl_f_op op =
  fun t f ->
    let hsl' = op t.hsl f in
    { hsl = hsl'; rgb = Rgb.of_hsl hsl' }

let adjust_hue =
  mk_apply_hsl_f_op Hsl.adjust_hue

let complement t =
  let hsl' = Hsl.complement t.hsl in
  { hsl = hsl'; rgb = Rgb.of_hsl hsl' }

let saturate =
  mk_apply_hsl_f_op Hsl.saturate

let desaturate =
  mk_apply_hsl_f_op Hsl.desaturate

let grayscale t =
  let hsl' = Hsl.grayscale t.hsl in
  { hsl = hsl'; rgb = Rgb.of_hsl hsl' }

let lighten =
  mk_apply_hsl_f_op Hsl.lighten

let darken =
  mk_apply_hsl_f_op Hsl.darken

module Infix = struct
  let ( *. ) t x =
    let rgb' = Rgb.Infix.(t.rgb *. x) in
    { rgb = rgb'; hsl = Hsl.of_rgb rgb' }

  let ( +. ) t x =
    let rgb' = Rgb.Infix.(t.rgb +. x) in
    { rgb = rgb'; hsl = Hsl.of_rgb rgb' }

  let ( -. ) t x =
    let rgb' = Rgb.Infix.(t.rgb -. x) in
    { rgb = rgb'; hsl = Hsl.of_rgb rgb' }

  let ( /. ) t x =
    let rgb' = Rgb.Infix.(t.rgb /. x) in
    { rgb = rgb'; hsl = Hsl.of_rgb rgb' }
end
