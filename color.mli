type rgb
type hsl
type rgba

module Rgb :
  sig
    type t = rgb
    val make : float -> float -> float -> t
    val of_hsl : hsl -> t
    val mix : ?balance:float -> t -> t -> t
    module Infix :
      sig
        val ( *. ) : rgb -> float -> rgb
        val ( +. ) : rgb -> float -> rgb
        val ( -. ) : rgb -> float -> rgb
        val ( /. ) : rgb -> float -> rgb
      end
  end

module Rgba :
  sig
    type t = rgba
    val make : float -> float -> float -> float -> rgba
  end

module Hsl :
  sig
    type t = hsl
    val make : float -> float -> float -> hsl
    val of_rgb : rgb -> hsl
    val adjust_hue : hsl -> float -> hsl
    val complement : hsl -> hsl
    val saturate : hsl -> float -> hsl
    val desaturate : hsl -> float -> hsl
    val grayscale : hsl -> hsl
    val lighten : hsl -> float -> hsl
    val darken : hsl -> float -> hsl
  end

type t

val rgb : t -> Rgb.t
val hsl : t -> Hsl.t

val adjust_hue : t -> float -> t
val complement : t -> t
val saturate : t -> float -> t
val desaturate : t -> float -> t
val grayscale : t -> t
val lighten : t -> float -> t
val darken : t -> float -> t
module Infix :
  sig
    val ( *. ) : t -> float -> t
    val ( +. ) : t -> float -> t
    val ( -. ) : t -> float -> t
    val ( /. ) : t -> float -> t
  end
