;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;   which can be found in the file CPL.TXT at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(in-ns 'int)
(clojure/refer 'clojure :exclude '(+ - * / == < <= > >= zero? pos? neg? inc dec vec max min aget alength))
(import '(clojure.lang Numbers$I))

(definline + [x y] `(. Numbers$I add ~x ~y))
(definline - [x y] `(. Numbers$I subtract ~x ~y))
(definline * [x y] `(. Numbers$I multiply ~x ~y))
(definline div [x y] `(. Numbers$I divide ~x ~y))
(definline == [x y] `(. Numbers$I equiv ~x ~y))
(definline < [x y] `(. Numbers$I lt ~x ~y))
(definline <= [x y] `(. Numbers$I lte ~x ~y))
(definline > [x y] `(. Numbers$I gt ~x ~y))
(definline >= [x y] `(. Numbers$I gte ~x ~y))
(definline zero? [x] `(. Numbers$I zero ~x))
(definline pos? [x] `(. Numbers$I pos ~x))
(definline neg? [x] `(. Numbers$I neg ~x))
(definline inc [x] `(. Numbers$I inc ~x))
(definline dec [x] `(. Numbers$I dec ~x))
(definline negate [x] `(. Numbers$I negate ~x))

(definline aget [xs i] `(. Numbers$I aget ~xs ~i))
(definline alength [xs] `(. Numbers$I alength ~xs))

(defn vec 
  ([size-or-seq] (. Numbers$I vec size-or-seq))
  ([size init] (. Numbers$I vec size init)))

(defn vmap 
  ([f xs] (. Numbers$I vmap f xs))
  ([f xs ys] (. Numbers$I vmap f xs ys)))

(definline v+n [xs y] `(. Numbers$I vsadd ~xs ~y))
(definline v-n [xs y] `(. Numbers$I vssub ~xs ~y))
(definline v|n [xs y] `(. Numbers$I vsdiv ~xs ~y))
(definline v*n [xs y] `(. Numbers$I vsmul ~xs ~y))
(definline n|v [x ys] `(. Numbers$I svdiv ~x ~ys))

(definline v*n+v [xs y zs] `(. Numbers$I vsmuladd ~xs ~y ~zs))
(definline v*n-v [xs y zs] `(. Numbers$I vsmulsub ~xs ~y ~zs))
(definline v*n+n [xs y z] `(. Numbers$I vsmuladd ~xs ~y ~z))
(definline v*n-n [xs y z] `(. Numbers$I vsmulssub ~xs ~y ~z))

(definline vabs [xs] `(. Numbers$I vabs ~xs))
(definline vnegateabs [xs] `(. Numbers$I vnegabs ~xs))
(definline vnegate [xs] `(. Numbers$I vneg ~xs))
(definline vsqr [xs] `(. Numbers$I vsqr ~xs))
(definline vsignedsqr [xs] `(. Numbers$I vsignedsqr ~xs))
(definline vreverse [xs] `(. Numbers$I vreverse ~xs))
(definline vrunningsum [xs] `(. Numbers$I vrunningsum ~xs))
(definline vsort [xs] `(. Numbers$I vsort ~xs))
(definline vmax [xs] `(. Numbers$I vmax ~xs))
(definline vmin [xs] `(. Numbers$I vmin ~xs))
(definline vmean [xs] `(. Numbers$I vmean ~xs))
(definline vrms [xs] `(. Numbers$I vrms ~xs))
(definline vsum [xs] `(. Numbers$I vsum ~xs))

(definline vclip [xs low high] `(. Numbers$I vclip ~xs ~low ~high))
(definline vclipcounts [xs low high] `(. Numbers$I vclipcounts ~xs ~low ~high))
(definline vthresh [xs thresh otherwise] `(. Numbers$I vthresh ~xs ~thresh ~otherwise))

(definline vdot [xs ys] `(. Numbers$I vdot ~xs ~ys))
(definline v== [xs ys] `(. Numbers$I vequiv ~xs ~ys))
(definline v+v [xs ys] `(. Numbers$I vadd ~xs ~ys))
(definline v-v [xs ys] `(. Numbers$I vsub ~xs ~ys))
(definline v*v [xs ys] `(. Numbers$I vmul ~xs ~ys))
(definline v|v [xs ys] `(. Numbers$I vdiv ~xs ~ys))
(definline vmaxv [xs ys] `(. Numbers$I vmax ~xs ~ys))
(definline vminv [xs ys] `(. Numbers$I vmin ~xs ~ys))

(definline v+v*v [xs ys zs] `(. Numbers$I vaddmul ~xs ~ys ~zs))
(definline v-v*v [xs ys zs] `(. Numbers$I vsubmul ~xs ~ys ~zs))

(definline v+v*n [xs ys z] `(. Numbers$I vaddsmul ~xs ~ys ~z))
(definline v-v*n [xs ys z] `(. Numbers$I vsubsmul ~xs ~ys ~z))
(definline v*v+n [xs ys z] `(. Numbers$I vmulsadd ~xs ~ys ~z))

(in-ns 'long)
(clojure/refer 'clojure :exclude '(+ - * / == < <= > >= zero? pos? neg? inc dec vec max min aget alength))
(import '(clojure.lang Numbers$L))

(definline + [x y] `(. Numbers$L add ~x ~y))
(definline - [x y] `(. Numbers$L subtract ~x ~y))
(definline * [x y] `(. Numbers$L multiply ~x ~y))
(definline div [x y] `(. Numbers$L divide ~x ~y))
(definline == [x y] `(. Numbers$L equiv ~x ~y))
(definline < [x y] `(. Numbers$L lt ~x ~y))
(definline <= [x y] `(. Numbers$L lte ~x ~y))
(definline > [x y] `(. Numbers$L gt ~x ~y))
(definline >= [x y] `(. Numbers$L gte ~x ~y))
(definline zero? [x] `(. Numbers$L zero ~x))
(definline pos? [x] `(. Numbers$L pos ~x))
(definline neg? [x] `(. Numbers$L neg ~x))
(definline inc [x] `(. Numbers$L inc ~x))
(definline dec [x] `(. Numbers$L dec ~x))
(definline negate [x] `(. Numbers$L negate ~x))

(definline aget [xs i] `(. Numbers$L aget ~xs ~i))
(definline alength [xs] `(. Numbers$L alength ~xs))

(defn vec 
  ([size-or-seq] (. Numbers$L vec size-or-seq))
  ([size init] (. Numbers$L vec size init)))

(defn vmap 
  ([f xs] (. Numbers$L vmap f xs))
  ([f xs ys] (. Numbers$L vmap f xs ys)))

(definline v+n [xs y] `(. Numbers$L vsadd ~xs ~y))
(definline v-n [xs y] `(. Numbers$L vssub ~xs ~y))
(definline v|n [xs y] `(. Numbers$L vsdiv ~xs ~y))
(definline v*n [xs y] `(. Numbers$L vsmul ~xs ~y))
(definline n|v [x ys] `(. Numbers$L svdiv ~x ~ys))

(definline v*n+v [xs y zs] `(. Numbers$L vsmuladd ~xs ~y ~zs))
(definline v*n-v [xs y zs] `(. Numbers$L vsmulsub ~xs ~y ~zs))
(definline v*n+n [xs y z] `(. Numbers$L vsmuladd ~xs ~y ~z))
(definline v*n-n [xs y z] `(. Numbers$L vsmulssub ~xs ~y ~z))

(definline vabs [xs] `(. Numbers$L vabs ~xs))
(definline vnegateabs [xs] `(. Numbers$L vnegabs ~xs))
(definline vnegate [xs] `(. Numbers$L vneg ~xs))
(definline vsqr [xs] `(. Numbers$L vsqr ~xs))
(definline vsignedsqr [xs] `(. Numbers$L vsignedsqr ~xs))
(definline vreverse [xs] `(. Numbers$L vreverse ~xs))
(definline vrunningsum [xs] `(. Numbers$L vrunningsum ~xs))
(definline vsort [xs] `(. Numbers$L vsort ~xs))
(definline vmax [xs] `(. Numbers$L vmax ~xs))
(definline vmin [xs] `(. Numbers$L vmin ~xs))
(definline vmean [xs] `(. Numbers$L vmean ~xs))
(definline vrms [xs] `(. Numbers$L vrms ~xs))
(definline vsum [xs] `(. Numbers$L vsum ~xs))

(definline vclip [xs low high] `(. Numbers$L vclip ~xs ~low ~high))
(definline vclipcounts [xs low high] `(. Numbers$L vclipcounts ~xs ~low ~high))
(definline vthresh [xs thresh otherwise] `(. Numbers$L vthresh ~xs ~thresh ~otherwise))

(definline vdot [xs ys] `(. Numbers$L vdot ~xs ~ys))
(definline v== [xs ys] `(. Numbers$L vequiv ~xs ~ys))
(definline v+v [xs ys] `(. Numbers$L vadd ~xs ~ys))
(definline v-v [xs ys] `(. Numbers$L vsub ~xs ~ys))
(definline v*v [xs ys] `(. Numbers$L vmul ~xs ~ys))
(definline v|v [xs ys] `(. Numbers$L vdiv ~xs ~ys))
(definline vmaxv [xs ys] `(. Numbers$L vmax ~xs ~ys))
(definline vminv [xs ys] `(. Numbers$L vmin ~xs ~ys))

(definline v+v*v [xs ys zs] `(. Numbers$L vaddmul ~xs ~ys ~zs))
(definline v-v*v [xs ys zs] `(. Numbers$L vsubmul ~xs ~ys ~zs))

(definline v+v*n [xs ys z] `(. Numbers$L vaddsmul ~xs ~ys ~z))
(definline v-v*n [xs ys z] `(. Numbers$L vsubsmul ~xs ~ys ~z))
(definline v*v+n [xs ys z] `(. Numbers$L vmulsadd ~xs ~ys ~z))

(in-ns 'float)
(clojure/refer 'clojure :exclude '(+ - * / == < <= > >= zero? pos? neg? inc dec vec max min aget alength))
(import '(clojure.lang Numbers$F))

(definline + [x y] `(. Numbers$F add ~x ~y))
(definline - [x y] `(. Numbers$F subtract ~x ~y))
(definline * [x y] `(. Numbers$F multiply ~x ~y))
(definline div [x y] `(. Numbers$F divide ~x ~y))
(definline == [x y] `(. Numbers$F equiv ~x ~y))
(definline < [x y] `(. Numbers$F lt ~x ~y))
(definline <= [x y] `(. Numbers$F lte ~x ~y))
(definline > [x y] `(. Numbers$F gt ~x ~y))
(definline >= [x y] `(. Numbers$F gte ~x ~y))
(definline zero? [x] `(. Numbers$F zero ~x))
(definline pos? [x] `(. Numbers$F pos ~x))
(definline neg? [x] `(. Numbers$F neg ~x))
(definline inc [x] `(. Numbers$F inc ~x))
(definline dec [x] `(. Numbers$F dec ~x))
(definline negate [x] `(. Numbers$F negate ~x))

(definline aget [xs i] `(. Numbers$F aget ~xs ~i))
(definline alength [xs] `(. Numbers$F alength ~xs))

(defn vec 
  ([size-or-seq] (. Numbers$F vec size-or-seq))
  ([size init] (. Numbers$F vec size init)))

(defn vmap 
  ([f xs] (. Numbers$F vmap f xs))
  ([f xs ys] (. Numbers$F vmap f xs ys)))

(definline v+n [xs y] `(. Numbers$F vsadd ~xs ~y))
(definline v-n [xs y] `(. Numbers$F vssub ~xs ~y))
(definline v|n [xs y] `(. Numbers$F vsdiv ~xs ~y))
(definline v*n [xs y] `(. Numbers$F vsmul ~xs ~y))
(definline n|v [x ys] `(. Numbers$F svdiv ~x ~ys))

(definline v*n+v [xs y zs] `(. Numbers$F vsmuladd ~xs ~y ~zs))
(definline v*n-v [xs y zs] `(. Numbers$F vsmulsub ~xs ~y ~zs))
(definline v*n+n [xs y z] `(. Numbers$F vsmuladd ~xs ~y ~z))
(definline v*n-n [xs y z] `(. Numbers$F vsmulssub ~xs ~y ~z))

(definline vabs [xs] `(. Numbers$F vabs ~xs))
(definline vnegateabs [xs] `(. Numbers$F vnegabs ~xs))
(definline vnegate [xs] `(. Numbers$F vneg ~xs))
(definline vsqr [xs] `(. Numbers$F vsqr ~xs))
(definline vsignedsqr [xs] `(. Numbers$F vsignedsqr ~xs))
(definline vreverse [xs] `(. Numbers$F vreverse ~xs))
(definline vrunningsum [xs] `(. Numbers$F vrunningsum ~xs))
(definline vsort [xs] `(. Numbers$F vsort ~xs))
(definline vmax [xs] `(. Numbers$F vmax ~xs))
(definline vmin [xs] `(. Numbers$F vmin ~xs))
(definline vmean [xs] `(. Numbers$F vmean ~xs))
(definline vrms [xs] `(. Numbers$F vrms ~xs))
(definline vsum [xs] `(. Numbers$F vsum ~xs))

(definline vclip [xs low high] `(. Numbers$F vclip ~xs ~low ~high))
(definline vclipcounts [xs low high] `(. Numbers$F vclipcounts ~xs ~low ~high))
(definline vthresh [xs thresh otherwise] `(. Numbers$F vthresh ~xs ~thresh ~otherwise))

(definline vdot [xs ys] `(. Numbers$F vdot ~xs ~ys))
(definline v== [xs ys] `(. Numbers$F vequiv ~xs ~ys))
(definline v+v [xs ys] `(. Numbers$F vadd ~xs ~ys))
(definline v-v [xs ys] `(. Numbers$F vsub ~xs ~ys))
(definline v*v [xs ys] `(. Numbers$F vmul ~xs ~ys))
(definline v|v [xs ys] `(. Numbers$F vdiv ~xs ~ys))
(definline vmaxv [xs ys] `(. Numbers$F vmax ~xs ~ys))
(definline vminv [xs ys] `(. Numbers$F vmin ~xs ~ys))

(definline v+v*v [xs ys zs] `(. Numbers$F vaddmul ~xs ~ys ~zs))
(definline v-v*v [xs ys zs] `(. Numbers$F vsubmul ~xs ~ys ~zs))

(definline v+v*n [xs ys z] `(. Numbers$F vaddsmul ~xs ~ys ~z))
(definline v-v*n [xs ys z] `(. Numbers$F vsubsmul ~xs ~ys ~z))
(definline v*v+n [xs ys z] `(. Numbers$F vmulsadd ~xs ~ys ~z))

(in-ns 'double)
(clojure/refer 'clojure :exclude '(+ - * / == < <= > >= zero? pos? neg? inc dec vec max min aget alength))
(import '(clojure.lang Numbers$D))

(definline + [x y] `(. Numbers$D add ~x ~y))
(definline - [x y] `(. Numbers$D subtract ~x ~y))
(definline * [x y] `(. Numbers$D multiply ~x ~y))
(definline div [x y] `(. Numbers$D divide ~x ~y))
(definline == [x y] `(. Numbers$D equiv ~x ~y))
(definline < [x y] `(. Numbers$D lt ~x ~y))
(definline <= [x y] `(. Numbers$D lte ~x ~y))
(definline > [x y] `(. Numbers$D gt ~x ~y))
(definline >= [x y] `(. Numbers$D gte ~x ~y))
(definline zero? [x] `(. Numbers$D zero ~x))
(definline pos? [x] `(. Numbers$D pos ~x))
(definline neg? [x] `(. Numbers$D neg ~x))
(definline inc [x] `(. Numbers$D inc ~x))
(definline dec [x] `(. Numbers$D dec ~x))
(definline negate [x] `(. Numbers$D negate ~x))

(definline aget [xs i] `(. Numbers$D aget ~xs ~i))
(definline alength [xs] `(. Numbers$D alength ~xs))

(defn vec 
  ([size-or-seq] (. Numbers$D vec size-or-seq))
  ([size init] (. Numbers$D vec size init)))

(defn vmap 
  ([f xs] (. Numbers$D vmap f xs))
  ([f xs ys] (. Numbers$D vmap f xs ys)))

(definline v+n [xs y] `(. Numbers$D vsadd ~xs ~y))
(definline v-n [xs y] `(. Numbers$D vssub ~xs ~y))
(definline v|n [xs y] `(. Numbers$D vsdiv ~xs ~y))
(definline v*n [xs y] `(. Numbers$D vsmul ~xs ~y))
(definline n|v [x ys] `(. Numbers$D svdiv ~x ~ys))

(definline v*n+v [xs y zs] `(. Numbers$D vsmuladd ~xs ~y ~zs))
(definline v*n-v [xs y zs] `(. Numbers$D vsmulsub ~xs ~y ~zs))
(definline v*n+n [xs y z] `(. Numbers$D vsmuladd ~xs ~y ~z))
(definline v*n-n [xs y z] `(. Numbers$D vsmulssub ~xs ~y ~z))

(definline vabs [xs] `(. Numbers$D vabs ~xs))
(definline vnegateabs [xs] `(. Numbers$D vnegabs ~xs))
(definline vnegate [xs] `(. Numbers$D vneg ~xs))
(definline vsqr [xs] `(. Numbers$D vsqr ~xs))
(definline vsignedsqr [xs] `(. Numbers$D vsignedsqr ~xs))
(definline vreverse [xs] `(. Numbers$D vreverse ~xs))
(definline vrunningsum [xs] `(. Numbers$D vrunningsum ~xs))
(definline vsort [xs] `(. Numbers$D vsort ~xs))
(definline vmax [xs] `(. Numbers$D vmax ~xs))
(definline vmin [xs] `(. Numbers$D vmin ~xs))
(definline vmean [xs] `(. Numbers$D vmean ~xs))
(definline vrms [xs] `(. Numbers$D vrms ~xs))
(definline vsum [xs] `(. Numbers$D vsum ~xs))

(definline vclip [xs low high] `(. Numbers$D vclip ~xs ~low ~high))
(definline vclipcounts [xs low high] `(. Numbers$D vclipcounts ~xs ~low ~high))
(definline vthresh [xs thresh otherwise] `(. Numbers$D vthresh ~xs ~thresh ~otherwise))

(definline vdot [xs ys] `(. Numbers$D vdot ~xs ~ys))
(definline v== [xs ys] `(. Numbers$D vequiv ~xs ~ys))
(definline v+v [xs ys] `(. Numbers$D vadd ~xs ~ys))
(definline v-v [xs ys] `(. Numbers$D vsub ~xs ~ys))
(definline v*v [xs ys] `(. Numbers$D vmul ~xs ~ys))
(definline v|v [xs ys] `(. Numbers$D vdiv ~xs ~ys))
(definline vmaxv [xs ys] `(. Numbers$D vmax ~xs ~ys))
(definline vminv [xs ys] `(. Numbers$D vmin ~xs ~ys))

(definline v+v*v [xs ys zs] `(. Numbers$D vaddmul ~xs ~ys ~zs))
(definline v-v*v [xs ys zs] `(. Numbers$D vsubmul ~xs ~ys ~zs))

(definline v+v*n [xs ys z] `(. Numbers$D vaddsmul ~xs ~ys ~z))
(definline v-v*n [xs ys z] `(. Numbers$D vsubsmul ~xs ~ys ~z))
(definline v*v+n [xs ys z] `(. Numbers$D vmulsadd ~xs ~ys ~z))
