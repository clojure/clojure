(in-ns 'clojure.core)

(def objc? (clojure.lang.ObjC/objc))

(defmacro dispatch-main [& body]
  `(clojure.lang.RT/dispatchInMain
    (fn [] ~@body)))

(defn sel [s]
  (clojure.lang.Selector. s))

(defn objc-class [s]
  (RT/objcClass (name s)))

(defmacro $ [& args]
  (let [is-class (= 1 (count args))
        t (first args)]
    (if is-class
      `(objc-class '~t)
      (let [args (vec (next args))
            has-params (even? (count args))
            args (partition 2 (if has-params args (conj args nil)))
            params (map second args)
            selector (str (subs (apply str (map first args)) 1) (if has-params ":" ""))]
        (if has-params
          `((clojure.lang.Selector. ~selector) ~t ~@params)
          `((clojure.lang.Selector. ~selector) ~t))))))

(def objc-types
  {:void \v
   :float \f
   :longlong \q
   :long \l
   :char \c
   :short \s
   :int \i
   :double \d
   :ulonglong \Q
   :ulong \L
   :uchar \C
   :ushort \S
   :uint \I
   :bool \b
   :cgpoint \P
   :nsrange \N
   :uiedge \E
   :cgsize \Z
   :cgafflinetransform \A
   :catransform3d \T
   :uioffset \O
   :cgrect \R
   :id \p
   :pointer \Y
   :cgfloat (if ($ ($ NSCommon) :cgfloatIsDouble) \d \f)})

(defmacro nsproxy [& methods]
  (let [has-class (not (list? (first methods)))
        clazz (when has-class (name (first methods)))
        methods (if has-class (next methods) methods)
        i (map (fn [[[ret & args] & body]]
                 (let [ret (objc-types ret)
                       types (map objc-types (map keyword (take-nth 3 (next args))))
                       sel (take-nth 3 args)
                       fields (take-nth 3 (next (next args)))
                       sel (if (pos? (count fields))
                             (reduce str (map #(str (name %) ":") sel))
                             (reduce str (map name sel)))]
                   `[~sel [[~ret ~@types] (fn [~@fields] ~@body)]]))
               methods)
        i (into {} i)]
    `($ ($ ($ NSProxyImpl) :alloc) :initWithClass ~clazz :map
        ~i)))

(defmethod print-method clojure.lang.RemoteRef
  [^clojure.lang.RemoteRef r, ^java.io.Writer w]
  (.write w (str "#remote-ref \"" (.getId r) "\"")))

(defmethod print-method clojure.lang.ObjCClass
  [^clojure.lang.ObjCClass r, ^java.io.Writer w]
  (.write w (str "#objc-class \"" (.getName r) "\"")))

(defn read-remote-ref [id]
  (.get (clojure.lang.RemoteRef. id)))

(defn read-sel [name]
  (clojure.lang.Selector. name))

(alter-var-root #'*data-readers*
                (fn [m] (merge m {'sel #'read-sel
                                  'remote-ref #'read-remote-ref
                                  'objc-class #'objc-class})))

(defn class->types [c] 
  (case c
    :long :longlong
    :integer :int
    :boolean :bool
    :float :float
    :double :double
    :short :short
    :void :void
    :character :char
    :remoteref :pointer
    (if ($ c :isKindOfClass ($ NSValue))
      (some #(if (= (val %) ($ ($ NSCommon) :signatureToType ($ c :objCType))) (key %)) objc-types)
      :id)))

(defn types-for-vals [vals]
  (map (comp objc-types class->types keyword clojure.string/lower-case #(.getSimpleName %) class) vals))

(defn ccall [fun types args]
  ($ ($ NSCommon) :ccall (name fun) :types (vec types) :args (vec args)))

(defmacro defc [n r types]
  (let [nn (name n)
        variadic? (= '& (last types))
        types (if variadic? (drop-last types) types)
        types (vec (map objc-types (cons r types)))]
    (if variadic?
      `(defn ~n [& args#] (ccall ~nn (apply conj ~types (types-for-vals (drop (dec (count ~types)) args#))) args#))
      `(defn ~n [& args#] (ccall ~nn ~types args#)))))

(defn remote-repl []
  (clojure.lang.RemoteRepl/listen))
