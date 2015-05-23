(ns mage.core
  (:refer-clojure :exclude [compile])
  (:require [mage.il :as il]
            [clojure.string :as string])
  (:import [clojure.lang RT Numbers Compiler LineNumberingTextReader
            Symbol Namespace IFn Var]
           [clojure.lang.CljCompiler.Ast RHC ParserContext
            LiteralExpr StaticMethodExpr InstanceMethodExpr StaticPropertyExpr
            InstancePropertyExpr InstanceFieldExpr MapExpr VarExpr InvokeExpr HostExpr]
           [System.IO FileInfo Path]
           [System.Reflection TypeAttributes MethodAttributes]
           AppDomain
           System.Reflection.Emit.OpCodes
           AssemblyName
           AssemblyBuilderAccess))

(defn field-map
  "Get a map of all of an object's fields. Reflects."
  [obj]
  (-> obj
      .GetType
      (.GetFields (enum-or BindingFlags/Instance BindingFlags/NonPublic BindingFlags/Public))
      (->> (mapcat #(vector (keyword (.Name %))
                            (.GetValue % obj)))
           (apply hash-map))))

(defn property-map
  "Get a map of all of an object's properties. Reflects."
  [obj]
  (-> obj
      .GetType
      (.GetProperties (enum-or BindingFlags/Instance BindingFlags/Public))
      (->> (mapcat #(vector (keyword (.Name %))
                            (try (.GetValue % obj nil)
                              (catch Exception e nil))))
           (apply hash-map))))

(defn data-map
  "Get a map of all of an object's fields and properties. Reflects."
  [obj]
  (cond
    (nil? obj) obj
    (.IsValueType (type obj)) obj
    (instance? System.Collections.IEnumerable obj) (map data-map obj)
    :else (merge {::type (type obj)}
                 (field-map obj)
                 (property-map obj))))

(defn analyze
  ([form] (analyze form RHC/Expression))
  ([form rhc] (Compiler/Analyze (ParserContext. rhc) form)))

(defn method
  ([type name & params] (.GetMethod type name (into-array Type params))))

(defn property-getter [type name]
  (method type (str "get_" name)))

(defn property-setter [type name]
  (method type (str "set_" name)))

(def genstr (comp str gensym str))

(defn emit-method! [typb name attributes return-type param-types body]
  (let [mthb (. typb (DefineMethod name
                                   attributes
                                   return-type
                                   (into-array Type param-types)))]
    (il/emit!
      (. mthb GetILGenerator)
      body)
    mthb))

(defn find-file [ns]
  (-> ns
      name
      (string/replace "." "/")
      (str ".clj")
      RT/FindFile
      ))

;; http://stackoverflow.com/questions/24922478/is-there-a-way-to-read-all-the-forms-in-a-clojure-file
(defn read-all
  [file]
  (let [rdr (PushbackTextReader. (.OpenText file))]
    (loop [forms []]
      (let [form (try (read rdr) (catch Exception e nil))]
        (if form
          (recur (conj forms form))
          (do (.Close rdr)
            forms))))))

(defn assembly-builder [name]
  (.. AppDomain
      CurrentDomain
      (DefineDynamicAssembly
        (AssemblyName. name)
        AssemblyBuilderAccess/RunAndSave)))

(defn module-builder [assb name]
  (. assb (DefineDynamicModule name)))

(defn type-builder
  ([modb name]
   (type-builder modb name Object TypeAttributes/Public))
  ([modb name super]
   (type-builder modb name super TypeAttributes/Public))
  ([modb name super attributes]
   (. modb (DefineType name attributes super))))

(defn append [a col]
  (concat col [a]))

(def load-constant)

(defn load-vector [v]
  [(load-constant (int (count v)))
   (il/newarr Object)
   (map (fn [i c]
          [(il/dup)
           (load-constant (int i))
           (load-constant c)
           (il/stelem (type c))])
        (range)
        v)
   (il/call (method clojure.lang.LazilyPersistentVector "createOwning" |System.Object[]|))])

(defn load-map [keyvals]
  (let [ks (take-nth 2 keyvals)
        vs (take-nth 2 (drop 1 keyvals))]
    [(load-constant (int (+ (count ks) (count vs))))
     (il/newarr Object)
     (map (fn [i kv]
            [(il/dup)
             (load-constant (int i))
             (load-constant kv)
             (il/stelem (type kv))])
          (range)
          (interleave ks vs))
     (il/call (method clojure.lang.PersistentArrayMap "createWithCheck" |System.Object[]|))]))


(defn load-constant [k]
  (cond 
    (instance? System.String k)       (il/ldstr k)
    (instance? System.Boolean k)      (if k (il/ldc-i4-1) (il/ldc-i4-0))
    (instance? System.Int32 k)        (il/ldc-i4 k)
    (instance? System.Int64 k)        (il/ldc-i8 k)
    (instance? System.Single k)       (il/ldc-r4 k)
    (instance? System.Double k)       (il/ldc-r8 k)
    
    (instance? clojure.lang.APersistentVector k) (load-vector k)
    (instance? clojure.lang.APersistentMap k) (load-map (seq k))
    
    ))

(defn to-address [t]
  (let [l (il/local t)]
    [(il/stloc l)
     (il/ldloca l)]))

;; TODO Emit is a bad name. should be e.g. Generate
(defprotocol Bytecode
  (bytecode [_]))

(defn cleanup-stack
  ([pcon]
   (if (.IsStatementContext pcon)
     (il/pop)))
  ([lasttype pcon]
   (if (and (not= System.Void lasttype)
            (.IsStatementContext pcon))
     (il/pop))))

(def intrinsics
  {(method clojure.lang.RT "uncheckedIntCast" Double)
   (il/conv-i4)
   
   (method clojure.lang.RT "uncheckedIntCast" Int64)
   (il/conv-i4)
   
   (method clojure.lang.RT "uncheckedIntCast" Single)
   (il/conv-i4)
   
   (method clojure.lang.RT "uncheckedIntCast" Int32)
   []
   
   (method clojure.lang.Numbers "unchecked_add" Double Int64)
   [(il/conv-r8)
    (il/add)]
      
   (method clojure.lang.Numbers "unchecked_add" Double Double)
   (il/add)
   
   (method clojure.lang.Numbers "unchecked_add" Int64 Int64)
   (il/add)
   
   (method clojure.lang.Numbers "unchecked_multiply" Int64 Int64)
   (il/mul)
   
   (method clojure.lang.Numbers "unchecked_multiply" Double Double)
   (il/mul)
   
   (method clojure.lang.Numbers "unchecked_multiply" Double Int64)
   [(il/conv-r8)
    (il/mul)]
    
   (method clojure.lang.Numbers "unchecked_multiply" Int64 Double)
   [(let [loc (il/local Double)]
      [(il/stloc loc)
       (il/conv-r8)
       (il/ldloc loc)
       (il/mul)])]
   })

(extend-protocol Bytecode
  LiteralExpr
  (bytecode
    [this]
    (let [data (data-map this)]
      (if-not (.IsStatementContext (data :ParsedContext))
        (load-constant (data :Val)))))
  
  StaticMethodExpr
  (bytecode
    [this]
    (let [data (data-map this)
          pcon (.ParsedContext this)
          args (map data-map (:_args data))
          method (:_method data)]
          
      [(->> args
            (map :ArgExpr)
            (map bytecode)) 
      
       (if-let [intrinsic-bytecode (intrinsics method)]
        intrinsic-bytecode
        (il/call method))
       
       (cleanup-stack (.ReturnType method) pcon)]))
  
  InstanceMethodExpr
  (bytecode
    [this]
    (let [data (data-map this)
          pcon (.ParsedContext this)
          target (:_target data)
          target-type (-> target .ClrType)
          args (map data-map (:_args data))
          method (:_method data)]
          
      [(bytecode target)
       (if (.IsValueType target-type)
         (to-address target-type))
       (->> args
            (map :ArgExpr)
            (map bytecode)) 
       (if (.IsValueType target-type)
         (il/call method)
         (il/callvirt method))
       (cleanup-stack (.ReturnType method)
                      pcon)]))
  
  StaticPropertyExpr
  (bytecode
    [this]
    (let [pcon (.ParsedContext this)
          return-type (.ClrType this)
          getter (-> this data-map :_tinfo .GetGetMethod)]
      [(il/call getter)
       (cleanup-stack return-type pcon)]))
  
  InstancePropertyExpr
  (bytecode
    [this]
    (let [data (data-map this)
          pcon (.ParsedContext this)
          return-type (.ClrType this)
          target (:_target data)
          getter (-> data :_tinfo .GetGetMethod)]
      [(bytecode target)
       (il/callvirt getter)
       (cleanup-stack return-type pcon)]))
  
  InstanceFieldExpr
  (bytecode
    [this]
    (let [data (data-map this)
          pcon (.ParsedContext this)
          target (:_target data)
          field (:_tinfo data)
          return-type (.FieldType field)]
      [(bytecode target)
       (il/ldfld field)
       (cleanup-stack return-type pcon)]))
  
  MapExpr
  (bytecode
    [this]
    (let [pcon (.ParsedContext this)
          ks (take-nth 2 (.KeyVals this))
          vs (take-nth 2 (drop 1 (.KeyVals this)))]
      [(load-constant (int (+ (count ks) (count vs))))
       (il/newarr Object)
       (map (fn [i kv]
              [(il/dup)
               (load-constant (int i))
               (bytecode kv)
               (il/stelem (.ClrType kv))])
            (range)
            (interleave ks vs))
       (il/call (method RT "mapUniqueKeys" |System.Object[]|))
       (cleanup-stack pcon)]
      ))
  
  ;; TODO should use constants
  VarExpr
  (bytecode
    [this]
    (let [pcon (.ParsedContext this)
          nsname  (.. this Var Namespace Name ToString)
          symname (.. this Var Symbol ToString)]
      [(load-constant nsname)
       (il/call (method Symbol "create" String))
       (load-constant symname)
       (il/call (method Symbol "create" String))
       (il/call (method Var "intern" Symbol Symbol))
       (il/call (method Var "getRawRoot"))
       (cleanup-stack pcon)
       ]))
  
  InvokeExpr
  (bytecode
    [this]
    (let [data (data-map this)
          pcon (.ParsedContext this)
          fexpr (:_fexpr data)
          args (:_args data)
          arity (count args)]
      [(bytecode fexpr)
       (il/castclass IFn)
       (map (fn [a]
              [(bytecode a)
               (if (and (.HasClrType a)
                        (.IsValueType (.ClrType a)))
                 (il/box (.ClrType a)))])
            args)
       (il/callvirt (apply method IFn "invoke" (repeat arity Object)))
       (cleanup-stack pcon)
       ]))
  )

(defn compile [filename]
  (if-let [^FileInfo file (RT/FindFile filename)]
    (let [forms (read-all file)
          dotname (string/replace filename "/" ".")
          dllname (str dotname ".dll")
          inname (-> filename
                     (string/replace (str Path/PathSeparator) "/")
                     (str "__init"))
          assb (assembly-builder dllname)
          modb (module-builder assb dllname)
          typb (type-builder modb (str "__Init__$" filename))
          ]
      
      ;; Initialize method
      (->> forms
           (map #(analyze % RHC/Expression))
           (map bytecode)
           (append (il/ret))
           (emit-method!
             typb
             "invoke"
             (enum-or MethodAttributes/Public)
             nil
             []))
      
      (. typb CreateType)
      (. assb Save (.. typb Module Name))
      )))

(compile "mage/test")

(-> (analyze '(+ 1 2))
    bytecode
    )

(comment
  ;; symbolic bytecode  
  (use 'clojure.pprint)
  (pprint
    (->> "mage/test.clj"
         RT/FindFile
         read-all
         (map analyze)
         (map bytecode)
         flatten
         (remove nil?)))
  
  ;; compile to file
  (binding [*unchecked-math* true]
    (compile "mage/test.clj"))
  
  ;; compile and run
  (. (Activator/CreateInstance (compile "mage/test.clj")) Initialize)
  
  
  (defn magefn* [name arity body] 
    (let [dllname (str name ".dll")
          assb (assembly-builder dllname)
          modb (module-builder assb dllname)
          ^TypeBuilder typb (type-builder modb (str "magefn$" name) clojure.lang.AFunction)]
      
      (emit-method!
        typb
        "HasArity"
        (enum-or MethodAttributes/Public MethodAttributes/Virtual)
        System.Boolean
        [Int32]
        [(il/ldc-i4 arity)
         
         (il/ldarg-1)
         (il/ceq)
         (il/ret)])
      BigInteger
      
      (emit-method!
        typb
        "invoke"
        (enum-or MethodAttributes/Public MethodAttributes/Virtual)
        Object
        (repeat arity Object)
        body)
      
      (Activator/CreateInstance (. typb CreateType))))
  
  (defmacro magefn [args & body]
    `(magefn* (genstr "magefn")
              ~(count args)
              (let ~(vec (interleave args (drop 1 (range))))
                [~@body]
                )
              ))
  
  
  (def magestr
    (magefn [a b]
            (il/ldarg a)
            (il/ldarg b)
            (il/call (method String "Concat" String String))
            (il/ret)))
  
  (def mageneg?
    (magefn [a]
            (il/ldarg-1)
            (il/ldc-i8 0)
            (il/clt)
            (il/ret)))
  
  (def mage-integer?
    (magefn [a]
            (let [tlab (il/label)
                  chtyp (fn [t] [(il/ldarg-1)
                                 (il/isinst t)
                                 (il/brtrue tlab)])]
              
              [(chtyp Int32)
               (chtyp Int64)
               (chtyp UInt32)
               (chtyp UInt64)
               (chtyp clojure.lang.BigInt)
               (chtyp clojure.lang.BigInteger)
               (chtyp Int16)
               (chtyp UInt16)
               (chtyp Char)
               (chtyp Byte)
               (chtyp SByte)
               (il/ldc-i4-0)
               (il/box Boolean)
               (il/ret)
               tlab
               (il/ldc-i4-1)
               (il/box Boolean)
               (il/ret)])))
  
  (->> (.GetMethods UnityEngine.MonoBehaviour)
       (filter #)
       )
  
  (mage-integer? "2.1")
  
  
  (let [fnt (magefn '(->> "fucking works"
                      (partition 2 1)
                      reverse
                      str))
        fni (Activator/CreateInstance fnt)]
    (fni))
  
  
  
  (pprint
    (->> '(str 1 2)
         analyze
         bytecode
         ))
  
  (->> (analyze '(fn [a] a))
       data-map
       :interfaces
       )
)