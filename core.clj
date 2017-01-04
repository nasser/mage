(ns mage.core
  (:refer-clojure :exclude [pop rem and or not type catch finally])
  (:require [clojure.string :as string])
  (:import [System.Reflection.Emit LocalBuilder Label ILGenerator OpCodes FieldBuilder]
           [System.Reflection TypeAttributes MethodAttributes MethodImplAttributes FieldAttributes]
           [System.Runtime.InteropServices CharSet]))

(defmulti emit-data
  "Emit bytecode for symbolic data object m. Internal."
  (fn [context m]
    (cond
      (::constructor     m) ::constructor
      (::exception       m) ::exception
      (::catch           m) ::catch
      (::finally         m) ::finally
      (::opcode          m) ::opcode
      (::label           m) ::label
      (::local           m) ::local
      (::field           m) ::field
      (::assembly        m) ::assembly
      (::module          m) ::module
      (::type            m) ::type
      (::method          m) ::method
      (::pinvoke-method  m) ::pinvoke-method
      :else                 [context m])))

(defn emit!
  ([stream] (emit! {} stream))
  ([initial-ctx stream]
   (reduce (fn [ctx s]
             (emit-data ctx s))
           initial-ctx
           (->> stream flatten (remove nil?)))))

;; TODO should we use a generic 'references' map instead of
;; specific maps e.g. assembly-builders, fields, locals, etc?
(defmethod emit-data ::assembly
  [context {:keys [::assembly ::access ::body] :as data}]
  (let [assembly-builder (.. AppDomain CurrentDomain
                             (DefineDynamicAssembly
                               (AssemblyName. assembly)
                               access))
        context* (-> context
                     (assoc ::assembly-builder assembly-builder)
                     (assoc-in [::assembly-builders data] assembly-builder)
                     (emit! body))]
    (.Save assembly-builder
           (str (.. assembly-builder GetName Name) ".dll"))
    context*))

(defmethod emit-data ::module
  [{:keys [::assembly-builder] :as context} {:keys [::module ::body] :as data}]
  (let [module-builder (. (clojure.core/or
                            assembly-builder
                            (.AssemblyBuilder clojure.lang.Compiler/EvalContext))
                          (DefineDynamicModule (str module)))
        context* (-> context
                     (assoc ::module-builder module-builder)
                     (assoc-in [::module-builders data] module-builder))]
    (emit! context* body)))

(defmethod emit-data ::type
  [{:keys [::module-builder] :as context}
   {:keys [::type ::attributes ::interfaces ::super ::generic-parameters ::body] :as data}]
  (let [module-builder (clojure.core/or
                         module-builder
                         (-> clojure.lang.Compiler/EvalContext
                             .AssemblyBuilder
                             (.GetModule "eval")))
        type-builder (if super
                       (. module-builder
                          (DefineType type attributes super))
                       (. module-builder
                          (DefineType type attributes)))
        generic-parameter-builders
        (when generic-parameters
          (.DefineGenericParameters
            type-builder
            (into-array String (map str generic-parameters))))
        generic-type-parameters
        (apply hash-map
               (interleave
                 generic-parameters
                 generic-parameter-builders))
        context* (-> context
                     (assoc
                       ::type-builder type-builder
                       ::fields {} ;; TODO clearing fields for new types - bad idea?
                       ::generic-type-parameters generic-type-parameters)
                     (assoc-in [::type-builders data] type-builder))]
    (doseq [interface interfaces]
      (.AddInterfaceImplementation type-builder interface))
    (let [context** (emit! context* body)]
      (. type-builder CreateType)
      context**)))

(defmethod emit-data ::field
  [{:keys [::type-builder ::fields] :as context} {:keys [::field ::type ::attributes] :as data}]
  (let [^FieldBuilder field (.DefineField type-builder (str field) type attributes)]
    (assoc-in context [::fields data] field)))

(defmethod emit-data ::method
  [{:keys [::type-builder ::fields ::generic-type-parameters] :as context}
   {:keys [::method ::attributes ::return-type ::parameter-types ::body] :as data}]
  (let [method-builder (. type-builder (DefineMethod
                                 method
                                 attributes
                                 (clojure.core/or
                                   (generic-type-parameters return-type)
                                   return-type)
                                 (->> parameter-types
                                      (map #(clojure.core/or (generic-type-parameters %) %))
                                      (into-array Type))))
        context* (-> context
                     (assoc
                       ::ilg (. method-builder GetILGenerator)
                       ::method-builder method-builder
                       ::labels {}
                       ::locals {})
                     (assoc-in [::method-builders data] method-builder))]
    (emit! context* body)))

(defmethod emit-data ::constructor
  [{:keys [::type-builder ::fields ::generic-type-parameters] :as context}
   {:keys [::name ::attributes ::calling-convention ::return-type ::parameter-types ::body] :as data}]
  (let [constructor-builder
        (. type-builder (DefineConstructor
                  attributes
                  calling-convention
                  (into-array Type
                              (map #(clojure.core/or (generic-type-parameters %) %)
                                   parameter-types))))
        context* (-> context
                     (assoc
                       ::ilg (. constructor-builder GetILGenerator)
                       ::method-builder constructor-builder
                       ::labels {}
                       ::locals {})
                     (assoc-in [::method-builders data] constructor-builder))]
    (emit! context* body)))

(defmethod emit-data ::pinvoke-method
  [{:keys [::type-builder] :as context}
   {:keys [::pinvoke-method
           ::dll-name
           ::entry-name
           ::attributes
           ::calling-convention
           ::return-type
           ::parameter-types
           ::method-impl-attributes
           ::native-calling-convention
           ::native-char-set]
    :as data}]
  (let [^MethodBuilder method-builder
        (.. type-builder (DefinePInvokeMethod
                           (str pinvoke-method)
                           (str dll-name)
                           (str entry-name)
                           (enum-or MethodAttributes/PinvokeImpl attributes)
                           calling-convention
                           return-type
                           (into-array Type parameter-types)
                           native-calling-convention
                           native-char-set))]
    ;; TODO enum-or or always from scratch?
    (.SetImplementationFlags method-builder
                             (enum-or (.GetMethodImplementationFlags method-builder)
                                      method-impl-attributes))
    (assoc-in context [::method-builders data] method-builder)))

(defmethod emit-data ::local
  [{:keys [::ilg ::type-builders] :as context} {:keys [::type ::local] :as data}]
  (let [t (clojure.core/or (type-builders type) type)
        ^LocalBuilder local-builder (.DeclareLocal ilg t)]
    (if-let [n local] (.SetLocalSymInfo local-builder (str n)))
    (assoc-in context [::locals data] local-builder)))

(defmethod emit-data ::label
  [{:keys [::ilg ::labels] :as context} data]
  (let [^Label label (clojure.core/or (labels data)
                         (.DefineLabel ilg))]
    (.MarkLabel ilg label)
    (assoc-in context [::labels data] label)))

(defmethod emit-data ::catch
  [{:keys [::ilg] :as context}
   {:keys [::catch ::body] :as data}]
  (.BeginCatchBlock ilg catch)
  (emit! context body))

(defmethod emit-data ::exception
  [{:keys [::ilg] :as context}
   {:keys [::body] :as data}]
  (.BeginExceptionBlock ilg)
  (let [context* (emit! context body)]
    (.EndExceptionBlock ilg)
    context*))

(defmethod emit-data ::finally
  [{:keys [::ilg] :as context}
   {:keys [::body] :as data}]
  (.BeginFinallyBlock ilg)
  (emit! context body))

(defmethod emit-data ::opcode
  [{:keys [::assembly-builders ::module-builders ::type-builders ::method-builders
           ::labels ::locals ::fields ::ilg ::type-builder]
    :or {labels {}
         fields {}
         locals {}
         assembly-builders {}
         module-builders {}
         type-builders {}
         method-builders {}}
    :as context}
   {:keys [::opcode ::argument] :as data}]
  (cond
    (nil? argument)
    (do
      (.Emit ilg opcode)
      context)
    
    (labels argument)
    (do (.Emit ilg opcode (labels argument))
      context)
    
    (fields argument)
    (do (.Emit ilg opcode (fields argument))
      context)
    
    (locals argument)
    (do (.Emit ilg opcode (locals argument))
      context)
    
    (assembly-builders argument) 
    (do (.Emit ilg opcode (assembly-builders argument))
      context)
    
    (module-builders argument)
    (do (.Emit ilg opcode (module-builders argument))
      context)
    
    (type-builders argument)
    (do (.Emit ilg opcode (type-builders argument))
      context)
    
    (method-builders argument)
    (do (.Emit ilg opcode (method-builders argument))
      context)
    
    ;; no emit-data! here because we dont mark the label?
    (::label argument)
    (let [^Label label (.DefineLabel ilg)]
      (.Emit ilg opcode label)
      (assoc-in context [::labels argument] label))
    
    (::field argument)
    (let [{:keys [::ilg ::fields] :as context*}
          (emit-data context argument)]
      (.Emit ilg opcode ^FieldBuilder (fields argument))
      context*)
    
    (::local argument)
    (let [{:keys [::ilg ::locals] :as context*}
          (emit-data context argument)]
      (.Emit ilg opcode ^LocalBuilder (locals argument))
      context*)
    
    (::assembly argument)
    (let [{:keys [::ilg ::assembly-builders] :as context*}
          (emit-data context argument)]
      (.Emit ilg opcode (assembly-builders argument))
      context*)
    
    (::module argument)
    (let [{:keys [::ilg ::module-builders] :as context*}
          (emit-data context argument)]
      (.Emit ilg opcode (module-builders argument))
      context*)
    
    (::type argument)
    (let [{:keys [::ilg ::type-builders] :as context*}
          (emit-data context argument)]
      (.Emit ilg opcode ^TypeBuilder (type-builders argument))
      context*)
    
    (::method argument)
    (let [{:keys [::ilg ::method-builders] :as context*}
          (emit-data context argument)]
      (.Emit ilg opcode ^MethodBuilder (method-builders argument))
      context*)
    
    :else
    (do
      (.Emit ilg opcode argument)
      context)))

;; OR
;; if argument in references?
;; (.Emit ilg opcode (references argument))
;; if argument is hashmap
;; (emit! argument)
;; (.Emit ilg opcode ???)


;;;; missing:
;; BeginExceptFilterBlock
;; BeginFaultBlock
;; BeginScope

(defmethod emit-data ::begin
  [{:keys [::ilg ::assembly-builder ::generic-type-parameters ::module-builder ::type-builder] :as context} {:keys [::begin ::argument]}]
  (case begin
    ;; builders
    :assembly           (assoc context
                          ::assembly-builder
                          (.. AppDomain CurrentDomain
                              (DefineDynamicAssembly
                                (argument ::name)
                                (argument ::access))))
    
    :module             (assoc context
                          ::module-builder
                          (. (clojure.core/or assembly-builder
                                 (.AssemblyBuilder clojure.lang.Compiler/EvalContext))
                             (DefineDynamicModule (argument ::name))))
    
    :type               (let [mb (clojure.core/or module-builder
                                     (-> clojure.lang.Compiler/EvalContext
                                         .AssemblyBuilder
                                         .GetModules ;; TODO better way to get eval module?
                                         first))
                              tb (if (argument ::super)
                                   (. mb (DefineType
                                                       (argument ::name)
                                                       (argument ::attributes)
                                                       (argument ::super)))
                                   (. mb (DefineType
                                                       (argument ::name)
                                                       (argument ::attributes))))
                              generic-parameter-builders
                              (when (argument ::generic-parameters)
                                (.DefineGenericParameters tb (into-array String (map str (argument ::generic-parameters)))))
                              generic-parameter-map
                              (apply hash-map
                                     (interleave
                                       (argument ::generic-parameters)
                                       generic-parameter-builders))]
                          (doseq [interface (argument ::interfaces)]
                            (.AddInterfaceImplementation tb interface))
                          (assoc context
                            ::generic-type-parameters generic-parameter-map
                            ::type-builder tb
                            ::fields {}))
    
    ;; TODO too simplistic? hang on to method-builder? 
    :method             (let [method-builder
                              (. type-builder (DefineMethod
                                                (argument ::name)
                                                (argument ::attributes)
                                                (clojure.core/or (generic-type-parameters
                                                      (argument ::return-type))
                                                    (argument ::return-type))
                                                (into-array Type
                                                  (map #(clojure.core/or (generic-type-parameters %) %)
                                                       (argument ::parameter-types)))))]
                          (assoc context
                            ::ilg (. method-builder GetILGenerator)
                            ::method-builder method-builder
                            ::labels {}
                            ::locals {}))
    
    :constructor        (assoc context
                          ::ilg
                          (.. type-builder (DefineConstructor
                                             (argument ::attributes)
                                             (argument ::calling-convention)
                                             (into-array Type
                                                (map #(clojure.core/or (generic-type-parameters %) %)
                                                     (argument ::parameter-types))))
                              GetILGenerator)
                          ::labels {}
                          ::locals {})
    
    ;; ilg
    :catch              (do (.BeginCatchBlock ilg argument)
                          context)
    :exception-filtered (do (.BeginExceptFilterBlock ilg)
                          context)
    :exception          (do (.BeginExceptionBlock ilg)
                          context)
    :fault              (do (.BeginFaultBlock ilg)
                          context)
    :finally            (do (.BeginFinallyBlock ilg)
                          context)
    :scope              (do (.BeginScope ilg)
                          context)))

(defmethod emit-data ::end
  [{:keys [::ilg ::assembly-builder ::type-builder] :as context} {:keys [::end]}]
  (case end
    ;; builders
    ;; TODO always save?
    :assembly    (do
                   (.Save assembly-builder
                          (str (.. assembly-builder GetName Name) ".dll"))
                   context)
    
    :module      (dissoc context ::module-builder)
    
    :type        (do (. type-builder CreateType)
                   (dissoc context ::type-builder ::fields ::generic-type-parameters))
    
    :method      (dissoc context ::ilg ::labels ::locals ::method-builder)
    
    :constructor (dissoc context ::ilg ::labels ::locals)
    
    ;; ilg
    :exception   (do (.EndExceptionBlock ilg)
                   context)
    :catch       context
    :finally     context
    :scope       (do (.EndScope ilg)
                   context))
  )

;;; constructor functions
(defn assembly
  ([name]
   (assembly name nil))
  ([name body]
   (assembly name AssemblyBuilderAccess/RunAndSave body))
  ([name access body]
   {::assembly name
    ::access access
    ::body body}))

(defn module 
  ([name]
   (module name nil))
  ([name body]
   {::module name
    ::body body}))

(defn type
  ([name]
   (type name nil))
  ([name body]
   (type name [] body))
  ([name interfaces body]
   (type name TypeAttributes/Public interfaces body))
  ([name attributes interfaces body]
   (type name attributes interfaces System.Object body))
  ([name attributes interfaces super body]
   (type name attributes interfaces super nil body))
  ([name attributes interfaces super generic-parameters body]
   {::type name 
    ::attributes attributes 
    ::interfaces interfaces 
    ::super super 
    ::generic-parameters generic-parameters
    ::body body}))

(defn method
  ([name return-type parameter-types body]
   (method name MethodAttributes/Public return-type parameter-types body))
  ([name attributes return-type parameter-types body]
   {::method name
    ::attributes attributes
    ::return-type return-type
    ::parameter-types parameter-types
    ::body body}))

;; try block
(defn exception
  ([] (exception nil))
  ([body] {::exception body}))

(defn catch
  ([t] (catch t nil))
  ([t body] {::catch t ::body body}))

(defn finally
  ([] (finally nil))
  ([body] {::finally body}))


(defn pinvoke-method
  ([name dll-name return-type parameter-types]
   (pinvoke-method name dll-name name return-type parameter-types))
  ([name dll-name entry-name return-type parameter-types]
   (pinvoke-method name dll-name entry-name (enum-or MethodAttributes/Public MethodAttributes/Static) return-type parameter-types))
  ([name dll-name entry-name attributes return-type parameter-types]
   (pinvoke-method name dll-name entry-name attributes return-type parameter-types CallingConventions/Standard))
  ([name dll-name entry-name attributes return-type parameter-types calling-convention]
   (pinvoke-method name dll-name entry-name attributes return-type parameter-types calling-convention CallingConvention/Winapi))
  ([name dll-name entry-name attributes return-type parameter-types calling-convention native-calling-convention]
   (pinvoke-method name dll-name entry-name attributes return-type parameter-types calling-convention native-calling-convention CharSet/Auto))
  ([name dll-name entry-name attributes return-type parameter-types calling-convention native-calling-convention native-char-set]
   (pinvoke-method name dll-name entry-name attributes return-type parameter-types calling-convention MethodImplAttributes/PreserveSig native-calling-convention native-char-set))
  ([name dll-name entry-name attributes return-type parameter-types calling-convention method-impl-attributes native-calling-convention native-char-set]
   {::pinvoke-method name
    ::dll-name dll-name 
    ::entry-name entry-name 
    ::attributes attributes 
    ::calling-convention calling-convention 
    ::return-type return-type 
    ::parameter-types parameter-types 
    ::method-impl-attributes method-impl-attributes 
    ::native-calling-convention native-calling-convention 
    ::native-char-set native-char-set}))

(defn constructor
  ([parameter-types body] (constructor CallingConventions/Standard parameter-types body))
  ([calling-convention parameter-types body]
   (constructor MethodAttributes/Public calling-convention parameter-types body))
  ([attributes calling-convention parameter-types body]
   {::constructor parameter-types
    ::attributes attributes
    ::calling-convention calling-convention
    ::parameter-types parameter-types ;; TODO duplicate?
    ::body body}))

(defn local
  ([] (local System.Object))
  ([t] (local t (gensym "local")))
  ([t i] {::local i ::type t}))

(defn field
  ([] (field System.Object))
  ([t] (field t (gensym "field")))
  ([t i] (field t i (enum-or FieldAttributes/InitOnly FieldAttributes/Private)))
  ([t i attr] {::field i ::type t ::attributes attr}))

(defn label
  ([] (label (gensym "label")))
  ([i] {::label i}))

(defmacro make-opcode-constructor-fns
  "Generate mage constructor functions for all MSIL bytecode"
  []
  (->> (.GetFields OpCodes)
       (map (fn [f]
              `(defn ~(-> f .Name string/lower-case (string/replace "_" "-") symbol)
                 ([] {::opcode ~(symbol "OpCodes" (.Name f) )})
                 ([arg#] {::opcode ~(symbol "OpCodes" (.Name f) ) ::argument arg#}))))
       (list* 'do)))
(make-opcode-constructor-fns)
