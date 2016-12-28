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
      (::opcode          m) ::opcode
      (::label           m) ::label
      (::local           m) ::local
      (::field           m) ::field
      (::pinvoke-method  m) ::pinvoke-method
      (::begin           m) ::begin
      (::end             m) ::end
      :else                 [context m])))

(defmethod emit-data ::opcode
  [{:keys [::ilg ::type-builder ::labels ::locals ::fields] :as context} {:keys [::opcode ::argument]}]
  (cond 
    (nil? argument)     (do (.Emit ilg opcode)
                          context)
    
    (labels argument)  (do (.Emit ilg opcode (labels argument))
                         context)
    
    (fields argument)  (do (.Emit ilg opcode (fields argument))
                         context)
    
    (locals argument)  (do (.Emit ilg opcode (locals argument))
                         context)
    
    (::label argument)  (let [^Label label (.DefineLabel ilg)]
                          (.Emit ilg opcode label)
                          (assoc-in context [::labels argument] label))
        
    (::field argument)  (let [^FieldBuilder field (.DefineField type-builder (str (::field argument)) (::type argument) (::attributes argument))]
                           (.Emit ilg opcode field)
                           (assoc-in context [::fields argument] field))
    
    (::local argument)  (let [^LocalBuilder local (.DeclareLocal ilg (::type argument))]
                          (if-let [name (::name argument)] (.SetLocalSymInfo local name))
                          (.Emit ilg opcode local)
                          (assoc-in context [::locals argument] local))
    
    :else               (do (.Emit ilg opcode argument)
                          context)))

(defmethod emit-data ::label
  [{:keys [::ilg ::labels] :as context} labelmap]
  (let [^Label label (clojure.core/or (labels labelmap)
                         (.DefineLabel ilg))]
    (.MarkLabel ilg label)
    (assoc-in context [::labels labelmap] label)))

(defmethod emit-data ::field
  [{:keys [::type-builder] :as context} {:keys [::field ::type ::attributes] :as fieldmap}]
  (let [^FieldBuilder field (.DefineField type-builder (str field) type attributes)]
    (assoc-in context [::fields fieldmap] field)))

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
           ::native-char-set]}]
  (let [^MethodBuilder mb
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
    (.SetImplementationFlags 
      mb
      (enum-or (.GetMethodImplementationFlags mb)
               method-impl-attributes)))
  context)

(defmethod emit-data ::local
  [{:keys [::ilg] :as context} localmap]
  (let [^LocalBuilder local (.DeclareLocal ilg (::type localmap))]
    (if-let [name (::name localmap)] (.SetLocalSymInfo local name))
    (assoc-in context [::locals localmap] local)))

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

(defn emit!
  ([stream] (emit! {} stream))
  ([initial-ctx stream]
   (reduce (fn [ctx s] (emit-data ctx s))
           initial-ctx
           (->> stream flatten (remove nil?)))))

;;; constructor functions
(defn assembly
  ([name body] (assembly name AssemblyBuilderAccess/RunAndSave body))
  ([name access body]
   [{::begin :assembly
     ::argument {::name (AssemblyName. name) ::access access}}
    body
    ;; TODO end assembly options?
    {::end :assembly}]))

(defn module 
  ([name body]
   [{::begin :module ::argument {::name name}}
    body
    {::end :module}]))

(defn type
  ([name body]
   (type name [] body))
  ([name interfaces body]
   (type name TypeAttributes/Public interfaces body))
  ([name attributes interfaces body]
   (type name attributes interfaces System.Object body))
  ([name attributes interfaces super body]
   (type name attributes interfaces super nil body))
  ([name attributes interfaces super generic-parameters body]
   [{::begin :type
     ::argument {::name name ::attributes attributes ::interfaces interfaces ::super super ::generic-parameters generic-parameters}}
    body
    {::end :type}]))

(defn method
  ([name return-type parameter-types body]
   (method name MethodAttributes/Public return-type parameter-types body))
  ([name attributes return-type parameter-types body]
   [{::begin :method ::argument {::name name
                                 ::attributes attributes
                                 ::return-type return-type
                                 ::parameter-types parameter-types}}
    body
    {::end :method}]))

(defn exception
  ([] (exception nil))
  ([body] [{::begin :exception} body {::end :exception}]))

(defn catch
  ([t] (catch t nil))
  ([t body] [{::begin :catch ::argument t} body {::end :catch}]))

(defn finally
  ([] (finally nil))
  ([body] [{::begin :finally} body {::end :finally}]))

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
   [{::begin :constructor ::argument {::attributes attributes
                                      ::calling-convention calling-convention
                                      ::parameter-types (into-array System.Type parameter-types)}}
    body
    {::end :constructor}]))

(defn local
  ([] (local System.Object))
  ([t] (local t (gensym "local")))
  ([t i] {::local i ::type t}))

(defn field
  ([] (field System.Object))
  ([t] (field t (enum-or FieldAttributes/InitOnly FieldAttributes/Private)))
  ([t attr] (field t attr (gensym "field")))
  ([t attr i] {::field i ::type t ::attributes attr}))

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