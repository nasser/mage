(ns mage.il
  (:refer-clojure :exclude [pop rem and or not])
  (:require [clojure.string :as string])
  (:import [System.Reflection.Emit LocalBuilder Label ILGenerator OpCodes]))

(defmulti emit-data
  "Emit bytecode for symbolic data object m. Internal."
  (fn [context m]
    (cond
      (::opcode m) ::opcode
      (::label  m) ::label
      (::local  m) ::local
      (::begin  m) ::begin
      (::end    m) ::end)))

(defmethod emit-data ::opcode
  [{:keys [::ilg ::labels ::locals] :as context} {:keys [::opcode ::argument]}]
  (cond 
    (nil? argument)     (do (.Emit ilg opcode)
                          context)
    
    (labels argument)  (do (.Emit ilg opcode (labels argument))
                         context)
    
    (locals argument)  (do (.Emit ilg opcode (locals argument))
                         context)
    
    (::label argument)  (let [^Label label (.DefineLabel ilg)]
                          (.Emit ilg opcode label)
                          (assoc-in context [::labels argument] label))
    
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

(defmethod emit-data ::local
  [{:keys [::ilg] :as context} localmap]
  (let [^LocalBuilder local (.DeclareLocal ilg (::type localmap))]
    (if-let [name (::name localmap)] (.SetLocalSymInfo local name))
    (assoc-in context [::locals localmap] local)))

(defmethod emit-data ::begin
  [{:keys [::ilg] :as context} {:keys [::begin ::argument]}]
  (case begin
    :catch (.BeginCatchBlock ilg argument)
    :exception-filtered (.BeginExceptFilterBlock ilg)
    :exception (.BeginExceptionBlock ilg)
    :fault (.BeginFaultBlock ilg)
    :finally (.BeginFinallyBlock ilg)
    :scope (.BeginScope ilg))
  context)

(defmethod emit-data ::end
  [{:keys [::ilg] :as context} {:keys [::end]}]
  (case end
    :exception (.EndExceptionBlock ilg)
    :scope (.EndScope ilg))
  context)

(defn emit!
  "Emit actual bytecode into an ILGenerator"
  [^ILGenerator ilg stream]
  (reduce (fn [ctx s]
            (emit-data ctx s))
          {::ilg ilg ::locals {} ::labels {}}
          (->> stream flatten (remove nil?))))

;;; constructor functions

(defn local
  ([] (local System.Object))
  ([t] (local t (gensym "local")))
  ([t i] {::local i ::type t}))

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