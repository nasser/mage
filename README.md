MAGE
====
Symbolic MSIL bytecode generation for ClojureCLR

Quick Example
-------------
```clojure
(require '[mage.core :as il])

(il/emit!
  (il/assembly "Example"
    [(il/module "Example.dll"
      [(il/type "ExampleType"
        [(il/method
          "AddIntegers"
          Int32 [Int32 Int32]
          [(il/ldarg-1)
           (il/ldarg-2)
           (il/sub)
           (il/ret)])])])]))

(.AddIntegers (ExampleType.) 5 6)
;; 11
```

Status
------
Very experimental and pre-alpha. Usage is reserved for the adventurous.

Overview
--------
MAGE wraps the entire CLR [`System.Reflection.Emit` namespace](https://msdn.microsoft.com/en-us/library/system.reflection.emit(v=vs.110).aspx) in a [gamma](https://github.com/kovasb/gamma)-style symbolic compiler. The goal is a functional, composable, data- and REPL-driven bytecode emission framework for the CLR. A tree of symbolic [MSIL bytecode](https://en.wikipedia.org/wiki/Common_Intermediate_Language) is built as Clojure data, and passed to an `emit!` function to generate usable MSIL bytecode on disk as a DLL file or directly in memory as C# types.

### Symbolic Bytecode
At the heart of MAGE is the symbolic representation of MSIL. Representing bytecode as persistent data allows it to be manipulated functionally before anything is generated. MAGE provides constructor functions that produce the maps that the emission logic expects.


#### Opcodes
The most basic forms are [MSIL opcodes](https://msdn.microsoft.com/en-us/library/system.reflection.emit.opcodes(v=vs.110).aspx)

```clojure
(il/ldnull)                ;; {:mage.core/opcode ldnull}
(il/add)                   ;; {:mage.core/opcode add}
(il/ldstr "Hello, World!") ;; {:mage.core/opcode ldstr, :mage.core/argument "Hello, World!"}
```

Opcodes optionally take a single argument. MSIL is a stack language, so all intermediate data is pushed and popped off of a stack. Adding the integers 13 and 42 would look like

```clojure
[(il/ldc-i4 13) ;; [{:mage.core/opcode ldc.i4, :mage.core/argument 13}
 (il/ldc-i4 42) ;;  {:mage.core/opcode ldc.i4, :mage.core/argument 42}
 (il/add)]      ;;  {:mage.core/opcode add}]
 ```

This will

1. **l**oa**d** **c**onstant **i**nteger of **4** bytes of value 13 on the stack
2. **l**oa**d** **c**onstant **i**nteger of **4** bytes of value 42 on the stack
3. Pop two elements off the stack, **add** them, and push the result back on the stack

Another property of MSIL being a stack language is that your symbolic bytecode is flat, and not a tree. For convenience, MAGE allows you to generate nested vectors of maps, and will `flatten` them before final emission.

#### Locals
Like gamma, MAGE supports local variables but uses Clojure `let` bindings and map equality semantics instead of introducing new constructs. Every reference to `i` in the example below will be the same local variable in the final bytecode.

```clojure
(let [i (il/local Int32)] ;; [{:mage.core/opcode ldc.i4, :mage.core/argument 8}
  [(il/ldc-i4 8)          ;;  {:mage.core/opcode stloc,
   (il/stloc i)           ;;   :mage.core/argument
   (il/ldc-i4 32)         ;;   {:mage.core/local local3112, :mage.core/type System.Int32}}
   (il/ldloc i)           ;;  {:mage.core/opcode ldc.i4, :mage.core/argument 32}
   (il/add)               ;;  {:mage.core/opcode ldloc,
   (il/ldloc i)           ;;   :mage.core/argument
   (il/mul)])             ;;   {:mage.core/local local3112, :mage.core/type System.Int32}}
                          ;;  {:mage.core/opcode add}
                          ;;  {:mage.core/opcode ldloc,
                          ;;   :mage.core/argument
                          ;;   {:mage.core/local local3112, :mage.core/type System.Int32}}
                          ;;  {:mage.core/opcode mul}]
```

#### Labels
MSIL uses labels and branching to implement loops and conditionals. This is an infinite loop printing out the numbers from 0 upwards.

```clojure
(let [start (il/label)         ;; [{:mage.core/opcode ldc.i4, :mage.core/argument 0}
      i (il/local Int32)]      ;;  {:mage.core/opcode stloc,
  [(il/ldc-i4 0)               ;;   :mage.core/argument
   (il/stloc i)                ;;   {:mage.core/local local3130, :mage.core/type System.Int32}}
   start                       ;;  {:mage.core/label label3129}
   (il/ldloc i)                ;;  {:mage.core/opcode ldloc,
   (il/ldc-i4-1)               ;;   :mage.core/argument
   (il/add)                    ;;   {:mage.core/local local3130, :mage.core/type System.Int32}}
   (il/stloc i)                ;;  {:mage.core/opcode ldc.i4.1}
   (il/call                    ;;  {:mage.core/opcode add}
     (il/find-method           ;;  {:mage.core/opcode stloc,
       System.Console          ;;   :mage.core/argument
       "WriteLine"             ;;   {:mage.core/local local3130, :mage.core/type System.Int32}}
       Int32))                 ;;  {:mage.core/opcode call,
   (il/br start)])             ;;   :mage.core/argument #<MonoMethod Void WriteLine(Int32)>}
                               ;;  {:mage.core/opcode br,
                               ;;   :mage.core/argument {:mage.core/label label3129}}]
```

#### Methods, Types, Modules, and Assemblies
Opcodes cannot exist on their own. They must be part of the body of a method, which must be part of a type, which must be in a module, which must be in an assembly.

```clojure
(il/assembly "Example"       ;; [{:mage.core/begin :assembly,
  (il/module "Example.dll"   ;;   :mage.core/argument
    (il/type "ExampleType"   ;;   {:mage.core/name #<AssemblyName Example>,
      (il/method             ;;    :mage.core/access RunAndSave}}
        "AddIntegers"        ;;  [{:mage.core/begin :module,
        Int32 [Int32 Int32]  ;;    :mage.core/argument {:mage.core/name "Example.dll"}}
        [(il/ldarg-1)        ;;   [{:mage.core/begin :type,
         (il/ldarg-2)        ;;     :mage.core/argument
         (il/add)]))))       ;;     {:mage.core/name "ExampleType",
                             ;;      :mage.core/attributes Public,
                             ;;      :mage.core/interfaces [],
                             ;;      :mage.core/super System.Object}}
                             ;;    [{:mage.core/begin :method,
                             ;;      :mage.core/argument
                             ;;      {:mage.core/name "AddIntegers",
                             ;;       :mage.core/attributes Public,
                             ;;       :mage.core/return-type System.Int32,
                             ;;       :mage.core/parameter-types [System.Int32, System.Int32]}}
                             ;;     [{:mage.core/opcode ldarg.1}
                             ;;      {:mage.core/opcode ldarg.2}
                             ;;      {:mage.core/opcode add}]
                             ;;     {:mage.core/end :method}]
                             ;;    {:mage.core/end :type}]
                             ;;   {:mage.core/end :module}]
                             ;;  {:mage.core/end :assembly}]
```

### Emission

`emit!` will flatten the given tree and emit MSIL bytecode to both disk as a DLL file and into memory. The resulting DLL file can be found at the CLR execution root directory, and the generated types can be used right away.

```clojure
(il/emit!
  (il/assembly "Example"
    (il/module "Example.dll"
      (il/type "ExampleType"
        (il/method
          "AddIntegers"
          Int32 [Int32 Int32]
          [(il/ldarg-1)
           (il/ldarg-2)
           (il/sub)
           (il/ret)])))))

(.AddIntegers (ExampleType.) 5 6)
;; 11
```

Future versions of MAGE will have a more nuanced `emit!` with better control over where the bytecode goes.

Rationale and History
---------------------
A big part of the [Arcadia](https://github.com/arcadia-unity/Arcadia) project is hacking the ClojureCLR compiler to improve performance, fix bugs, or support Unity's various restricted export platforms. For much of the first year of the project, this hacking took place at the level of the C# source code of the compiler, which was found to be slow, error prone, and dangerous.

In pursuit of a rapid, iterative, safe, and well reasoned approach to compiler hacking, and in reaction to conversations with fellow Kitchen Table Coder [Kovas Boguta](https://github.com/kovasb), MAGE was designed. In his talks about gamma, Kovas has said that "shader coding should be normal Clojure coding." In that same spirit, MAGE strives to make compiler development more like normal Clojure development. With a REPL, persistent data, and functional programming, building a compiler's emitter is a much more reasonable project.

MAGE was built to support MAGIC, which specifically compiles Clojure forms into MSIL bytecode. MAGE is agnostic to Clojure's semantics, and wraps the CLR's emission machinery in a general way.

Name
----
MAGE stands for Morgan And Grand Emitter. It is named after the [Morgan Avenue and Grand Street intersection](https://www.google.com/maps/place/Grand+St+%26+Morgan+Ave,+Brooklyn,+NY+11237/@40.7133714,-73.9348001,17z/data=!3m1!4b1!4m2!3m1!1s0x89c25eab5ea3b021:0x77aaab63f0e3d135) in Brooklyn, the location of the [Kitchen Table Coders](http://kitchentablecoders.com/) studio where the library was developed.

Legal
-----
Copyright © 2015 Ramsey Nasser

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at

```
http://www.apache.org/licenses/LICENSE-2.0
```

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.