(ns kilppari-reagent.parsing
  (:require [instaparse.core :as insta :refer-macros [defparser]]
            [cljs.core.match :refer-macros [match]]))

(defparser turtle-parser
  "S = line* <end?>
<line> = (<comment> <eol>) | <empty-line> | (<whitespace*> command <comment?> <eol>)
<empty-line> = <whitespace*> <eol>
<command> = let | move | turn-right | turn-left | repeat | pen | function | call
<whitespace> = ' ' | '\t'
<comment> = <whitespace*> <#'//[ a-zA-Z0-9]*'>
let = <'let '> var-name n
pen = <'pen '> ('up' | 'down')
move = <'move '> n
turn-right = <'turn-right '> n
turn-left = <'turn-left '> n
repeat = <'repeat '> n <comment?> <eol> line* <end>
function = <'fn '> fn-name args? <comment?> <eol> line* <endfn>
args = <whitespace+> #'[a-zA-Z0-9]+'
call = <'call '> fn-name args?
end = <whitespace*> <'end'>
endfn = <whitespace*> <'endfn'>
fn-name = #'[a-zA-Z0-9]+'
var-name = #'[a-zA-Z0-9]+'
n = #'[012345679]+'
eol = '\n'")

(defn str->int [s]
  (js/parseInt s))

(defn arg-val [x]
  {:type :value :value x})

(defn args-val [xs]
  (into [] (map arg-val xs)))

;; TODO: I missed there's a insta/transform function
(defn process-line [line]
  (match line
    [:move [:n n]] [:move {:args (arg-val (str->int n))}]
    [:turn-right [:n n]] [:turn-right {:args (arg-val (str->int n))}]
    [:turn-left [:n n]] [:turn-left {:args (arg-val (str->int n))}]
    [:pen x] [:pen {:args (arg-val (keyword x))}]

    ;; broken from here
    [:let [:var-name v] [:n n]] [:let {:args (args-val [v (str->int n)])}]
    [:repeat [:n n] & x] [:repeat {:args (args-val [(str->int n) (into [] (map process-line x))])}]
    [:function
     [:fn-name fn-name]
     [:args args]
     & x] [:function {:args (args-val [fn-name
                                       {:args args
                                        :instructions
                                        (into [] (map process-line x))}])}]
    [:function
     [:fn-name fn-name]
     & x] [:function {:args (args-val [fn-name
                                       {:args nil
                                        :instructions
                                        (into [] (map process-line x))}])}]
    [:call [:fn-name fn-name] [:args args]] [:call {:args (args-val [fn-name (str->int args)])}]
    [:call [:fn-name fn-name]] [:call {:args (arg-val fn-name)}]
    [:end] [:end {:args []}]
    :else line))

(defn parse-turtle [str]
  (let [res (insta/parse turtle-parser str)]
    (if (= (first res) :S)
      [:success (->> (rest res)
                     (map process-line)
                     (into []))]
      [:failure (pr-str res)])))
