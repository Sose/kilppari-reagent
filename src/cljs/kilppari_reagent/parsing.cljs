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

;; TODO: I missed there's a insta/transform function
(defn process-line [line]
  (match line
    [:move [:n n]] [:move (str->int n)]
    [:turn-right [:n n]] [:turn-right (str->int n)]
    [:turn-left [:n n]] [:turn-left (str->int n)]
    [:pen x] [:pen (keyword x)]
    [:let [:var-name v] [:n n]] [:let v (str->int n)]
    [:repeat [:n n] & x] [:repeat (str->int n) (into [] (map process-line x))]
    [:function [:fn-name fn-name] & x] [:function fn-name
                                        {:instructions (into [] (map process-line x))}]
    [:function [:fn-name fn-name] [:args args] & x] [:function fn-name
                                                     {:args args
                                                      :instructions (into [] (map process-line x))}]
    [:call [:fn-name fn-name]] [:call fn-name]
    [:call [:fn-name fn-name] [:args args]] [:call fn-name (str->int args)]
    [:end] [:end]
    :else line))

(defn parse-turtle [str]
  (let [res (turtle-parser str)]
    (when (= (first res) :S)
      (->> (rest res)
           (map process-line)
           (into [])))))
