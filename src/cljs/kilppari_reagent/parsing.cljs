(ns kilppari-reagent.parsing
  (:require [instaparse.core :as insta :refer-macros [defparser]]
            [cljs.core.match :refer-macros [match]]))

(def as-and-bs
  (insta/parser
   "S = AB*
    AB = A B
    A = 'a'+
    B = 'b'+"))

(defparser turtle-parser
  "S = line* <end?>
<line> = (<comment> <eol>) | <empty-line> | (<whitespace*> command <comment?> <eol>)
<empty-line> = <whitespace*> <eol>
<command> = move | turn-right | turn-left | repeat | pen
<whitespace> = ' ' | '\t'
<comment> = <whitespace>* <#'//[ a-zA-Z0-9]*'>
pen = <'pen '> ('up' | 'down')
move = <'move '> n
turn-right = <'turn-right '> n
turn-left = <'turn-left '> n
repeat = <'repeat '> n <comment?> <eol> line* <end>
end = <whitespace*> <'end'>
n = #'[012345679]+'
eol = '\n'")

(defn str->int [s]
  (js/parseInt s))

(defn mangle-line [line]
  (match line
    [:move [:n n]] [:move (str->int n)]
    [:turn-right [:n n]] [:turn-right (str->int n)]
    [:turn-left [:n n]] [:turn-left (str->int n)]
    [:pen x] [:pen (keyword x)]
    [:repeat [:n n] & x] [:repeat (str->int n) (into [] (map mangle-line x))]
    [:end] [:end]
    :else [:FAIL]))

(defn parse-turtle [str]
  (let [res (turtle-parser str)]
    (when (= (first res) :S)
      (into [] (->> (rest res)
                    (map mangle-line))))))
