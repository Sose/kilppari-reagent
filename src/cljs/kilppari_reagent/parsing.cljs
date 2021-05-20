(ns kilppari-reagent.parsing
  (:require [instaparse.core :as insta :refer-macros [defparser]]
            [cljs.core.match :refer-macros [match]]))

(defparser turtle-parser
  "S = line* <end?>
<line> = (<comment> <eol>) | <empty-line> | (<whitespace*> command <comment?> <eol>)
<empty-line> = <whitespace*> <eol>
<command> = move | turn-right | turn-left | repeat | pen | function | call
<whitespace> = ' ' | '\t'
<comment> = <whitespace>* <#'//[ a-zA-Z0-9]*'>
pen = <'pen '> ('up' | 'down')
move = <'move '> n
turn-right = <'turn-right '> n
turn-left = <'turn-left '> n
repeat = <'repeat '> n <comment?> <eol> line* <end>
function = <'fn '> fn-name <comment?> <eol> line* <endfn>
call = <'call '> fn-name
end = <whitespace*> <'end'>
endfn = <whitespace*> <'endfn'>
fn-name = #'[a-zA-Z0-9]+'
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
    [:function [:fn-name fn-name] & x] [:function fn-name (into [] (map mangle-line x))]
    [:call [:fn-name fn-name]] [:call fn-name]
    [:end] [:end]
    :else line))

(defn parse-turtle [str]
  (let [res (turtle-parser str)]
    (when (= (first res) :S)
      (->> (rest res)
           (map mangle-line)
           (into [])))))
