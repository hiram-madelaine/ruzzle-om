(defproject ruzzle-om "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2173"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]
                 [om "0.5.2-SNAPSHOT"]]

  :plugins [[lein-cljsbuild "1.0.2"]]

  :source-paths ["src"]

  :cljsbuild {
    :builds [{:id "ruzzle-om"
              :source-paths ["src"]
              :compiler {
                :output-to "ruzzle_om.js"
                :output-dir "out"
                :optimizations :none
                :source-map true}}]})
