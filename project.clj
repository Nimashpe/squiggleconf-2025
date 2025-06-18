(defproject squiggleconf-2025 "0.1.0"
  :description "SquiggleConf 2025 Notes and Examples"
  :url "https://2025.squiggleconf.com"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/tools.cli "1.0.219"]
                 [org.clojure/data.json "2.4.0"]]
  :main ^:skip-aot squiggleconf.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}
             :dev {:dependencies [[org.clojure/test.check "1.1.1"]
                                  [midje "1.10.9"]]
                   :plugins [[lein-midje "3.2.2"]
                             [lein-cljfmt "0.9.2"]
                             [lein-kibit "0.1.8"]
                             [lein-ancient "1.0.0-RC3"]
                             [jonase/eastwood "1.4.0"]]}}
  :plugins [[lein-cljfmt "0.9.2"]]
  :cljfmt {:indents {#".*" [[:inner 0]]}})