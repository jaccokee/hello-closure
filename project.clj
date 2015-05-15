(defproject hello-world "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [joda-time/joda-time "2.1"]]
  :main ^:skip-aot hello-world.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
