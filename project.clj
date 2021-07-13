(defproject bau "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [funcool/datoteka "1.2.0"]
                 [borkdude/sci "0.1.1-alpha.10"]]
  :main ^:skip-aot bau.core
  :profiles {:uberjar {:aot :all}}
  :repl-options {:init-ns bau.core})
