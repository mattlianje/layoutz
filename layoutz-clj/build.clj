(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'xyz.matthieucourt/layoutz)
(def version "0.1.2")
(def class-dir "target/classes")
(def jar-file (format "target/%s-%s.jar" (name lib) version))
(def src-dirs ["src"])

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar [_]
  (clean nil)
  (let [basis (b/create-basis {:project "deps.edn"})]
    (b/write-pom {:class-dir class-dir
                  :lib lib
                  :version version
                  :basis basis
                  :src-dirs src-dirs
                  :pom-data [[:licenses
                              [:license
                               [:name "Apache-2.0"]
                               [:url "https://www.apache.org/licenses/LICENSE-2.0"]]]]
                  :scm {:url "https://github.com/mattlianje/layoutz"
                        :connection "scm:git:git://github.com/mattlianje/layoutz.git"
                        :developerConnection "scm:git:ssh://git@github.com/mattlianje/layoutz.git"
                        :tag (str "v" version)}})
    (b/copy-dir {:src-dirs src-dirs :target-dir class-dir})
    (b/jar {:class-dir class-dir :jar-file jar-file})))

(defn deploy [_]
  (jar nil)
  ((requiring-resolve 'deps-deploy.deps-deploy/deploy)
   {:installer :remote
    :artifact (b/resolve-path jar-file)
    :pom-file (b/pom-path {:lib lib :class-dir class-dir})}))
