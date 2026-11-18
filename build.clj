(ns build
  (:refer-clojure :exclude [test])
  (:require [clojure.tools.build.api :as b]))

(def lib 'io.dominic/specel)
(def version "0.1.0-SNAPSHOT")
(def class-dir "target/classes")
(def jar-file (format "target/%s-%s.jar" (name lib) version))
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn clean
  [_]
  (b/delete {:path "target"}))

(defn test
  "Runs the test suite. Throws if any tests fail."
  [_]
  (let [test-basis (b/create-basis {:project "deps.edn" :aliases [:test]})
        {:keys [exit]} (b/process (b/java-command {:basis test-basis
                                                   :main 'clojure.main
                                                   :main-args ["-m" "specel.test-runner"]}))]
    (when-not (zero? exit)
      (throw (ex-info "Tests failed" {:exit exit})))))

(defn jar
  "Builds the source JAR and POM."
  [_]
  (clean nil)
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis @basis
                :src-pom :none
                :src-dirs ["src"]
                :pom-data [[:description "Hegel generators for clojure.spec."]
                           [:url "https://github.com/dkarvik/specel"]
                           [:scm
                            [:url "https://github.com/dkarvik/specel"]]
                           [:licenses
                            [:license
                             [:name "Eclipse Public License 1.0"]
                             [:url "https://www.eclipse.org/legal/epl-v10.html"]
                             [:distribution "repo"]]]]})
  (b/copy-dir {:src-dirs ["src"] :target-dir class-dir})
  (b/copy-file {:src "LICENSE" :target (str class-dir "/META-INF/LICENSE")})
  (b/jar {:class-dir class-dir :jar-file jar-file})
  {:jar-file jar-file
   :pom-file (b/pom-path {:lib lib :class-dir class-dir})})

(defn install
  "Tests, builds, and installs the JAR in the local Maven repository.

  Options:
    :skip-tests - skip the test suite, default = false"
  [{:keys [skip-tests]}]
  (when-not skip-tests
    (test nil))
  (jar nil)
  (b/install {:basis @basis
              :lib lib
              :version version
              :class-dir class-dir
              :jar-file jar-file}))

(defn deploy
  "Tests, builds, and deploys a snapshot to Clojars using Maven's clojars server.

  Options:
    :skip-tests - skip the test suite, default = false"
  [{:keys [skip-tests]}]
  (when-not skip-tests
    (test nil))
  (let [{:keys [pom-file]} (jar nil)
        {:keys [exit]} (b/process {:command-args
                                   ["mvn" "--batch-mode" "--no-transfer-progress"
                                    "org.apache.maven.plugins:maven-deploy-plugin:3.2.0:deploy-file"
                                    (str "-Dfile=" jar-file)
                                    (str "-DpomFile=" pom-file)
                                    "-DrepositoryId=clojars"
                                    "-Durl=https://clojars.org/repo"]})]
    (when-not (zero? exit)
      (throw (ex-info "Clojars deployment failed" {:exit exit})))))
