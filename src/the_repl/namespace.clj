(ns the-repl.namespace
  (:require [clojure.tools.namespace.dependency :as dep]
            [ns-tracker.core :as ns-track]
            [ns-tracker.dependency :as ns-dep]))


(defonce dep-graph (atom {}))


(defn- update-dependency-graph!
  [ns-decls]
  (let [dependency-graph (atom (#'ns-track/update-dependency-graph (ns-dep/graph) ns-decls))]
    (reset! dep-graph (dep/->MapDependencyGraph (:dependencies @dependency-graph)
                                                (:dependents @dependency-graph)))))


(defn get-topo-sorted-nses
  [ns-decls]
  (let [_                 (update-dependency-graph! ns-decls)
        topo-sorted       (dep/topo-sort @dep-graph)
        nses-with-no-deps (clojure.set/difference (set (map #(second %) ns-decls))
                                                  (set topo-sorted))]
    (concat topo-sorted nses-with-no-deps)))