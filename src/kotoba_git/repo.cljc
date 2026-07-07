(ns kotoba-git.repo
  "A repo is a single arrangement db holding both objects
   (kotoba-git.object) and refs (kotoba-git.refs) as quads — git is not a
   separate content-addressed store bolted onto the Datomic-shaped
   kotobase-peer stack, it is a schema within it. Object subjects are
   content hashes (blob/tree/commit CIDs); ref subjects are repo ids —
   disjoint subject namespaces, so both coexist in one db without
   collision."
  (:require [arrangement.core :as arr]))

(defn empty-repo [] (arr/empty-db))

;; A repo's objects and refs are not secret at this layer — identity
;; pass-throughs satisfying arrangement.core/commit!'s mandatory blind/
;; encrypt contract (ADR-2607051000) without adding privacy semantics this
;; domain doesn't need.
(defn- identity-blind [v] (str v))
(defn- identity-encrypt [bytes] bytes)

(defn persist!
  "Snapshot the whole repo db (objects + refs together) to content-
   addressed storage via arrangement.core/commit!, returning the new
   snapshot CID."
  [put! db prev-cid]
  (arr/commit! put! db prev-cid arr/current-schema-version identity-blind identity-encrypt))
