(ns kotoba-git.repo-test
  (:require [clojure.test :refer [deftest is]]
            [kotoba-git.object :as obj]
            [kotoba-git.refs :as refs]
            [kotoba-git.repo :as repo]))

(deftest objects-and-refs-coexist-in-one-repo-db
  (let [db0 (repo/empty-repo)
        [db1 blob-cid] (obj/write-blob db0 (.getBytes "hi" "UTF-8"))
        [db2 tree-cid] (obj/write-tree db1 [{:name "f.txt" :cid blob-cid :kind :blob}])
        [db3 commit-cid] (obj/write-commit db2 {:tree tree-cid :parents [] :author "a"
                                                  :message "m" :ts 1})
        db (refs/set-ref db3 "repo1" "refs/heads/main" commit-cid)]
    ;; disjoint subject namespaces (content-hash CIDs vs repo ids) coexist
    ;; without collision in the same arrangement db
    (is (= commit-cid (refs/get-ref db "repo1" "refs/heads/main")))
    (is (= commit-cid (:cid (assoc (obj/read-commit db commit-cid) :cid commit-cid))))))

(deftest persist-returns-a-snapshot-cid-of-the-whole-repo
  (let [store (atom {})
        put! (fn [cid bytes] (swap! store assoc cid bytes))
        db0 (repo/empty-repo)
        [db1 blob-cid] (obj/write-blob db0 (.getBytes "hi" "UTF-8"))
        [db2 commit-cid] (obj/write-commit db1 {:tree blob-cid :parents [] :author "a"
                                                  :message "m" :ts 1})
        db (refs/set-ref db2 "repo1" "refs/heads/main" commit-cid)
        snapshot-cid (repo/persist! put! db nil)]
    (is (string? snapshot-cid))))
