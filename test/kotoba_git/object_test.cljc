(ns kotoba-git.object-test
  (:require [clojure.test :refer [deftest is testing]]
            [arrangement.core :as arr]
            [ipld.core :as ipld]
            [kotoba-git.object :as obj]
            [kotoba-git.repo :as repo]))

(deftest blob-roundtrip
  (let [db0 (repo/empty-repo)
        bytes (.getBytes "hello, kotoba-git" "UTF-8")
        [db cid] (obj/write-blob db0 bytes)]
    (is (string? cid))
    (is (= (seq bytes) (seq (obj/read-blob db cid))))
    (testing "content-addressing is deterministic"
      (let [[_ cid2] (obj/write-blob db0 bytes)]
        (is (= cid cid2))))))

(deftest tree-roundtrip
  (let [db0 (repo/empty-repo)
        [db1 blob-cid] (obj/write-blob db0 (.getBytes "contents" "UTF-8"))
        [db tree-cid] (obj/write-tree db1 [{:name "b.txt" :cid blob-cid :kind :blob}
                                            {:name "a.txt" :cid blob-cid :kind :blob}])
        tree (obj/read-tree db tree-cid)]
    (is (= :tree (:kind tree)))
    (testing "entries are sorted by name (git tree ordering)"
      (is (= ["a.txt" "b.txt"] (mapv :name (:entries tree)))))
    (is (every? #(= :blob (:kind %)) (:entries tree)))
    (is (every? #(= blob-cid (:cid %)) (:entries tree)))))

(deftest nested-tree-roundtrip
  (let [db0 (repo/empty-repo)
        [db1 blob-cid] (obj/write-blob db0 (.getBytes "nested" "UTF-8"))
        [db2 inner-cid] (obj/write-tree db1 [{:name "f.txt" :cid blob-cid :kind :blob}])
        [db outer-cid] (obj/write-tree db2 [{:name "dir" :cid inner-cid :kind :tree}])
        outer (obj/read-tree db outer-cid)]
    (is (= [{:name "dir" :cid inner-cid :kind :tree}] (:entries outer)))))

(deftest commit-roundtrip
  (let [db0 (repo/empty-repo)
        [db1 blob-cid] (obj/write-blob db0 (.getBytes "v1" "UTF-8"))
        [db2 tree-cid] (obj/write-tree db1 [{:name "f.txt" :cid blob-cid :kind :blob}])
        [db commit-cid] (obj/write-commit db2 {:tree tree-cid :parents []
                                                 :author "did:key:zAlice" :message "initial"
                                                 :ts 1000})
        commit (obj/read-commit db commit-cid)]
    (is (= tree-cid (:tree commit)))
    (is (= [] (:parents commit)))
    (is (= "did:key:zAlice" (:author commit)))
    (is (= "initial" (:message commit)))
    (is (= 1000 (:ts commit)))))

(deftest merge-commit-has-multiple-parents
  (let [db0 (repo/empty-repo)
        [db1 tree-cid] (obj/write-tree db0 [])
        [db2 c1] (obj/write-commit db1 {:tree tree-cid :parents [] :author "a" :message "c1" :ts 1})
        [db3 c2] (obj/write-commit db2 {:tree tree-cid :parents [] :author "a" :message "c2" :ts 2})
        [db merge] (obj/write-commit db3 {:tree tree-cid :parents [c1 c2] :author "a" :message "merge" :ts 3})]
    (is (= [c1 c2] (:parents (obj/read-commit db merge))))))

(deftest objects-are-real-arrangement-datoms
  (testing "commit/tree is a real ipld/link, so arrangement.core/refs-to finds it for free"
    (let [db0 (repo/empty-repo)
          [db1 tree-cid] (obj/write-tree db0 [])
          [db commit-cid] (obj/write-commit db1 {:tree tree-cid :parents [] :author "a"
                                                    :message "m" :ts 1})]
      (is (= {"commit/tree" #{commit-cid}} (arr/refs-to db (ipld/link tree-cid)))))))

(deftest reading-an-absent-object-returns-nil
  (let [db (repo/empty-repo)
        bogus-cid (first (obj/write-blob db (.getBytes "never-written-under-this-cid" "UTF-8")))]
    (is (nil? (obj/read-blob db "bafkreidoesnotexist")))
    (is (nil? (obj/read-tree db "bafyreidoesnotexist")))
    (is (nil? (obj/read-commit db "bafyreidoesnotexist")))
    (testing "and a real CID never written to THIS db is equally absent"
      (is (nil? (obj/read-blob (repo/empty-repo) bogus-cid))))))

(deftest empty-tree-has-a-stable-cid-and-no-entries
  (let [db0 (repo/empty-repo)
        [db1 cid1] (obj/write-tree db0 [])
        [db cid2] (obj/write-tree db1 [])]
    (is (= cid1 cid2) "content-addressing is deterministic even for the empty tree")
    (is (= {:kind :tree :entries []} (obj/read-tree db cid1)))))

(deftest three-level-nested-tree-roundtrip
  (let [db0 (repo/empty-repo)
        [db1 blob-cid] (obj/write-blob db0 (.getBytes "deep" "UTF-8"))
        [db2 leaf-cid] (obj/write-tree db1 [{:name "leaf.txt" :cid blob-cid :kind :blob}])
        [db3 mid-cid] (obj/write-tree db2 [{:name "mid" :cid leaf-cid :kind :tree}])
        [db top-cid] (obj/write-tree db3 [{:name "top" :cid mid-cid :kind :tree}])]
    (is (= [{:name "top" :cid mid-cid :kind :tree}] (:entries (obj/read-tree db top-cid))))
    (is (= [{:name "mid" :cid leaf-cid :kind :tree}] (:entries (obj/read-tree db mid-cid))))
    (is (= [{:name "leaf.txt" :cid blob-cid :kind :blob}] (:entries (obj/read-tree db leaf-cid))))))

(deftest same-content-different-position-same-blob-cid
  (testing "git-style dedup: identical bytes anywhere in the tree share one blob object"
    (let [db0 (repo/empty-repo)
          [db1 blob-cid] (obj/write-blob db0 (.getBytes "shared" "UTF-8"))
          [db tree-cid] (obj/write-tree db1 [{:name "a.txt" :cid blob-cid :kind :blob}
                                              {:name "b/c.txt" :cid blob-cid :kind :blob}])]
      (is (= 2 (count (:entries (obj/read-tree db tree-cid)))))
      (is (apply = (map :cid (:entries (obj/read-tree db tree-cid))))))))
