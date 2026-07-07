(ns kotoba-git.log-test
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba-git.object :as obj]
            [kotoba-git.repo :as repo]
            [kotoba-git.log :as log]))

(defn- utf8-bytes [s]
  #?(:clj (.getBytes ^String s "UTF-8")
     :cljs (.encode (js/TextEncoder.) s)))

(defn- linear-history [db0]
  (let [[db1 tree0] (obj/write-tree db0 [])
        [db2 c1] (obj/write-commit db1 {:tree tree0 :parents [] :author "a" :message "c1" :ts 1})
        [db3 c2] (obj/write-commit db2 {:tree tree0 :parents [c1] :author "a" :message "c2" :ts 2})
        [db c3] (obj/write-commit db3 {:tree tree0 :parents [c2] :author "a" :message "c3" :ts 3})]
    {:db db :c1 c1 :c2 c2 :c3 c3}))

(deftest log-walks-first-parent-newest-first
  (let [{:keys [db c1 c2 c3]} (linear-history (repo/empty-repo))]
    (is (= [c3 c2 c1] (mapv :cid (log/log db c3))))
    (is (= ["c3" "c2" "c1"] (mapv :message (log/log db c3))))))

(deftest ancestors-covers-merge-commits
  (let [db0 (repo/empty-repo)
        [db1 tree0] (obj/write-tree db0 [])
        [db2 c1] (obj/write-commit db1 {:tree tree0 :parents [] :author "a" :message "c1" :ts 1})
        [db3 branch-a] (obj/write-commit db2 {:tree tree0 :parents [c1] :author "a" :message "a" :ts 2})
        [db4 branch-b] (obj/write-commit db3 {:tree tree0 :parents [c1] :author "a" :message "b" :ts 2})
        [db merge] (obj/write-commit db4 {:tree tree0 :parents [branch-a branch-b] :author "a" :message "merge" :ts 3})]
    (is (= #{c1 branch-a branch-b merge} (log/ancestors db merge)))))

(deftest missing-since-finds-new-commit-tree-and-blob
  (let [db0 (repo/empty-repo)
        [db1 blob1] (obj/write-blob db0 (utf8-bytes "v1"))
        [db2 tree1] (obj/write-tree db1 [{:name "f.txt" :cid blob1 :kind :blob}])
        [db3 c1] (obj/write-commit db2 {:tree tree1 :parents [] :author "a" :message "c1" :ts 1})
        [db4 blob2] (obj/write-blob db3 (utf8-bytes "v2"))
        [db5 tree2] (obj/write-tree db4 [{:name "f.txt" :cid blob2 :kind :blob}])
        [db c2] (obj/write-commit db5 {:tree tree2 :parents [c1] :author "a" :message "c2" :ts 2})]
    (testing "peer already has everything up to c1"
      (let [have (conj (log/ancestors db c1) tree1 blob1)
            missing (log/missing-since db c2 have)]
        (is (= #{c2 tree2 blob2} missing))))
    (testing "peer has nothing"
      (is (= #{c1 tree1 blob1 c2 tree2 blob2} (log/missing-since db c2 #{}))))
    (testing "peer already has EVERYTHING reachable -- nothing left to send"
      (let [have (log/missing-since db c2 #{})]
        (is (= #{} (log/missing-since db c2 have)))))))

(deftest nil-head-cid-is-an-empty-history
  (let [db (repo/empty-repo)]
    (is (= #{} (log/ancestors db nil)))
    (is (= [] (log/log db nil)))
    (is (= #{} (log/missing-since db nil #{})))))

(deftest log-of-a-single-root-commit
  (let [db0 (repo/empty-repo)
        [db1 tree0] (obj/write-tree db0 [])
        [db c1] (obj/write-commit db1 {:tree tree0 :parents [] :author "a" :message "root" :ts 1})]
    (is (= [c1] (mapv :cid (log/log db c1))))
    (is (= #{c1} (log/ancestors db c1)))))
