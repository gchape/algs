(ns algs.avl
  (:refer-clojure :exclude [find remove])
  (:require [clojure.inspector :as inspector]))

(defn- height [tree]
  (if (nil? tree)
    0
    (inc (max (height (:left tree))
              (height (:right tree))))))

(defn node
  ([key]
   {:key key
    :height 0
    :left nil
    :right nil})
  ([key left right]
   {:key key
    :height (-> (max (height left)
                     (height right))
                (inc))
    :left left
    :right right}))

(defn- rotate-left
  [tree]
  (let [r (:right tree)]
    (node
     (:key r)
     (node
      (:key tree)
      (:left tree)
      (:left r))
     (:right r))))

(defn- rotate-right
  [tree]
  (let [l (:left tree)]
    (node
     (:key l)
     (:left l)
     (node
      (:key tree)
      (:right l)
      (:right tree)))))

(defn- balance
  [tree]
  (- (height (:right  tree))
     (height (:left tree))))

(defn- fix-balance
  [tree]
  (let [blnc (balance tree)]
    (cond
      (> blnc 1)
      (let [r (:right tree)
            blnc (balance r)]
        (if (< blnc 0)
          (rotate-left
           (assoc tree :right (rotate-right r)))
          (rotate-left tree)))

      (< blnc -1)
      (let [l (:left tree)
            blnc (balance l)]
        (if (< blnc 0)
          (rotate-right
           (assoc tree :left (rotate-left l)))
          (rotate-right tree)))

      :else
      tree)))

(defn- insert- [tree k]
  (cond
    (nil? tree)
    (node k)

    (< k (:key tree))
    (fix-balance
     (node (:key tree)
           (insert- (:left tree) k)
           (:right tree)))

    (> k (:key tree))
    (fix-balance
     (node (:key tree)
           (:left tree)
           (insert- (:right tree) k)))

    :else tree))

(defn- leftmost-node [tree]
  (if (nil? (:left tree))
    tree
    (recur (:left tree))))

(defn- remove-
  [tree k]
  (cond
    (nil? tree)
    nil

    (< k (:key tree))
    (fix-balance
     (assoc tree :left
            (remove- (:left tree) k)))

    (> k (:key tree))
    (fix-balance
     (assoc tree :right
            (remove- (:right tree) k)))

    :else
    (cond
      (and (nil? (:left tree))
           (nil? (:right tree)))
      nil

      (nil? (:left tree))
      (:right tree)

      (nil? (:right tree))
      (:left tree)

      :else
      (let [succ (leftmost-node (:right tree))]
        (fix-balance
         (assoc tree
                :key
                succ
                :right
                (remove- (:right tree) (:key succ))))))))

(defn find [tree k]
  (cond
    (nil? tree)
    nil

    (< k (:key tree))
    (find (:left tree) k)

    (> k (:key tree))
    (find (:right tree) k)

    :else
    (:key tree)))

(defn insert [tree k]
  (let [new-tree (insert- tree k)]
    (fix-balance new-tree)))

(defn remove
  [tree k]
  (fix-balance
   (remove- tree k)))
