(ns main.clj.threading-macro)

;; cond->

(def maps
  [{:k1 "k1" :k2 {:h1 "h1" :h2 "h2"} :k3 {:j2 "j2"}}
   {:k1 "k2" :k2 "k2"}
   {:k1 "k1" :k2 {:h1 "h1" :h3 "h3"} :k3 {:j1 "j1"}}])

(defn same-initial? [m]
  (apply = (map (fn [k] (-> k name first)) (keys m))))

(defn shape-up [m]
  (cond-> m
    :always (assoc-in [:k3 :j1] "default")
    (same-initial? m) (assoc :same true)
    (map? (:k2 m)) (assoc :k2 (apply str (vals (:k2 m))))))

(def signals [2 4 5 2 10])

(defn process [signals opts]
  (let [{:keys [bypass? interpolate? noise? cutoff?]} opts]
    (cond->> signals
      (< (count signals) 10) (map inc)
      interpolate? (mapcat range)
      bypass? (filter bypass?)
      noise? (random-sample noise?)
      cutoff? (take-while #(< % cutoff?)))))

(process signals {:bypass? even? :interpolate true :noise? 0.5 :cutoff? 200})
