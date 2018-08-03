(ns clj-shop-online.mgm)

(def products-final
  [{:id 1 :price 100 :name "lookup 1" :category "discount"}
   {:id 2 :price 200 :name "lookup 2" :category "discount"}
   {:id 3 :price 300 :name "lookup 3" :category "top"}
   {:id 4 :price 400 :name "lookup 4" :category "top"}])

(def client1
  {:id   1
   :name "a client 1"})

(def client2
  {:id   2
   :name "a client 2"})

(def product1
  {:id 1 :name "lookup 1" :price 100})

(def product2
  {:id 2 :name "lookup 2" :price 200})

(defmacro in-cat
  [category all-products]
  `(filterv #(= (% :category) ~category) ~all-products))

(defmacro with-ids
  [id-list all-products]
  `(filterv (fn [product#]
              (some #(= (product# :id) %) ~id-list))
            ~all-products))

(defn discount-prods2 [all-prods] (in-cat "discount" all-prods))
(defn top-prods2 [all-prods] (in-cat "top" all-prods))
(defn err-prods [all-prods] (with-ids [3 1 2 4] all-prods))
;;;;
(def context
  {:client   client1
   :products [product1 product2]})

(defn change-price
  "10 + => (change-price product1 30 +)
  150% => (change-price product1 150 pecentage)"
  [product amount price-func]
  (update product :price #(int (price-func % amount))))

(defn adj-price-client                                     
  [product {client-id :id} {target-id :id}]
  (when (= client-id target-id)
    (change-price product 100 *)))

(defn adj-price-prod
  [product {target-id :id} price-func]
  (if (= (product :id) target-id)
    (price-func product)
    product)
  )

(defn percentage
  [ratio base]
  (* base (+ 1 (/ ratio 100))))



(defmacro comp-fns
  [& fns]
  `(comp ~@fns))
;no apply - apply used if argument is a list, e.g. if fns is [+ -], but not just + -
; below is the usage of the macro
;(def z (comp-fns #(change-price % 10 +)
;                 #(change-price % 50 -)))
;(map z products)


(defn apply-fns-list-to-prod-list
  "(xxx [z d] (in-cat \"discount\" products-final))
  [[z d] discount-prods]"
  [fn-li filtered-products]
  (map (apply comp fn-li) filtered-products))


(defn merge-prices
  [old-li new-li]
  (map (fn [old-product]
         (reduce (fn [old nw]
                   (if (= (old :id) (nw :id))
                     (merge old nw)
                     old)
                   ) old-product new-li)
         ) old-li))

;(def z (comp-fns #(change-price % 10 +)
;                 #(change-price % 50 -)))
;(def d (comp-fns #(change-price % 1000 +)
;                 #(change-price % 1000 +)))
;(def e (comp-fns #(change-price % 1 -)))

;(final-prices [
;     [[z d] discount-prods2]
;     [[z d] top-prods2]
;     [[e] err-prods]
;     ]
;    products-final )
(defn final-prices
  [pair-ordered-list all-prods]
  (loop [fn-filter-pair pair-ordered-list
         prods all-prods]
    (let [cur-pair (first fn-filter-pair)
          cur-fn-li (first cur-pair)
          cur-prod-filter (last cur-pair)]
      (if (empty? fn-filter-pair)
        prods
        (recur (rest fn-filter-pair)
               (merge-prices prods
                             (apply-fns-list-to-prod-list cur-fn-li
                                                          (cur-prod-filter prods)))
               )))))
