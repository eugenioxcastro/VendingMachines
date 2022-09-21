(ns primer-proyecto.core
  (:gen-class) 
  (:require [clojure.java.io :as reader]))

;lee y regresa la lista del investario y monedas. Se guarda en un arreglo las dos strs de mis lineas de maquina
(defn read-machine-file-by-lines [file-to-read]
  (with-open [rdr (clojure.java.io/reader (clojure.java.io/resource file-to-read))]
    (reduce conj [] (line-seq rdr))))

;funcion auxiliar que convierte de srt a lista
(defn get-machine-info [machine-file]
  (map read-string machine-file))

 
(defn get-products [machine-file]
  (first (get-machine-info (read-machine-file-by-lines machine-file))))

 
(defn get-coins [machine-file]
  (second (get-machine-info (read-machine-file-by-lines machine-file))))

;Función para sobreescribir una lista en la base de datos, modificando los productos
(defn persist-products [new-products old-coins machine]
  (spit (clojure.java.io/resource machine) (prn-str new-products))
  (spit (clojure.java.io/resource machine) (pr-str old-coins) :append true))

;Función para sobreescribir una lista en la base de datos, modificando las monedas
(defn persist-coins [new-coins old-products machine]
  (spit (clojure.java.io/resource machine) (prn-str old-products))
  (spit (clojure.java.io/resource machine) (pr-str new-coins) :append true))

;funcion que regresa un solo producto, dado un id y la lista entera de productos
(defn get-product-by-id [product-id products]
  (cond (empty? products) '()
        :else
        (if (= (ffirst products) product-id) (first products) (get-product-by-id product-id (rest products)))))

;te trae el id del producto que quiere comprar una transaccion
(defn get-transaction-product-id [transaction]
  (ffirst transaction))

;te regresa el precio de un producto
(defn get-product-price [product]
  (nth product 2))

;te trae la secuencia de monedas insertadas por el cliente en una transaccion
(defn get-transaction-coins [transaction]
  (fnext transaction))

; invierte la lista de monedas
(defn get-reversed-coins [machine]
  (reverse (get-coins machine)))

;te regresa una moneda en especifico dado el tipo de moneda y todas las monedas
(defn get-coin-by-coin-type [coin-type coins]
  (cond (nil? coins) '()
        :else
        (if (= (ffirst coins) coin-type) (first coins) (get-coin-by-coin-type coin-type (rest coins)))))

;función auxiliar para reemplazar un valor en una lista por la posicion dada en posicion dada
(defn replace-nth [list n item]
  (if (= n 0)
    (cons item (rest list))
    (cons (first list) (replace-nth (rest list) (- n 1) item))))

;te regresa la lista de productos actualizada por inventario, esta lista será escrita en nuestra base de datos
(defn update-product-stock-and-return-all-products [products product-id new-stock]
  (map (fn [x] (if (= (first x) product-id) (replace-nth x 3 new-stock) x)) products))

;te regresa la lista de monedas actualizada por cantidad, esta lista será escrita en nuestra base de datos
(defn update-coin-inventory-and-return-all-coins [coins coin-type new-inventory]
  (map (fn [x] (if (= (first x) coin-type) (replace-nth x 1 new-inventory) x)) coins))

;primera condicion. Valida si hay inventario de un producto
(defn check-product-inventory [product-id list-of-products]
  (cond (empty? list-of-products) false
      :else
       (if (and (= (ffirst list-of-products) product-id) (>= (last (first list-of-products))1)) true
          (check-product-inventory product-id (rest list-of-products)))))

;segunda condicion. Valida si se insertaron suficientes monedas como para hacer la compra.
(defn enough-money-to-purchase? [product-id products transaction-coins]
  (cond (empty? products) false
      :else
       (if (and (= (ffirst products) product-id) (<= (get-product-price (get-product-by-id product-id products)) (apply + transaction-coins))) true 
          (enough-money-to-purchase? product-id (rest products) transaction-coins))))

; aumentamos el numero de monedas en nuestro inventorio por cada compra
(defn increase-coin-inventory [transaction-coins machine]
  (if (empty? transaction-coins) '()
        [(persist-coins (update-coin-inventory-and-return-all-coins (get-coins machine) (first transaction-coins) (+ (fnext (get-coin-by-coin-type (first transaction-coins) (get-coins machine))) 1)) (get-products machine) machine)
        (increase-coin-inventory (rest transaction-coins) machine)]))

; A partir de aqui se usa funciones para reducir el inventario de monedas por el cambio que se le da al cliente
(defn decrease-coin-inventory [change-given machine]
  (if (empty? change-given) '()
        [(persist-coins (update-coin-inventory-and-return-all-coins (get-coins machine) (first change-given) (- (fnext (get-coin-by-coin-type (first change-given) (get-coins machine))) 1)) (get-products machine) machine)
        (decrease-coin-inventory (rest change-given) machine)]))

; Esta funcion reduce el inventario del producto comprado
(defn diminish-product-stock [product-id machine]
  (persist-products (update-product-stock-and-return-all-products (get-products machine) product-id (- (last (get-product-by-id product-id (get-products machine))) 1))(get-coins machine) machine))

;funcion auxiliar para cambiar de numero a lista de monedas en el cambio
(defn insert-n [list item n]
  (if (= n 0)
    (cons item list)
    (cons (first list) (insert-n (rest list) item (- n 1)))))

;en esta funcion recibimos el numero de cambio como entero y lo convertimos a una lista de monedas, las cuales seran restadas de nuestro inventario, representando que se dieron al cliente.
(defn return-change-in-coins [change coins-in-reverse change-given-so-far]
  (cond (<= change 0) change-given-so-far
        (>= (quot change (ffirst coins-in-reverse)) 1)
            (cond (>= (second (first coins-in-reverse)) 1)
                (return-change-in-coins (- change (ffirst coins-in-reverse)) coins-in-reverse (insert-n change-given-so-far (ffirst coins-in-reverse) (count change-given-so-far))))
        :else (return-change-in-coins change (rest coins-in-reverse) change-given-so-far)))

; automata
(defn transition [state coin]
  (+ state coin))

;originalmente funcion revisa-estados, para el automata
 
(defn give-change [transaction-coins actual-state product-price]
  (if (empty? transaction-coins)
    (cond (> actual-state product-price) (- actual-state product-price)
          :else 0)
    (give-change (rest transaction-coins) (transition actual-state (first transaction-coins)) product-price)))


;Se empiezan a hacer los cambios en la base de datos, para este momento la compra ya paso las condiciones.
(defn perform-changes-in-inventories [transaction machine]
  (println "Tu compra fue aceptada")
  (diminish-product-stock (get-transaction-product-id transaction) machine)
  (decrease-coin-inventory (return-change-in-coins (give-change (get-transaction-coins transaction) 0 (get-product-price (get-product-by-id (get-transaction-product-id transaction) (get-products machine))))
                                                   (get-reversed-coins machine)
                                                   '()) machine)
  (increase-coin-inventory (get-transaction-coins transaction) machine))

;Funciones de reporte

; regresa la mutiplicacion de la moneda por el numero en inventario y lo suma
(defn sum-of-coins [machine]
  (apply + (map (fn [x] (* (first x) (fnext x))) (get-coins machine))))

;con esta funcion se pretende revisar que producto tiene un inventario de menos de 5, en ese caso, se imprime la lista con los nombres de los productos
(defn stock-of-product-low [machine]
  (map second (filter (fn [x] (<= (last x) 5)) (get-products machine))))

;funcion que regresa las monedas casi llenas en el inventario, el filtro es 20 monedas
(defn coins-with-inventory-almost-full [coins]
  (cond (empty? coins) coins
        :else
        (map first (filter (fn [c] (<= (- (last c) (second c)) 30)) coins))))

;funcion que regresa monedas con un inventario menor a 30
(defn coins-with-inventory-almost-empty [coins]
  (cond (empty? coins) coins
        :else
        (map first (filter (fn [x] (<= (fnext x) 30)) coins))))

;agarra una transaccion y valida condiciones en ella. Si las cumple, se efectuan cambios en la base de datos, pues se hace la compra
(defn perform-purchase-of-transaction [transaction machine]
  (if (check-product-inventory (get-transaction-product-id transaction) (get-products machine))
    (if (enough-money-to-purchase? (get-transaction-product-id transaction) (get-products machine) (get-transaction-coins transaction))
      (perform-changes-in-inventories transaction machine)
    (println "Te falta dinero, no fue aceptada la transaccion"))
  (println "No existe tu producto o se acabo el inventorio, lo sentimos")))

(defn machine-report [machine]
  (println "---Se terminaron las transaciones--- \n")
  (print "1. Las ganancias netas de hoy fueron ") (println (sum-of-coins machine))
  (print "2. Los productos con poco inventario son ") (println (stock-of-product-low machine))
  (print "3. Las monedas con el inventario casi lleno son: ") (println (coins-with-inventory-almost-full (get-coins machine)))
  (print "4. Las monedas con inventario menor a 30 son ") (println (coins-with-inventory-almost-empty (get-coins machine)))
  (println "GRACIAS POR USAR LA MAQUINA, NOS VEMOS!")
  (println "\n"))

; Con esta funcion leemos todas las trnsacciones y empezamos a iterar por cada una de ellas
(defn iterate-on-transactions [transactions machine]
  (if (empty? transactions) (machine-report machine)
      [(perform-purchase-of-transaction (first transactions) machine)
       (iterate-on-transactions (rest transactions) machine)]))

; se declara globalmente el archivo de las trasancciones, se abre, lee y cierra. Debe de recibir un str
(defn open-and-read-transactions [transaction-machine-files]
  (def transactions (read-string (slurp (clojure.java.io/resource (first transaction-machine-files)))))
  (iterate-on-transactions transactions (second transaction-machine-files)))
; -------------------------------------------------------------------------------------------------------------------

(def files '(("transaction1.txt" "machine1.txt")("transaction2.txt" "machine2.txt")("transaction3.txt" "machine3.txt")
             ("transaction4.txt" "machine4.txt")("transaction5.txt" "machine5.txt")("transaction6.txt" "machine6.txt")
             ("transaction7.txt" "machine7.txt")("transaction8.txt" "machine8.txt")("transaction9.txt" "machine9.txt")
             ("transaction10.txt" "machine10.txt")("transaction11.txt" "machine11.txt")("transaction12.txt" "machine12.txt")
             ("transaction13.txt" "machine13.txt") ("transaction14.txt" "machine14.txt")
             ("transaction15.txt" "machine15.txt") ("transaction16.txt" "machine16.txt")
             ("transaction17.txt" "machine17.txt") ("transaction18.txt" "machine18.txt")
             ("transaction19.txt" "machine19.txt") ("transaction20.txt" "machine20.txt")))
(time (doall (repeat 100 (pmap open-and-read-transactions files))))