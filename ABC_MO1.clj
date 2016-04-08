;; gorilla-repl.fileformat = 1

;; **
;;; #ABC-MO1
;;; Uses GA to create vectors of summary statistics which are scored through a connection to R
;;; 
;; **

;; @@
(ns abc-mo
  (:require [gorilla-plot.core :as plot]
            [clojure.repl :as repl]
            [clojure.pprint :as pprint]
            [clojure.walk :as walk]
            [clojure.zip :as zip]
            [gorilla-repl.table :as table]
            [gorilla-repl.html :as html]
    		[darwin.core :as darwin]
            [darwin.evolution.metrics :as metrics]
            [algebolic.expression.core :as expression]
            [algebolic.expression.tree :as tree]
            [algebolic.expression.genetics :as genetics]
            [algebolic.expression.score :as score]
            [algebolic.expression.render :as spea-render]
            [algebolic.expression.interpreter :as interpreter]
            [darwin.evolution.core :as evolution]
            [darwin.evolution.metrics :as metrics]
            [darwin.evolution.reproduction :as reproduction]
            [darwin.evolution.scoring :as scoring]
            [darwin.evolution.selection :as selection]
            [darwin.evolution.transform :as transform]
            [darwin.evolution.pareto :as pareto]
            [darwin.algorithms.spea2 :as spea2]
            [criterium.core :as criterium])
 
  
  
  )
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

(import '(rabc.RABC))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(def maxNumStatistics 17)
(def maxSubsetSize 6)
(def rn_delete 0.7)
(def rn_alter 0.3)

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;abc-mo/rn_alter</span>","value":"#'abc-mo/rn_alter"}
;; <=

;; @@
;;these are all 1 referenced
(defn create-genotype

  [maxSubsetSize numStatistics]
  
  (let [geno (vec (repeatedly (+ 1 (rand-int (- maxSubsetSize 1))) #(+ (rand-int (- numStatistics 1) ) 1)))]
    
      (vec (distinct geno))
     )    
    )
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;abc-mo/create-genotype</span>","value":"#'abc-mo/create-genotype"}
;; <=

;; @@
(defn distribute-parent   
 ([parent-1 prob]
  (let [{c1 true c2 false} (group-by (fn [x] (<= prob (rand))) parent-1)]    
[c1 c2]  
   ))
  )
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;abc-mo/distribute-parent</span>","value":"#'abc-mo/distribute-parent"}
;; <=

;; @@
(defn cross-over
  
  [parent-1 parent-2] 
  
  (let [
         offspring (mapv vec (mapv set (mapv concat (distribute-parent parent-1 0.5) (reverse (distribute-parent parent-2 0.5)))))]
    
    (if (or (zero? (count (offspring 0))) (zero? (count (offspring 1))))
      	
      [parent-1 parent-2]
      [(offspring 0)(offspring 1)]
      )
    ))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;abc-mo/cross-over</span>","value":"#'abc-mo/cross-over"}
;; <=

;; @@
(defn gene-add
  [indiv]
   
  (if (< (count indiv) maxSubsetSize) (into []  (set (concat indiv (vector (+ (rand-int (- maxNumStatistics 1) ) 1))))) indiv)
  )
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;abc-mo/gene-add</span>","value":"#'abc-mo/gene-add"}
;; <=

;; @@
(defn gene-tweak
  [indv]
  (let [
         n (rand-int (count indv)) 
         new-gene  (+ (rand-int (- maxNumStatistics 1) ) 1)
         ]
    (assoc indv n new-gene)
    ))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;abc-mo/gene-tweak</span>","value":"#'abc-mo/gene-tweak"}
;; <=

;; @@
(defn gene-delete [indv] 
  (let [new-indv (subvec indv 1 (count indv))]
    (if (= new-indv []) indv
      new-indv))
  )
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;abc-mo/gene-delete</span>","value":"#'abc-mo/gene-delete"}
;; <=

;; @@
(defn mutate
  "takes two genotypes and returns a mutated genotype"
  [indv]
  (let [rn (rand)]
    (if (>= rn rn_delete)
      (gene-delete indv)
    (if (>= rn rn_alter)
        (gene-tweak indv)
		(gene-add indv)
      )
      )
    )
  )
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;abc-mo/mutate</span>","value":"#'abc-mo/mutate"}
;; <=

;; @@
(defn random-initial-population 
  
 [size]
 
  (repeatedly size #(create-genotype maxSubsetSize maxNumStatistics)) 
       
  )
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;abc-mo/random-initial-population</span>","value":"#'abc-mo/random-initial-population"}
;; <=

;; @@
(defn SaveZ [zg str]
  (spit str (pr-str zg))
  
  )
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;abc-mo/SaveZ</span>","value":"#'abc-mo/SaveZ"}
;; <=

;; @@
(defn task [zg]
  (print (:age zg))
  (SaveZ zg "MO_ABC_Zeitgeist.txt")
  )
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;abc-mo/task</span>","value":"#'abc-mo/task"}
;; <=

;; @@
(def initial-zeitgeist (evolution/make-zeitgeist (random-initial-population 100)))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;abc-mo/initial-zeitgeist</span>","value":"#'abc-mo/initial-zeitgeist"}
;; <=

;; @@
(rabc.RABC/Init)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(defn score
  [indv] 
    
  (rabc.RABC/Score (into-array Integer/TYPE (vec (flatten indv))))

  )
  


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;abc-mo/score</span>","value":"#'abc-mo/score"}
;; <=

;; @@
(def generation-config			      
  (let [size-limit 120                   
        min-size 20
        ea-config (spea2/spea2-config
                    {:goals [:error :complexity]
                     :archive-size 100
                     :binary-ops [{:op cross-over :repeat 40}]
                     :unary-ops [{:op mutate :repeat 20}]})
        score-functions {:complexity (fn [x] (count x))
                         :error (fn [e] (score e))}]
    {:ea-config              ea-config
     :score-functions        score-functions
     :reporting-function     (fn [z] (print ".") (flush))}))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;abc-mo/generation-config</span>","value":"#'abc-mo/generation-config"}
;; <=

;; @@
(time (def result (evolution/run-evolution generation-config initial-zeitgeist (fn [zg gc] (task zg) (>= (:age zg) 300)))))
;; @@

;; @@
(def result (read-string (slurp "MO_ABC_Zeitgeist.txt") ))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;abc-mo/result</span>","value":"#'abc-mo/result"}
;; <=

;; @@
(score [12 9])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>-501.4508711761089</span>","value":"-501.4508711761089"}
;; <=

;; @@
(mapv #(println (:genotype %)) (sort-by :error (:elite result)))
;; @@
;; ->
;;; [4 12 9 5]
;;; [4 12 9]
;;; [4 12 5]
;;; [12 9]
;;; [5 4 12 9 5]
;;; [12 11 9]
;;; [4 12 11 9]
;;; [12 11]
;;; [12 3]
;;; [4 12 11 9]
;;; [12]
;;; [12 9 5 16]
;;; [12 9]
;;; [4 12 9 5]
;;; [12 9]
;;; [12 9]
;;; [4 12 9 8]
;;; [12]
;;; [12 9 5]
;;; [4 9 5]
;;; [12 11]
;;; [11 9]
;;; [12 11]
;;; [12 11]
;;; [4 13 12 5]
;;; [4 12 11 9 10]
;;; [0 12 5]
;;; [12 9]
;;; [12]
;;; [1 4 12 11 9 5]
;;; [12 9]
;;; [12 9]
;;; [12 9]
;;; [12]
;;; [4 9 5]
;;; [4 12]
;;; [12 9]
;;; [12 9]
;;; [12 9]
;;; [12]
;;; [3 12 9]
;;; [3 12 2 11 9]
;;; [4 12 9 5]
;;; [12 11]
;;; [12]
;;; [12 11 9]
;;; [12]
;;; [12 11]
;;; [12 11 9]
;;; [12 9 8]
;;; [12 11 9]
;;; [12 9]
;;; [12 11 9]
;;; [12]
;;; [12]
;;; [12 9 5]
;;; [12]
;;; [12]
;;; [12]
;;; [12 5]
;;; [4 12 5]
;;; [12 11]
;;; [12]
;;; [12]
;;; [12 5]
;;; [12]
;;; [4 12 11 5]
;;; [12]
;;; [4 12 9 12]
;;; [4 12 5]
;;; [12 11 9]
;;; [12]
;;; [11]
;;; [12]
;;; [12]
;;; [12 11 9]
;;; [12]
;;; [12]
;;; [4 11]
;;; [4 12]
;;; [12]
;;; [12]
;;; [12]
;;; [11]
;;; [12]
;;; [11]
;;; [12]
;;; [12]
;;; [12]
;;; [11]
;;; [12]
;;; [12]
;;; [12]
;;; [12]
;;; [12]
;;; [12]
;;; [12]
;;; [12]
;;; [12]
;;; [11]
;;; 
;; <-
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil]"}
;; <=

;; **
;;; #Pareto Analysis
;;; 
;; **

;; @@
(plot/list-plot (:min (:error @metrics/metrics)) :joined true)
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"2f338efd-8d25-4fb2-8500-1b4f5694e423","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"2f338efd-8d25-4fb2-8500-1b4f5694e423","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"2f338efd-8d25-4fb2-8500-1b4f5694e423"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"2f338efd-8d25-4fb2-8500-1b4f5694e423","values":[{"x":0,"y":0.008531730981252705},{"x":1,"y":0.02076549491738955},{"x":2,"y":0.0011158219693906402},{"x":3,"y":4.84281649845264E-4},{"x":4,"y":0.005646796440295088},{"x":5,"y":7.252000462342201E-4},{"x":6,"y":0.0039049782918780807},{"x":7,"y":0.0039049782918780807},{"x":8,"y":0.0019583083106060606},{"x":9,"y":0.0015114093242277749},{"x":10,"y":0.0015114093242277749},{"x":11,"y":0.0025955155652253037},{"x":12,"y":0.0013654068912803963},{"x":13,"y":0.0011816186424080666},{"x":14,"y":8.407414231448307E-4},{"x":15,"y":7.590935081303041E-4},{"x":16,"y":2.676410031350329E-4},{"x":17,"y":2.676410031350329E-4},{"x":18,"y":0.0011515438447547988},{"x":19,"y":7.19843043959223E-5},{"x":20,"y":7.689659465238696E-4},{"x":21,"y":0.003346756661130823},{"x":22,"y":0.003346756661130823},{"x":23,"y":0.003997606578878599},{"x":24,"y":0.0011255676008146143},{"x":25,"y":0.0011255676008146143},{"x":26,"y":0.005957629565480627},{"x":27,"y":6.454091338222723E-4},{"x":28,"y":6.454091338222723E-4},{"x":29,"y":0.008488422200830792},{"x":30,"y":0.008718588124023485},{"x":31,"y":0.01826545538944302},{"x":32,"y":7.688425457663683E-5},{"x":33,"y":2.0228279904133117E-4},{"x":34,"y":0.002069943790325768},{"x":35,"y":0.002069943790325768},{"x":36,"y":0.0013494078621901773},{"x":37,"y":0.0013494078621901773},{"x":38,"y":0.002228882374591512},{"x":39,"y":0.004378830848614013},{"x":40,"y":8.28512375420587E-4},{"x":41,"y":0.0015246814152517496},{"x":42,"y":0.0061759018169538216},{"x":43,"y":0.003145933300698678},{"x":44,"y":0.005565993010973158},{"x":45,"y":0.006229014842125902},{"x":46,"y":8.232700552650574E-4},{"x":47,"y":0.008005721690793388},{"x":48,"y":0.005475561408661789},{"x":49,"y":8.804403708433384E-4},{"x":50,"y":0.0010039838545496949},{"x":51,"y":0.0035214543899075856},{"x":52,"y":0.0010801269357724186},{"x":53,"y":0.005105188733303012},{"x":54,"y":0.007891148904159206},{"x":55,"y":0.014889575569420055},{"x":56,"y":0.001277541065793475},{"x":57,"y":0.001277541065793475},{"x":58,"y":0.0034236883286939968},{"x":59,"y":6.101298023060142E-4},{"x":60,"y":6.098626815775887E-5},{"x":61,"y":8.146591187798702E-4},{"x":62,"y":8.195810489441957E-5},{"x":63,"y":0.0013808909530862445},{"x":64,"y":0.001119609841332303},{"x":65,"y":5.881347951970373E-4},{"x":66,"y":2.4629833577605176E-4},{"x":67,"y":6.983118678625111E-4},{"x":68,"y":3.2217447200244553E-4},{"x":69,"y":3.2217447200244553E-4},{"x":70,"y":0.004708149872296397},{"x":71,"y":0.006309721548693026},{"x":72,"y":0.004952429471788644},{"x":73,"y":0.011099758289232375},{"x":74,"y":0.011075035317472892},{"x":75,"y":0.0018141606570090119},{"x":76,"y":0.0018141606570090119},{"x":77,"y":0.003067347085572547},{"x":78,"y":0.002860774185960868},{"x":79,"y":0.002860774185960868},{"x":80,"y":0.005321849208360208},{"x":81,"y":0.001411227502784329},{"x":82,"y":0.001411227502784329},{"x":83,"y":0.002149825164788899},{"x":84,"y":0.004663154734886099},{"x":85,"y":0.010761323699876146},{"x":86,"y":0.004651217449510359},{"x":87,"y":0.001204018512731997},{"x":88,"y":3.6143739269123287E-4},{"x":89,"y":3.6143739269123287E-4},{"x":90,"y":0.0016343146182393742},{"x":91,"y":0.0011439642118566962},{"x":92,"y":8.833402552288483E-4},{"x":93,"y":8.833402552288483E-4},{"x":94,"y":5.253390256684254E-4},{"x":95,"y":5.253390256684254E-4},{"x":96,"y":0.002922917450303264},{"x":97,"y":0.0020683579263742757},{"x":98,"y":0.0020683579263742757},{"x":99,"y":8.368123523594395E-5}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"2f338efd-8d25-4fb2-8500-1b4f5694e423\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"2f338efd-8d25-4fb2-8500-1b4f5694e423\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"2f338efd-8d25-4fb2-8500-1b4f5694e423\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"2f338efd-8d25-4fb2-8500-1b4f5694e423\", :values ({:x 0, :y 0.008531730981252705} {:x 1, :y 0.02076549491738955} {:x 2, :y 0.0011158219693906402} {:x 3, :y 4.84281649845264E-4} {:x 4, :y 0.005646796440295088} {:x 5, :y 7.252000462342201E-4} {:x 6, :y 0.0039049782918780807} {:x 7, :y 0.0039049782918780807} {:x 8, :y 0.0019583083106060606} {:x 9, :y 0.0015114093242277749} {:x 10, :y 0.0015114093242277749} {:x 11, :y 0.0025955155652253037} {:x 12, :y 0.0013654068912803963} {:x 13, :y 0.0011816186424080666} {:x 14, :y 8.407414231448307E-4} {:x 15, :y 7.590935081303041E-4} {:x 16, :y 2.676410031350329E-4} {:x 17, :y 2.676410031350329E-4} {:x 18, :y 0.0011515438447547988} {:x 19, :y 7.19843043959223E-5} {:x 20, :y 7.689659465238696E-4} {:x 21, :y 0.003346756661130823} {:x 22, :y 0.003346756661130823} {:x 23, :y 0.003997606578878599} {:x 24, :y 0.0011255676008146143} {:x 25, :y 0.0011255676008146143} {:x 26, :y 0.005957629565480627} {:x 27, :y 6.454091338222723E-4} {:x 28, :y 6.454091338222723E-4} {:x 29, :y 0.008488422200830792} {:x 30, :y 0.008718588124023485} {:x 31, :y 0.01826545538944302} {:x 32, :y 7.688425457663683E-5} {:x 33, :y 2.0228279904133117E-4} {:x 34, :y 0.002069943790325768} {:x 35, :y 0.002069943790325768} {:x 36, :y 0.0013494078621901773} {:x 37, :y 0.0013494078621901773} {:x 38, :y 0.002228882374591512} {:x 39, :y 0.004378830848614013} {:x 40, :y 8.28512375420587E-4} {:x 41, :y 0.0015246814152517496} {:x 42, :y 0.0061759018169538216} {:x 43, :y 0.003145933300698678} {:x 44, :y 0.005565993010973158} {:x 45, :y 0.006229014842125902} {:x 46, :y 8.232700552650574E-4} {:x 47, :y 0.008005721690793388} {:x 48, :y 0.005475561408661789} {:x 49, :y 8.804403708433384E-4} {:x 50, :y 0.0010039838545496949} {:x 51, :y 0.0035214543899075856} {:x 52, :y 0.0010801269357724186} {:x 53, :y 0.005105188733303012} {:x 54, :y 0.007891148904159206} {:x 55, :y 0.014889575569420055} {:x 56, :y 0.001277541065793475} {:x 57, :y 0.001277541065793475} {:x 58, :y 0.0034236883286939968} {:x 59, :y 6.101298023060142E-4} {:x 60, :y 6.098626815775887E-5} {:x 61, :y 8.146591187798702E-4} {:x 62, :y 8.195810489441957E-5} {:x 63, :y 0.0013808909530862445} {:x 64, :y 0.001119609841332303} {:x 65, :y 5.881347951970373E-4} {:x 66, :y 2.4629833577605176E-4} {:x 67, :y 6.983118678625111E-4} {:x 68, :y 3.2217447200244553E-4} {:x 69, :y 3.2217447200244553E-4} {:x 70, :y 0.004708149872296397} {:x 71, :y 0.006309721548693026} {:x 72, :y 0.004952429471788644} {:x 73, :y 0.011099758289232375} {:x 74, :y 0.011075035317472892} {:x 75, :y 0.0018141606570090119} {:x 76, :y 0.0018141606570090119} {:x 77, :y 0.003067347085572547} {:x 78, :y 0.002860774185960868} {:x 79, :y 0.002860774185960868} {:x 80, :y 0.005321849208360208} {:x 81, :y 0.001411227502784329} {:x 82, :y 0.001411227502784329} {:x 83, :y 0.002149825164788899} {:x 84, :y 0.004663154734886099} {:x 85, :y 0.010761323699876146} {:x 86, :y 0.004651217449510359} {:x 87, :y 0.001204018512731997} {:x 88, :y 3.6143739269123287E-4} {:x 89, :y 3.6143739269123287E-4} {:x 90, :y 0.0016343146182393742} {:x 91, :y 0.0011439642118566962} {:x 92, :y 8.833402552288483E-4} {:x 93, :y 8.833402552288483E-4} {:x 94, :y 5.253390256684254E-4} {:x 95, :y 5.253390256684254E-4} {:x 96, :y 0.002922917450303264} {:x 97, :y 0.0020683579263742757} {:x 98, :y 0.0020683579263742757} {:x 99, :y 8.368123523594395E-5})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; @@
(defn pareto-plot-population
  [[k1 k2] result]
  (let [pareto-front (pareto/non-dominated-individuals [k1 k2] (:elite result))
        coord-extract (fn [i] [(k1 i) (k2 i)])]
    (plot/compose
      (plot/list-plot (map coord-extract (:elite result)) :colour "red")
      (plot/list-plot (map coord-extract (:rabble result)) :colour "blue")
          (plot/list-plot (map coord-extract pareto-front) :colour "#ff29d2")
  )))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;abc-mo/pareto-plot-population</span>","value":"#'abc-mo/pareto-plot-population"}
;; <=

;; @@
(pareto-plot-population [:error :complexity] result)
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"8c5f5951-ca78-4887-8214-bbdef68f452c","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"8c5f5951-ca78-4887-8214-bbdef68f452c","field":"data.y"}}],"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"data":[{"name":"8c5f5951-ca78-4887-8214-bbdef68f452c","values":[{"x":-503.5174459789853,"y":4},{"x":-499.4316262054572,"y":1},{"x":-502.35880671632475,"y":2},{"x":-498.73696841008416,"y":4},{"x":-502.3705588233252,"y":6},{"x":-508.56533183071815,"y":3},{"x":-499.48891630458525,"y":1},{"x":-502.9787357048972,"y":2},{"x":-499.280885069778,"y":3},{"x":-497.4714785157141,"y":1},{"x":-501.1275794490446,"y":3},{"x":-494.0348073998511,"y":1},{"x":-510.43588457822347,"y":4},{"x":-502.62187237841397,"y":3},{"x":-502.8438268680818,"y":4},{"x":-501.86573464763046,"y":2},{"x":-503.0805637517149,"y":3},{"x":-500.92782668756934,"y":3},{"x":-502.01037541176476,"y":2},{"x":-498.3287184512659,"y":1},{"x":-505.30387833890757,"y":2},{"x":-501.1417323312967,"y":1},{"x":-496.5436508055277,"y":1},{"x":-498.99696635796084,"y":2},{"x":-506.1817616007353,"y":5},{"x":-499.1338913027853,"y":1},{"x":-497.7627094158679,"y":2},{"x":-505.9227055143765,"y":3},{"x":-499.3692325689115,"y":2},{"x":-498.8413763442127,"y":4},{"x":-497.6862699610767,"y":1},{"x":-501.3000053462572,"y":2},{"x":-496.153581628895,"y":1},{"x":-502.53298432920525,"y":2},{"x":-499.83573891185296,"y":3},{"x":-501.8857720787021,"y":2},{"x":-495.40245640724294,"y":1},{"x":-502.84881186802943,"y":2},{"x":-501.635988853623,"y":3},{"x":-499.4539405605884,"y":1},{"x":-505.06874927345825,"y":2},{"x":-504.56327125776,"y":1},{"x":-502.75212065070184,"y":5},{"x":-497.5569049984877,"y":1},{"x":-502.84395921021724,"y":2},{"x":-510.1062536672432,"y":3},{"x":-502.3035488183546,"y":2},{"x":-496.48449199028033,"y":1},{"x":-502.89634770810227,"y":2},{"x":-497.97627836819885,"y":1},{"x":-505.06413980153013,"y":4},{"x":-498.7501444771246,"y":1},{"x":-501.9903259134594,"y":2},{"x":-497.8842421524649,"y":1},{"x":-500.0720904703339,"y":1},{"x":-499.8954921635523,"y":1},{"x":-498.4903206905261,"y":1},{"x":-503.87760277885207,"y":2},{"x":-501.77420243383244,"y":1},{"x":-497.78292313900545,"y":2},{"x":-498.8445033042809,"y":1},{"x":-502.32222227503604,"y":2},{"x":-497.4604935151835,"y":1},{"x":-498.62049749202833,"y":3},{"x":-497.1918111745184,"y":1},{"x":-500.94984983521306,"y":2},{"x":-498.2503121978751,"y":1},{"x":-496.8897792138621,"y":1},{"x":-495.57404910139024,"y":1},{"x":-500.22891901283447,"y":2},{"x":-498.6205955745078,"y":3},{"x":-496.77155737736234,"y":1},{"x":-497.00761484610206,"y":1},{"x":-500.4237642441543,"y":3},{"x":-504.4522461004044,"y":2},{"x":-502.14340937575736,"y":1},{"x":-499.2655119829775,"y":2},{"x":-497.07401587016113,"y":1},{"x":-505.8518221013117,"y":4},{"x":-504.550525012022,"y":4},{"x":-497.99806810870814,"y":1},{"x":-503.11607309186,"y":3},{"x":-500.5755317079077,"y":3},{"x":-496.82008790239274,"y":1},{"x":-501.3726390839969,"y":4},{"x":-499.0538208859894,"y":1},{"x":-500.13017498166187,"y":3},{"x":-494.4470267067836,"y":1},{"x":-503.3128998055534,"y":1},{"x":-501.54880241344944,"y":5},{"x":-496.73286252532887,"y":1},{"x":-497.97922887850535,"y":3},{"x":-496.2010956046016,"y":1},{"x":-501.0318931498808,"y":1},{"x":-504.1989665439404,"y":4},{"x":-504.1213858456525,"y":2},{"x":-497.0960320277515,"y":1},{"x":-506.3061354641027,"y":2},{"x":-502.13060957653033,"y":3},{"x":-502.43186757997705,"y":1}]},{"name":"59ee1558-fad8-45e0-a353-da4d28b87534","values":[{"x":-502.1266608411025,"y":3},{"x":-496.0752635215373,"y":1},{"x":-498.68626832871956,"y":1},{"x":-500.0705699190743,"y":2},{"x":-501.92180454220954,"y":2},{"x":10000.0,"y":1},{"x":-505.905221219608,"y":3},{"x":-500.93664640501277,"y":3},{"x":-496.3758730134132,"y":3},{"x":-493.9302155256942,"y":1},{"x":-501.6617282270505,"y":3},{"x":-495.8196713903637,"y":1},{"x":-499.00791560757517,"y":1},{"x":-500.9885727901993,"y":1},{"x":-504.2258683673203,"y":3},{"x":-499.0182184947877,"y":1},{"x":-498.1230597951998,"y":1},{"x":-497.29672311058215,"y":2},{"x":-498.1758972962788,"y":1},{"x":-506.54551426959904,"y":3},{"x":-500.6368489097459,"y":3},{"x":-503.7388471573078,"y":2},{"x":-496.642831147594,"y":1},{"x":-502.9651847036066,"y":5},{"x":-497.5637009599594,"y":1},{"x":-496.96471873008073,"y":1},{"x":-494.4172001875669,"y":1},{"x":-501.8465327391548,"y":2},{"x":-508.40179937712594,"y":2},{"x":-500.53412575170904,"y":1},{"x":-495.61562693038366,"y":1},{"x":-497.48190772767305,"y":2},{"x":-496.9373129047651,"y":1},{"x":-505.0826307091763,"y":3},{"x":-503.21869622170243,"y":3},{"x":-501.37780951523325,"y":2},{"x":-503.5803879138354,"y":2},{"x":-497.42056013439833,"y":1},{"x":-507.04935885702344,"y":3},{"x":-497.9702464274901,"y":3},{"x":-501.0108970817769,"y":4},{"x":-499.83835422202264,"y":1},{"x":-499.6298105975235,"y":1},{"x":-495.1976783179367,"y":1},{"x":-499.61205773847394,"y":3},{"x":-500.221280663317,"y":5},{"x":-496.62108069816304,"y":1},{"x":-496.4535813883933,"y":6},{"x":-498.1349375874193,"y":2},{"x":-499.93494340676773,"y":2},{"x":-501.93509611844235,"y":3},{"x":-493.5770592400136,"y":1},{"x":-497.94470931829494,"y":3},{"x":-494.33245415649026,"y":1},{"x":-499.23423409480364,"y":3},{"x":-499.6224725947163,"y":3},{"x":-498.6321249048563,"y":1},{"x":-503.6247588937538,"y":2},{"x":-493.7348760711889,"y":1},{"x":-497.62137726334726,"y":3},{"x":-498.5371556055293,"y":1},{"x":-504.8364368849209,"y":2},{"x":-494.8258082476922,"y":1},{"x":-502.1560608281764,"y":2},{"x":-502.3739838416211,"y":2},{"x":10000.0,"y":1},{"x":-499.03329916817904,"y":2},{"x":-499.2500528351739,"y":1},{"x":-508.8022748645485,"y":3},{"x":-496.4025148179654,"y":1},{"x":-500.74784825263134,"y":2},{"x":-499.58939292999895,"y":3},{"x":-498.0234820607484,"y":2},{"x":-496.5795570836029,"y":1},{"x":-500.2844186054292,"y":2},{"x":-498.8754039697213,"y":2},{"x":-499.92934578547903,"y":2},{"x":-502.00361748969243,"y":3},{"x":-501.0222348241351,"y":2},{"x":-502.5164198922748,"y":3},{"x":-501.33851625826213,"y":1},{"x":-503.9669256019367,"y":3},{"x":-498.1369598521311,"y":1},{"x":-499.25774893928747,"y":2},{"x":-505.41147178492326,"y":2},{"x":-503.9044079107466,"y":3},{"x":-499.48333276038534,"y":1},{"x":-503.40330989728125,"y":2},{"x":-497.37328875080647,"y":1},{"x":-498.2343018695137,"y":1},{"x":-503.3447889173644,"y":2},{"x":-505.8974893645782,"y":3},{"x":-499.3243035119383,"y":1},{"x":-497.1588576952334,"y":1},{"x":-499.2589091007569,"y":2},{"x":-505.97285987492944,"y":4},{"x":-498.2697718366958,"y":4},{"x":-493.7097351026998,"y":1},{"x":-500.4604848073217,"y":2},{"x":-497.2918536198266,"y":1}]},{"name":"4b4c9d3b-7d1d-4786-b7ea-513643da79ee","values":[{"x":-510.43588457822347,"y":4},{"x":-504.56327125776,"y":1},{"x":-510.1062536672432,"y":3},{"x":-506.3061354641027,"y":2}]}],"marks":[{"type":"symbol","from":{"data":"8c5f5951-ca78-4887-8214-bbdef68f452c"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"red"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}},{"type":"symbol","from":{"data":"59ee1558-fad8-45e0-a353-da4d28b87534"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"blue"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}},{"type":"symbol","from":{"data":"4b4c9d3b-7d1d-4786-b7ea-513643da79ee"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"#ff29d2"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"8c5f5951-ca78-4887-8214-bbdef68f452c\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"8c5f5951-ca78-4887-8214-bbdef68f452c\", :field \"data.y\"}}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"8c5f5951-ca78-4887-8214-bbdef68f452c\", :values ({:x -503.5174459789853, :y 4} {:x -499.4316262054572, :y 1} {:x -502.35880671632475, :y 2} {:x -498.73696841008416, :y 4} {:x -502.3705588233252, :y 6} {:x -508.56533183071815, :y 3} {:x -499.48891630458525, :y 1} {:x -502.9787357048972, :y 2} {:x -499.280885069778, :y 3} {:x -497.4714785157141, :y 1} {:x -501.1275794490446, :y 3} {:x -494.0348073998511, :y 1} {:x -510.43588457822347, :y 4} {:x -502.62187237841397, :y 3} {:x -502.8438268680818, :y 4} {:x -501.86573464763046, :y 2} {:x -503.0805637517149, :y 3} {:x -500.92782668756934, :y 3} {:x -502.01037541176476, :y 2} {:x -498.3287184512659, :y 1} {:x -505.30387833890757, :y 2} {:x -501.1417323312967, :y 1} {:x -496.5436508055277, :y 1} {:x -498.99696635796084, :y 2} {:x -506.1817616007353, :y 5} {:x -499.1338913027853, :y 1} {:x -497.7627094158679, :y 2} {:x -505.9227055143765, :y 3} {:x -499.3692325689115, :y 2} {:x -498.8413763442127, :y 4} {:x -497.6862699610767, :y 1} {:x -501.3000053462572, :y 2} {:x -496.153581628895, :y 1} {:x -502.53298432920525, :y 2} {:x -499.83573891185296, :y 3} {:x -501.8857720787021, :y 2} {:x -495.40245640724294, :y 1} {:x -502.84881186802943, :y 2} {:x -501.635988853623, :y 3} {:x -499.4539405605884, :y 1} {:x -505.06874927345825, :y 2} {:x -504.56327125776, :y 1} {:x -502.75212065070184, :y 5} {:x -497.5569049984877, :y 1} {:x -502.84395921021724, :y 2} {:x -510.1062536672432, :y 3} {:x -502.3035488183546, :y 2} {:x -496.48449199028033, :y 1} {:x -502.89634770810227, :y 2} {:x -497.97627836819885, :y 1} {:x -505.06413980153013, :y 4} {:x -498.7501444771246, :y 1} {:x -501.9903259134594, :y 2} {:x -497.8842421524649, :y 1} {:x -500.0720904703339, :y 1} {:x -499.8954921635523, :y 1} {:x -498.4903206905261, :y 1} {:x -503.87760277885207, :y 2} {:x -501.77420243383244, :y 1} {:x -497.78292313900545, :y 2} {:x -498.8445033042809, :y 1} {:x -502.32222227503604, :y 2} {:x -497.4604935151835, :y 1} {:x -498.62049749202833, :y 3} {:x -497.1918111745184, :y 1} {:x -500.94984983521306, :y 2} {:x -498.2503121978751, :y 1} {:x -496.8897792138621, :y 1} {:x -495.57404910139024, :y 1} {:x -500.22891901283447, :y 2} {:x -498.6205955745078, :y 3} {:x -496.77155737736234, :y 1} {:x -497.00761484610206, :y 1} {:x -500.4237642441543, :y 3} {:x -504.4522461004044, :y 2} {:x -502.14340937575736, :y 1} {:x -499.2655119829775, :y 2} {:x -497.07401587016113, :y 1} {:x -505.8518221013117, :y 4} {:x -504.550525012022, :y 4} {:x -497.99806810870814, :y 1} {:x -503.11607309186, :y 3} {:x -500.5755317079077, :y 3} {:x -496.82008790239274, :y 1} {:x -501.3726390839969, :y 4} {:x -499.0538208859894, :y 1} {:x -500.13017498166187, :y 3} {:x -494.4470267067836, :y 1} {:x -503.3128998055534, :y 1} {:x -501.54880241344944, :y 5} {:x -496.73286252532887, :y 1} {:x -497.97922887850535, :y 3} {:x -496.2010956046016, :y 1} {:x -501.0318931498808, :y 1} {:x -504.1989665439404, :y 4} {:x -504.1213858456525, :y 2} {:x -497.0960320277515, :y 1} {:x -506.3061354641027, :y 2} {:x -502.13060957653033, :y 3} {:x -502.43186757997705, :y 1})} {:name \"59ee1558-fad8-45e0-a353-da4d28b87534\", :values ({:x -502.1266608411025, :y 3} {:x -496.0752635215373, :y 1} {:x -498.68626832871956, :y 1} {:x -500.0705699190743, :y 2} {:x -501.92180454220954, :y 2} {:x 10000.0, :y 1} {:x -505.905221219608, :y 3} {:x -500.93664640501277, :y 3} {:x -496.3758730134132, :y 3} {:x -493.9302155256942, :y 1} {:x -501.6617282270505, :y 3} {:x -495.8196713903637, :y 1} {:x -499.00791560757517, :y 1} {:x -500.9885727901993, :y 1} {:x -504.2258683673203, :y 3} {:x -499.0182184947877, :y 1} {:x -498.1230597951998, :y 1} {:x -497.29672311058215, :y 2} {:x -498.1758972962788, :y 1} {:x -506.54551426959904, :y 3} {:x -500.6368489097459, :y 3} {:x -503.7388471573078, :y 2} {:x -496.642831147594, :y 1} {:x -502.9651847036066, :y 5} {:x -497.5637009599594, :y 1} {:x -496.96471873008073, :y 1} {:x -494.4172001875669, :y 1} {:x -501.8465327391548, :y 2} {:x -508.40179937712594, :y 2} {:x -500.53412575170904, :y 1} {:x -495.61562693038366, :y 1} {:x -497.48190772767305, :y 2} {:x -496.9373129047651, :y 1} {:x -505.0826307091763, :y 3} {:x -503.21869622170243, :y 3} {:x -501.37780951523325, :y 2} {:x -503.5803879138354, :y 2} {:x -497.42056013439833, :y 1} {:x -507.04935885702344, :y 3} {:x -497.9702464274901, :y 3} {:x -501.0108970817769, :y 4} {:x -499.83835422202264, :y 1} {:x -499.6298105975235, :y 1} {:x -495.1976783179367, :y 1} {:x -499.61205773847394, :y 3} {:x -500.221280663317, :y 5} {:x -496.62108069816304, :y 1} {:x -496.4535813883933, :y 6} {:x -498.1349375874193, :y 2} {:x -499.93494340676773, :y 2} {:x -501.93509611844235, :y 3} {:x -493.5770592400136, :y 1} {:x -497.94470931829494, :y 3} {:x -494.33245415649026, :y 1} {:x -499.23423409480364, :y 3} {:x -499.6224725947163, :y 3} {:x -498.6321249048563, :y 1} {:x -503.6247588937538, :y 2} {:x -493.7348760711889, :y 1} {:x -497.62137726334726, :y 3} {:x -498.5371556055293, :y 1} {:x -504.8364368849209, :y 2} {:x -494.8258082476922, :y 1} {:x -502.1560608281764, :y 2} {:x -502.3739838416211, :y 2} {:x 10000.0, :y 1} {:x -499.03329916817904, :y 2} {:x -499.2500528351739, :y 1} {:x -508.8022748645485, :y 3} {:x -496.4025148179654, :y 1} {:x -500.74784825263134, :y 2} {:x -499.58939292999895, :y 3} {:x -498.0234820607484, :y 2} {:x -496.5795570836029, :y 1} {:x -500.2844186054292, :y 2} {:x -498.8754039697213, :y 2} {:x -499.92934578547903, :y 2} {:x -502.00361748969243, :y 3} {:x -501.0222348241351, :y 2} {:x -502.5164198922748, :y 3} {:x -501.33851625826213, :y 1} {:x -503.9669256019367, :y 3} {:x -498.1369598521311, :y 1} {:x -499.25774893928747, :y 2} {:x -505.41147178492326, :y 2} {:x -503.9044079107466, :y 3} {:x -499.48333276038534, :y 1} {:x -503.40330989728125, :y 2} {:x -497.37328875080647, :y 1} {:x -498.2343018695137, :y 1} {:x -503.3447889173644, :y 2} {:x -505.8974893645782, :y 3} {:x -499.3243035119383, :y 1} {:x -497.1588576952334, :y 1} {:x -499.2589091007569, :y 2} {:x -505.97285987492944, :y 4} {:x -498.2697718366958, :y 4} {:x -493.7097351026998, :y 1} {:x -500.4604848073217, :y 2} {:x -497.2918536198266, :y 1})} {:name \"4b4c9d3b-7d1d-4786-b7ea-513643da79ee\", :values ({:x -510.43588457822347, :y 4} {:x -504.56327125776, :y 1} {:x -510.1062536672432, :y 3} {:x -506.3061354641027, :y 2})}), :marks ({:type \"symbol\", :from {:data \"8c5f5951-ca78-4887-8214-bbdef68f452c\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"red\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}} {:type \"symbol\", :from {:data \"59ee1558-fad8-45e0-a353-da4d28b87534\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"blue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}} {:type \"symbol\", :from {:data \"4b4c9d3b-7d1d-4786-b7ea-513643da79ee\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"#ff29d2\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}})}}"}
;; <=

;; **
;;; Timings of the various steps in each generation.
;;; 
;;; - "Red" - selection time
;;; - "Green" Reproduction time 
;;; - "Blue"  Scoring time
;; **

;; @@
(plot/compose
  (plot/list-plot (:time @metrics/metrics) :colour "yellow" :joined true
                  :plot-range [:all [0 (apply max (:time @metrics/metrics))]])
  (plot/list-plot (:selection-time @metrics/metrics) :colour "red" :joined true)
  (plot/list-plot (:reproduction-time @metrics/metrics) :colour "green" :joined true)
  (plot/list-plot (:scoring-time @metrics/metrics) :colour "blue" :joined true))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"e6b772fc-a3d6-423c-b78b-8441baca2046","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":[0,239]}],"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"data":[{"name":"e6b772fc-a3d6-423c-b78b-8441baca2046","values":[{"x":0,"y":239},{"x":1,"y":185},{"x":2,"y":166},{"x":3,"y":108},{"x":4,"y":115},{"x":5,"y":159},{"x":6,"y":120},{"x":7,"y":124},{"x":8,"y":146},{"x":9,"y":117},{"x":10,"y":111},{"x":11,"y":105},{"x":12,"y":101},{"x":13,"y":83},{"x":14,"y":85},{"x":15,"y":65},{"x":16,"y":103},{"x":17,"y":69},{"x":18,"y":68},{"x":19,"y":80},{"x":20,"y":101},{"x":21,"y":80},{"x":22,"y":81},{"x":23,"y":80},{"x":24,"y":111},{"x":25,"y":85},{"x":26,"y":111},{"x":27,"y":99},{"x":28,"y":65},{"x":29,"y":72},{"x":30,"y":61},{"x":31,"y":70},{"x":32,"y":72},{"x":33,"y":61},{"x":34,"y":87},{"x":35,"y":100},{"x":36,"y":92},{"x":37,"y":66},{"x":38,"y":59},{"x":39,"y":62},{"x":40,"y":59},{"x":41,"y":63},{"x":42,"y":62},{"x":43,"y":60},{"x":44,"y":61},{"x":45,"y":74},{"x":46,"y":61},{"x":47,"y":70},{"x":48,"y":68},{"x":49,"y":61},{"x":50,"y":60},{"x":51,"y":61},{"x":52,"y":84},{"x":53,"y":78},{"x":54,"y":65},{"x":55,"y":60},{"x":56,"y":62},{"x":57,"y":62},{"x":58,"y":72},{"x":59,"y":61},{"x":60,"y":58},{"x":61,"y":73},{"x":62,"y":61},{"x":63,"y":61},{"x":64,"y":66},{"x":65,"y":63},{"x":66,"y":62},{"x":67,"y":61},{"x":68,"y":65},{"x":69,"y":65},{"x":70,"y":72},{"x":71,"y":64},{"x":72,"y":64},{"x":73,"y":62},{"x":74,"y":61},{"x":75,"y":65},{"x":76,"y":69},{"x":77,"y":60},{"x":78,"y":66},{"x":79,"y":62},{"x":80,"y":65},{"x":81,"y":59},{"x":82,"y":66},{"x":83,"y":65},{"x":84,"y":61},{"x":85,"y":63},{"x":86,"y":73},{"x":87,"y":77},{"x":88,"y":64},{"x":89,"y":60},{"x":90,"y":63},{"x":91,"y":60},{"x":92,"y":62},{"x":93,"y":65},{"x":94,"y":70},{"x":95,"y":59},{"x":96,"y":64},{"x":97,"y":64},{"x":98,"y":60},{"x":99,"y":63}]},{"name":"fa2dff72-c52e-4ef8-915c-f518cabf22ef","values":[{"x":0,"y":107},{"x":1,"y":167},{"x":2,"y":156},{"x":3,"y":100},{"x":4,"y":109},{"x":5,"y":148},{"x":6,"y":109},{"x":7,"y":115},{"x":8,"y":139},{"x":9,"y":103},{"x":10,"y":103},{"x":11,"y":99},{"x":12,"y":93},{"x":13,"y":76},{"x":14,"y":80},{"x":15,"y":59},{"x":16,"y":97},{"x":17,"y":64},{"x":18,"y":64},{"x":19,"y":69},{"x":20,"y":89},{"x":21,"y":72},{"x":22,"y":75},{"x":23,"y":73},{"x":24,"y":105},{"x":25,"y":79},{"x":26,"y":102},{"x":27,"y":93},{"x":28,"y":61},{"x":29,"y":67},{"x":30,"y":58},{"x":31,"y":67},{"x":32,"y":66},{"x":33,"y":57},{"x":34,"y":82},{"x":35,"y":94},{"x":36,"y":88},{"x":37,"y":62},{"x":38,"y":56},{"x":39,"y":58},{"x":40,"y":56},{"x":41,"y":59},{"x":42,"y":58},{"x":43,"y":56},{"x":44,"y":57},{"x":45,"y":63},{"x":46,"y":56},{"x":47,"y":66},{"x":48,"y":63},{"x":49,"y":57},{"x":50,"y":57},{"x":51,"y":57},{"x":52,"y":80},{"x":53,"y":75},{"x":54,"y":61},{"x":55,"y":57},{"x":56,"y":58},{"x":57,"y":59},{"x":58,"y":69},{"x":59,"y":57},{"x":60,"y":55},{"x":61,"y":61},{"x":62,"y":58},{"x":63,"y":58},{"x":64,"y":62},{"x":65,"y":60},{"x":66,"y":59},{"x":67,"y":57},{"x":68,"y":61},{"x":69,"y":62},{"x":70,"y":69},{"x":71,"y":61},{"x":72,"y":59},{"x":73,"y":58},{"x":74,"y":58},{"x":75,"y":60},{"x":76,"y":65},{"x":77,"y":56},{"x":78,"y":60},{"x":79,"y":58},{"x":80,"y":61},{"x":81,"y":56},{"x":82,"y":63},{"x":83,"y":62},{"x":84,"y":58},{"x":85,"y":59},{"x":86,"y":70},{"x":87,"y":74},{"x":88,"y":59},{"x":89,"y":57},{"x":90,"y":59},{"x":91,"y":57},{"x":92,"y":59},{"x":93,"y":61},{"x":94,"y":67},{"x":95,"y":56},{"x":96,"y":61},{"x":97,"y":61},{"x":98,"y":57},{"x":99,"y":60}]},{"name":"7cbc0bf3-1c13-4d2f-a2c7-cc1f1bee5640","values":[{"x":0,"y":122},{"x":1,"y":9},{"x":2,"y":6},{"x":3,"y":5},{"x":4,"y":3},{"x":5,"y":6},{"x":6,"y":5},{"x":7,"y":4},{"x":8,"y":4},{"x":9,"y":7},{"x":10,"y":4},{"x":11,"y":4},{"x":12,"y":3},{"x":13,"y":3},{"x":14,"y":3},{"x":15,"y":3},{"x":16,"y":3},{"x":17,"y":2},{"x":18,"y":2},{"x":19,"y":3},{"x":20,"y":4},{"x":21,"y":5},{"x":22,"y":4},{"x":23,"y":4},{"x":24,"y":3},{"x":25,"y":3},{"x":26,"y":3},{"x":27,"y":3},{"x":28,"y":3},{"x":29,"y":3},{"x":30,"y":2},{"x":31,"y":2},{"x":32,"y":4},{"x":33,"y":3},{"x":34,"y":3},{"x":35,"y":4},{"x":36,"y":3},{"x":37,"y":3},{"x":38,"y":2},{"x":39,"y":2},{"x":40,"y":2},{"x":41,"y":2},{"x":42,"y":3},{"x":43,"y":3},{"x":44,"y":3},{"x":45,"y":3},{"x":46,"y":3},{"x":47,"y":2},{"x":48,"y":2},{"x":49,"y":2},{"x":50,"y":2},{"x":51,"y":2},{"x":52,"y":3},{"x":53,"y":2},{"x":54,"y":3},{"x":55,"y":2},{"x":56,"y":3},{"x":57,"y":2},{"x":58,"y":2},{"x":59,"y":3},{"x":60,"y":2},{"x":61,"y":10},{"x":62,"y":2},{"x":63,"y":2},{"x":64,"y":3},{"x":65,"y":2},{"x":66,"y":2},{"x":67,"y":3},{"x":68,"y":3},{"x":69,"y":2},{"x":70,"y":2},{"x":71,"y":2},{"x":72,"y":3},{"x":73,"y":2},{"x":74,"y":2},{"x":75,"y":2},{"x":76,"y":3},{"x":77,"y":3},{"x":78,"y":4},{"x":79,"y":2},{"x":80,"y":3},{"x":81,"y":2},{"x":82,"y":2},{"x":83,"y":2},{"x":84,"y":2},{"x":85,"y":2},{"x":86,"y":2},{"x":87,"y":2},{"x":88,"y":3},{"x":89,"y":2},{"x":90,"y":2},{"x":91,"y":2},{"x":92,"y":2},{"x":93,"y":2},{"x":94,"y":2},{"x":95,"y":2},{"x":96,"y":2},{"x":97,"y":2},{"x":98,"y":3},{"x":99,"y":2}]},{"name":"3232102c-e4b8-4181-bd3b-0f8d732ec7d6","values":[{"x":0,"y":10},{"x":1,"y":9},{"x":2,"y":4},{"x":3,"y":3},{"x":4,"y":3},{"x":5,"y":5},{"x":6,"y":6},{"x":7,"y":5},{"x":8,"y":3},{"x":9,"y":7},{"x":10,"y":4},{"x":11,"y":2},{"x":12,"y":5},{"x":13,"y":4},{"x":14,"y":2},{"x":15,"y":3},{"x":16,"y":3},{"x":17,"y":3},{"x":18,"y":2},{"x":19,"y":8},{"x":20,"y":8},{"x":21,"y":3},{"x":22,"y":2},{"x":23,"y":3},{"x":24,"y":3},{"x":25,"y":3},{"x":26,"y":6},{"x":27,"y":3},{"x":28,"y":1},{"x":29,"y":2},{"x":30,"y":1},{"x":31,"y":1},{"x":32,"y":2},{"x":33,"y":1},{"x":34,"y":2},{"x":35,"y":2},{"x":36,"y":1},{"x":37,"y":1},{"x":38,"y":1},{"x":39,"y":2},{"x":40,"y":1},{"x":41,"y":2},{"x":42,"y":1},{"x":43,"y":1},{"x":44,"y":1},{"x":45,"y":8},{"x":46,"y":2},{"x":47,"y":2},{"x":48,"y":3},{"x":49,"y":2},{"x":50,"y":1},{"x":51,"y":2},{"x":52,"y":1},{"x":53,"y":1},{"x":54,"y":1},{"x":55,"y":1},{"x":56,"y":1},{"x":57,"y":1},{"x":58,"y":1},{"x":59,"y":1},{"x":60,"y":1},{"x":61,"y":2},{"x":62,"y":1},{"x":63,"y":1},{"x":64,"y":1},{"x":65,"y":1},{"x":66,"y":1},{"x":67,"y":1},{"x":68,"y":1},{"x":69,"y":1},{"x":70,"y":1},{"x":71,"y":1},{"x":72,"y":2},{"x":73,"y":2},{"x":74,"y":1},{"x":75,"y":3},{"x":76,"y":1},{"x":77,"y":1},{"x":78,"y":2},{"x":79,"y":2},{"x":80,"y":1},{"x":81,"y":1},{"x":82,"y":1},{"x":83,"y":1},{"x":84,"y":1},{"x":85,"y":2},{"x":86,"y":1},{"x":87,"y":1},{"x":88,"y":2},{"x":89,"y":1},{"x":90,"y":2},{"x":91,"y":1},{"x":92,"y":1},{"x":93,"y":2},{"x":94,"y":1},{"x":95,"y":1},{"x":96,"y":1},{"x":97,"y":1},{"x":98,"y":0},{"x":99,"y":1}]}],"marks":[{"type":"line","from":{"data":"e6b772fc-a3d6-423c-b78b-8441baca2046"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"yellow"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"fa2dff72-c52e-4ef8-915c-f518cabf22ef"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"red"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"7cbc0bf3-1c13-4d2f-a2c7-cc1f1bee5640"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"green"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"3232102c-e4b8-4181-bd3b-0f8d732ec7d6"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"blue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"e6b772fc-a3d6-423c-b78b-8441baca2046\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain [0 239]}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"e6b772fc-a3d6-423c-b78b-8441baca2046\", :values ({:x 0, :y 239} {:x 1, :y 185} {:x 2, :y 166} {:x 3, :y 108} {:x 4, :y 115} {:x 5, :y 159} {:x 6, :y 120} {:x 7, :y 124} {:x 8, :y 146} {:x 9, :y 117} {:x 10, :y 111} {:x 11, :y 105} {:x 12, :y 101} {:x 13, :y 83} {:x 14, :y 85} {:x 15, :y 65} {:x 16, :y 103} {:x 17, :y 69} {:x 18, :y 68} {:x 19, :y 80} {:x 20, :y 101} {:x 21, :y 80} {:x 22, :y 81} {:x 23, :y 80} {:x 24, :y 111} {:x 25, :y 85} {:x 26, :y 111} {:x 27, :y 99} {:x 28, :y 65} {:x 29, :y 72} {:x 30, :y 61} {:x 31, :y 70} {:x 32, :y 72} {:x 33, :y 61} {:x 34, :y 87} {:x 35, :y 100} {:x 36, :y 92} {:x 37, :y 66} {:x 38, :y 59} {:x 39, :y 62} {:x 40, :y 59} {:x 41, :y 63} {:x 42, :y 62} {:x 43, :y 60} {:x 44, :y 61} {:x 45, :y 74} {:x 46, :y 61} {:x 47, :y 70} {:x 48, :y 68} {:x 49, :y 61} {:x 50, :y 60} {:x 51, :y 61} {:x 52, :y 84} {:x 53, :y 78} {:x 54, :y 65} {:x 55, :y 60} {:x 56, :y 62} {:x 57, :y 62} {:x 58, :y 72} {:x 59, :y 61} {:x 60, :y 58} {:x 61, :y 73} {:x 62, :y 61} {:x 63, :y 61} {:x 64, :y 66} {:x 65, :y 63} {:x 66, :y 62} {:x 67, :y 61} {:x 68, :y 65} {:x 69, :y 65} {:x 70, :y 72} {:x 71, :y 64} {:x 72, :y 64} {:x 73, :y 62} {:x 74, :y 61} {:x 75, :y 65} {:x 76, :y 69} {:x 77, :y 60} {:x 78, :y 66} {:x 79, :y 62} {:x 80, :y 65} {:x 81, :y 59} {:x 82, :y 66} {:x 83, :y 65} {:x 84, :y 61} {:x 85, :y 63} {:x 86, :y 73} {:x 87, :y 77} {:x 88, :y 64} {:x 89, :y 60} {:x 90, :y 63} {:x 91, :y 60} {:x 92, :y 62} {:x 93, :y 65} {:x 94, :y 70} {:x 95, :y 59} {:x 96, :y 64} {:x 97, :y 64} {:x 98, :y 60} {:x 99, :y 63})} {:name \"fa2dff72-c52e-4ef8-915c-f518cabf22ef\", :values ({:x 0, :y 107} {:x 1, :y 167} {:x 2, :y 156} {:x 3, :y 100} {:x 4, :y 109} {:x 5, :y 148} {:x 6, :y 109} {:x 7, :y 115} {:x 8, :y 139} {:x 9, :y 103} {:x 10, :y 103} {:x 11, :y 99} {:x 12, :y 93} {:x 13, :y 76} {:x 14, :y 80} {:x 15, :y 59} {:x 16, :y 97} {:x 17, :y 64} {:x 18, :y 64} {:x 19, :y 69} {:x 20, :y 89} {:x 21, :y 72} {:x 22, :y 75} {:x 23, :y 73} {:x 24, :y 105} {:x 25, :y 79} {:x 26, :y 102} {:x 27, :y 93} {:x 28, :y 61} {:x 29, :y 67} {:x 30, :y 58} {:x 31, :y 67} {:x 32, :y 66} {:x 33, :y 57} {:x 34, :y 82} {:x 35, :y 94} {:x 36, :y 88} {:x 37, :y 62} {:x 38, :y 56} {:x 39, :y 58} {:x 40, :y 56} {:x 41, :y 59} {:x 42, :y 58} {:x 43, :y 56} {:x 44, :y 57} {:x 45, :y 63} {:x 46, :y 56} {:x 47, :y 66} {:x 48, :y 63} {:x 49, :y 57} {:x 50, :y 57} {:x 51, :y 57} {:x 52, :y 80} {:x 53, :y 75} {:x 54, :y 61} {:x 55, :y 57} {:x 56, :y 58} {:x 57, :y 59} {:x 58, :y 69} {:x 59, :y 57} {:x 60, :y 55} {:x 61, :y 61} {:x 62, :y 58} {:x 63, :y 58} {:x 64, :y 62} {:x 65, :y 60} {:x 66, :y 59} {:x 67, :y 57} {:x 68, :y 61} {:x 69, :y 62} {:x 70, :y 69} {:x 71, :y 61} {:x 72, :y 59} {:x 73, :y 58} {:x 74, :y 58} {:x 75, :y 60} {:x 76, :y 65} {:x 77, :y 56} {:x 78, :y 60} {:x 79, :y 58} {:x 80, :y 61} {:x 81, :y 56} {:x 82, :y 63} {:x 83, :y 62} {:x 84, :y 58} {:x 85, :y 59} {:x 86, :y 70} {:x 87, :y 74} {:x 88, :y 59} {:x 89, :y 57} {:x 90, :y 59} {:x 91, :y 57} {:x 92, :y 59} {:x 93, :y 61} {:x 94, :y 67} {:x 95, :y 56} {:x 96, :y 61} {:x 97, :y 61} {:x 98, :y 57} {:x 99, :y 60})} {:name \"7cbc0bf3-1c13-4d2f-a2c7-cc1f1bee5640\", :values ({:x 0, :y 122} {:x 1, :y 9} {:x 2, :y 6} {:x 3, :y 5} {:x 4, :y 3} {:x 5, :y 6} {:x 6, :y 5} {:x 7, :y 4} {:x 8, :y 4} {:x 9, :y 7} {:x 10, :y 4} {:x 11, :y 4} {:x 12, :y 3} {:x 13, :y 3} {:x 14, :y 3} {:x 15, :y 3} {:x 16, :y 3} {:x 17, :y 2} {:x 18, :y 2} {:x 19, :y 3} {:x 20, :y 4} {:x 21, :y 5} {:x 22, :y 4} {:x 23, :y 4} {:x 24, :y 3} {:x 25, :y 3} {:x 26, :y 3} {:x 27, :y 3} {:x 28, :y 3} {:x 29, :y 3} {:x 30, :y 2} {:x 31, :y 2} {:x 32, :y 4} {:x 33, :y 3} {:x 34, :y 3} {:x 35, :y 4} {:x 36, :y 3} {:x 37, :y 3} {:x 38, :y 2} {:x 39, :y 2} {:x 40, :y 2} {:x 41, :y 2} {:x 42, :y 3} {:x 43, :y 3} {:x 44, :y 3} {:x 45, :y 3} {:x 46, :y 3} {:x 47, :y 2} {:x 48, :y 2} {:x 49, :y 2} {:x 50, :y 2} {:x 51, :y 2} {:x 52, :y 3} {:x 53, :y 2} {:x 54, :y 3} {:x 55, :y 2} {:x 56, :y 3} {:x 57, :y 2} {:x 58, :y 2} {:x 59, :y 3} {:x 60, :y 2} {:x 61, :y 10} {:x 62, :y 2} {:x 63, :y 2} {:x 64, :y 3} {:x 65, :y 2} {:x 66, :y 2} {:x 67, :y 3} {:x 68, :y 3} {:x 69, :y 2} {:x 70, :y 2} {:x 71, :y 2} {:x 72, :y 3} {:x 73, :y 2} {:x 74, :y 2} {:x 75, :y 2} {:x 76, :y 3} {:x 77, :y 3} {:x 78, :y 4} {:x 79, :y 2} {:x 80, :y 3} {:x 81, :y 2} {:x 82, :y 2} {:x 83, :y 2} {:x 84, :y 2} {:x 85, :y 2} {:x 86, :y 2} {:x 87, :y 2} {:x 88, :y 3} {:x 89, :y 2} {:x 90, :y 2} {:x 91, :y 2} {:x 92, :y 2} {:x 93, :y 2} {:x 94, :y 2} {:x 95, :y 2} {:x 96, :y 2} {:x 97, :y 2} {:x 98, :y 3} {:x 99, :y 2})} {:name \"3232102c-e4b8-4181-bd3b-0f8d732ec7d6\", :values ({:x 0, :y 10} {:x 1, :y 9} {:x 2, :y 4} {:x 3, :y 3} {:x 4, :y 3} {:x 5, :y 5} {:x 6, :y 6} {:x 7, :y 5} {:x 8, :y 3} {:x 9, :y 7} {:x 10, :y 4} {:x 11, :y 2} {:x 12, :y 5} {:x 13, :y 4} {:x 14, :y 2} {:x 15, :y 3} {:x 16, :y 3} {:x 17, :y 3} {:x 18, :y 2} {:x 19, :y 8} {:x 20, :y 8} {:x 21, :y 3} {:x 22, :y 2} {:x 23, :y 3} {:x 24, :y 3} {:x 25, :y 3} {:x 26, :y 6} {:x 27, :y 3} {:x 28, :y 1} {:x 29, :y 2} {:x 30, :y 1} {:x 31, :y 1} {:x 32, :y 2} {:x 33, :y 1} {:x 34, :y 2} {:x 35, :y 2} {:x 36, :y 1} {:x 37, :y 1} {:x 38, :y 1} {:x 39, :y 2} {:x 40, :y 1} {:x 41, :y 2} {:x 42, :y 1} {:x 43, :y 1} {:x 44, :y 1} {:x 45, :y 8} {:x 46, :y 2} {:x 47, :y 2} {:x 48, :y 3} {:x 49, :y 2} {:x 50, :y 1} {:x 51, :y 2} {:x 52, :y 1} {:x 53, :y 1} {:x 54, :y 1} {:x 55, :y 1} {:x 56, :y 1} {:x 57, :y 1} {:x 58, :y 1} {:x 59, :y 1} {:x 60, :y 1} {:x 61, :y 2} {:x 62, :y 1} {:x 63, :y 1} {:x 64, :y 1} {:x 65, :y 1} {:x 66, :y 1} {:x 67, :y 1} {:x 68, :y 1} {:x 69, :y 1} {:x 70, :y 1} {:x 71, :y 1} {:x 72, :y 2} {:x 73, :y 2} {:x 74, :y 1} {:x 75, :y 3} {:x 76, :y 1} {:x 77, :y 1} {:x 78, :y 2} {:x 79, :y 2} {:x 80, :y 1} {:x 81, :y 1} {:x 82, :y 1} {:x 83, :y 1} {:x 84, :y 1} {:x 85, :y 2} {:x 86, :y 1} {:x 87, :y 1} {:x 88, :y 2} {:x 89, :y 1} {:x 90, :y 2} {:x 91, :y 1} {:x 92, :y 1} {:x 93, :y 2} {:x 94, :y 1} {:x 95, :y 1} {:x 96, :y 1} {:x 97, :y 1} {:x 98, :y 0} {:x 99, :y 1})}), :marks ({:type \"line\", :from {:data \"e6b772fc-a3d6-423c-b78b-8441baca2046\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"yellow\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"fa2dff72-c52e-4ef8-915c-f518cabf22ef\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"red\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"7cbc0bf3-1c13-4d2f-a2c7-cc1f1bee5640\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"green\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"3232102c-e4b8-4181-bd3b-0f8d732ec7d6\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"blue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
;; <=

;; **
;;; Spread of complexity in population
;; **

;; @@
(plot/histogram (map :complexity (:rabble result)))
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"4758d724-1b70-41e8-96af-30a149c14d49","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"4758d724-1b70-41e8-96af-30a149c14d49","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"4758d724-1b70-41e8-96af-30a149c14d49"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"4758d724-1b70-41e8-96af-30a149c14d49","values":[{"x":1.0,"y":0},{"x":1.25,"y":78.0},{"x":1.5,"y":0.0},{"x":1.75,"y":0.0},{"x":2.0,"y":0.0},{"x":2.25,"y":16.0},{"x":2.5,"y":0.0},{"x":2.75,"y":0.0},{"x":3.0,"y":0.0},{"x":3.25,"y":6.0},{"x":3.5,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"4758d724-1b70-41e8-96af-30a149c14d49\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"4758d724-1b70-41e8-96af-30a149c14d49\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"4758d724-1b70-41e8-96af-30a149c14d49\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"4758d724-1b70-41e8-96af-30a149c14d49\", :values ({:x 1.0, :y 0} {:x 1.25, :y 78.0} {:x 1.5, :y 0.0} {:x 1.75, :y 0.0} {:x 2.0, :y 0.0} {:x 2.25, :y 16.0} {:x 2.5, :y 0.0} {:x 2.75, :y 0.0} {:x 3.0, :y 0.0} {:x 3.25, :y 6.0} {:x 3.5, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; @@
(plot/list-plot (:mean (:complexity @metrics/metrics)) :joined true)
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"b1138c65-b230-4ba5-9d6b-3035a4af42f1","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"b1138c65-b230-4ba5-9d6b-3035a4af42f1","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"b1138c65-b230-4ba5-9d6b-3035a4af42f1"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"b1138c65-b230-4ba5-9d6b-3035a4af42f1","values":[{"x":0,"y":2.04},{"x":1,"y":1.56},{"x":2,"y":1.4},{"x":3,"y":1.495},{"x":4,"y":1.515},{"x":5,"y":1.505},{"x":6,"y":1.435},{"x":7,"y":1.41},{"x":8,"y":1.435},{"x":9,"y":1.44},{"x":10,"y":1.37},{"x":11,"y":1.39},{"x":12,"y":1.445},{"x":13,"y":1.36},{"x":14,"y":1.365},{"x":15,"y":1.34},{"x":16,"y":1.305},{"x":17,"y":1.23},{"x":18,"y":1.295},{"x":19,"y":1.255},{"x":20,"y":1.17},{"x":21,"y":1.185},{"x":22,"y":1.22},{"x":23,"y":1.215},{"x":24,"y":1.19},{"x":25,"y":1.25},{"x":26,"y":1.195},{"x":27,"y":1.22},{"x":28,"y":1.25},{"x":29,"y":1.225},{"x":30,"y":1.225},{"x":31,"y":1.2},{"x":32,"y":1.215},{"x":33,"y":1.255},{"x":34,"y":1.155},{"x":35,"y":1.16},{"x":36,"y":1.165},{"x":37,"y":1.21},{"x":38,"y":1.22},{"x":39,"y":1.24},{"x":40,"y":1.25},{"x":41,"y":1.325},{"x":42,"y":1.27},{"x":43,"y":1.235},{"x":44,"y":1.255},{"x":45,"y":1.225},{"x":46,"y":1.185},{"x":47,"y":1.16},{"x":48,"y":1.155},{"x":49,"y":1.15},{"x":50,"y":1.12},{"x":51,"y":1.14},{"x":52,"y":1.155},{"x":53,"y":1.14},{"x":54,"y":1.18},{"x":55,"y":1.25},{"x":56,"y":1.3},{"x":57,"y":1.35},{"x":58,"y":1.335},{"x":59,"y":1.27},{"x":60,"y":1.22},{"x":61,"y":1.12},{"x":62,"y":1.125},{"x":63,"y":1.11},{"x":64,"y":1.155},{"x":65,"y":1.11},{"x":66,"y":1.12},{"x":67,"y":1.165},{"x":68,"y":1.13},{"x":69,"y":1.17},{"x":70,"y":1.16},{"x":71,"y":1.145},{"x":72,"y":1.135},{"x":73,"y":1.12},{"x":74,"y":1.195},{"x":75,"y":1.26},{"x":76,"y":1.315},{"x":77,"y":1.275},{"x":78,"y":1.315},{"x":79,"y":1.28},{"x":80,"y":1.255},{"x":81,"y":1.25},{"x":82,"y":1.27},{"x":83,"y":1.32},{"x":84,"y":1.285},{"x":85,"y":1.24},{"x":86,"y":1.205},{"x":87,"y":1.235},{"x":88,"y":1.205},{"x":89,"y":1.225},{"x":90,"y":1.22},{"x":91,"y":1.2},{"x":92,"y":1.23},{"x":93,"y":1.185},{"x":94,"y":1.23},{"x":95,"y":1.255},{"x":96,"y":1.22},{"x":97,"y":1.25},{"x":98,"y":1.265},{"x":99,"y":1.265}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"b1138c65-b230-4ba5-9d6b-3035a4af42f1\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"b1138c65-b230-4ba5-9d6b-3035a4af42f1\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"b1138c65-b230-4ba5-9d6b-3035a4af42f1\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"b1138c65-b230-4ba5-9d6b-3035a4af42f1\", :values ({:x 0, :y 2.04} {:x 1, :y 1.56} {:x 2, :y 1.4} {:x 3, :y 1.495} {:x 4, :y 1.515} {:x 5, :y 1.505} {:x 6, :y 1.435} {:x 7, :y 1.41} {:x 8, :y 1.435} {:x 9, :y 1.44} {:x 10, :y 1.37} {:x 11, :y 1.39} {:x 12, :y 1.445} {:x 13, :y 1.36} {:x 14, :y 1.365} {:x 15, :y 1.34} {:x 16, :y 1.305} {:x 17, :y 1.23} {:x 18, :y 1.295} {:x 19, :y 1.255} {:x 20, :y 1.17} {:x 21, :y 1.185} {:x 22, :y 1.22} {:x 23, :y 1.215} {:x 24, :y 1.19} {:x 25, :y 1.25} {:x 26, :y 1.195} {:x 27, :y 1.22} {:x 28, :y 1.25} {:x 29, :y 1.225} {:x 30, :y 1.225} {:x 31, :y 1.2} {:x 32, :y 1.215} {:x 33, :y 1.255} {:x 34, :y 1.155} {:x 35, :y 1.16} {:x 36, :y 1.165} {:x 37, :y 1.21} {:x 38, :y 1.22} {:x 39, :y 1.24} {:x 40, :y 1.25} {:x 41, :y 1.325} {:x 42, :y 1.27} {:x 43, :y 1.235} {:x 44, :y 1.255} {:x 45, :y 1.225} {:x 46, :y 1.185} {:x 47, :y 1.16} {:x 48, :y 1.155} {:x 49, :y 1.15} {:x 50, :y 1.12} {:x 51, :y 1.14} {:x 52, :y 1.155} {:x 53, :y 1.14} {:x 54, :y 1.18} {:x 55, :y 1.25} {:x 56, :y 1.3} {:x 57, :y 1.35} {:x 58, :y 1.335} {:x 59, :y 1.27} {:x 60, :y 1.22} {:x 61, :y 1.12} {:x 62, :y 1.125} {:x 63, :y 1.11} {:x 64, :y 1.155} {:x 65, :y 1.11} {:x 66, :y 1.12} {:x 67, :y 1.165} {:x 68, :y 1.13} {:x 69, :y 1.17} {:x 70, :y 1.16} {:x 71, :y 1.145} {:x 72, :y 1.135} {:x 73, :y 1.12} {:x 74, :y 1.195} {:x 75, :y 1.26} {:x 76, :y 1.315} {:x 77, :y 1.275} {:x 78, :y 1.315} {:x 79, :y 1.28} {:x 80, :y 1.255} {:x 81, :y 1.25} {:x 82, :y 1.27} {:x 83, :y 1.32} {:x 84, :y 1.285} {:x 85, :y 1.24} {:x 86, :y 1.205} {:x 87, :y 1.235} {:x 88, :y 1.205} {:x 89, :y 1.225} {:x 90, :y 1.22} {:x 91, :y 1.2} {:x 92, :y 1.23} {:x 93, :y 1.185} {:x 94, :y 1.23} {:x 95, :y 1.255} {:x 96, :y 1.22} {:x 97, :y 1.25} {:x 98, :y 1.265} {:x 99, :y 1.265})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=
