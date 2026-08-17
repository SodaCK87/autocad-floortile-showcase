;; =========================================================================
;; FLR_Tests — FLR_Core 無頭斷言測試
;; 執行：accoreconsole /s FLR_Tests.scr
;; =========================================================================
;; 全部為純邏輯，不需 AutoCAD API，可完全自動驗證。

(setq *PASS* 0 *FAIL* 0 *FAILED* '())

(defun T= (name got want / ok)
  (setq ok (cond ((and (numberp got) (numberp want)) (equal got want 1e-6))
                 (T (equal got want))))
  (if ok (setq *PASS* (1+ *PASS*))
         (progn (setq *FAIL* (1+ *FAIL*) *FAILED* (cons name *FAILED*))
                (princ (strcat "\n  [FAIL] " name
                               "  got=" (vl-princ-to-string got)
                               "  want=" (vl-princ-to-string want)))))
  ok)

(defun T-true (name got) (T= name (if got T nil) T))
(defun T-nil  (name got) (T= name (if got T nil) nil))

;;; ---------------- 測試資料 ----------------
(setq SQ100  '((0.0 0.0) (100.0 0.0) (100.0 100.0) (0.0 100.0)))
(setq UNIT   '((0.0 0.0) (1.0 0.0) (1.0 1.0) (0.0 1.0)))
;; L 形：右上缺 4x4
(setq LSHAPE '((0.0 0.0) (10.0 0.0) (10.0 6.0) (6.0 6.0) (6.0 10.0) (0.0 10.0)))
;; U 形：上緣中央被咬掉 x[4,6] y[5,10]
(setq USHAPE '((0.0 0.0) (10.0 0.0) (10.0 10.0) (6.0 10.0)
               (6.0 5.0) (4.0 5.0) (4.0 10.0) (0.0 10.0)))
;; 大區域，上緣有凹槽（牆端點停在磚內 → 產生 U 形）
(setq NOTCH  '((-5.0 -5.0) (15.0 -5.0) (15.0 15.0) (6.0 15.0)
               (6.0 5.0) (4.0 5.0) (4.0 15.0) (-5.0 15.0)))
;; 大區域，右上缺角（→ 產生 L 形角磚）
(setq CORNER '((-5.0 -5.0) (15.0 -5.0) (15.0 6.0) (6.0 6.0) (6.0 15.0) (-5.0 15.0)))

;; 使用者的 A1001 平面圖（7 個區域，2026-08-13 由圖面實際傾印）。
;; 0.4 的效能病灶（-6.78861e-05 的繪圖誤差）與 0.5 的違規病灶都出自這張圖，
;; 兩者都只在**真實幾何**上重現得出來——合成的兩個矩形測不到。
;; 磚 30×30、縫 0.3、交丁 1/2、下限 25% 下的實測基準（675 組全精算）：
;;   Pareto 前緣＝18 刀/違規 93、19 刀/53、20 刀/25，最低 25 片在 (0.00, 14.60)。
(setq A1001 (list
  '((1082.5 -341.5) (1242.5 -341.5) (1242.5 -609.5) (1082.5 -609.5))
  '((812.5 -443.5) (1068.5 -443.5) (1068.5 -571.5) (978.5 -571.5)
    (978.5 -609.5) (812.5 -609.5))
  '((928.5 -6.78861e-05) (928.5 -430.5) (1069.5 -430.5) (1069.5 -328.5)
    (1245.0 -328.5) (1245.0 0.0))
  '((799.5 -612.0) (799.5 -430.5) (916.5 -430.5) (916.5 -310.5)
    (0.0 -310.5) (0.0 -612.0))
  '((628.5 -298.5) (916.5 -298.5) (916.5 -6.78861e-05) (628.5 -6.78861e-05))
  '((353.5 0.0) (616.5 0.0) (616.5 -298.5) (353.5 -298.5))
  '((0.0 0.0) (341.5 0.0) (341.5 -298.5) (0.0 -298.5))))

(defun mkcfg (tw th gap stag ox oy)
  (list (cons 'tw tw) (cons 'th th) (cons 'gap gap) (cons 'stagger stag)
        (cons 'ox ox) (cons 'oy oy) (cons 'tol 1e-6)
        (cons 'mincut 0.25) (cons 'sizeq 0.5) (cons 'maxcand 24)))

(defun bboxes (rs) (mapcar 'FLR:BBox rs))

;;; ---- 0.6 的幾何基本函式原文，作為 §5d 的對照組（0.8 走訪改寫）----
;;
;; 0.8 把六個基本函式從 `(nth i poly)` 改成游標走訪（O(n²) → O(n)）。
;; **純重構最危險的地方是頭尾相接那一輪的順序**——多一輪、少一輪、或是
;; prev/cur/next 的相位差一格，在凸多邊形上通常還是對的，只有在特定形狀上錯。
;; 那種錯不會拋例外，只會讓面積或凹頂點數靜默偏掉。
;; 所以舊實作要留著逐項比對，不能只靠「既有斷言還是綠的」。

(defun TREF:SignedArea2 (poly / n i a b s)
  (setq s 0.0 n (length poly) i 0)
  (if (< n 3)
    0.0
    (progn
      (while (< i n)
        (setq a (nth i poly)
              b (nth (rem (1+ i) n) poly)
              s (+ s (- (* (car a) (cadr b)) (* (car b) (cadr a)))))
        (setq i (1+ i)))
      (/ s 2.0))))

(defun TREF:PtInPoly2 (pt poly / n i a b inside px py ca cb den xx)
  (setq inside nil n (length poly) i 0 px (car pt) py (cadr pt))
  (while (< i n)
    (setq a  (nth i poly)
          b  (nth (rem (1+ i) n) poly)
          ca (> (cadr a) py)
          cb (> (cadr b) py))
    (if (or (and ca (not cb)) (and cb (not ca)))
      (progn
        (setq den (- (cadr b) (cadr a)))
        (if (not (equal den 0.0 FLR:EPS))
          (progn
            (setq xx (+ (car a) (/ (* (- (car b) (car a)) (- py (cadr a))) den)))
            (if (< px xx) (setq inside (not inside)))))))
    (setq i (1+ i)))
  inside)

(defun TREF:Clip1_2 (poly axis dir val / out n i a b ia ib den tt px py)
  (setq out '() n (length poly) i 0)
  (while (< i n)
    (setq a  (nth i poly)
          b  (nth (rem (1+ i) n) poly)
          ia (>= (* dir (- (nth axis a) val)) (- FLR:EPS))
          ib (>= (* dir (- (nth axis b) val)) (- FLR:EPS))
          den (- (nth axis b) (nth axis a)))
    (setq tt (if (equal den 0.0 FLR:EPS) 0.0 (/ (- val (nth axis a)) den)))
    (setq px (+ (car a)  (* tt (- (car b)  (car a))))
          py (+ (cadr a) (* tt (- (cadr b) (cadr a)))))
    (cond
      ((and ia ib)       (setq out (cons b out)))
      ((and ia (not ib)) (setq out (cons (list px py) out)))
      ((and (not ia) ib) (setq out (cons b (cons (list px py) out)))))
    (setq i (1+ i)))
  (reverse out))

(defun TREF:Clean2 (poly tol / out n i a b c cr dab keep)
  (if (< (length poly) 3)
    poly
    (progn
      (setq out '() n (length poly) i 0)
      (while (< i n)
        (setq b (nth i poly)
              c (nth (rem (1+ i) n) poly))
        (if (> (distance b c) tol) (setq out (cons b out)))
        (setq i (1+ i)))
      (setq poly (reverse out))
      (if (< (length poly) 3)
        poly
        (progn
          (setq out '() n (length poly) i 0)
          (while (< i n)
            (setq b   (nth i poly)
                  a   (nth (rem (+ i (1- n)) n) poly)
                  c   (nth (rem (1+ i) n) poly)
                  dab (distance a b)
                  keep T)
            (if (< dab tol)
              (setq keep nil)
              (progn
                (setq cr (- (* (- (car b) (car a)) (- (cadr c) (cadr b)))
                            (* (- (cadr b) (cadr a)) (- (car c) (car b)))))
                (if (< (abs (/ cr dab)) tol) (setq keep nil))))
            (if keep (setq out (cons b out)))
            (setq i (1+ i)))
          (reverse out))))))

;; 刻意呼叫 TREF:Clean2 而不是 FLR:Clean——對照組要能獨立於受測程式碼
(defun TREF:ReflexCount2 (poly tol / n i a b c cr dab sgn cnt)
  (setq poly (TREF:Clean2 poly tol) n (length poly))
  (if (< n 4)
    0
    (progn
      (setq sgn (if (>= (TREF:SignedArea2 poly) 0.0) 1.0 -1.0) cnt 0 i 0)
      (while (< i n)
        (setq a   (nth (rem (+ i (1- n)) n) poly)
              b   (nth i poly)
              c   (nth (rem (1+ i) n) poly)
              cr  (- (* (- (car b) (car a)) (- (cadr c) (cadr b)))
                     (* (- (cadr b) (cadr a)) (- (car c) (car b))))
              dab (distance a b))
        (if (< (* sgn (/ cr dab)) (- tol)) (setq cnt (1+ cnt)))
        (setq i (1+ i)))
      cnt)))

(defun TREF:Cross2 (pc axis v tol / n i a b oth out d tt)
  (setq n (length pc) i 0 out '() oth (- 1 axis))
  (while (< i n)
    (setq a (nth i pc) b (nth (rem (1+ i) n) pc)
          d (- (nth axis b) (nth axis a)))
    (if (> (abs d) tol)
      (progn
        (setq tt (/ (- v (nth axis a)) d))
        (if (and (> tt (- tol)) (< tt (+ 1.0 tol)))
          (setq out (cons (+ (nth oth a) (* tt (- (nth oth b) (nth oth a)))) out)))))
    (setq i (1+ i)))
  (vl-sort out '<))

;; 1.4.1 的 FLR:LegScan 原文，作為 §7c 的對照組。
;; 1.4.2 把它拆成吃現成條帶的 FLR:LegScanS 並對軸向矩形跳過 y 向分解，
;; 那是**靠推導**省下來的（見 FLR_Core.lsp 的註）——推導只能靠逐片比對來檢驗，
;; 所以舊實作要留著。這裡刻意不呼叫核心的任何新函式，才是獨立的第二意見。
(defun TREF:LegScan2 (pc tw th minr tol / c0 c1 mn)
  (setq c0 0 c1 0 mn nil)
  (foreach s (FLR:Strips pc 0 tol)
    (if (< (car s)  (- (* tw minr) tol)) (setq c0 (1+ c0)))
    (if (< (cadr s) (- (* th minr) tol)) (setq c0 (1+ c0)))
    (setq mn (if mn (min mn (car s) (cadr s)) (min (car s) (cadr s)))))
  (foreach s (FLR:Strips pc 1 tol)
    (if (< (car s)  (- (* th minr) tol)) (setq c1 (1+ c1)))
    (if (< (cadr s) (- (* tw minr) tol)) (setq c1 (1+ c1)))
    (setq mn (if mn (min mn (car s) (cadr s)) (min (car s) (cadr s)))))
  (list (max c0 c1) (if mn mn 0.0)))

;;; ================================================================
(defun c:FLRTEST (/ t0 t1 a b c d p r st cfg regs rbs res top n bb)
  (princ "\n================ FLR_Core 斷言測試 ================")

  ;; ---- 0. 載入斷言（cond 錯位不會報錯，只有 defun 存在性能證明結構完整）----
  (princ "\n-- 0. 載入 --")
  (foreach f '(FLR:SignedArea FLR:Area FLR:BBox FLR:BBoxHit FLR:PtInPoly
               FLR:Clip1 FLR:ClipRect FLR:Clean FLR:ReflexCount
               FLR:FindOverlaps FLR:Cfg FLR:MakeGrid FLR:Floor FLR:Ceil
               FLR:ClipTileByRegions FLR:RectCut FLR:SubtractRect FLR:SubtractRects
               FLR:ClassifyTile FLR:Round
               FLR:Evaluate FLR:Layout FLR:StatsOf FLR:Modp FLR:Candidates FLR:Better
               ;; 0.3 新增
               FLR:SymErr FLR:EdgeCuts
               ;; 0.4
               FLR:ShortN FLR:PreRank FLR:IsRect FLR:AxisTol
               FLR:Optimize FLR:Insert FLR:CellEstimate FLR:CandCount FLR:GroupSizes
               ;; 0.5
               FLR:AxisWalls FLR:Phases FLR:BadWeight FLR:BadWindows
               ;; 1.4（排序目標可選）
               FLR:GoalOf FLR:GoalLabel FLR:GoalCfg FLR:GoalKeys
               FLR:BetterG FLR:InsertG FLR:AxisSym FLR:NestClass FLR:StripCutsOf
               FLR:WasteWeight FLR:SlabRects FLR:RegionRects FLR:WithRects FLR:CellHome
               ;; 0.2 新增
               FLR:SegX FLR:X3 FLR:AnyVertexIn FLR:EdgeCross FLR:Frac
               FLR:Cross FLR:SpanAt FLR:Strips FLR:MinLeg FLR:BadLeg FLR:LegScan
               FLR:LegScanS FLR:RectFrag
               ;; 1.4.4（切割清單）
               FLR:CutList FLR:CutListOf FLR:CutRep FLR:SigBad FLR:SigLess
               FLR:StripCuts
               FLR:RepOf FLR:IsRect FLR:StagOffs FLR:AxisEdges FLR:AxisSizes
               FLR:PreRank FLR:ShortN FLR:WorkCount FLR:Pack FLR:Nest
               FLR:RotPt FLR:RotPoly)
    (T-true (strcat "defun " (vl-symbol-name f)) (boundp f)))

  ;; ---- 1. 面積 ----
  (princ "\n-- 1. 面積 --")
  (T= "Area 單位方"      (FLR:Area UNIT)   1.0)
  (T= "Area 100x100"     (FLR:Area SQ100)  10000.0)
  (T= "Area L形"         (FLR:Area LSHAPE) 84.0)     ; 100 - 16
  (T= "Area U形"         (FLR:Area USHAPE) 90.0)     ; 100 - 2x5
  (T= "SignedArea CW為負" (< (FLR:SignedArea (reverse UNIT)) 0.0) T)

  ;; ---- 2. BBox ----
  (princ "\n-- 2. BBox --")
  (T= "BBox L形" (FLR:BBox LSHAPE) '(0.0 0.0 10.0 10.0))
  (T-true "BBoxHit 相交"  (FLR:BBoxHit '(0.0 0.0 10.0 10.0) '(5.0 5.0 15.0 15.0)))
  (T-nil  "BBoxHit 分離"  (FLR:BBoxHit '(0.0 0.0 10.0 10.0) '(20.0 20.0 30.0 30.0)))
  (T-nil  "BBoxHit 僅共邊" (FLR:BBoxHit '(0.0 0.0 10.0 10.0) '(10.0 0.0 20.0 10.0)))

  ;; ---- 3. 點在多邊形內（曾因 /= 比 T/nil 而爆，故重點測）----
  (princ "\n-- 3. PtInPoly --")
  (T-true "內部點"       (FLR:PtInPoly '(5.0 5.0)  SQ100))
  (T-nil  "外部點"       (FLR:PtInPoly '(-1.0 5.0) SQ100))
  (T-nil  "遠外部點"     (FLR:PtInPoly '(500.0 5.0) SQ100))
  (T-true "L形凸側內"    (FLR:PtInPoly '(2.0 8.0)  LSHAPE))
  (T-nil  "L形缺角內"    (FLR:PtInPoly '(8.0 8.0)  LSHAPE))

  ;; ---- 4. 矩形裁剪 ----
  (princ "\n-- 4. ClipRect --")
  (setq p (FLR:ClipRect SQ100 10.0 10.0 20.0 20.0))
  (T= "全包含→面積100" (FLR:Area p) 100.0)
  (setq p (FLR:ClipRect SQ100 -10.0 -10.0 10.0 10.0))
  (T= "部分重疊→面積100" (FLR:Area p) 100.0)
  (T-nil "完全在外→nil" (FLR:ClipRect SQ100 200.0 200.0 300.0 300.0))
  (setq p (FLR:ClipRect LSHAPE 0.0 0.0 10.0 10.0))
  (T= "L形自裁→面積不變" (FLR:Area p) 84.0)

  ;; ---- 5. 形狀判定（U 形規則的核心）----
  (princ "\n-- 5. ReflexCount --")
  (T= "矩形→0 凹點"  (FLR:ReflexCount SQ100  1e-6) 0)
  (T= "L形→1 凹點"   (FLR:ReflexCount LSHAPE 1e-6) 1)
  (T= "U形→2 凹點"   (FLR:ReflexCount USHAPE 1e-6) 2)
  (T= "反向L形→1"    (FLR:ReflexCount (reverse LSHAPE) 1e-6) 1)

  ;; ---- 5b. Clean：連續重複點每組只能留一個 ----
  ;; 2026-08-12 實機抓到：舊版把一組重複點的**兩份都刪掉**，角點消失、
  ;; 多邊形短路成對角線，畫面出現三角形。S-H 在邊端點落在裁切線上時必產生重複點。
  (princ "\n-- 5b. Clean 重複點 --")
  (setq p '((0.0 0.0) (10.0 0.0) (10.0 0.0) (10.0 10.0) (0.0 10.0)))
  (setq r (FLR:Clean p 1e-6))
  (T= "含1組重複點→仍是 4 點" (length r) 4)
  (T= "面積不變 100"          (FLR:Area r) 100.0)
  (setq p '((0.0 0.0) (10.0 0.0) (10.0 10.0) (10.0 10.0) (0.0 10.0) (0.0 0.0)))
  (setq r (FLR:Clean p 1e-6))
  (T= "頭尾+中間重複→4 點"   (length r) 4)
  (T= "面積不變 100(2)"       (FLR:Area r) 100.0)
  (setq p '((0.0 0.0) (0.0 0.0) (0.0 0.0) (10.0 0.0) (10.0 10.0) (0.0 10.0)))
  (setq r (FLR:Clean p 1e-6))
  (T= "三連重複→4 點"        (length r) 4)
  (T= "面積不變 100(3)"       (FLR:Area r) 100.0)
  ;; 共線點仍要被清掉
  (setq r (FLR:Clean '((0.0 0.0) (5.0 0.0) (10.0 0.0)
                       (10.0 10.0) (0.0 10.0)) 1e-6))
  (T= "共線中點被清除→4 點"  (length r) 4)
  ;; 重複＋共線混合
  (setq r (FLR:Clean '((0.0 0.0) (5.0 0.0) (5.0 0.0) (10.0 0.0)
                       (10.0 10.0) (10.0 10.0) (0.0 10.0)) 1e-6))
  (T= "重複+共線混合→4 點"   (length r) 4)
  (T= "面積不變 100(4)"       (FLR:Area r) 100.0)

  ;; ---- 5d. 游標走訪改寫：六個基本函式必須與 0.6 的原文逐項相同（0.8）----
  ;;
  ;; 0.8 把 `(nth i poly)` 換成游標走訪（O(n²) → O(n)）。**純重構，輸出必須一字不差。**
  ;; 最危險的是頭尾相接那一輪：多一輪、少一輪、或 prev/cur/next 相位差一格，
  ;; 在凸多邊形上通常還是對的，只有特定形狀會錯，而且不會拋例外——只會讓面積或
  ;; 凹頂點數靜默偏掉。故留一份 0.6 的原文（TREF:*，見檔頭）逐項比對。
  ;;
  ;; 語料要用**真實佈置產生的碎片**：S-H 會在裁切線上留重複點與零寬贅邊，
  ;; 而那正是頭尾相接那一輪最容易出錯的輸入。合成的矩形測不到。
  (princ "\n-- 5d. 走訪改寫 --")
  ;; 對照組自己也要先確認存在——名字打錯的話下面會直接擲例外中斷整場測試，
  ;; 而不是給一個看得懂的紅燈（section 0 只驗成品的符號，不放測試自己的）
  (foreach f '(TREF:SignedArea2 TREF:PtInPoly2 TREF:Clip1_2 TREF:Clean2
               TREF:ReflexCount2 TREF:Cross2 TREF:LegScan2)
    (T-true (strcat "對照組 " (vl-symbol-name f)) (boundp f)))
  (setq p '())                                   ; p 當語料清單用
  (foreach rg A1001 (setq p (cons rg p)))        ; 原始區域（4~6 頂點，含繪圖誤差那一個）
  (foreach shape (list SQ100 LSHAPE USHAPE CORNER
                       '((0.0 0.0) (30.0 0.0) (0.0 30.0))
                       '((0.0 0.0) (10.0 0.0) (10.0 0.0) (10.0 10.0) (0.0 10.0))
                       '((0.0 0.0) (5.0 0.0) (10.0 0.0) (10.0 10.0) (0.0 10.0)))
    (setq p (cons shape p)))
  ;; 真實碎片
  (setq regs (list '((0.0 0.0) (300.0 0.0) (300.0 80.0) (80.0 80.0) (80.0 300.0) (0.0 300.0))
                   '((350.0 0.0) (650.0 0.0) (650.0 200.0) (350.0 200.0)))
        rbs  (bboxes regs)
        cfg  (mkcfg 30.0 30.0 0.3 0.5 7.0 3.0))
  (foreach cell (FLR:MakeGrid cfg '(0.0 0.0 650.0 300.0))
    ;; 未經 Clean 的 S-H 原始輸出也要進語料——重複點正是 Clean 第一步要處理的
    (foreach rg regs
      (setq r (FLR:ClipRect rg (car cell) (cadr cell) (caddr cell) (cadddr cell)))
      (if (and r (>= (length r) 3)) (setq p (cons r p)))))
  (setq n 0 a 0 b 0 c 0)
  (foreach q p
    (setq a (1+ a))
    (if (not (equal (FLR:SignedArea q) (TREF:SignedArea2 q) 1e-12)) (setq n (1+ n)))
    (if (not (equal (FLR:Clean q 1e-6) (TREF:Clean2 q 1e-6) 1e-12)) (setq b (1+ b)))
    (if (/= (FLR:ReflexCount q 1e-6) (TREF:ReflexCount2 q 1e-6)) (setq c (1+ c))))
  (T-true (strcat "語料夠多（" (itoa a) " 個多邊形）") (> a 150))
  (T= "SignedArea 與 0.6 原文相同（不符數）"  n 0)
  (T= "Clean 與 0.6 原文相同（不符數）"       b 0)
  (T= "ReflexCount 與 0.6 原文相同（不符數）" c 0)
  ;; Clip1：四個半平面 × 每個語料。裁切值刻意落在頂點上（den=0 與重複點的來源）
  (setq n 0 c 0)
  (foreach q p
    (foreach ax '(0 1)
      (foreach dr '(1 -1)
        (foreach v '(0.0 7.5 30.0 80.0 300.5)
          (setq c (1+ c))
          (if (not (equal (FLR:Clip1 q ax dr v) (TREF:Clip1_2 q ax dr v) 1e-12))
            (setq n (1+ n)))))))
  (T= "Clip1 與 0.6 原文相同（不符數）" n 0)
  (T-true (strcat "Clip1 測了 " (itoa c) " 種組合") (> c 3000))
  ;; Cross：兩軸 × 多個掃描位置
  (setq n 0)
  (foreach q p
    (foreach ax '(0 1)
      (foreach v '(-1.0 0.0 5.0 15.0 40.0 100.0)
        (if (not (equal (FLR:Cross q ax v 1e-6) (TREF:Cross2 q ax v 1e-6) 1e-12))
          (setq n (1+ n))))))
  (T= "Cross 與 0.6 原文相同（不符數）" n 0)
  ;; PtInPoly：格點掃描，內外都要掃到
  (setq n 0 b 0)
  (foreach q p
    (foreach pt '((5.0 5.0) (50.0 50.0) (0.0 0.0) (85.0 12.0) (-5.0 40.0)
                  (1000.0 -400.0) (1100.0 -500.0) (900.0 -300.0))
      (if (FLR:PtInPoly pt q) (setq b (1+ b)))
      (if (not (eq (and (FLR:PtInPoly pt q) T) (and (TREF:PtInPoly2 pt q) T)))
        (setq n (1+ n)))))
  (T= "PtInPoly 與 0.6 原文相同（不符數）" n 0)
  ;; 沒有這條的話，上面那個 0 也可能是「兩邊都一律回 nil」
  (T-true (strcat "PtInPoly 真的有回過 T（" (itoa b) " 次）") (> b 20))
  ;; 空清單不可以炸——0.8 的走訪版對 nil 會走到 (car nil)，故各自加了守門
  (T= "SignedArea 吃空清單" (FLR:SignedArea '()) 0.0)
  (T-nil "PtInPoly 吃空清單" (FLR:PtInPoly '(0.0 0.0) '()))
  (T-nil "Clip1 吃空清單"    (FLR:Clip1 '() 0 1 0.0))
  (T-nil "Cross 吃空清單"    (FLR:Cross '() 0 0.0 1e-6))
  (T-nil "EdgeCross 吃空清單" (FLR:EdgeCross '() SQ100))
  ;; 還原共用測試資料
  (setq regs (list SQ100) rbs (bboxes regs) cfg (mkcfg 10.0 10.0 0.0 0.0 0.0 0.0))

  ;; ---- 5c. 尺寸分群：完整連結，誤差必須真的 ≤ 容差 ----
  ;; 0.1 版是單一連結（只跟前一個比），會鏈式串連：0..10 以容差 1.0 分群
  ;; 竟然只剩一組、代表值 10、誤差 10.0。而「代表值取該組最大值」的前提
  ;; 就是「組內落差 ≤ 容差」，串連直接違反前提。0.2 改成跟該組最小值比。
  (princ "\n-- 5c. GroupSizes（完整連結）--")
  ;; 間距依序為 4.2 / 2.8 / 0.25 / 3.05 / 4.85 / 4.2
  (setq a '(7.9 12.1 14.9 15.15 18.2 23.05 27.25))
  (T= "容差 0.01 → 7 種" (length (FLR:GroupSizes a 0.01)) 7)
  (T= "容差 0.5  → 6 種" (length (FLR:GroupSizes a 0.5))  6)  ; 併 0.25 那組
  (T= "容差 2.0  → 6 種" (length (FLR:GroupSizes a 2.0))  6)  ; 2.8 仍 > 2.0
  (T= "容差 3.0  → 6 種" (length (FLR:GroupSizes a 3.0))  6)  ; 14.9+15.15 落差 0.25
  (T= "容差 5.0  → 3 種" (length (FLR:GroupSizes a 5.0))  3)
  ;; 單調性：容差變大，組數只能持平或變少，絕不可增加
  (setq b 999 st T)
  (foreach q '(0.01 0.1 0.25 0.5 1.0 2.0 3.0 5.0 10.0)
    (setq c (length (FLR:GroupSizes a q)))
    (if (> c b) (setq st nil))
    (setq b c))
  (T-true "組數隨容差單調不增" st)
  ;; 【核心不變量】任何一個尺寸與它的代表值之差都不得超過容差。
  ;; 這條就是 0.1 版真正壞掉的地方，也是這次改動的全部理由。
  (defun maxerr (sizes grp / e best)
    (setq best 0.0)
    (foreach s sizes
      (setq e nil)
      (foreach g grp
        (if (and (>= g (- s 1e-9)) (or (null e) (< (- g s) e))) (setq e (- g s))))
      (if (and e (> e best)) (setq best e)))
    best)
  (foreach q '(0.01 0.5 1.0 2.0 3.0 5.0 10.0)
    (T-true (strcat "容差 " (rtos q 2 2) " 誤差 <= 容差")
            (<= (maxerr a (FLR:GroupSizes a q)) (+ q 1e-9))))
  ;; 鏈式串連的原始反例：0.1 版在此回 1 組、誤差 10
  (setq b '(0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0))
  (T= "0..10 容差 1.0 → 6 種" (length (FLR:GroupSizes b 1.0)) 6)
  (T-true "0..10 誤差 <= 1.0" (<= (maxerr b (FLR:GroupSizes b 1.0)) (+ 1.0 1e-9)))
  ;; 代表值取該組實際最大值——切最大的再修，不會切過頭
  (T= "14.9/15.15 同組 → 取 15.15"
      (nth 2 (FLR:GroupSizes a 0.5)) 15.15)
  (T-true "代表值皆為原始值之一"
          (vl-every '(lambda (g) (vl-some '(lambda (s) (equal s g 1e-9)) a))
                    (FLR:GroupSizes a 0.5)))
  ;; RepOf：每個尺寸都要對應到 >= 自己的最小代表值
  (T= "RepOf 12.1 → 12.1" (FLR:RepOf 12.1 (FLR:GroupSizes a 0.5)) 12.1)
  (T= "RepOf 14.9 → 15.15" (FLR:RepOf 14.9 (FLR:GroupSizes a 0.5)) 15.15)

  ;; ---- 6. 區域重疊偵測 ----
  (princ "\n-- 6. FindOverlaps --")
  (setq a '((0.0 0.0) (10.0 0.0) (10.0 10.0) (0.0 10.0))
        b '((5.0 5.0) (15.0 5.0) (15.0 15.0) (5.0 15.0))
        c '((20.0 0.0) (30.0 0.0) (30.0 10.0) (20.0 10.0)))
  (T= "重疊被偵測"   (length (FLR:FindOverlaps (list a b) 1e-6)) 1)
  (T= "分離不誤報"   (length (FLR:FindOverlaps (list a c) 1e-6)) 0)
  (T= "僅共邊不誤報" (length (FLR:FindOverlaps
                               (list a '((10.0 0.0) (20.0 0.0) (20.0 10.0) (10.0 10.0)))
                               1e-6)) 0)
  ;; 0.1 版只做「ri 的某頂點落在 rj 內」這個單向測試，
  ;; 下面兩種很常見的重疊完全測不到（實測皆回 nil）。
  ;; ① 房間裡再框一小塊：外框的角點全在內框之外
  (T= "包含被偵測"
      (length (FLR:FindOverlaps
                (list SQ100 '((20.0 20.0) (40.0 20.0) (40.0 40.0) (20.0 40.0))) 1e-6)) 1)
  (T= "包含（順序相反）亦被偵測"
      (length (FLR:FindOverlaps
                (list '((20.0 20.0) (40.0 20.0) (40.0 40.0) (20.0 40.0)) SQ100) 1e-6)) 1)
  ;; ② 兩條帶狀區域十字交叉：雙方頂點都在對方之外
  (T= "十字交叉被偵測"
      (length (FLR:FindOverlaps
                (list '((0.0 40.0) (100.0 40.0) (100.0 60.0) (0.0 60.0))
                      '((40.0 0.0) (60.0 0.0) (60.0 100.0) (40.0 100.0))) 1e-6)) 1)
  ;; 三區兩兩重疊 → 三組
  (T= "三區兩兩重疊 → 3 組"
      (length (FLR:FindOverlaps
                (list '((0.0 0.0) (10.0 0.0) (10.0 10.0) (0.0 10.0))
                      '((5.0 5.0) (15.0 5.0) (15.0 15.0) (5.0 15.0))
                      '((2.0 2.0) (12.0 2.0) (12.0 12.0) (2.0 12.0))) 1e-6)) 3)
  ;; 端點接觸不算相交，否則相鄰房間會被誤報
  (T-nil "共點不誤報"
         (FLR:SegX '(0.0 0.0) '(10.0 0.0) '(10.0 0.0) '(10.0 10.0)))
  (T-true "真交叉判為相交"
          (FLR:SegX '(0.0 0.0) '(10.0 10.0) '(0.0 10.0) '(10.0 0.0)))

  ;; ---- 7. 網格 ----
  (princ "\n-- 7. MakeGrid --")
  (setq cfg (mkcfg 10.0 10.0 0.0 0.0 0.0 0.0))
  (T= "100x100 / 磚10 無縫 → 100 格"
      (length (FLR:MakeGrid cfg '(0.0 0.0 100.0 100.0))) 100)
  (setq cfg (mkcfg 10.0 10.0 0.0 0.0 5.0 0.0))
  (T= "偏移5 → 110 格"
      (length (FLR:MakeGrid cfg '(0.0 0.0 100.0 100.0))) 110)
  ;; 交丁：第 0 列不偏、第 1 列偏半磚
  (setq cfg (mkcfg 10.0 10.0 0.0 0.5 0.0 0.0))
  (setq r (FLR:MakeGrid cfg '(0.0 0.0 20.0 20.0)))
  (setq a (car (vl-remove-if-not '(lambda (x) (and (= (nth 4 x) 0) (= (nth 5 x) 0))) r))
        b (car (vl-remove-if-not '(lambda (x) (and (= (nth 4 x) 1) (= (nth 5 x) 0))) r)))
  (T= "交丁 第0列 x=0"    (car a) 0.0)
  (T= "交丁 第1列 x=5"    (car b) 5.0)
  (setq cfg (mkcfg 10.0 10.0 0.0 0.0 0.0 0.0))
  (setq r (FLR:MakeGrid cfg '(0.0 0.0 20.0 20.0)))
  (setq b (car (vl-remove-if-not '(lambda (x) (and (= (nth 4 x) 1) (= (nth 5 x) 0))) r)))
  (T= "正鋪 第1列 x=0"    (car b) 0.0)
  ;; 1/3 交丁必須每 3 列準確歸位。0.1 版用 (r*stag - floor(r*stag)) 直接算，
  ;; r=3 時得 0.9999.. 而不是 0，該列整個偏掉一個間距、逐列累積漂移。
  (T= "Frac(3 x 1/3) = 0"  (FLR:Frac (* 3.0 (/ 1.0 3.0))) 0.0)
  (T= "Frac(1.5) = 0.5"    (FLR:Frac 1.5) 0.5)
  (T= "Frac(-0.5) = 0.5"   (FLR:Frac -0.5) 0.5)
  (setq cfg (mkcfg 10.0 10.0 0.0 (/ 1.0 3.0) 0.0 0.0))
  (setq r (FLR:MakeGrid cfg '(0.0 0.0 10.0 60.0)))
  (setq a (car (vl-remove-if-not '(lambda (x) (and (= (nth 4 x) 0) (= (nth 5 x) 0))) r))
        b (car (vl-remove-if-not '(lambda (x) (and (= (nth 4 x) 3) (= (nth 5 x) 0))) r)))
  (T= "1/3 交丁 第3列與第0列同 x" (car b) (car a))
  ;; 使用者填的是 0.3333，要吸附回真分數才不會漂移
  (T= "SnapStag 0.3333 → 1/3" (FLR:SnapStag 0.3333) (/ 1.0 3.0))
  (T= "SnapStag 0.5 不動"     (FLR:SnapStag 0.5) 0.5)
  (T= "SnapStag 0.37 不亂吸"  (FLR:SnapStag 0.37) 0.37)

  ;; ---- 8. 單片磚對多區域裁切：對半分 ----
  (princ "\n-- 8. 對半分（牆完全穿過磚）--")
  (setq regs (list '((0.0 0.0) (4.0 0.0) (4.0 10.0) (0.0 10.0))      ; 牆左
                   '((6.0 0.0) (10.0 0.0) (10.0 10.0) (6.0 10.0)))   ; 牆右
        rbs  (bboxes regs)
        cfg  (mkcfg 10.0 10.0 0.0 0.0 0.0 0.0))
  (setq res (FLR:ClipTileByRegions '(0.0 0.0 10.0 10.0) regs rbs 1e-6))
  (T= "裂成 2 片" (length res) 2)
  (T= "左片面積40" (nth 2 (nth 0 res)) 40.0)
  (T= "右片面積40" (nth 2 (nth 1 res)) 40.0)
  (setq res (FLR:ClassifyTile '(0.0 0.0 10.0 10.0) regs rbs cfg))
  (T= "分類=cut"        (cdr (assoc 'kind res)) 'cut)
  (T= "片數=2"          (cdr (assoc 'pieces res)) 2)
  (T-nil "兩片皆合格(4>=2.5)" (cdr (assoc 'bad res)))

  ;; 兩片都太窄 → 違規
  (setq regs (list '((0.0 0.0) (2.0 0.0) (2.0 10.0) (0.0 10.0))
                   '((8.0 0.0) (10.0 0.0) (10.0 10.0) (8.0 10.0)))
        rbs  (bboxes regs))
  (setq res (FLR:ClassifyTile '(0.0 0.0 10.0 10.0) regs rbs cfg))
  (T-true "兩片皆 2 < 2.5 → 違規" (cdr (assoc 'bad res)))

  ;; ---- 9. U 形不被允許 ----
  (princ "\n-- 9. U 形判定 --")
  (setq regs (list NOTCH) rbs (bboxes regs))
  (setq res (FLR:ClassifyTile '(0.0 0.0 10.0 10.0) regs rbs cfg))
  (T= "牆端點停在磚內 → ushape" (cdr (assoc 'kind res)) 'ushape)
  (T= "U形仍為單一連通片"       (cdr (assoc 'pieces res)) 1)

  (setq regs (list CORNER) rbs (bboxes regs))
  (setq res (FLR:ClassifyTile '(0.0 0.0 10.0 10.0) regs rbs cfg))
  (T= "L形角磚 → cut(允許)" (cdr (assoc 'kind res)) 'cut)
  (T= "L形面積84（bbox仍滿格，不可用bbox判整磚）" (cdr (assoc 'area res)) 84.0)
  ;; 條帶分解：左帶 6 寬 10 高、右帶 4 寬 6 高 → 要量 6、4、6
  ;; （0.1 版只報 (6 6)，漏掉右側那條 4 寬的子片）
  (T= "L形裁切尺寸 = {4,6}"
      (FLR:GroupSizes (cdr (assoc 'cuts res)) 1e-6) '(4.0 6.0))

  (setq regs (list SQ100) rbs (bboxes regs))
  (setq res (FLR:ClassifyTile '(10.0 10.0 20.0 20.0) regs rbs cfg))
  (T= "完整磚 → full"   (cdr (assoc 'kind res)) 'full)
  (T= "完整磚 無切割線" (cdr (assoc 'cuts res)) '())

  ;; ---- 9b. 條帶分解（FLR-01 / FLR-02 的修正）----
  ;; 0.1 版：下限檢查用碎片 bbox，L 形的 bbox 仍是滿格 → 兩腳只有 2 的 L 磚
  ;;         bad=nil 完全漏檢；裁切尺寸又用 bbox 中心決定量哪一側，
  ;;         L 形的中心落在被挖掉的角裡 → 每條切割線都判到反側（報 28，實際 2）。
  (princ "\n-- 9b. Strips / MinLeg / BadLeg --")
  ;; 矩形碎片：只有一條條帶，結果必須與 0.1 版完全相同（不可造成回歸）
  (setq p '((0.0 0.0) (6.0 0.0) (6.0 10.0) (0.0 10.0)))
  (T= "矩形 6x10 → 1 條帶"   (length (FLR:Strips p 0 1e-6)) 1)
  (T= "矩形 6x10 條帶 (6 10)" (car (FLR:Strips p 0 1e-6)) '(6.0 10.0))
  (T= "矩形 6x10 最小腳寬 6"  (FLR:MinLeg p 1e-6) 6.0)
  (T= "矩形 6x10 裁切 {6}"    (FLR:StripCuts p 10.0 10.0 1e-6) '(6.0))
  (setq res (FLR:ClassifyTile '(0.0 0.0 10.0 10.0)
                              (list '((0.0 0.0) (6.0 0.0) (6.0 10.0) (0.0 10.0)))
                              (list '(0.0 0.0 6.0 10.0)) cfg))
  (T= "留左片→量6" (cdr (assoc 'cuts res)) '(6.0))
  (setq res (FLR:ClassifyTile '(0.0 0.0 10.0 10.0)
                              (list '((6.0 0.0) (10.0 0.0) (10.0 10.0) (6.0 10.0)))
                              (list '(6.0 0.0 10.0 10.0)) cfg))
  (T= "留右片→量4" (cdr (assoc 'cuts res)) '(4.0))
  ;; 【核心案例】兩腳都只有 2 的 L 形（磚 30x30，下限 1/4 = 7.5）
  (setq p '((0.0 0.0) (30.0 0.0) (30.0 2.0) (2.0 2.0) (2.0 30.0) (0.0 30.0)))
  (T= "L(2,2) 最小腳寬 = 2"  (FLR:MinLeg p 1e-6) 2.0)
  (T-true "L(2,2) 判為違規"  (FLR:BadLeg p 30.0 30.0 0.25 1e-6))
  (T-true "L(2,2) 裁切含 2"
          (vl-some '(lambda (s) (equal s 2.0 1e-6)) (FLR:StripCuts p 30.0 30.0 1e-6)))
  ;; 兩種方位都要抓得到（0.1 版兩種都漏）
  (setq p '((0.0 0.0) (30.0 0.0) (30.0 2.0) (10.0 2.0) (10.0 30.0) (0.0 30.0)))
  (T= "L(10,2) 最小腳寬 = 2" (FLR:MinLeg p 1e-6) 2.0)
  (T-true "L(10,2) 判為違規" (FLR:BadLeg p 30.0 30.0 0.25 1e-6))
  (setq p '((0.0 0.0) (30.0 0.0) (30.0 10.0) (2.0 10.0) (2.0 30.0) (0.0 30.0)))
  (T= "L(2,10) 最小腳寬 = 2" (FLR:MinLeg p 1e-6) 2.0)
  (T-true "L(2,10) 判為違規" (FLR:BadLeg p 30.0 30.0 0.25 1e-6))
  ;; 合格的 L 不可被誤判
  (setq p '((0.0 0.0) (30.0 0.0) (30.0 12.0) (12.0 12.0) (12.0 30.0) (0.0 30.0)))
  (T= "L(12,12) 最小腳寬 = 12" (FLR:MinLeg p 1e-6) 12.0)
  (T-nil "L(12,12) 合格"       (FLR:BadLeg p 30.0 30.0 0.25 1e-6))
  ;; 整磚：無裁切尺寸、腳寬即磚寬
  (setq p '((0.0 0.0) (30.0 0.0) (30.0 30.0) (0.0 30.0)))
  (T= "整磚最小腳寬 30"  (FLR:MinLeg p 1e-6) 30.0)
  (T= "整磚無裁切尺寸"   (FLR:StripCuts p 30.0 30.0 1e-6) '())
  (T-nil "整磚不違規"    (FLR:BadLeg p 30.0 30.0 0.25 1e-6))
  ;; 長寬不同的磚：門檻必須各自對應到該軸，不可混用
  (setq p '((0.0 0.0) (60.0 0.0) (60.0 6.0) (0.0 6.0)))   ; 60x30 磚切成 60x6
  (T-true "60x30 磚：高 6 < 7.5 → 違規" (FLR:BadLeg p 60.0 30.0 0.25 1e-6))
  (setq p '((0.0 0.0) (12.0 0.0) (12.0 30.0) (0.0 30.0))) ; 60x30 磚切成 12x30
  (T-true "60x30 磚：寬 12 < 15 → 違規" (FLR:BadLeg p 60.0 30.0 0.25 1e-6))
  (setq p '((0.0 0.0) (20.0 0.0) (20.0 30.0) (0.0 30.0)))
  (T-nil "60x30 磚：寬 20 >= 15 → 合格" (FLR:BadLeg p 60.0 30.0 0.25 1e-6))
  ;; 斜向碎片：條帶長取三處最小值，往尖端收窄的那頭才抓得到
  (setq p '((0.0 0.0) (30.0 0.0) (0.0 30.0)))
  (T-true "直角三角形最小腳寬 < 3" (< (FLR:MinLeg p 1e-6) 3.0))

  ;; ---- 9c. LegScan：違規「邊數」與最窄腳寬（1.3.2）----
  ;; 使用者問「一塊磁磚違規兩次也只算一次，能不能更詳細」。這一段釘住兩件事：
  ;;   ① 邊數的定義（寬與高各算一次）
  ;;   ② **邊數 0 ⟺ 不違規**——若兩者會不一致，清單就會出現
  ;;      「違規 1 片、腳 0」這種自相矛盾的行，而且不會有任何錯誤訊息。
  (princ "\n-- 9c. LegScan --")
  ;; 角料 2x3（磚 30x30，下限 1/4 = 7.5）：寬不足一次、高不足一次 = 2
  (setq p '((0.0 0.0) (2.0 0.0) (2.0 3.0) (0.0 3.0)))
  (T= "角料 2x3 → 邊數 2"    (car (FLR:LegScan p 30.0 30.0 0.25 1e-6)) 2)
  (T= "角料 2x3 → 最窄 2"    (cadr (FLR:LegScan p 30.0 30.0 0.25 1e-6)) 2.0)
  ;; 只有一個方向不足的邊料：算 1，不可因為兩個方向的分解而變成 2
  (setq p '((0.0 0.0) (5.0 0.0) (5.0 30.0) (0.0 30.0)))
  (T= "邊料 5x30 → 邊數 1"   (car (FLR:LegScan p 30.0 30.0 0.25 1e-6)) 1)
  ;; 合格的碎片：邊數必須是 0（否則會憑空多出違規）
  (setq p '((0.0 0.0) (20.0 0.0) (20.0 30.0) (0.0 30.0)))
  (T= "合格 20x30 → 邊數 0"  (car (FLR:LegScan p 30.0 30.0 0.25 1e-6)) 0)
  (T= "合格 20x30 → 最窄 20" (cadr (FLR:LegScan p 30.0 30.0 0.25 1e-6)) 20.0)
  ;; 整磚：不違規，最窄＝磚寬（全案最小值才不會被整磚拉低）
  (setq p '((0.0 0.0) (30.0 0.0) (30.0 30.0) (0.0 30.0)))
  (T= "整磚 → 邊數 0"        (car (FLR:LegScan p 30.0 30.0 0.25 1e-6)) 0)
  (T= "整磚 → 最窄 30"       (cadr (FLR:LegScan p 30.0 30.0 0.25 1e-6)) 30.0)
  ;; 兩腳都只有 2 的 L 形：x 向分解出兩條條帶，各自寬或高不足
  (setq p '((0.0 0.0) (30.0 0.0) (30.0 2.0) (2.0 2.0) (2.0 30.0) (0.0 30.0)))
  (T-true "L(2,2) 邊數 >= 2" (>= (car (FLR:LegScan p 30.0 30.0 0.25 1e-6)) 2))
  (T= "L(2,2) 最窄 2"        (cadr (FLR:LegScan p 30.0 30.0 0.25 1e-6)) 2.0)
  ;; BadLeg 改由 LegScan 實作，兩者不可能不一致——這幾條是把它釘住
  (foreach q (list '((0.0 0.0) (2.0 0.0) (2.0 3.0) (0.0 3.0))
                   '((0.0 0.0) (5.0 0.0) (5.0 30.0) (0.0 30.0))
                   '((0.0 0.0) (20.0 0.0) (20.0 30.0) (0.0 30.0))
                   '((0.0 0.0) (30.0 0.0) (30.0 30.0) (0.0 30.0)))
    (T= "BadLeg ⟺ 邊數>0"
        (if (FLR:BadLeg q 30.0 30.0 0.25 1e-6) T nil)
        (if (> (car (FLR:LegScan q 30.0 30.0 0.25 1e-6)) 0) T nil)))
  ;; 長寬不同的磚：門檻要各自對應到該軸（60x30 切成 60x6 只有高不足＝1）
  (setq p '((0.0 0.0) (60.0 0.0) (60.0 6.0) (0.0 6.0)))
  (T= "60x30 磚切 60x6 → 邊數 1" (car (FLR:LegScan p 60.0 30.0 0.25 1e-6)) 1)

  ;; ---- 10. 整體評估 ----
  (princ "\n-- 10. Evaluate --")
  (setq regs (list SQ100) rbs (bboxes regs))
  (setq cfg (mkcfg 10.0 10.0 0.0 0.0 0.0 0.0))
  (setq res (FLR:Evaluate cfg regs rbs '(0.0 0.0 100.0 100.0)))
  (T= "對齊：整磚100"   (cdr (assoc 'full res)) 100)
  (T= "對齊：裁切0"     (cdr (assoc 'cut res)) 0)
  (T= "對齊：尺寸種類0" (cdr (assoc 'cutsizes res)) 0)
  (T= "對齊：面積10000" (cdr (assoc 'area res)) 10000.0)

  (setq cfg (mkcfg 10.0 10.0 0.0 0.0 5.0 0.0))
  (setq res (FLR:Evaluate cfg regs rbs '(0.0 0.0 100.0 100.0)))
  (T= "偏移5：整磚90"    (cdr (assoc 'full res)) 90)
  (T= "偏移5：裁切20"    (cdr (assoc 'cut res)) 20)
  (T= "偏移5：尺寸種類1" (cdr (assoc 'cutsizes res)) 1)
  (T= "偏移5：面積不變"  (cdr (assoc 'area res)) 10000.0)
  (T= "偏移5：無違規"    (cdr (assoc 'bad res)) 0)

  (setq cfg (mkcfg 10.0 10.0 0.0 0.0 8.0 0.0))
  (setq res (FLR:Evaluate cfg regs rbs '(0.0 0.0 100.0 100.0)))
  (T= "偏移8：尺寸種類2" (cdr (assoc 'cutsizes res)) 2)
  (T= "偏移8：違規10片(右側寬2<2.5)" (cdr (assoc 'bad res)) 10)

  ;; ---- 10b. Layout（預覽／繪圖用，須與 Evaluate 一致）----
  (princ "\n-- 10b. Layout --")
  (setq cfg (mkcfg 10.0 10.0 0.0 0.0 8.0 0.0))
  (setq res (FLR:Evaluate cfg regs rbs '(0.0 0.0 100.0 100.0)))
  (setq p   (FLR:Layout   cfg regs rbs '(0.0 0.0 100.0 100.0)))
  (T= "Layout 片數 = Evaluate 片數" (length p) (cdr (assoc 'tiles res)))
  ;; 把每片 parts 的多邊形面積加總，須等於 Evaluate 的總面積
  (setq a 0.0)
  (foreach it p
    (foreach pt (cdr (assoc 'parts (cdr it)))
      (setq a (+ a (FLR:Area (nth 1 pt))))))
  (T= "Layout 多邊形面積加總 = Evaluate 面積" a (cdr (assoc 'area res)))
  (T-true "每片皆帶 parts"
          (vl-every '(lambda (it) (> (length (cdr (assoc 'parts (cdr it)))) 0)) p))
  ;; parts 的多邊形必須是可直接繪製的合法環（>=3 點）
  (T-true "parts 皆為合法多邊形"
          (vl-every '(lambda (it)
                       (vl-every '(lambda (pt) (>= (length (nth 1 pt)) 3))
                                 (cdr (assoc 'parts (cdr it)))))
                    p))

  ;; ---- 10c. StatsOf 必須與 Evaluate 完全一致（封死 TAD-01 那類問題）----
  (princ "\n-- 10c. StatsOf ≡ Evaluate --")
  (foreach ofs '(0.0 5.0 8.0 3.7)
    (setq cfg (mkcfg 10.0 10.0 0.0 0.0 ofs 0.0))
    (setq res (FLR:Evaluate cfg regs rbs '(0.0 0.0 100.0 100.0)))
    (setq b   (FLR:StatsOf (FLR:Layout cfg regs rbs '(0.0 0.0 100.0 100.0)) cfg))
    (foreach fld '(full cut ushape bad badlegs minleg tiles cutsizes)
      (T= (strcat "ox=" (rtos ofs 2 1) " " (vl-symbol-name fld))
          (cdr (assoc fld b)) (cdr (assoc fld res))))
    (T= (strcat "ox=" (rtos ofs 2 1) " area")
        (cdr (assoc 'area b)) (cdr (assoc 'area res))))

  ;; 分區小計加總須等於總面積
  (princ "\n-- 10d. 分區小計 --")
  (setq regs (list '((0.0 0.0) (100.0 0.0) (100.0 60.0) (0.0 60.0))
                   '((0.0 64.0) (48.0 64.0) (48.0 140.0) (0.0 140.0))
                   '((52.0 64.0) (100.0 64.0) (100.0 140.0) (52.0 140.0)))
        rbs (bboxes regs))
  (setq cfg (mkcfg 10.0 10.0 0.0 0.0 0.0 0.0))
  (setq b (FLR:StatsOf (FLR:Layout cfg regs rbs '(0.0 0.0 100.0 140.0)) cfg))
  (T= "分區數 = 3" (length (cdr (assoc 'byregion b))) 3)
  (setq a 0.0)
  (foreach r (cdr (assoc 'byregion b)) (setq a (+ a (nth 2 r))))
  (T= "分區面積加總 = 總面積" a (cdr (assoc 'area b)))
  (T= "區0 面積 6000" (nth 2 (assoc 0 (cdr (assoc 'byregion b)))) 6000.0)
  (T= "區1 面積 3648" (nth 2 (assoc 1 (cdr (assoc 'byregion b)))) (* 48.0 76.0))
  (T= "區2 面積 3648" (nth 2 (assoc 2 (cdr (assoc 'byregion b)))) (* 48.0 76.0))
  ;; 分區記錄改為 (idx 片數 面積 整磚 裁切 違規)，欄位語意須與表頭一致
  (T= "分區記錄 6 欄" (length (assoc 0 (cdr (assoc 'byregion b)))) 6)
  (setq a 0 c 0)
  (foreach r (cdr (assoc 'byregion b))
    (setq a (+ a (nth 3 r)) c (+ c (nth 4 r)))
    ;; 片數必須等於整磚＋裁切，否則欄位對不起來
    (T= (strcat "區" (itoa (nth 0 r)) " 片數 = 整磚+裁切")
        (nth 1 r) (+ (nth 3 r) (nth 4 r))))
  ;; 三區互不相鄰（隔 4 單位牆、磚寬 10 會跨區），分區加總 >= 總計
  (T-true "分區整磚加總 >= 總整磚" (>= a (cdr (assoc 'full b))))
  (T-true "分區裁切加總 >= 總裁切" (>= c (cdr (assoc 'cut b))))
  (setq regs (list SQ100) rbs (bboxes regs))

  ;; ---- 10e. 排序規則⑤：總片數並列時比裁切磚數 ----
  ;; GUI 實測發現：前四項並列時，整磚73/裁切46 會排在 整磚87/裁切32 前面。
  (princ "\n-- 10e. 排序 tie-break --")
  (setq a (list (cons 'ushape 0) (cons 'cutsizes 6) (cons 'bad 0)
                (cons 'tiles 119) (cons 'cut 32) (cons 'full 87))
        b (list (cons 'ushape 0) (cons 'cutsizes 6) (cons 'bad 0)
                (cons 'tiles 119) (cons 'cut 46) (cons 'full 73)))
  (T-true "裁切少者勝"     (FLR:Better a b))
  (T-nil  "裁切多者不勝"   (FLR:Better b a))
  ;; 前四項仍優先於裁切數
  (setq b (subst (cons 'cutsizes 5) (assoc 'cutsizes b) b))
  (T-true "刀數優先於裁切數" (FLR:Better b a))
  ;; 完全相同 → 兩邊皆非「較優」，排序才不會震盪
  (T-nil "完全相同不判優" (FLR:Better a a))

  ;; ---- 10f. 規模估計（大平面保護用）----
  (princ "\n-- 10f. 規模估計 --")
  (setq cfg (mkcfg 10.0 10.0 0.0 0.0 0.0 0.0))
  ;; 100x100 / 磚10 → 實際 100 格，估計值須略大於實際且同一量級
  (setq a (FLR:CellEstimate cfg '(0.0 0.0 100.0 100.0))
        b (length (FLR:MakeGrid cfg '(0.0 0.0 100.0 100.0))))
  (T-true "估計格數 >= 實際"   (>= a b))
  (T-true "估計格數不過度高估" (<= a (* b 2)))
  ;; 候選組合數必須等於兩軸候選數相乘——大平面耗時正比於它
  (setq regs (list SQ100) rbs (bboxes regs))
  (T= "候選組合數 = |cx| x |cy|"
      (FLR:CandCount regs '(0.0 0.0 100.0 100.0) cfg)
      (* (length (FLR:Candidates regs '(0.0 0.0 100.0 100.0) cfg 0))
         (length (FLR:Candidates regs '(0.0 0.0 100.0 100.0) cfg 1))))
  ;; 候選上限必須真的生效，否則大平面會爆炸。
  ;; 【0.5】上限管的是**頂點候選**——置中解與低違規窗口一律優先放進去，
  ;; 上限跟著放寬（窗口是 A1001 那組 2.8% 解唯一的來路，被截掉就沒意義了）。
  ;; maxcand 要放在最前面，assoc 取的是第一筆（mkcfg 自己帶了一個 24）。
  (setq cfg (append (list (cons 'maxcand 3)) (mkcfg 10.0 10.0 0.0 0.0 0.0 0.0)))
  (T-true "maxcand 生效"
          (<= (length (FLR:Candidates regs '(0.0 0.0 100.0 100.0) cfg 0))
              (+ 3 FLR:BADCAND)))

  ;; ---- 10g. 面積守恆不變量（擋住整類幾何錯誤的守門員）----
  ;; 縫寬 0 時磚無縫鋪滿平面 → 所有裁切碎片的面積總和必須「剛好」等於區域面積。
  ;; 舊版 FLR:Clean 把重複點兩份都刪掉會讓角點消失、面積憑空少掉，這條會直接抓到。
  ;; 各種邊界方向都要測：正交、格線對齊、45 度、菱形。
  (princ "\n-- 10g. 面積守恆 --")
  (foreach shape
    (list (cons "格線對齊矩形" '((0.0 0.0) (300.0 0.0) (300.0 300.0) (0.0 300.0)))
          (cons "格線對齊L形"  '((0.0 0.0) (300.0 0.0) (300.0 150.0)
                                 (150.0 150.0) (150.0 300.0) (0.0 300.0)))
          (cons "非對齊矩形"   '((7.0 3.0) (287.0 3.0) (287.0 211.0) (7.0 211.0)))
          (cons "直角三角形"   '((0.0 0.0) (300.0 0.0) (0.0 300.0)))
          (cons "45度梯形"     '((0.0 0.0) (300.0 0.0) (300.0 200.0) (100.0 200.0)))
          (cons "菱形"         '((150.0 0.0) (300.0 150.0) (150.0 300.0) (0.0 150.0))))
    (setq regs (list (cdr shape))
          rbs  (mapcar 'FLR:BBox regs)
          cfg  (list (cons 'tw 30.0) (cons 'th 30.0) (cons 'gap 0.0)
                     (cons 'stagger 0.5) (cons 'ox 0.0) (cons 'oy 0.0)
                     (cons 'tol 1e-6) (cons 'mincut 0.25)
                     (cons 'sizeq 0.5) (cons 'maxcand 24) (cons 'deds nil)))
    (setq a 0.0 n 0)
    (foreach it (FLR:Layout cfg regs rbs (FLR:BBox (cdr shape)))
      (foreach pt (cdr (assoc 'parts (cdr it)))
        (setq a (+ a (FLR:Area (nth 1 pt))))
        ;; 任何碎片都不得少於 3 點（少於 3 點代表角點被吃掉）
        (if (< (length (nth 1 pt)) 3) (setq n (1+ n)))))
    (T= (strcat (car shape) " 面積守恆") a (FLR:Area (cdr shape)))
    (T= (strcat (car shape) " 無退化碎片") n 0))
  ;; 還原共用測試資料——本段換過 regs/rbs，不還原的話第 11 段會沿用到菱形
  (setq regs (list SQ100) rbs (bboxes regs))

  ;; ---- 7b. 內部包含判定：快路徑必須與慢路徑一模一樣（1.4）----
  ;; 這是整個 1.4 效能最佳化裡**唯一會答錯的地方**：判定說「這格完全在內部」
  ;; 而其實不是，該格就會被當成整磚，統計與圖形一起錯，且沒有任何錯誤訊息。
  ;; 故這裡不抽樣——逐格把兩條路徑的每一欄都比過。
  (princ "\n-- 7b. 內部包含判定 --")
  ;; 分解的面積必須守恆。矩形分解漏一塊或多一塊，這條會直接抓到。
  (foreach shape (list (cons "矩形" SQ100) (cons "L形" LSHAPE)
                       (cons "U形" USHAPE) (cons "缺角" CORNER))
    (setq a 0.0)
    (foreach r (FLR:SlabRects (cdr shape) 1e-6 1e-6)
      (setq a (+ a (* (- (nth 2 r) (nth 0 r)) (- (nth 3 r) (nth 1 r))))))
    (T= (strcat (car shape) " 分解面積守恆") a (FLR:Area (cdr shape))))
  ;; 斜邊一律沒有快路徑——板條內跨距會變，取中點取樣得到的矩形會超出區域
  (T-nil "含斜邊 → 不分解（無快路徑）"
         (FLR:SlabRects '((0.0 0.0) (100.0 0.0) (100.0 50.0) (50.0 100.0) (0.0 100.0))
                        1e-6 1e-6))
  (T-nil "三角形 → 不分解"
         (FLR:SlabRects '((0.0 0.0) (100.0 0.0) (0.0 100.0)) 1e-6 1e-6))

  ;; 逐格比對：同一組 cfg，一份帶矩形分解、一份不帶
  ;; L 形（凹角）＋另一個矩形區域＋一個扣除物，三種會讓快路徑答錯的情形都在
  (setq regs (list '((0.0 0.0) (300.0 0.0) (300.0 80.0) (80.0 80.0) (80.0 300.0) (0.0 300.0))
                   '((350.0 0.0) (650.0 0.0) (650.0 200.0) (350.0 200.0)))
        rbs  (bboxes regs)
        cfg  (append (mkcfg 30.0 30.0 0.3 0.5 7.0 3.0)
                     (list (cons 'deds '((120.0 120.0 160.0 160.0)
                                         (500.0 80.0 540.0 120.0)))))
        c    (FLR:WithRects cfg regs))
  (T-true "cfg 帶得出矩形分解" (and (FLR:Cfg 'rrects c) T))
  (setq n 0 a 0 b 0)
  (foreach cell (FLR:MakeGrid cfg '(0.0 0.0 650.0 300.0))
    (setq p  (FLR:ClassifyTile cell regs rbs cfg)     ; 慢路徑
          r  (FLR:ClassifyTile cell regs rbs c))      ; 快路徑
    ;; 直接數「判定答得出來」的格數。數整磚總數是不對的——不走快路徑的整磚
    ;; （跨在兩塊板條之間的那些）也會被算進去，那條斷言就證明不了快路徑有生效。
    (if (FLR:CellHome cell (FLR:Cfg 'rrects c) rbs (FLR:Cfg 'deds cfg) 1e-6)
      (setq b (1+ b)))
    ;; 逐欄比對（parts 的頂點順序兩條路徑不保證相同，故比區域索引、片數與面積）
    (if (not (and (eq (cdr (assoc 'kind p))    (cdr (assoc 'kind r)))
                  (equal (cdr (assoc 'area p)) (cdr (assoc 'area r)) 1e-6)
                  (eq (cdr (assoc 'bad p))     (cdr (assoc 'bad r)))
                  (=  (cdr (assoc 'badlegs p)) (cdr (assoc 'badlegs r)))
                  (=  (cdr (assoc 'pieces p))  (cdr (assoc 'pieces r)))
                  (eq (cdr (assoc 'dedhit p))  (cdr (assoc 'dedhit r)))
                  (equal (cdr (assoc 'minleg p)) (cdr (assoc 'minleg r)) 1e-6)
                  (= (length (cdr (assoc 'cuts p))) (length (cdr (assoc 'cuts r))))
                  (= (length (cdr (assoc 'parts p))) (length (cdr (assoc 'parts r))))
                  (equal (mapcar 'car (cdr (assoc 'parts p)))
                         (mapcar 'car (cdr (assoc 'parts r))))
                  (equal (apply '+ (mapcar 'caddr (cdr (assoc 'parts p))))
                         (apply '+ (mapcar 'caddr (cdr (assoc 'parts r)))) 1e-6)))
      (setq n (1+ n)))
    (setq a (1+ a)))
  (T= "快慢兩路徑逐格完全一致（不一致格數）" n 0)
  (T-true "測資夠大（格數 > 200）" (> a 200))
  (T-true (strcat "快路徑真的有生效（內部格 " (itoa b) "/" (itoa a) "）") (> b 30))
  ;; 也不可以「全部都判成內部」——那代表判定太寬鬆，邊界格會被誤判成整磚
  (T-true "邊界格沒有被誤判為內部" (< b a))
  ;; 端到端：整份統計必須一字不差
  (T= "Evaluate 帶不帶分解結果相同"
      (FLR:Evaluate cfg regs rbs '(0.0 0.0 650.0 300.0))
      (FLR:Evaluate c   regs rbs '(0.0 0.0 650.0 300.0)))
  ;; 扣除物碰得到的格子不可以走快路徑
  (T-nil "碰到扣除物 → 不判為內部"
         (FLR:CellHome '(120.0 120.0 150.0 150.0) (FLR:Cfg 'rrects c) rbs
                       (FLR:Cfg 'deds cfg) 1e-6))
  ;; 兩個區域重疊處也不可以（那一格會被裁成兩片）
  (setq regs (list SQ100 '((50.0 50.0) (150.0 50.0) (150.0 150.0) (50.0 150.0)))
        rbs  (bboxes regs)
        c    (FLR:WithRects (mkcfg 10.0 10.0 0.0 0.0 0.0 0.0) regs))
  (T-nil "落在兩區重疊處 → 不判為內部"
         (FLR:CellHome '(60.0 60.0 70.0 70.0) (FLR:Cfg 'rrects c) rbs nil 1e-6))
  (T= "只落在單一區域內 → 回該區索引"
      (FLR:CellHome '(10.0 10.0 20.0 20.0) (FLR:Cfg 'rrects c) rbs nil 1e-6) 0)
  ;; 還原共用測試資料
  (setq regs (list SQ100) rbs (bboxes regs) cfg (mkcfg 10.0 10.0 0.0 0.0 0.0 0.0))

  ;; ---- 7c. 條帶分解去重：矩形快路徑必須與兩軸全掃一模一樣（1.4.2）----
  ;;
  ;; 1.4.2 把 FLR:LegScan 拆成吃現成 x 向條帶的 FLR:LegScanS，並對「4 頂點的
  ;; 軸向矩形」整段跳過 y 向分解（推導見 FLR_Core.lsp 該函式上方的註）。
  ;; **推導對不對，唯一的檢驗是拿舊實作逐片比對**——這裡就放一份 1.4.1 的原文，
  ;; 兩者不符即為紅燈。答錯的代價與 7b 同一類：違規邊數或最窄靜默變動，沒有錯誤訊息。
  (princ "\n-- 7c. 條帶去重 --")
  (T-true "1.4.2 新函式都在" (and (and FLR:LegScanS T) (and FLR:RectFrag T)))
  (T-true "矩形 → RectFrag 為真"
          (FLR:RectFrag '((0.0 0.0) (30.0 0.0) (30.0 12.0) (0.0 12.0)) 1e-6))
  (T-nil "L 形（6 頂點）→ 非矩形" (FLR:RectFrag LSHAPE 1e-6))
  (T-nil "三角形（3 頂點）→ 非矩形"
         (FLR:RectFrag '((0.0 0.0) (30.0 0.0) (0.0 30.0)) 1e-6))
  (T-nil "斜置四邊形 → 非矩形"
         (FLR:RectFrag '((0.0 0.0) (30.0 1.0) (29.0 31.0) (-1.0 30.0)) 1e-6))
  ;; 【最重要的一條】頂邊差 6.79e-5 的繪圖誤差（A1001 真實出現過，見 FLR:AxisTol）。
  ;; 那不是矩形——axis-1 會多出一條 6.79e-5 寬的贅條帶而多算一次違規邊。
  ;; 快路徑若用寬容差把它收進來，違規數就會靜默少一。
  (setq p '((0.0 0.0) (30.0 0.0) (30.0 30.0) (0.0 29.9999321)))
  (T-nil "頂邊差 6.79e-5 → 不判為矩形（不可放寬容差）" (FLR:RectFrag p 1e-6))
  (T= "而且它的違規邊數與舊實作相同"
      (FLR:LegScanS p (FLR:Strips p 0 1e-6) 30.0 30.0 0.25 1e-6)
      (TREF:LegScan2 p 30.0 30.0 0.25 1e-6))
  ;; 各種形狀 × 兩種磚（正方與長方，門檻不對稱才驗得出兩軸有沒有搞混）
  (setq n 0 a 0)
  (foreach shape (list SQ100 LSHAPE USHAPE CORNER
                       '((0.0 0.0) (30.0 0.0) (30.0 12.0) (0.0 12.0))   ; 邊料
                       '((0.0 0.0) (2.0 0.0) (2.0 3.0) (0.0 3.0))       ; 角料
                       '((0.0 0.0) (30.0 0.0) (0.0 30.0))               ; 直角三角
                       '((0.0 0.0) (30.0 0.0) (30.0 30.0) (0.0 29.9999321)))
    (foreach tile '((30.0 30.0) (60.0 30.0) (30.0 60.0))
      (setq a (1+ a))
      (if (not (equal (FLR:LegScanS shape (FLR:Strips shape 0 1e-6)
                                    (car tile) (cadr tile) 0.25 1e-6)
                      (TREF:LegScan2 shape (car tile) (cadr tile) 0.25 1e-6)
                      1e-9))
        (setq n (1+ n)))))
  (T= "各形狀 × 各磚型與舊實作相同（不符數）" n 0)
  (T-true "測資夠多（> 20 組）" (> a 20))
  ;; 逐格比對：真實佈置產生的碎片才有 S-H 的贅點與零寬邊，合成形狀測不到
  (setq regs (list '((0.0 0.0) (300.0 0.0) (300.0 80.0) (80.0 80.0) (80.0 300.0) (0.0 300.0))
                   '((350.0 0.0) (650.0 0.0) (650.0 200.0) (350.0 200.0)))
        rbs  (bboxes regs)
        cfg  (mkcfg 30.0 30.0 0.3 0.5 7.0 3.0)
        n 0 a 0 b 0)
  (foreach cell (FLR:MakeGrid cfg '(0.0 0.0 650.0 300.0))
    (foreach pt (FLR:ClipTileByRegions cell regs rbs 1e-6)
      (setq p (nth 1 pt) a (1+ a))
      (if (FLR:RectFrag p 1e-6) (setq b (1+ b)))
      (if (not (equal (FLR:LegScanS p (FLR:Strips p 0 1e-6) 30.0 30.0 0.25 1e-6)
                      (TREF:LegScan2 p 30.0 30.0 0.25 1e-6) 1e-9))
        (setq n (1+ n)))))
  (T= "真實碎片逐片與舊實作相同（不符數）" n 0)
  (T-true (strcat "測資夠大（碎片 " (itoa a) " 片）") (> a 120))
  ;; 快路徑要真的有生效，否則上面那些「相同」只是證明了兩條都走舊路
  (T-true (strcat "矩形快路徑有生效（" (itoa b) "/" (itoa a) " 片）") (> b 80))
  (T-true "非矩形碎片仍走兩軸全掃" (< b a))
  ;; 舊簽章的 FLR:LegScan 必須與 1.4.1 的行為完全相同（既有斷言與探針都還在用）
  (T= "FLR:LegScan 舊簽章行為不變"
      (FLR:LegScan LSHAPE 30.0 30.0 0.25 1e-6)
      (TREF:LegScan2 LSHAPE 30.0 30.0 0.25 1e-6))
  ;; 還原共用測試資料
  (setq regs (list SQ100) rbs (bboxes regs) cfg (mkcfg 10.0 10.0 0.0 0.0 0.0 0.0))

  ;; ---- 10g. 切割清單必須與統計表對得起來（1.4.4）----
  ;;
  ;; 這份清單是**要交到師傅手上照著切的**，所以錯了不會有錯誤訊息，
  ;; 只會切出一批不能用的料。鎖住的是「它與統計表是同一批磚」：
  ;;   ① 面積：清單總面積 ＋ 整磚面積 ＝ 統計表淨面積
  ;;   ② 片數：清單片數 ＝ 非整磚的碎片數（**不是**統計表的裁切磚數——
  ;;      跨區磚在統計表只計一次，現場卻要切兩片，見 FLR:CutList 的註）
  ;;   ③ 尺寸：清單上的每一個維度，不是磚的滿格尺寸就必須在統計表的尺寸清單裡
  ;;      （師傅照表切，表上沒有的尺寸不可以出現在清單上）
  (princ "\n-- 10g. 切割清單 --")
  (setq regs (list '((0.0 0.0) (300.0 0.0) (300.0 80.0) (80.0 80.0) (80.0 300.0) (0.0 300.0))
                   '((350.0 0.0) (650.0 0.0) (650.0 200.0) (350.0 200.0)))
        rbs  (bboxes regs)
        cfg  (append (mkcfg 30.0 30.0 0.3 0.5 7.0 3.0) (list (cons 'deds nil)))
        p    (FLR:Layout cfg regs rbs '(0.0 0.0 650.0 300.0))
        st   (FLR:StatsOf p cfg)
        r    (FLR:CutList p cfg))
  (T-true "清單不是空的" (> (length r) 0))
  ;; ① 面積守恆
  (setq a 0.0 b 0)
  (foreach row r (setq a (+ a (nth 2 row)) b (+ b (nth 1 row))))
  (T= "清單面積 ＋ 整磚面積 ＝ 統計淨面積"
      (+ a (* (cdr (assoc 'full st)) 30.0 30.0))
      (cdr (assoc 'area st)))
  ;; ② 片數＝非整磚的碎片數（獨立數一次，不從清單反推）
  (setq c 0)
  (foreach it p
    (if (and (/= (cdr (assoc 'kind (cdr it))) 'full)
             (/= (cdr (assoc 'kind (cdr it))) 'none))
      (foreach pt (cdr (assoc 'parts (cdr it)))
        (if (FLR:Strips (nth 1 pt) 0 1e-6) (setq c (1+ c))))))
  (T= "清單片數 ＝ 非整磚碎片數" b c)
  ;; 跨區磚讓它必然多於統計表的裁切磚數——這條同時證明兩者量的不是同一件事
  (T-true "片數不小於統計表的裁切磚數" (>= b (cdr (assoc 'cut st))))
  ;; ③ 尺寸一律是表上的代表值或滿格
  (setq n 0)
  (foreach row r
    (foreach s (car row)
      (foreach v (list (car s) (cadr s))
        (if (not (or (equal v 30.0 1e-9)
                     (vl-some '(lambda (u) (equal u v 1e-9)) (cdr (assoc 'sizes st)))))
          (setq n (1+ n))))))
  (T= "清單尺寸都在統計表的尺寸清單裡（例外數）" n 0)
  ;; 兩個來源必須數出同一份答案——分區鋪磚時是「框選圖面讀回多段線」那條路
  ;; 在算整層樓的清單，它沒有 layout、也沒有 'cuts，只有多邊形本身。
  (setq a '())
  (foreach it p
    (if (and (/= (cdr (assoc 'kind (cdr it))) 'full)
             (/= (cdr (assoc 'kind (cdr it))) 'none))
      (foreach pt (cdr (assoc 'parts (cdr it)))
        (setq a (cons (list (nth 1 pt) (nth 2 pt)) a)))))
  (T= "由 layout 與由純多邊形導出的清單相同"
      r (FLR:CutListOf (reverse a) cfg))
  ;; 順序不同也要得到同一份（框選回來的順序由 ssget 決定，不是佈置順序）。
  ;; 這條初版是紅的：排序只比第一條帶的寬，(20.3 15.2) 與 (20.3 16.4) 比不出
  ;; 大小，vl-sort 就隨輸入順序決定 → 同一張圖匯出兩次得到不同的 CSV。
  (T= "碎片順序顛倒不影響結果（尺寸／片數／違規）"
      (mapcar '(lambda (x) (list (car x) (nth 1 x) (nth 3 x))) r)
      (mapcar '(lambda (x) (list (car x) (nth 1 x) (nth 3 x))) (FLR:CutListOf a cfg)))
  ;; 面積是逐片累加的，順序一反最後幾個位元就不同（實測 1458.0 vs 1458.0000000000002）
  ;; ——這條刻意用容差比，並在 CSV 端固定四捨五入到兩位，讓它永遠浮不上來
  (T-true "面積相同（浮點累加順序不同，用容差）"
          (equal (mapcar '(lambda (x) (nth 2 x)) r)
                 (mapcar '(lambda (x) (nth 2 x)) (FLR:CutListOf a cfg)) 1e-6))
  ;; 直接驗排序鍵是**全序**，不要只驗它的症狀
  (T-true "SigLess：寬不同時比得出來"
          (and (FLR:SigLess '((20.3 15.2)) '((21.0 15.2)))
               (not (FLR:SigLess '((21.0 15.2)) '((20.3 15.2))))))
  (T-true "SigLess：寬相同時往下比長（初版就是這裡比不出來）"
          (and (FLR:SigLess '((20.3 15.2)) '((20.3 16.4)))
               (not (FLR:SigLess '((20.3 16.4)) '((20.3 15.2))))))
  (T-true "SigLess：前綴相同時條帶少者在前"
          (and (FLR:SigLess '((20.3 15.2)) '((20.3 15.2) (5.0 5.0)))
               (not (FLR:SigLess '((20.3 15.2) (5.0 5.0)) '((20.3 15.2))))))
  (T-nil "SigLess：完全相同 → 兩邊都不小於"
         (or (FLR:SigLess '((20.3 15.2)) '((20.3 15.2)))
             (FLR:SigLess '((20.3 15.2)) '((20.3 15.2)))))
  ;; 反對稱性：清單裡任兩列都必須比得出大小（否則 vl-sort 的結果不可重現）
  (setq n 0)
  (foreach x r
    (foreach y r
      (if (and (not (equal (car x) (car y) 1e-9))
               (eq (FLR:SigLess (car x) (car y)) (FLR:SigLess (car y) (car x))))
        (setq n (1+ n)))))
  (T= "清單內任兩列都排得出先後（比不出來的對數）" n 0)
  ;; L 形要算一片，不可以照條帶拆成兩片
  (setq a '((0.0 0.0) (10.0 0.0) (10.0 6.0) (6.0 6.0) (6.0 10.0) (0.0 10.0)))
  (T= "L 形碎片的簽章有兩條條帶" (length (FLR:Strips a 0 1e-6)) 2)
  ;; 違規由代表值判定，故同一列必然同號
  (T-true "2x3 角料（下限 25%）判違規"
          (FLR:SigBad '((2.0 3.0)) 30.0 30.0 0.25 1e-6))
  (T-nil "20x30 邊料合格"
         (FLR:SigBad '((20.0 30.0)) 30.0 30.0 0.25 1e-6))
  (T-true "L 形只要有一條腿不足就違規"
          (FLR:SigBad '((20.0 30.0) (2.0 4.0)) 30.0 30.0 0.25 1e-6))
  ;; 60x30 磚：兩軸門檻不同，別用錯
  (T-true "60x30 磚：高 6 < 7.5 → 違規"
          (FLR:SigBad '((60.0 6.0)) 60.0 30.0 0.25 1e-6))
  (T-nil "60x30 磚：寬 20 >= 15 → 合格"
         (FLR:SigBad '((20.0 30.0)) 60.0 30.0 0.25 1e-6))
  ;; 代表值取用：滿格那一維不進分群
  (T= "滿格維度維持磚尺寸" (FLR:CutRep 30.0 30.0 '(12.0 18.0) 1e-6) 30.0)
  (T= "裁切維度取分群代表值" (FLR:CutRep 11.8 30.0 '(12.0 18.0) 1e-6) 12.0)
  ;; 排序：片數多者在前
  (setq a T)
  (setq b nil)
  (foreach row r
    (if (and b (> (nth 1 row) b)) (setq a nil))
    (setq b (nth 1 row)))
  (T-true "清單依片數由多到少" a)
  ;; 整磚不可以進清單——它不必切
  (setq cfg (append (mkcfg 10.0 10.0 0.0 0.0 0.0 0.0) (list (cons 'deds nil)))
        regs (list SQ100) rbs (bboxes regs)
        p (FLR:Layout cfg regs rbs '(0.0 0.0 100.0 100.0)))
  (T= "全整磚 → 清單為空" (length (FLR:CutList p cfg)) 0)
  ;; 還原共用測試資料
  (setq regs (list SQ100) rbs (bboxes regs) cfg (mkcfg 10.0 10.0 0.0 0.0 0.0 0.0))

  ;; ---- 10h. 整磚的量測結果是常數（1.4 效能最佳化的護欄）----
  ;; 1.4 起整磚**整段跳過條帶分解**（實測省掉每組候選 43.9% 的成本），
  ;; 改填已知常數。這一節就是那組常數的定義：跳過之後這些值必須與跳過之前一模一樣。
  ;; 最危險的一條是「最窄」——填 nil 的話 FLR:Evaluate 會當成 0.0，
  ;; 而「整張圖都是整磚 → 最窄 0」不會有任何錯誤訊息，只會靜默地把
  ;; 對稱目標的第二比較鍵毀掉，並在統計行印一個假的 0。
  (princ "\n-- 10h. 整磚跳過量測 --")
  (setq cfg (append (mkcfg 10.0 10.0 0.0 0.0 0.0 0.0) (list (cons 'deds nil)))
        r   (FLR:ClassifyTile '(30.0 30.0 40.0 40.0) (list SQ100) (bboxes (list SQ100)) cfg))
  (T= "整磚 kind"        (cdr (assoc 'kind r))    'full)
  (T= "整磚無裁切尺寸"    (cdr (assoc 'cuts r))    '())
  (T-nil "整磚不違規"     (cdr (assoc 'bad r)))
  (T= "整磚違規邊數 0"    (cdr (assoc 'badlegs r)) 0)
  (T= "整磚最窄＝磚短邊"  (cdr (assoc 'minleg r))  10.0)
  ;; 排料由呼叫端依 kind 算成一整片（FLR:Nest／FLR:Evaluate 都只看 kind），
  ;; 故整磚的 'nest 是空的——這是契約，不是漏填
  (T= "整磚 nest 為空"    (cdr (assoc 'nest r))    '())
  ;; 長寬不同的磚：最窄必須取**短邊**，不是磚寬
  (setq cfg (append (mkcfg 60.0 30.0 0.0 0.0 0.0 0.0) (list (cons 'deds nil))))
  (setq r (FLR:ClassifyTile '(60.0 30.0 120.0 60.0)
                            (list '((0.0 0.0) (300.0 0.0) (300.0 300.0) (0.0 300.0)))
                            (bboxes (list '((0.0 0.0) (300.0 0.0) (300.0 300.0) (0.0 300.0))))
                            cfg))
  (T= "60x30 整磚最窄＝30" (cdr (assoc 'minleg r)) 30.0)
  ;; 全整磚的圖：整張圖的最窄必須是磚短邊而不是 0
  (setq regs (list SQ100) rbs (bboxes regs)
        cfg  (append (mkcfg 10.0 10.0 0.0 0.0 0.0 0.0) (list (cons 'deds nil))))
  (setq res (FLR:Evaluate cfg regs rbs '(0.0 0.0 100.0 100.0)))
  (T= "全整磚 → 片數 100"   (cdr (assoc 'tiles res))  100)
  (T= "全整磚 → 裁切 0"     (cdr (assoc 'cut res))    0)
  (T= "全整磚 → 刀數 0"     (cdr (assoc 'cutsizes res)) 0)
  (T= "全整磚 → 最窄 10 而非 0" (cdr (assoc 'minleg res)) 10.0)
  ;; 跳過與不跳過必須完全一致：同一張圖裡整磚與裁切磚並存時，
  ;; StatsOf（走 Layout）與 Evaluate（走精簡路徑）本來就有 §10c 的一致性斷言，
  ;; 這裡再補一條「有整磚也有裁切磚」的最窄比對，確保跳過沒有污染全域最小值
  (setq regs (list '((0.0 0.0) (95.0 0.0) (95.0 62.0) (0.0 62.0)))
        rbs  (bboxes regs)
        cfg  (append (mkcfg 30.0 30.0 0.0 0.5 0.0 0.0) (list (cons 'deds nil))))
  (setq res (FLR:Evaluate cfg regs rbs '(0.0 0.0 95.0 62.0))
        b   (FLR:StatsOf (FLR:Layout cfg regs rbs '(0.0 0.0 95.0 62.0)) cfg))
  (T= "混合圖：最窄 Evaluate ＝ StatsOf"
      (cdr (assoc 'minleg res)) (cdr (assoc 'minleg b)))
  (T-true "混合圖：最窄小於磚寬（真的抓到邊磚）"
          (< (cdr (assoc 'minleg res)) 30.0))
  (T-true "混合圖：整磚與裁切磚都有"
          (and (> (cdr (assoc 'full res)) 0) (> (cdr (assoc 'cut res)) 0)))
  ;; 還原共用測試資料
  (setq regs (list SQ100) rbs (bboxes regs) cfg (mkcfg 10.0 10.0 0.0 0.0 0.0 0.0))

  ;; ---- 11. 候選與最佳化 ----
  (princ "\n-- 11. Optimize --")
  (setq cfg (mkcfg 10.0 10.0 0.0 0.0 0.0 0.0))
  (setq a (FLR:Candidates regs '(0.0 0.0 100.0 100.0) cfg 0))
  (T-true "候選含縫置中解0" (vl-some '(lambda (u) (equal u 0.0 1e-6)) a))
  (T-true "候選含磚置中解5" (vl-some '(lambda (u) (equal u 5.0 1e-6)) a))

  ;; ---- 11b. 每一區各自的中線也要在候選裡（0.3，使用者回饋）----
  ;; 原本只放「所有區域整體外框」的中線。多房間時那對每一間都不是中線
  ;; ——使用者選了「自動置中對稱」卻拿到左 2.4 / 右 24.6 的結果。
  (princ "\n-- 11b. 每區各自對稱 --")
  (setq b (list '((0.0 0.0) (300.0 0.0) (300.0 200.0) (0.0 200.0))
                '((350.0 0.0) (650.0 0.0) (650.0 200.0) (350.0 200.0)))
        cfg (mkcfg 30.0 30.0 0.3 0.5 0.0 0.0)
        a   (FLR:Candidates b '(0.0 0.0 650.0 200.0) cfg 0))
  ;; 第 1 區（0..300）自己的磚置中：中線 150 → (150 − 15) mod 30.3
  (T-true "候選含第1區的磚置中"
          (vl-some '(lambda (u) (equal u (FLR:Modp (- 150.0 15.0) 30.3) 1e-6)) a))
  ;; 第 2 區（350..650）自己的磚置中：中線 500
  (T-true "候選含第2區的磚置中"
          (vl-some '(lambda (u) (equal u (FLR:Modp (- 500.0 15.0) 30.3) 1e-6)) a))
  (T-true "候選仍含整體外框的磚置中"
          (vl-some '(lambda (u) (equal u (FLR:Modp (- 325.0 15.0) 30.3) 1e-6)) a))
  (T-true "候選無重複"
          (= (length a) (length (append a nil))))

  ;; ---- 11c. 對稱度（只量不排序）----
  ;; 使用者回報「不是選自動對稱？怎麼最左、最右數字不一樣？」——
  ;; 排序準則裡本來就沒有對稱性，把它量出來標在推薦清單上，才挑得到對稱解。
  (princ "\n-- 11c. SymErr --")
  ;; 單一區域 0..300、間距 30.3：完美對稱時兩端邊料相等
  (setq b   (list '((0.0 0.0) (300.0 0.0) (300.0 200.0) (0.0 200.0)))
        cfg (mkcfg 30.0 30.0 0.3 0.0 (FLR:Modp (- 150.0 15.0) 30.3)
                                     (FLR:Modp (- 100.0 15.0) 30.3)))
  (T-true "各區磚置中時對稱誤差為 0" (< (FLR:SymErr b cfg) 1e-6))
  ;; 刻意偏移 1 個單位 → 兩端各差 1，誤差 2
  (setq cfg (mkcfg 30.0 30.0 0.3 0.0 (+ 1.0 (FLR:Modp (- 150.0 15.0) 30.3))
                                     (FLR:Modp (- 100.0 15.0) 30.3)))
  (T= "偏移 1 個單位 → 誤差 2" (FLR:SymErr b cfg) 2.0)
  (T-true "誤差恆為非負" (>= (FLR:SymErr b (mkcfg 30.0 30.0 0.3 0.0 7.0 3.0)) 0.0))
  ;; 兩端邊料：邊界落在磚邊上＝整磚（回 tw），不可回 0
  (setq a (FLR:EdgeCuts 0.0 300.0 0.0 30.0 30.3 1e-6))
  (T= "起點對齊磚邊 → 整磚" (car a) 30.0)
  (setq a (FLR:EdgeCuts 0.0 300.0 5.0 30.0 30.3 1e-6))
  (T-true "偏移後兩端都在 0~tw 之間"
          (and (> (car a) 0.0) (<= (car a) 30.0) (> (cadr a) 0.0) (<= (cadr a) 30.0)))
  ;; 還原第 11 段的共用測試資料——本段換過 cfg／regs，
  ;; 不還原的話下面的 Optimize 會拿 30x30 的磚去鋪 100x100（同 10g 的舊帳）
  (setq regs (list SQ100) rbs (bboxes regs) cfg (mkcfg 10.0 10.0 0.0 0.0 0.0 0.0))
  (setq a (FLR:Candidates regs '(0.0 0.0 100.0 100.0) cfg 0))

  (setq t0 (getvar "MILLISECS"))
  (setq top (FLR:Optimize cfg regs rbs '(0.0 0.0 100.0 100.0) 5))
  (setq t1 (getvar "MILLISECS"))
  (T-true "回傳非空" (> (length top) 0))
  (setq res (car top))
  (T= "最佳解 尺寸種類=0" (cdr (assoc 'cutsizes res)) 0)
  (T= "最佳解 無U形"      (cdr (assoc 'ushape res)) 0)
  (T= "最佳解 整磚100"    (cdr (assoc 'full res)) 100)
  (princ (strcat "\n  [INFO] Optimize 耗時 " (itoa (- t1 t0)) " ms，候選 "
                 (itoa (length a)) "x" (itoa (length a)) " 組合"))

  ;; 排序單調性。0.5 起清單尾端會附掛「違規更低」的方案，
  ;; 那幾筆本來就不照 FLR:Better 排（附掛的意義就是排序準則挑不到它們），
  ;; 故只驗前段。
  (setq n 0 a nil)
  (foreach x top
    (if (not (equal (cdr (assoc 'why x)) 'lowbad))
      (progn
        (if a (T-true (strcat "排序單調 #" (itoa n))
                      (or (FLR:Better a x) (not (FLR:Better x a)))))
        (setq a x n (1+ n)))))

  ;; ---- 12. 三區共用網格（磚牆/輕隔間切三塊）----
  (princ "\n-- 12. 三區對齊 --")
  (setq regs (list '((0.0 0.0) (100.0 0.0) (100.0 60.0) (0.0 60.0))
                   '((0.0 64.0) (48.0 64.0) (48.0 140.0) (0.0 140.0))
                   '((52.0 64.0) (100.0 64.0) (100.0 140.0) (52.0 140.0)))
        rbs (bboxes regs))
  (T= "三區無重疊" (length (FLR:FindOverlaps regs 1e-6)) 0)
  (setq cfg (mkcfg 10.0 10.0 0.0 0.0 0.0 0.0))
  (setq t0 (getvar "MILLISECS"))
  (setq res (FLR:Evaluate cfg regs rbs '(0.0 0.0 100.0 140.0)))
  (setq t1 (getvar "MILLISECS"))
  (princ (strcat "\n  [INFO] 三區 " (itoa (cdr (assoc 'tiles res)))
                 " 片，單次評估 " (itoa (- t1 t0)) " ms"))
  (T-true "三區有磚" (> (cdr (assoc 'tiles res)) 0))
  (T= "三區總面積 = 6000+3648+3648"
      (cdr (assoc 'area res)) (+ 6000.0 (* 48.0 76.0) (* 48.0 76.0)))

  (setq t0 (getvar "MILLISECS"))
  (setq top (FLR:Optimize cfg regs rbs '(0.0 0.0 100.0 140.0) 5))
  (setq t1 (getvar "MILLISECS"))
  (princ (strcat "\n  [INFO] 三區 Optimize 耗時 " (itoa (- t1 t0)) " ms，回傳 "
                 (itoa (length top)) " 名"))
  (T-true "三區最佳解無U形" (= (cdr (assoc 'ushape (car top))) 0))

  ;; ---- 13. 扣除物（柱／機坑／管道間／樓梯）----
  (princ "\n-- 13. 扣除物 --")
  (setq regs (list SQ100) rbs (bboxes regs))

  ;; 13a. 切割型態分類（U 形禁則必須在分解前判定）
  (setq p '(0.0 0.0 10.0 10.0))                     ; 碎片 bbox
  (T= "橫貫→split" (FLR:RectCut p '(4.0 -2.0 6.0 12.0) 1e-6) 'split)
  (T= "縱貫→split" (FLR:RectCut p '(-2.0 4.0 12.0 6.0) 1e-6) 'split)
  (T= "吃角→corner"(FLR:RectCut p '(8.0 8.0 12.0 12.0) 1e-6) 'corner)
  (T= "碰一邊→notch"(FLR:RectCut p '(4.0 -2.0 6.0 5.0) 1e-6) 'notch)
  (T= "正中央→hole" (FLR:RectCut p '(4.0 4.0 6.0 6.0) 1e-6) 'hole)

  ;; 13b. 幾何分解
  (setq r (FLR:SubtractRect SQ100 '(40.0 40.0 60.0 60.0) 1e-6))
  (T= "中央挖洞→4 碎片" (length r) 4)
  (setq a 0.0) (foreach f r (setq a (+ a (FLR:Area f))))
  (T= "碎片面積和 = 10000-400" a 9600.0)
  (setq r (FLR:SubtractRect SQ100 '(40.0 -10.0 60.0 110.0) 1e-6))
  (T= "橫貫→2 碎片" (length r) 2)
  (setq r (FLR:SubtractRect SQ100 '(200.0 200.0 300.0 300.0) 1e-6))
  (T= "不相交→原樣 1 片" (length r) 1)
  (setq r (FLR:SubtractRects (list SQ100)
                             '((10.0 10.0 20.0 20.0) (80.0 80.0 90.0 90.0)) 1e-6))
  (setq a 0.0) (foreach f r (setq a (+ a (FLR:Area f))))
  (T= "扣兩個矩形面積正確" a (- 10000.0 100.0 100.0))

  ;; 13c. 套進分類：柱在磚中央 → 不允許
  (defun cfgd (deds / c) (setq c (mkcfg 10.0 10.0 0.0 0.0 0.0 0.0))
    (append c (list (cons 'deds deds))))
  (setq res (FLR:ClassifyTile '(0.0 0.0 10.0 10.0) regs rbs
                              (cfgd '((4.0 4.0 6.0 6.0)))))
  (T= "柱在磚中央 → ushape" (cdr (assoc 'kind res)) 'ushape)
  (T= "扣除後面積 96"       (cdr (assoc 'area res)) 96.0)
  (T= "分解成 4 碎片"       (length (cdr (assoc 'parts res))) 4)
  (T-true "標記 dedhit"     (cdr (assoc 'dedhit res)))

  ;; 柱吃掉一角 → L 形，允許
  (setq res (FLR:ClassifyTile '(0.0 0.0 10.0 10.0) regs rbs
                              (cfgd '((8.0 8.0 12.0 12.0)))))
  (T= "柱吃角 → cut(允許)" (cdr (assoc 'kind res)) 'cut)
  (T= "扣除後面積 96"      (cdr (assoc 'area res)) 96.0)

  ;; 柱橫貫磚 → 乾淨切開，允許
  (setq res (FLR:ClassifyTile '(0.0 0.0 10.0 10.0) regs rbs
                              (cfgd '((4.0 -2.0 6.0 12.0)))))
  (T= "柱橫貫 → cut(允許)" (cdr (assoc 'kind res)) 'cut)
  (T= "扣除後面積 80"      (cdr (assoc 'area res)) 80.0)

  ;; 柱只碰一邊 → 凹槽，不允許
  (setq res (FLR:ClassifyTile '(0.0 0.0 10.0 10.0) regs rbs
                              (cfgd '((4.0 -2.0 6.0 5.0)))))
  (T= "柱咬一口 → ushape" (cdr (assoc 'kind res)) 'ushape)

  ;; 柱完全蓋住磚 → 該磚消失
  (setq res (FLR:ClassifyTile '(0.0 0.0 10.0 10.0) regs rbs
                              (cfgd '((-1.0 -1.0 11.0 11.0)))))
  (T= "柱蓋滿 → none" (cdr (assoc 'kind res)) 'none)

  ;; 13d. 含扣除物時 StatsOf 仍須 ≡ Evaluate
  (princ "\n-- 13d. 含扣除物 StatsOf ≡ Evaluate --")
  (setq cfg (cfgd '((23.0 23.0 37.0 37.0) (71.0 8.0 79.0 44.0))))
  (setq res (FLR:Evaluate cfg regs rbs '(0.0 0.0 100.0 100.0)))
  (setq b   (FLR:StatsOf (FLR:Layout cfg regs rbs '(0.0 0.0 100.0 100.0)) cfg))
  (foreach fld '(full cut ushape bad dedhit tiles cutsizes)
    (T= (strcat "含扣除物 " (vl-symbol-name fld))
        (cdr (assoc fld b)) (cdr (assoc fld res))))
  (T= "含扣除物 area" (cdr (assoc 'area b)) (cdr (assoc 'area res)))
  ;; 淨面積 = 房間 - 兩個扣除物
  (T= "淨面積 = 10000-196-288"
      (cdr (assoc 'area res)) (- 10000.0 (* 14.0 14.0) (* 8.0 36.0)))
  ;; 繪圖碎片面積和必須等於淨面積（畫出來的 = 算出來的）
  (setq a 0.0)
  (foreach it (FLR:Layout cfg regs rbs '(0.0 0.0 100.0 100.0))
    (foreach pt (cdr (assoc 'parts (cdr it))) (setq a (+ a (FLR:Area (nth 1 pt))))))
  (T= "繪圖碎片面積和 = 淨面積" a (cdr (assoc 'area res)))

  ;; 13e. 互相重疊的扣除物：必須排容，不可扣兩次
  ;; 0.1 版逐個扣除物 (- area (Area ov))，實測 900-400-400 = 100，正解 200。
  (princ "\n-- 13e. 扣除物排容 --")
  (setq regs (list '((0.0 0.0) (30.0 0.0) (30.0 30.0) (0.0 30.0)))
        rbs  (bboxes regs))
  (setq cfg (append (mkcfg 30.0 30.0 0.0 0.0 0.0 0.0)
                    (list (cons 'deds '((0.0 0.0 20.0 20.0) (10.0 10.0 30.0 30.0))))))
  (setq res (FLR:ClassifyTile '(0.0 0.0 30.0 30.0) regs rbs cfg))
  (T= "兩扣除物重疊 → 面積 200（非 100）" (cdr (assoc 'area res)) 200.0)
  ;; 碎片面積和必須等於回報面積（畫出來的＝算出來的）
  (setq a 0.0)
  (foreach pt (cdr (assoc 'parts res)) (setq a (+ a (FLR:Area (nth 1 pt)))))
  (T= "碎片面積和 = 回報面積" a (cdr (assoc 'area res)))
  ;; 完全重合的兩個扣除物只能扣一次
  (setq cfg (append (mkcfg 30.0 30.0 0.0 0.0 0.0 0.0)
                    (list (cons 'deds '((5.0 5.0 15.0 15.0) (5.0 5.0 15.0 15.0))))))
  (setq res (FLR:ClassifyTile '(0.0 0.0 30.0 30.0) regs rbs cfg))
  (T= "完全重合只扣一次" (cdr (assoc 'area res)) (- 900.0 100.0))
  ;; 不重疊的兩個扣除物結果不變（不可造成回歸）
  (setq cfg (append (mkcfg 30.0 30.0 0.0 0.0 0.0 0.0)
                    (list (cons 'deds '((2.0 2.0 6.0 6.0) (20.0 20.0 26.0 26.0))))))
  (setq res (FLR:ClassifyTile '(0.0 0.0 30.0 30.0) regs rbs cfg))
  (T= "不重疊照常相加扣除" (cdr (assoc 'area res)) (- 900.0 16.0 36.0))

  ;; ---- 14. 1D 前篩 ----
  ;; 正交區域時 x 向裁切尺寸只由 ox 決定、y 向只由 oy 決定，且不必造網格。
  ;; 這一段就是把「前篩結果 ≡ 全算結果」鎖死。
  (princ "\n-- 14. PreRank 前篩 --")
  (setq regs (list '((0.0 0.0) (100.0 0.0) (100.0 50.0) (0.0 50.0))
                   '((7.0 60.0) (143.0 60.0) (143.0 110.0) (7.0 110.0)))
        rbs  (bboxes regs))
  (T-true "正交區域 → IsRect"    (FLR:IsRect regs 0.03))
  (T-nil  "含斜邊 → 非 IsRect"
          (FLR:IsRect (list '((0.0 0.0) (10.0 0.0) (0.0 10.0))) 0.03))
  ;; ---- 14b. 軸向容差要與磚同量綱（0.4，實測 A1001 平面圖）----
  ;; 真實圖面某區頂邊的兩端是 y=-0.0000679 與 y=0.0：在 316.5 的跨距上差 0.00007，
  ;; 那是繪圖誤差不是斜牆。舊版拿 1e-6（IsRect）／1e-9（AxisEdges）去比，
  ;; 整張圖被判成「有斜邊界」→ 前篩整包關掉 → 576 組全算約 290 秒。
  (princ "\n-- 14b. AxisTol --")
  (setq cfg (mkcfg 30.0 30.0 0.3 0.5 0.0 0.0))
  (T= "磚 30 → 容差 0.03" (FLR:AxisTol cfg) 0.03)
  (T-true "容差有下限（磚尺寸怪值時不失效）"
          (>= (FLR:AxisTol (mkcfg 0.0 0.0 0.0 0.0 0.0 0.0)) 1e-6))
  (setq b (list (list '(0.0 0.0) '(300.0 -6.78861e-05) '(300.0 -200.0) '(0.0 -200.0))))
  (T-nil  "0.00007 的繪圖誤差：舊容差判成斜邊" (FLR:IsRect b 1e-6))
  (T-true "0.00007 的繪圖誤差：新容差判成軸向" (FLR:IsRect b (FLR:AxisTol cfg)))
  ;; 底邊是平的、頂邊差 0.00007。舊容差只收得到底邊那一條，
  ;; 前篩因此**少看到一整段真實的邊界**，排序當然失準。
  (T= "舊容差：只收到 1 條（頂邊被漏掉）" (length (FLR:AxisEdges b 1 1e-9)) 1)
  (T= "新容差：兩條水平邊都收到"          (length (FLR:AxisEdges b 1 (FLR:AxisTol cfg))) 2)
  ;; 真正的斜牆不可以被容差吃掉——45 度邊在 300 的跨距上差 300，遠大於 0.03
  (T-nil "真正的斜牆仍判為非軸向"
         (FLR:IsRect (list '((0.0 0.0) (300.0 0.0) (300.0 -100.0) (0.0 -200.0)))
                     (FLR:AxisTol cfg)))
  (setq regs (list '((0.0 0.0) (100.0 0.0) (100.0 50.0) (0.0 50.0))
                   '((7.0 60.0) (143.0 60.0) (143.0 110.0) (7.0 110.0)))
        rbs  (bboxes regs))
  (T= "正鋪 → 1 個列偏移類別"    (length (FLR:StagOffs 0.0)) 1)
  (T= "1/2 交丁 → 2 個類別"      (length (FLR:StagOffs 0.5)) 2)
  (T= "1/3 交丁 → 3 個類別"      (length (FLR:StagOffs (/ 1.0 3.0))) 3)
  (T-nil "怪比例 → nil（退回全算）" (FLR:StagOffs 0.1234567))
  (setq a (FLR:AxisEdges regs 0 0.03))
  (T= "x 向軸邊 4 條" (length a) 4)
  ;; 逐一比對：前篩算出的尺寸集合必須與完整佈置實算的一模一樣
  (setq n 0)
  (foreach ox '(0.0 3.0 7.0 11.5 19.0 26.5)
    (foreach oy '(0.0 5.0 11.0 23.0)
      (setq cfg (list (cons 'tw 30.0) (cons 'th 30.0) (cons 'gap 0.0)
                      (cons 'stagger 0.0) (cons 'ox ox) (cons 'oy oy)
                      (cons 'tol 1e-6) (cons 'mincut 0.25)
                      (cons 'sizeq 0.0001) (cons 'deds nil)))
      (setq a (FLR:GroupSizes
                (append (FLR:AxisSizes (FLR:AxisEdges regs 0 0.03) ox 30.0 30.0 '(0.0) 1e-6)
                        (FLR:AxisSizes (FLR:AxisEdges regs 1 0.03) oy 30.0 30.0 '(0.0) 1e-6))
                0.0001)
            b (cdr (assoc 'sizes (FLR:Evaluate cfg regs rbs '(0.0 0.0 143.0 110.0)))))
      (if (not (equal a b)) (setq n (1+ n)))))
  (T= "24 組偏移：前篩 ≡ 實算，0 筆不符" n 0)
  ;; 交丁時 x 向要對每個列偏移類別各算一次再取聯集
  (setq cfg (list (cons 'tw 30.0) (cons 'th 30.0) (cons 'gap 0.0)
                  (cons 'stagger 0.5) (cons 'ox 4.0) (cons 'oy 0.0)
                  (cons 'tol 1e-6) (cons 'mincut 0.25)
                  (cons 'sizeq 0.0001) (cons 'deds nil)))
  (T= "交丁 1/2 前篩 ≡ 實算"
      (FLR:GroupSizes
        (append (FLR:AxisSizes (FLR:AxisEdges regs 0 0.03) 4.0 30.0 30.0 (FLR:StagOffs 0.5) 1e-6)
                (FLR:AxisSizes (FLR:AxisEdges regs 1 0.03) 0.0 30.0 30.0 '(0.0) 1e-6))
        0.0001)
      (cdr (assoc 'sizes (FLR:Evaluate cfg regs rbs '(0.0 0.0 143.0 110.0)))))
  ;; 前篩適用性
  (setq cfg (mkcfg 30.0 30.0 0.0 0.0 0.0 0.0))
  (T-true "正交 → 前篩可用"
          (FLR:PreRank regs '(0.0 0.0 143.0 110.0) cfg
                       (FLR:Candidates regs '(0.0 0.0 143.0 110.0) cfg 0)
                       (FLR:Candidates regs '(0.0 0.0 143.0 110.0) cfg 1) 10))
  ;; 【0.4 改掉】舊版斜邊一出現就整包退回全算。實測代價：使用者的平面圖
  ;; 7 個區域裡只有 1 個有 1 條斜邊，就讓 576 組全部精算（約 288 秒）。
  ;; AxisEdges 本來就忽略斜邊，而前篩只負責排序、選出來的仍逐一精算，
  ;; 所以斜邊只影響排序品質，不影響正確性——改成照跑並加寬 shortlist。
  (T-true "斜邊 → 前篩照跑（不再整包退回全算）"
          (FLR:PreRank (list '((0.0 0.0) (100.0 0.0) (0.0 100.0)))
                       '(0.0 0.0 100.0 100.0) cfg
                       '(0.0 5.0) '(0.0 5.0) 10))
  (T-nil  "怪交丁比例 → 仍退回全算（唯一剩下的情形）"
          (FLR:PreRank regs '(0.0 0.0 143.0 110.0)
                       (mkcfg 30.0 30.0 0.0 0.1234567 0.0 0.0)
                       '(0.0 5.0) '(0.0 5.0) 10))
  ;; shortlist 長度：全軸向時精確，加寬沒意義；有斜邊時排序有誤差，要加寬補償
  (T= "全軸向 → shortlist 10 組"   (FLR:ShortN 5 999 T nil)   10)
  (T= "有斜邊 → shortlist 加寬到 20" (FLR:ShortN 5 999 nil nil) 20)
  (T= "候選比 shortlist 少時不超額" (FLR:ShortN 5 6 nil nil)    6)
  (T-true "加寬之後仍遠少於全算"
          (< (FLR:ShortN 5 576 nil nil) 576))
  ;; 時間預算制：每組耗時已由呼叫端實測，shortlist = 預算 ÷ 每組耗時。
  ;; 實測依據——A1001 平面圖每組 563 ms，而全算的第一名排在前篩第 24 名，
  ;; 15 秒的預算給出 26 組，剛好涵蓋得到。
  (T= "每組 563ms / 預算 15 秒 → 26 組"
      (FLR:ShortN 5 576 T (list (cons 'perms 563.0) (cons 'budget 15000.0))) 26)
  (T-true "預算算出來比下限小時，仍不低於下限"
          (>= (FLR:ShortN 5 576 T (list (cons 'perms 9000.0) (cons 'budget 15000.0))) 10))
  (T= "小圖（每組 20ms）→ 預算吃得下全部候選"
      (FLR:ShortN 5 100 T (list (cons 'perms 20.0) (cons 'budget 15000.0))) 100)
  (T= "沒給 perms → 退回下限規則"
      (FLR:ShortN 5 576 T (list (cons 'budget 15000.0))) 10)
  (T-true "時間預算是正數" (> FLR:TIMEBUDGET 0.0))
  (T= "預設預算下的組數與明寫 budget 相同"
      (FLR:ShortN 5 576 T (list (cons 'perms 563.0)))
      (FLR:ShortN 5 576 T (list (cons 'perms 563.0) (cons 'budget FLR:TIMEBUDGET))))
  ;; 送去精算的組數不可超過候選總數
  (T-true "WorkCount <= 候選總數"
          (<= (FLR:WorkCount regs '(0.0 0.0 143.0 110.0) cfg 5)
              (FLR:CandCount regs '(0.0 0.0 143.0 110.0) cfg)))
  ;; 前篩不可把已知最佳解漏掉：SQ100 對齊解 (0,0) 刀數 0，必須留在名單內
  (setq regs (list SQ100) rbs (bboxes regs))
  (setq cfg (mkcfg 10.0 10.0 0.0 0.0 0.0 0.0))
  (setq a (FLR:PreRank regs '(0.0 0.0 100.0 100.0) cfg
                       (FLR:Candidates regs '(0.0 0.0 100.0 100.0) cfg 0)
                       (FLR:Candidates regs '(0.0 0.0 100.0 100.0) cfg 1) 10))
  (T-true "最佳解 (0,0) 在前篩名單內"
          (vl-some '(lambda (p) (and (equal (car p) 0.0 1e-6) (equal (cadr p) 0.0 1e-6))) a))

  ;; ---- 14c. 違規的 1D 模型（0.5）----
  ;; 使用者 2026-08-13 問「違規真的壓不到 5% 以下嗎？」——A1001 平面圖 675 組
  ;; 全精算的答案是壓得到（2.8%），但那組起鋪點**不在候選集裡**，而且前篩與排序
  ;; 都以刀數為主鍵，於是連被精算的機會都沒有。這一段釘住補起來的三件事。
  (princ "\n-- 14c. 違規 1D 模型 --")
  (setq regs (list SQ100) rbs (bboxes regs)
        cfg  (mkcfg 30.0 30.0 0.0 0.0 0.0 0.0))
  ;; 牆要帶長度：違規片數按牆長加權，不是按牆的條數
  (setq a (FLR:AxisWalls regs 0 (FLR:AxisTol cfg)))
  (T= "x 向兩道牆" (length a) 2)
  (T-true "每道牆長 100" (and (equal (nth 2 (car a)) 100.0 1e-9)
                              (equal (nth 2 (cadr a)) 100.0 1e-9)))
  (T-true "兩道牆的內部側相反"
          (< (* (nth 1 (car a)) (nth 1 (cadr a))) 0.0))
  ;; 100 寬、磚 30 無縫、下限 7.5：
  ;;   起鋪 0 → 左牆整磚、右牆邊料 10 → 都合規
  ;;   起鋪 5 → 兩端邊料都是 5 → 都違規，各 100/30 列
  (T= "起鋪 0 → 無違規"
      (FLR:BadWeight a 0.0 30.0 30.0 7.5 '(0.0) 30.0) 0.0)
  (T-true "起鋪 5 → 兩端都違規（≈6.67 片）"
          (equal (FLR:BadWeight a 5.0 30.0 30.0 7.5 '(0.0) 30.0)
                 (* 2.0 (/ 100.0 30.0)) 1e-9))
  ;; 交丁：相位各算一次再平均，故同樣的牆只會有一半的列違規
  (T-true "交丁 1/2 → 只有一半的列違規"
          (equal (FLR:BadWeight a 5.0 30.0 30.0 7.5 (FLR:StagOffs 0.5) 30.0)
                 (/ 100.0 30.0) 1e-9))
  ;; 窗口：找得到、且找到的真的是 0
  (setq b (FLR:BadWindows regs cfg 0 3))
  (T-true "回傳窗口" (and b (> (length b) 0)))
  (T= "窗口中點的違規真的是 0"
      (FLR:BadWeight a (car b) 30.0 30.0 7.5 '(0.0) 30.0) 0.0)
  (T-true "窗口數不超過要求" (<= (length (FLR:BadWindows regs cfg 0 2)) 2))
  (T-nil  "沒有軸向牆 → nil"
          (FLR:BadWindows (list '((0.0 0.0) (100.0 10.0) (50.0 100.0))) cfg 0 3))
  ;; 候選集要含得到窗口——這正是 A1001 漏掉 2.8% 那組的原因
  (setq a (FLR:Candidates regs '(0.0 0.0 100.0 100.0) cfg 0))
  (T-true "候選含低違規窗口"
          (vl-some '(lambda (u) (equal u (car b) (FLR:AxisTol cfg))) a))

  ;; 去重容差：繪圖誤差不可以製造出兩個「其實一樣」的候選
  ;; （實測後果：推薦清單的第 2、3 名是同一個方案，統計數字一模一樣）
  (setq b   (list (list '(0.0 0.0) '(300.0 -6.78861e-05)
                        '(300.0 -200.0) '(0.0 -200.0))
                  (list '(350.0 0.0) '(650.0 0.0) '(650.0 -200.0) '(350.0 -200.0)))
        cfg (mkcfg 30.0 30.0 0.3 0.5 0.0 0.0)
        a   (FLR:Candidates b '(0.0 0.0 650.0 200.0) cfg 1)
        n   0)
  (foreach u a
    (foreach v a
      (if (and (not (eq u v)) (equal u v (FLR:AxisTol cfg))) (setq n (1+ n)))))
  (T= "候選集無「差在繪圖誤差」的重複" n 0)

  ;; 前篩名額要分給違規——不分的話低違規解連精算的機會都沒有
  (setq cfg (mkcfg 30.0 30.0 0.3 0.5 0.0 0.0)
        rbs (bboxes b)
        c   (FLR:Candidates b '(0.0 0.0 650.0 200.0) cfg 0)
        p   (FLR:Candidates b '(0.0 0.0 650.0 200.0) cfg 1))
  (T-true "BADSHARE 在 0~0.5 之間（刀數側不可被吃掉一半以上）"
          (and (> FLR:BADSHARE 0.0) (<= FLR:BADSHARE 0.5)))
  (setq a (FLR:PreRank b '(0.0 0.0 650.0 200.0) cfg c p 10))
  (T= "shortlist 仍是 10 組（分兩半不是變兩倍）" (length a) 10)
  ;; 全候選裡違規估計的最低值，必須有人在名單內達到
  ;; （不能指名某一組——同分的組合往往不只一個，指名等於在測排序的巧合）
  ;; 磚 30／縫 0.3／交丁 1/2／下限 25% 下，某組起鋪點的違規估計（兩軸相加）
  (defun bw2 (rg ox oy)
    (+ (FLR:BadWeight (FLR:AxisWalls rg 0 0.03) ox 30.3 30.0 7.5 '(0.0 0.5) 30.3)
       (FLR:BadWeight (FLR:AxisWalls rg 1 0.03) oy 30.3 30.0 7.5 '(0.0)     30.3)))
  (setq res nil)
  (foreach ox c
    (foreach oy p
      (setq t0 (bw2 b ox oy))
      (if (or (null res) (< t0 res)) (setq res t0))))
  (T-true "shortlist 裡有人達到違規估計的最低值"
          (vl-some '(lambda (u) (equal (bw2 b (car u) (cadr u)) res 1e-9)) a))
  ;; ---- 真實平面圖的回歸（A1001）----
  ;; 合成的兩個矩形測不出這個病：它的候選集剛好就含得到低違規解。
  ;; 使用者實際遇到的是這張圖——y 軸的零違規窗口是 [12.60, 14.60]，
  ;; 而頂點與中線給出的最近候選是 12.30，**差 0.3 就掉出窗口**，
  ;; 於是 2.8% 的解從來沒被精算過（實測 675 組全精算才找得到）。
  (setq regs A1001
        p    (FLR:Candidates regs '(0.0 -612.0 1245.0 0.0) cfg 1))
  (T-true "A1001：y 候選落在零違規窗口 [12.60, 14.60] 內"
          (vl-some '(lambda (u) (and (>= u 12.6) (<= u 14.6))) p))
  (T-true "A1001：那個候選確實來自 BadWindows（頂點與中線給不出來）"
          (vl-some '(lambda (u) (and (>= u 12.6) (<= u 14.6)))
                   (FLR:BadWindows regs cfg 1 FLR:BADCAND)))
  (T= "A1001：y 軸的違規下限是 0"
      (FLR:BadWeight (FLR:AxisWalls regs 1 0.03)
                     (car (FLR:BadWindows regs cfg 1 1)) 30.3 30.0 7.5 '(0.0) 30.3)
      0.0)
  ;; x 軸則因為交丁 1/2 而不可能到 0：每道垂直牆被兩個相位各切一次，
  ;; 17 道牆的可行區間交集是空的。實測下限約 12.6 片（正鋪時是 0）。
  (T-true "A1001：x 軸受交丁所限，下限 > 0"
          (> (FLR:BadWeight (FLR:AxisWalls regs 0 0.03)
                            (car (FLR:BadWindows regs cfg 0 1)) 30.3 30.0 7.5
                            '(0.0 0.5) 30.3)
             0.0))
  (setq st (subst '(stagger . 0.0) (assoc 'stagger cfg) cfg))
  (T= "A1001：正鋪時 x 軸下限是 0"
      (FLR:BadWeight (FLR:AxisWalls regs 0 0.03)
                     (car (FLR:BadWindows regs st 0 1)) 30.3 30.0 7.5 '(0.0) 30.3)
      0.0)
  ;; 對照組：只照刀數挑（BADSHARE=0）時挑不到——證明這條測的是新行為，不是巧合
  (setq c   (FLR:Candidates regs '(0.0 -612.0 1245.0 0.0) cfg 0)
        res nil)
  (foreach ox c
    (foreach oy p
      (setq t0 (bw2 regs ox oy))
      (if (or (null res) (< t0 res)) (setq res t0))))
  (setq a (FLR:PreRank regs '(0.0 -612.0 1245.0 0.0) cfg c p 26))
  (T-true "A1001：新前篩挑得到違規估計最低的組合"
          (vl-some '(lambda (u) (equal (bw2 regs (car u) (cadr u)) res 1e-9)) a))
  (setq t1 FLR:BADSHARE FLR:BADSHARE 0.0
        a  (FLR:PreRank regs '(0.0 -612.0 1245.0 0.0) cfg c p 26))
  (T-nil "A1001：舊前篩（只照刀數）挑不到（對照組）"
         (vl-some '(lambda (u) (equal (bw2 regs (car u) (cadr u)) res 1e-9)) a))
  (setq FLR:BADSHARE t1)

  ;; 附掛的低違規方案：必須真的更低，且標得出來
  (setq top (FLR:Optimize cfg b rbs '(0.0 0.0 650.0 200.0) 5))
  (setq n 0 t0 nil)
  (foreach r top
    (if (equal (cdr (assoc 'why r)) 'lowbad) (setq n (1+ n))
      (if (or (null t0) (< (cdr (assoc 'bad r)) t0)) (setq t0 (cdr (assoc 'bad r))))))
  (T-true "附掛數不超過上限" (<= n FLR:EXTRABAD))
  (setq a T)
  (foreach r top
    (if (and (equal (cdr (assoc 'why r)) 'lowbad)
             (>= (cdr (assoc 'bad r)) t0))
      (setq a nil)))
  (T-true "附掛的方案違規一定低於前段最低" a)
  ;; 附掛不可與前段重複（同一組 ox/oy 出現兩次＝清單裡兩列一模一樣）
  (setq n 0)
  (foreach u top
    (foreach v top
      (if (and (not (eq u v))
               (equal (cdr (assoc 'ox u)) (cdr (assoc 'ox v)) 1e-9)
               (equal (cdr (assoc 'oy u)) (cdr (assoc 'oy v)) 1e-9))
        (setq n (1+ n)))))
  (T= "推薦清單無重複方案" n 0)
  ;; 還原共用測試資料
  (setq regs (list SQ100) rbs (bboxes regs) cfg (mkcfg 10.0 10.0 0.0 0.0 0.0 0.0))

  ;; ---- 15. 邊料互補排料 ----
  (princ "\n-- 15. Nest 排料 --")
  (T= "空清單 → 0 片"       (FLR:Pack '() 30.0 0.0) 0)
  (T= "3 條 10 → 1 片"      (FLR:Pack '(10.0 10.0 10.0) 30.0 0.0) 1)
  (T= "4 條 10 → 2 片"      (FLR:Pack '(10.0 10.0 10.0 10.0) 30.0 0.0) 2)
  (T= "12+18 互補 → 1 片"   (FLR:Pack '(12.0 18.0) 30.0 0.0) 1)
  (T= "12+18 加鋸縫 → 2 片" (FLR:Pack '(12.0 18.0) 30.0 0.5) 2)
  (T= "單條大於半片 → 各 1 片" (FLR:Pack '(20.0 20.0) 30.0 0.0) 2)
  ;; 100x120 房間、30x30 磚無縫對齊 → 12 整磚 + 4 片 10 寬的邊磚
  ;; 邊磚三條可從同一片母磚切出 → 實需 12+2 = 14 片，而不是 16 片
  (setq regs (list '((0.0 0.0) (100.0 0.0) (100.0 120.0) (0.0 120.0)))
        rbs  (bboxes regs))
  (setq cfg (append (mkcfg 30.0 30.0 0.0 0.0 0.0 0.0)
                    (list (cons 'deds nil) (cons 'kerf 0.0))))
  (setq p (FLR:Layout cfg regs rbs '(0.0 0.0 100.0 120.0)))
  (setq b (FLR:StatsOf p cfg) r (FLR:Nest p cfg))
  (T= "整磚 12"        (cdr (assoc 'whole r)) 12)
  (T= "裁切磚 4"       (cdr (assoc 'cut b))   4)
  (T= "邊料排料 → 2 片母磚" (cdr (assoc 'xbins r)) 2)
  (T= "實需母磚 14"    (cdr (assoc 'need r))  14)
  (T-true "實需 < 每片各一"  (< (cdr (assoc 'need r)) (cdr (assoc 'tiles b))))
  ;; 材料守恆：母磚總面積必須蓋得住鋪設淨面積
  (T-true "母磚面積 >= 鋪設面積"
          (>= (* (cdr (assoc 'need r)) 900.0) (- (cdr (assoc 'used r)) 1e-6)))
  (T= "廢料 = 母磚面積 - 鋪設面積"
      (cdr (assoc 'scrap r)) (- (* (cdr (assoc 'need r)) 900.0) (cdr (assoc 'used r))))
  (T-true "廢料率介於 0~1"
          (and (>= (cdr (assoc 'rate r)) 0.0) (< (cdr (assoc 'rate r)) 1.0)))
  ;; 全整磚時排料不可多算
  (setq regs (list SQ100) rbs (bboxes regs))
  (setq cfg (append (mkcfg 10.0 10.0 0.0 0.0 0.0 0.0)
                    (list (cons 'deds nil) (cons 'kerf 0.0))))
  (setq r (FLR:Nest (FLR:Layout cfg regs rbs '(0.0 0.0 100.0 100.0)) cfg))
  (T= "全整磚 → 實需 = 整磚數 100" (cdr (assoc 'need r)) 100)
  (T= "全整磚 → 廢料 0"           (cdr (assoc 'scrap r)) 0.0)

  ;; ---- 16. 旋轉（45° 斜鋪）----
  ;; 把區域轉進磚座標系鋪完再轉回來。磚在該座標系仍是軸向矩形，
  ;; 所以條帶分解、U 形判定、最佳化全部照用，不需要外部引擎。
  (princ "\n-- 16. 斜鋪 --")
  (setq p '((0.0 0.0) (200.0 0.0) (200.0 150.0) (0.0 150.0)))
  (setq a (FLR:RotPoly p (/ pi 4.0) '(0.0 0.0)))
  (T= "旋轉不改面積" (FLR:Area a) (FLR:Area p))
  (setq b (FLR:RotPoly a (- (/ pi 4.0)) '(0.0 0.0)))
  (T-true "轉去再轉回 = 原多邊形"
          (vl-every '(lambda (u v) (< (distance u v) 1e-9)) b p))
  (T= "0 度不動" (FLR:RotPoly p 0.0 '(5.0 5.0)) p)
  ;; 45 度鋪設：面積必須完全守恆
  (setq regs (list (FLR:RotPoly p (- (/ pi 4.0)) '(100.0 75.0)))
        rbs  (bboxes regs))
  (setq cfg (append (mkcfg 30.0 30.0 0.0 0.5 0.0 0.0) (list (cons 'deds nil))))
  (setq a 0.0)
  (foreach it (FLR:Layout cfg regs rbs (FLR:BBox (car regs)))
    (foreach pt (cdr (assoc 'parts (cdr it))) (setq a (+ a (FLR:Area (nth 1 pt))))))
  (T= "45 度鋪設面積守恆" a 30000.0)

  ;; ---- 17. 排序目標可選（1.4）----
  ;; 這一節鎖住的是「換目標會換答案，而且換得對」。
  ;; 排序準則錯了不會有任何錯誤訊息——只會安靜地推薦錯的方案，
  ;; 而使用者無從分辨「這是最好的」與「這是排錯的」。
  (princ "\n-- 17. 排序目標 --")
  (T= "未指定 → 最少刀"     (FLR:GoalOf nil)     'cuts)
  (T= "不認得的值 → 最少刀" (FLR:GoalOf 'nosuch) 'cuts)
  (T= "waste 照收"          (FLR:GoalOf 'waste)  'waste)
  (T= "sym 照收"            (FLR:GoalOf 'sym)    'sym)
  (T= "三個目標各有標籤"    (length FLR:GOALS)   3)
  (setq a T)
  (foreach g '(cuts waste sym)
    (if (or (null (FLR:GoalLabel g)) (= (FLR:GoalLabel g) "")) (setq a nil)))
  (T-true "每個目標的標籤非空" a)

  ;; 同一組資料、三個目標、三個不同的贏家——這正是這個功能的全部意義
  (setq a (list (cons 'ushape 0) (cons 'cutsizes 5) (cons 'bad 10) (cons 'tiles 100)
                (cons 'cut 20) (cons 'need 90) (cons 'sym 0.0) (cons 'minleg 12.0))
        b (list (cons 'ushape 0) (cons 'cutsizes 3) (cons 'bad 4) (cons 'tiles 100)
                (cons 'cut 20) (cons 'need 95) (cons 'sym 8.0) (cons 'minleg 20.0)))
  (T-true "最少刀：刀少者勝"     (FLR:BetterG b a 'cuts))
  (T-true "最少廢料：母磚少者勝" (FLR:BetterG a b 'waste))
  (T-true "對稱：對稱誤差小者勝" (FLR:BetterG a b 'sym))
  (T-nil  "最少刀：刀多者不勝"   (FLR:BetterG a b 'cuts))
  (T-nil  "最少廢料：母磚多者不勝" (FLR:BetterG b a 'waste))
  (T-nil  "對稱：誤差大者不勝"   (FLR:BetterG b a 'sym))
  (T-nil  "完全相同不判優（任一目標）"
          (or (FLR:BetterG a a 'cuts) (FLR:BetterG a a 'waste) (FLR:BetterG a a 'sym)))
  ;; 舊簽章必須完全等同「最少刀」，否則既有呼叫端會被靜默改掉行為
  (T= "FLR:Better ＝ 目標 cuts"
      (list (FLR:Better a b) (FLR:Better b a))
      (list (FLR:BetterG a b 'cuts) (FLR:BetterG b a 'cuts)))

  ;; 對稱同分才輪到「最寬」；違規排在這兩者之後（A1001 實測後的裁決，
  ;; 見 FLR:GoalKeys 的註：違規一放前面，對稱目標就退化成違規目標）
  (setq c (subst (cons 'minleg 20.0) (assoc 'minleg a) a))
  (T-true "對稱同分 → 邊磚寬者勝" (FLR:BetterG c a 'sym))
  (setq c (subst (cons 'bad 0) (assoc 'bad b) b))
  (T-true "對稱不同 → 違規再低也不勝" (FLR:BetterG a c 'sym))
  (setq c (subst (cons 'bad 0) (assoc 'bad a) a))
  (T-true "對稱與最寬都同分 → 違規少者勝" (FLR:BetterG c a 'sym))

  ;; 缺欄位（目標是 waste 但沒開 'nest）不可亂排：其餘欄位相同時應不分優劣
  (setq c (subst (cons 'need 90) (assoc 'need b) b)
        c (subst (cons 'cutsizes 5) (assoc 'cutsizes c) c)
        c (subst (cons 'bad 10) (assoc 'bad c) c))
  (T-nil "缺 need 時不亂排"
         (or (FLR:BetterG (vl-remove (assoc 'need a) a) (vl-remove (assoc 'need c) c) 'waste)
             (FLR:BetterG (vl-remove (assoc 'need c) c) (vl-remove (assoc 'need a) a) 'waste)))

  ;; InsertG 要照目標插，不能永遠照刀數
  (setq r (FLR:InsertG a (list b) 'waste))
  (T= "InsertG 依目標排序" (cdr (assoc 'need (car r))) 90)
  (setq r (FLR:InsertG a (list b) 'cuts))
  (T= "InsertG 最少刀時反過來" (cdr (assoc 'cutsizes (car r))) 3)

  ;; GoalCfg：只有 waste 需要每組都算排料
  (T-true "waste 會補 nest"
          (cdr (assoc 'nest (FLR:GoalCfg (list (cons 'goal 'waste))))))
  (T-nil  "cuts 不補 nest"
          (cdr (assoc 'nest (FLR:GoalCfg (list (cons 'goal 'cuts))))))
  (T-nil  "沒給目標也不補 nest" (cdr (assoc 'nest (FLR:GoalCfg '()))))

  ;; ---- 17b. 排料歸類：Evaluate 與 Nest 必須是同一套 ----
  ;; 排序用的母磚數與統計表上的叫料量若不一致，使用者會看到
  ;; 「推薦說 804，表上寫 812」——而兩個數字都沒有錯誤訊息可查。
  (princ "\n-- 17b. 排料一致性 --")
  (T= "StripCuts ＝ StripCutsOf∘Strips"
      (FLR:StripCuts LSHAPE 10.0 10.0 1e-6)
      (FLR:StripCutsOf (FLR:Strips LSHAPE 0 1e-6) 10.0 10.0 1e-6))
  (T= "多條帶 → 各吃一片母磚"
      (FLR:NestClass (FLR:Strips LSHAPE 0 1e-6) 10.0 10.0 1e-6) 'lwhole)
  (T= "全寬條 → 吃高度"
      (FLR:NestClass '((10.0 4.0)) 10.0 10.0 1e-6) '(1 . 4.0))
  (T= "角料 → 吃寬度"
      (FLR:NestClass '((4.0 4.0)) 10.0 10.0 1e-6) '(0 . 4.0))
  ;; 100x120 房間、30x30 磚（同 §15 那組，該處已驗出實需 14 片）
  (setq regs (list '((0.0 0.0) (100.0 0.0) (100.0 120.0) (0.0 120.0)))
        rbs  (bboxes regs)
        cfg  (append (mkcfg 30.0 30.0 0.0 0.0 0.0 0.0)
                     (list (cons 'deds nil) (cons 'kerf 0.0) (cons 'nest T))))
  (setq res (FLR:Evaluate cfg regs rbs '(0.0 0.0 100.0 120.0))
        r   (FLR:Nest (FLR:Layout cfg regs rbs '(0.0 0.0 100.0 120.0)) cfg))
  (T= "Evaluate 的母磚數 ＝ Nest 的" (cdr (assoc 'need res)) (cdr (assoc 'need r)))
  (T= "且等於 §15 驗過的 14 片"      (cdr (assoc 'need res)) 14)
  ;; 沒開 'nest 就不該算（那是熱路徑上白花的成本）
  (setq c (vl-remove (assoc 'nest cfg) cfg))
  (T-nil "未開 nest 時不算母磚數"
         (cdr (assoc 'need (FLR:Evaluate c regs rbs '(0.0 0.0 100.0 120.0)))))

  ;; ---- 17b2. 廢料的 1D 模型（前篩用）----
  ;; 這支的存在理由就是「借刀數排序挑不到省料的方案」（A1001 實測真值第 490 名）。
  ;; 它要抓得到的核心現象只有一個：**互補的兩條邊料共用一片母磚**。
  ;; 磚 30、無縫、起鋪 0；牆 ((座標 側 長度) ...)，側 −1 表示邊料寬 = 座標 mod 磚距
  (T= "沒有牆 → 不吃母磚" (FLR:WasteWeight '() 0.0 30.0 30.0 30.0 0.0 '(0.0)) 0)
  (T= "牆落在磚邊 → 不產生邊料"
      (FLR:WasteWeight '((30.0 -1.0 30.0)) 0.0 30.0 30.0 30.0 0.0 '(0.0)) 0)
  (T= "12 與 18 互補 → 1 片母磚"
      (FLR:WasteWeight '((12.0 -1.0 30.0) (18.0 -1.0 30.0)) 0.0 30.0 30.0 30.0 0.0 '(0.0))
      1)
  (T= "同樣兩條加鋸縫 → 2 片"
      (FLR:WasteWeight '((12.0 -1.0 30.0) (18.0 -1.0 30.0)) 0.0 30.0 30.0 30.0 0.5 '(0.0))
      2)
  ;; 牆越長切出的條越多 → 吃掉的母磚必須跟著變多，否則長牆與短牆會被當成一樣好
  (T-true "牆長 → 母磚多"
          (> (FLR:WasteWeight '((12.0 -1.0 300.0)) 0.0 30.0 30.0 30.0 0.0 '(0.0))
             (FLR:WasteWeight '((12.0 -1.0 30.0))  0.0 30.0 30.0 30.0 0.0 '(0.0))))
  ;; 起鋪點挪一下讓兩條邊料湊得進同一片母磚，估計值必須真的下降
  ;; ——前篩能不能挑到省料的方案，靠的就是這個落差量得出來。
  ;; 牆在 10 與 25：起鋪 0 → 邊料 10+25=35 > 30，兩片；起鋪 5 → 5+20=25，一片。
  (T= "起鋪 0 → 2 片"
      (FLR:WasteWeight '((10.0 -1.0 30.0) (25.0 -1.0 30.0)) 0.0 30.0 30.0 30.0 0.0 '(0.0))
      2)
  (T= "起鋪 5 → 1 片（湊進同一片母磚）"
      (FLR:WasteWeight '((10.0 -1.0 30.0) (25.0 -1.0 30.0)) 5.0 30.0 30.0 30.0 0.0 '(0.0))
      1)

  ;; ---- 17c. 對稱：Evaluate 帶的值與前篩的估計都必須等於 FLR:SymErr ----
  ;; 前篩若與精算對不上，就會出現「清單裡的方案不是前篩挑的那些」。
  (princ "\n-- 17c. 對稱一致性 --")
  (setq regs (list SQ100 '((150.0 0.0) (280.0 0.0) (280.0 90.0) (150.0 90.0)))
        rbs  (bboxes regs))
  (setq a T b T)
  (foreach ox '(0.0 1.7 3.3 7.5)
    (foreach oy '(0.0 2.4 6.6)
      (setq cfg (append (mkcfg 10.0 10.0 0.5 0.0 ox oy) (list (cons 'deds nil)))
            res (FLR:Evaluate cfg regs rbs '(0.0 0.0 280.0 100.0))
            c   (max (car (FLR:AxisSym rbs 0 ox 10.0 10.5 1e-6))
                     (car (FLR:AxisSym rbs 1 oy 10.0 10.5 1e-6))))
      (if (not (equal (cdr (assoc 'sym res)) (FLR:SymErr regs cfg) 1e-9)) (setq a nil))
      (if (not (equal c (FLR:SymErr regs cfg) 1e-9)) (setq b nil))))
  (T-true "Evaluate 的 sym ＝ FLR:SymErr"        a)
  (T-true "前篩的對稱估計是精確值（兩軸取大）"  b)

  ;; ---- 17d. 前篩會依目標給出不同名單 ----
  ;; 目標若沒傳到前篩，就會發生「照刀數挑進來、照對稱排名次」
  ;; ——名單本身就沒有對稱好的方案，排得再對也選不到。
  (princ "\n-- 17d. 前篩依目標 --")
  (setq cfg (append (mkcfg 10.0 10.0 0.5 0.5 0.0 0.0) (list (cons 'deds nil))))
  (setq a (FLR:Candidates regs '(0.0 0.0 280.0 100.0) cfg 0)
        b (FLR:Candidates regs '(0.0 0.0 280.0 100.0) cfg 1))
  (setq r  (FLR:PreRank regs '(0.0 0.0 280.0 100.0)
                        (append cfg (list (cons 'goal 'cuts))) a b 8)
        st (FLR:PreRank regs '(0.0 0.0 280.0 100.0)
                        (append cfg (list (cons 'goal 'sym)))  a b 8))
  (T= "兩份名單長度相同" (length r) (length st))
  (T-true "換目標會換名單" (not (equal r st)))
  ;; 對稱目標的第一名，其對稱誤差必須是全體候選裡最小的（前篩對稱是精確的）
  (setq c nil)
  (foreach ox a
    (foreach oy b
      (setq p (max (car (FLR:AxisSym rbs 0 ox 10.0 10.5 1e-6))
                   (car (FLR:AxisSym rbs 1 oy 10.0 10.5 1e-6))))
      (if (or (null c) (< p c)) (setq c p))))
  (T= "對稱目標的前篩第一名＝全體最小對稱誤差"
      (max (car (FLR:AxisSym rbs 0 (car (car st)) 10.0 10.5 1e-6))
           (car (FLR:AxisSym rbs 1 (cadr (car st)) 10.0 10.5 1e-6)))
      c)
  ;; ---- 17e. 走完整條鏈：FLR:Optimize 三個目標各自排對 ----
  ;; 前面幾節分別驗了比較準則與前篩，但目標要**一路傳到底**才有用
  ;; ——漏傳一段的症狀是「照 A 挑進來、照 B 排名次」，名次看起來只是怪怪的。
  (princ "\n-- 17e. Optimize 依目標 --")
  (setq regs (list '((0.0 0.0) (95.0 0.0) (95.0 62.0) (0.0 62.0)))
        rbs  (bboxes regs))
  (foreach g '(cuts waste sym)
    (setq cfg (append (mkcfg 10.0 10.0 0.5 0.5 0.0 0.0)
                      (list (cons 'deds nil) (cons 'kerf 0.0) (cons 'goal g)))
          top (FLR:Optimize cfg regs rbs '(0.0 0.0 95.0 62.0) 5))
    (T-true (strcat (vl-symbol-name g) "：有推薦結果") (> (length top) 0))
    ;; 前段（不含附掛的 [低違規]）必須是照該目標由優到劣
    (setq a T c nil)
    (foreach r top
      (if (not (equal (cdr (assoc 'why r)) 'lowbad))
        (progn
          (if (and c (FLR:BetterG r c g)) (setq a nil))
          (setq c r))))
    (T-true (strcat (vl-symbol-name g) "：前段依該目標遞減") a)
    ;; waste 才算母磚數，其他目標不算（成本要花在刀口上）
    (if (eq g 'waste)
      (T-true "waste：每筆都有母磚數"
              (vl-every '(lambda (r) (cdr (assoc 'need r))) top))
      (T-nil  (strcat (vl-symbol-name g) "：不算母磚數")
              (vl-some '(lambda (r) (cdr (assoc 'need r))) top))))

  ;; ---- 18. 起鋪點精修（1.4.5）----
  ;; 候選集是啟發式產生的，實測有漏（見 CHANGELOG v1.4.5）。精修沿兩軸各掃一遍補救。
  ;; 這一節守三件會靜默出錯的事：
  ;;   ① 點數由預算算出來——算錯只會讓精修變慢或變粗，不會報錯
  ;;   ② 精修**只能變好不能變壞**（單調性）。這是整個功能的安全性質：
  ;;      它會改掉使用者拿到的第 1 名，變壞的話沒有任何症狀指得到這裡
  ;;   ③ 第二軸要掃過第一軸的結果，不是原點那條線
  (princ "\n-- 18. 起鋪點精修 --")

  ;; ① FLR:RefineN：預算 ÷ 每組耗時 ÷ 2 軸，含上下限
  (T= "RefineN：8000/2/62 ≈ 64"     (FLR:RefineN '() 62.0) 64)
  ;; 下限 8 點的成本是 2×8×per，允許到預算兩倍（8000×2÷16 = 1000 ms/組）
  (T= "RefineN：每組 900 ms 仍取下限 8" (FLR:RefineN '() 900.0) 8)
  ;; 【規模量測抓到的】再慢下去，下限就會把預算的承諾蓋掉——多房 7×7 每組 890 ms
  ;; 時精修要 14.2 秒而預算是 8 秒。超過兩倍就不精修，並回 0 讓呼叫端略過。
  (T= "RefineN：慢到連下限都撐破預算 → 0" (FLR:RefineN '() 1100.0) 0)
  (T= "RefineN：邊界 1000 ms 仍精修"      (FLR:RefineN '() 1000.0) 8)
  ;; 小圖每組 1 ms 會算出四千點，那個解析度沒有意義，純粹空轉 → 上限 240
  (T= "RefineN：每組極快時封頂 240" (FLR:RefineN '() 1.0) 240)
  (T= "RefineN：沒有實測耗時→32"    (FLR:RefineN '() nil) 32)
  (T= "RefineN：cfg 的 refbudget 蓋過預設"
      (FLR:RefineN (list (cons 'refbudget 1240.0)) 10.0) 62)
  ;; 【1.4.7 對話框開放這個欄位之後，0 變成使用者填得到的值】
  ;; 它的意思是「完全不精修」，不是「用預設」——(null 0.0) 為 nil，
  ;; 所以不會退回 FLR:REFINEBUDGET，這條就是鎖住那個分岔。
  (T= "RefineN：預算 0 → 不精修" (FLR:RefineN (list (cons 'refbudget 0.0)) 30.0) 0)
  ;; 使用者填的是**秒**，cfg 收的是毫秒。少乘 1000 的話 8 秒會變成 8 毫秒
  ;; ——結果是精修悄悄關掉（0 點），而畫面上什麼都不會說。
  (T= "RefineN：8 秒＝8000 毫秒" (FLR:RefineN (list (cons 'refbudget 8000.0)) 30.0) 133)
  (T= "RefineN：誤當成 8 毫秒的話會變 0"
      (FLR:RefineN (list (cons 'refbudget 8.0)) 30.0) 0)
  ;; 上限 120 秒仍受 240 點封頂：再加預算只是空轉
  (T= "RefineN：預算 120 秒仍封頂 240"
      (FLR:RefineN (list (cons 'refbudget 120000.0)) 30.0) 240)

  ;; ---- 階層式取樣點：預算越多結果不會更差，要由建構保證 ----
  ;; 【這一組守的是一個查不到原因的症狀】舊寫法是 k×(1/npts)，31 點與 63 點的
  ;; 格點除了 0 以外毫無交集，於是加預算＝換一組取樣點。實測踩到「4 秒的對稱
  ;; 比 8 秒好」。精修是確定性的，那不是雜訊，但看起來像。
  (T= "RefineFrac：層 0 是 8 等分" (FLR:RefineFrac 0) 0.0)
  (T= "RefineFrac：0 → 1/8"        (FLR:RefineFrac 1) 0.125)
  (T= "RefineFrac：7 → 7/8"        (FLR:RefineFrac 7) 0.875)
  (T= "RefineFrac：層 1 補中點 1/16" (FLR:RefineFrac 8) 0.0625)
  (T= "RefineFrac：層 1 末 15/16"  (FLR:RefineFrac 15) 0.9375)
  (T= "RefineFrac：層 2 起 1/32"   (FLR:RefineFrac 16) 0.03125)
  (T= "RefineFrac：層 2 末 31/32"  (FLR:RefineFrac 31) 0.96875)
  (T= "RefineFrac：層 3 起 1/64"   (FLR:RefineFrac 32) 0.015625)
  ;; 全部落在 [0,1) 且互不重複——重複就是白跑一次評估，落在 1.0 則與 0.0 同點
  (setq a T c '() )
  (setq p 0)
  (while (< p 64)
    (setq r (FLR:RefineFrac p))
    (if (or (< r 0.0) (>= r 1.0)) (setq a nil))
    (if (vl-some '(lambda (u) (equal u r 1e-12)) c) (setq a nil))
    (setq c (cons r c) p (1+ p)))
  (T-true "RefineFrac：前 64 個都在 [0,1) 且互不重複" a)
  ;; **巢狀**：前 n 個必然包含前 m 個（m<n）。這正是舊寫法沒有的性質。
  (setq a T p 0)
  (while (< p 16)
    (if (not (vl-some '(lambda (u) (equal u (FLR:RefineFrac p) 1e-12))
                      (mapcar 'FLR:RefineFrac '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
                                                16 17 18 19 20 21 22 23 24 25 26 27
                                                28 29 30 31))))
      (setq a nil))
    (setq p (1+ p)))
  (T-true "RefineFrac：前 16 個是前 32 個的子集（巢狀）" a)

  (setq regs (list '((0.0 0.0) (95.0 0.0) (95.0 62.0) (0.0 62.0)))
        rbs  (bboxes regs)
        bb   '(0.0 0.0 95.0 62.0))
  ;; 由建構保證的性質要**用行為驗一次**：點數加倍，結果不可以變差。
  ;; 舊寫法在這一條上會紅——那才是使用者真正踩到的東西。
  (foreach g '(cuts waste sym)
    (setq cfg (FLR:WithRects
                (append (mkcfg 10.0 10.0 0.5 0.5 0.0 0.0)
                        (list (cons 'deds nil) (cons 'kerf 0.0)
                              (cons 'goal g) (cons 'nest T)))
                regs)
          a   (FLR:Evaluate cfg regs rbs bb)
          c   (FLR:RefineAxis cfg regs rbs bb a g 0 8)
          d   (FLR:RefineAxis cfg regs rbs bb a g 0 16))
    (T-nil (strcat (vl-symbol-name g) "：點數加倍不會變差（8 → 16）")
           (FLR:BetterG c d g)))

  (foreach g '(cuts waste sym)
    (setq cfg (FLR:WithRects
                (append (mkcfg 10.0 10.0 0.5 0.5 0.0 0.0)
                        (list (cons 'deds nil) (cons 'kerf 0.0)
                              (cons 'goal g) (cons 'nest T)))
                regs)
          a   (FLR:Evaluate cfg regs rbs bb)                    ; 起點
          c   (FLR:Refine cfg regs rbs bb a g 16))
    ;; ② 單調性：精修結果不可以比起點差
    (T-nil (strcat (vl-symbol-name g) "：精修不會變差")
           (FLR:BetterG a c g))
    ;; 單軸也一樣，而且另一軸必須原封不動
    (setq d (FLR:RefineAxis cfg regs rbs bb a g 0 16))
    (T-nil (strcat (vl-symbol-name g) "：單軸精修不會變差") (FLR:BetterG a d g))
    (T= (strcat (vl-symbol-name g) "：掃 x 時 oy 不動")
        (cdr (assoc 'oy d)) (cdr (assoc 'oy a))))

  ;; ③ 第二軸掃的是穿過第一軸結果的那條線。
  ;; 寫成「兩軸各自從起點掃再取較好者」的話，兩軸的改善會互相抵銷掉一個
  ;; ——而結果仍然「有改善」，看起來完全正常。這裡直接驗最終的 ox 等於
  ;; 第一軸單獨掃出來的 ox（第二軸只動 oy）。
  (setq cfg (FLR:WithRects
              (append (mkcfg 10.0 10.0 0.5 0.5 0.0 0.0)
                      (list (cons 'deds nil) (cons 'kerf 0.0)
                            (cons 'goal 'sym) (cons 'nest T)))
              regs)
        a (FLR:Evaluate cfg regs rbs bb)
        d (FLR:RefineAxis cfg regs rbs bb a 'sym 0 16)
        c (FLR:Refine cfg regs rbs bb a 'sym 16))
  (T= "精修：最終 ox 來自第一軸的掃描" (cdr (assoc 'ox c)) (cdr (assoc 'ox d)))

  ;; ④ Optimize 的 'refine 明確給 nil 就不精修（測試要固定結果時用得到）
  (setq cfg (append (mkcfg 10.0 10.0 0.5 0.5 0.0 0.0)
                    (list (cons 'deds nil) (cons 'kerf 0.0) (cons 'goal 'sym)
                          (cons 'refine nil)))
        top (FLR:Optimize cfg regs rbs bb 5))
  (T-true "refine=nil：仍有推薦結果" (> (length top) 0))
  ;; 關掉精修時，第 1 名一定落在候選集上；開著時可能不在（那正是精修的用途）
  (T-true "refine=nil：第 1 名的 ox 在候選集裡"
          (vl-some '(lambda (u) (equal u (cdr (assoc 'ox (car top))) 1e-9))
                   (FLR:Candidates regs bb cfg 0)))

  ;; 還原共用測試資料
  (setq regs (list SQ100) rbs (bboxes regs) cfg (mkcfg 10.0 10.0 0.0 0.0 0.0 0.0))

  ;; ---- 總結 ----
  (princ "\n\n================ 結果 ================")
  (princ (strcat "\nPASS " (itoa *PASS*) "   FAIL " (itoa *FAIL*)))
  (if (> *FAIL* 0)
    (progn (princ "\n失敗項目：")
           (foreach f (reverse *FAILED*) (princ (strcat "\n  - " f)))))
  (princ "\n======================================")
  (princ)
)
