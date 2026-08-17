;; =========================================================================
;; FLR_Core — 地面磁磚鋪設　純邏輯核心
;; 版本：0.9
;; 最後更新：2026-08-14
;; =========================================================================
;;
;; 【定位】
;;   本檔**完全不碰 AutoCAD API**（無 vla-/vlax-/ssget/entget/grread）。
;;   所有幾何、網格、統計、最佳化都在這裡，可用 accoreconsole 無頭全自動驗證。
;;   繪圖與 UI 由 FLR_FloorTile.lsp 負責。
;;
;; 【座標與單位】
;;   所有長度皆為「圖面單位」。輸入值 = 圖面值，不做換算。
;;   面積輸出為圖面單位平方；轉 m² 由 UI 層依單位設定處理。
;;
;; 【多邊形表示法】
;;   poly = ((x y) (x y) ...)　不重複頭尾點，隱含封閉。
;;
;; 【版本沿革】
;;   0.1  初版：S-H 解析裁切、網格、分類統計、起鋪點候選集最佳化
;;   0.2  依 檢核紀錄_v1.0.md 修正與擴充：
;;        - FLR-01/02 裁切量測改「條帶分解」，修 L 形角磚下限漏檢與尺寸取到補數
;;        - FLR-03    尺寸分群由單一連結改**完整連結**，保證誤差 ≤ 容差
;;        - FLR-04    重疊偵測補雙向頂點測試與邊相交，抓得到包含與十字交叉
;;        - FLR-05    面積改由**碎片加總**，修互相重疊的扣除物被扣兩次
;;        - 新增 1D 前篩（FLR:PreRank），起鋪最佳化省去 |cx|×|cy| 次完整佈置
;;        - 新增 邊料互補排料（FLR:Nest），叫料量改用實算而非猜耗損率
;;        - 新增 旋轉輔助（FLR:RotPoly），支援 45° 斜鋪
;;   0.3  依 2026-08-13 使用者回饋（「不是選自動對稱？怎麼最左、最右數字不一樣？」）：
;;        - FLR:Candidates 原本只放「所有區域**整體外框**」的中線兩解。多房間時
;;          那對每一間都不是中線——實測 L 形(0..300)＋矩形(350..650) 時「磚置中」
;;          給矩形的邊料是 2.4 / 24.6。現在**每一區各自的中線也放進候選**。
;;        - 新增 FLR:SymErr／FLR:EdgeCuts：量「兩端邊料差」，供推薦清單標示對稱度。
;;          **只量不排序**——排序準則維持不變，由使用者自己權衡刀數與對稱。
;;   0.4  依 2026-08-13 使用者回饋（「掃描好久…」）修最佳化效能：
;;        - FLR:AxisTol：軸向判定改用「與磚同量綱」的容差，不可用浮點等值容差
;;        - FLR:PreRank：一條斜邊不再關掉整個前篩（斜邊只影響排序、不影響正確性）
;;        - FLR:ShortN：shortlist 長度改為時間預算制（預算 ÷ 實測每組耗時）
;;   0.5  依 2026-08-13 使用者提問（「違規真的壓不到 5% 以下嗎？」）實測後補：
;;        - 新增 FLR:AxisWalls／FLR:BadWeight／FLR:BadWindows：違規的 1D 模型，
;;          算得出「違規最少的起鋪窗口」，其中點加入候選集（原本搆不到）
;;        - FLR:Candidates 去重容差改用 FLR:AxisTol（原本 1e-6 擋不住繪圖誤差，
;;          實測讓推薦清單第 2、3 名是同一個方案）
;;        - FLR:PreRank 的名額分兩半：一半給刀數、一半給違規（原本全給刀數，
;;          低違規解連被精算的機會都沒有）
;;        - FLR:Optimize 在前 topn 之後附掛「違規更低」的方案
;;   0.6  排序目標可選（FLR 1.4）。1.2.2 起「對稱度只量不排序」，實際上是把
;;        取捨丟回給使用者，而清單本身就是照刀數挑出來的——對稱好的方案
;;        根本不在裡面。改成使用者選目標，整條鏈（候選前篩→精算→排序）跟著換：
;;        - FLR:GoalKeys／FLR:BetterG：三個目標各一串比較鍵（cuts／waste／sym）
;;        - FLR:Evaluate 多回 'sym 與 'need（母磚數，僅 'nest 開啟時算）
;;        - FLR:NestClass：排料歸類移到 FLR:ClassifyTile 分解條帶時順手做，
;;          FLR:Nest 與 FLR:Evaluate 共用同一份歸類（原本是兩份程式碼）
;;        - FLR:AxisSym／FLR:PreRank：前篩多一組「對稱」排序，且**是精確的**
;;          （對稱誤差本來就是 1D 量：x 只跟 ox 有關、y 只跟 oy 有關）
;;        目標的鍵序、以及前篩對三個目標各準到什麼程度，都是 A1001 實測定的，
;;        數字與結論見 CHANGELOG v1.4。
;;        另有兩項效能改動（同樣先量再改，見 Tools\FLR_CostProbe.ps1）：
;;        - 整磚跳過條帶分解（isfull 提到量測迴圈之前，不需新判定）
;;        - FLR:SlabRects／FLR:CellHome：軸向區域分解成矩形，完全落在內部的格子
;;          連 Sutherland-Hodgman 都不必跑
;;        A1001 每組候選 302 → 125 ms，全算 702 組 212 → 79.6 秒
;;        （全算是實測值，不是 125 ms × 702——矩形分解等成本只算一次）。
;;   0.7  條帶分解去重（FLR 1.4.2），純效能、行為零變動：
;;        - FLR:LegScanS：吃現成的 x 向條帶。舊版 FLR:LegScan 在內部又算了一次
;;          **同參數**的 FLR:Strips，而呼叫端本來就要一份給裁切尺寸與排料歸類用
;;          ——同一片碎片的 x 向分解一輪跑了兩次。
;;        - 軸向矩形碎片整段跳過 y 向分解（c1 ≡ c0，推導見 FLR:LegScanS 上方）。
;;          A1001 的非整磚碎片 286/301＝95% 命中。
;;        - FLR:ClipTileByRegions／FLR:ClassifyTile：同一個多邊形不再算兩次面積。
;;        A1001 同一次 run 實測：量測段 125 → 47 ms（−62.4%）、
;;        每組候選 213.7 → 135.7 ms（−36.5%）。
;;        **絕對值不可跨機比較**——同一台筆電不同 run 之間就會差近一倍，
;;        故 Tools\FLR_CostProbe.lsp 改成同一次 run 內量新舊兩條路徑。
;;   0.8  多邊形走訪由 `(nth i poly)` 改為游標走訪（FLR 1.4.3），
;;        純重構、輸出一字不差：SignedArea／PtInPoly／Clip1／Clean／ReflexCount／
;;        EdgeCross／Cross／SpanAt／Strips／SlabRects／IsRect／AxisEdges／AxisWalls／
;;        BadWindows／RectFrag／ClipTileByRegions／CellHome，另加 UI 層的
;;        FragCells／DrawPreview。`nth` 每次都從頭數 cdr，寫在 while 裡就是 O(n²)。
;;        Clip1 與 Cross 另外把 `(nth axis a)` 從一輪三次降到零次（沿用上一輪的 bv）。
;;        BBoxHit／CellHome 的四個欄位改 car/cadr/caddr/cadddr 直取。
;;        對照組（0.6 原文）留在 FLR_Tests.lsp §5d 逐項比對——純重構最容易錯的是
;;        頭尾相接那一輪的順序，而那種錯不拋例外、只讓數字靜默偏掉。
;;
;; =========================================================================

;;; ---------------------------------------------------------------- 常數
(setq FLR:EPS 1e-9)

;;; ================================================================
;;;  一、基礎幾何
;;; ================================================================

;;; ---- 【0.8】多邊形一律用「游標走訪」，不要用 (nth i poly) ----
;;
;; `(nth i poly)` 每次都從頭數 i 個 cdr，寫在 while 迴圈裡就是 O(n²)。
;; 本節的函式全都在最佳化的熱路徑上（每組候選 × 每一格 × 每一區），
;; 而 AutoLISP 的每一次 nth 都是一次內建呼叫＋一輪指標追蹤。
;;
;; 兩種走訪形狀，改寫時**逐一對應原本的 i 序**，浮點運算的順序與運算元一字未改
;; （只換取得元素的方式）——否則 1e-16 的差異會讓面積守恆那類斷言翻掉：
;;
;;   邊走訪 (a=p[i], b=p[i+1 mod n])：
;;     (setq a (car poly))
;;     (foreach b (append (cdr poly) (list (car poly))) ... (setq a b))
;;
;;   前中後走訪 (a=p[i-1], b=p[i], c=p[i+1]，皆 mod n)：
;;     (setq a (last poly) b (car poly))
;;     (foreach c (append (cdr poly) (list (car poly))) ... (setq a b b c))
;;
;; `append` 只做一次、O(n)；`last` 也只做一次。整體 O(n²) → O(n)。

;; 有向面積（正 = 逆時針）
(defun FLR:SignedArea (poly / a s)
  (if (< (length poly) 3)
    0.0
    (progn
      (setq s 0.0 a (car poly))
      (foreach b (append (cdr poly) (list (car poly)))
        (setq s (+ s (- (* (car a) (cadr b)) (* (car b) (cadr a))))
              a b))
      (/ s 2.0))))

(defun FLR:Area (poly) (abs (FLR:SignedArea poly)))

;; 邊界框 → (xmin ymin xmax ymax)
(defun FLR:BBox (poly / x1 y1 x2 y2)
  (if (null poly)
    nil
    (progn
      (setq x1 (caar poly) y1 (cadar poly) x2 x1 y2 y1)
      (foreach p poly
        (setq x1 (min x1 (car p)) y1 (min y1 (cadr p))
              x2 (max x2 (car p)) y2 (max y2 (cadr p))))
      (list x1 y1 x2 y2))))

;; 【0.8】四個欄位改用 car/cadr/caddr/cadddr 直取，不走 nth。
;; 這支被「每一格 × 每一區」呼叫（FLR:ClipTileByRegions、FLR:CellHome），
;; 8 次 nth 換成 8 次單層存取，在這個呼叫次數下量得出來。
(defun FLR:BBoxHit (b1 b2)
  (and b1 b2
       (< (car b1) (caddr b2)) (> (caddr b1) (car b2))
       (< (cadr b1) (cadddr b2)) (> (cadddr b1) (cadr b2))))

;; 點在多邊形內（ray casting）
;; 註：AutoLISP 的 /= 只吃數值與字串，不能拿來比 T/nil，故用 cond 明寫互斥。
(defun FLR:PtInPoly (pt poly / a inside px py ca cb den xx)
  (if (null poly)
    nil
    (progn
      (setq inside nil px (car pt) py (cadr pt) a (car poly))
      (foreach b (append (cdr poly) (list (car poly)))
        (setq ca (> (cadr a) py)
              cb (> (cadr b) py))
        (if (or (and ca (not cb)) (and cb (not ca)))
          (progn
            (setq den (- (cadr b) (cadr a)))
            (if (not (equal den 0.0 FLR:EPS))
              (progn
                (setq xx (+ (car a) (/ (* (- (car b) (car a)) (- py (cadr a))) den)))
                (if (< px xx) (setq inside (not inside)))))))
        (setq a b))
      inside)))

;; 兩線段是否「真的」相交（端點接觸不算）——共邊的相鄰區域不該被判為重疊
(defun FLR:SegX (p1 p2 p3 p4 / d1 d2 d3 d4)
  (setq d1 (FLR:X3 p3 p4 p1) d2 (FLR:X3 p3 p4 p2)
        d3 (FLR:X3 p1 p2 p3) d4 (FLR:X3 p1 p2 p4))
  (and (< (* d1 d2) 0.0) (< (* d3 d4) 0.0)))

(defun FLR:X3 (a b c)
  (- (* (- (car b) (car a)) (- (cadr c) (cadr a)))
     (* (- (cadr b) (cadr a)) (- (car c) (car a)))))

;; 旋轉輔助（供 45° 斜鋪：把區域轉進「磚座標系」，鋪完再把碎片轉回來）
(defun FLR:RotPt (p ca sa pv)
  (list (+ (car pv)  (- (* ca (- (car p)  (car pv))) (* sa (- (cadr p) (cadr pv)))))
        (+ (cadr pv) (+ (* sa (- (car p)  (car pv))) (* ca (- (cadr p) (cadr pv)))))))

(defun FLR:RotPoly (poly ang pv / ca sa)
  (if (equal ang 0.0 1e-12)
    poly
    (progn (setq ca (cos ang) sa (sin ang))
           (mapcar '(lambda (p) (FLR:RotPt p ca sa pv)) poly))))

;;; ================================================================
;;;  二、Sutherland-Hodgman 矩形裁剪
;;; ================================================================
;; 裁剪視窗必為凸形（磚一律是矩形），主體可為任意凹多邊形。
;; 注意：S-H 無法產生分離片。分離情形由「逐區域各裁一次」處理，
;;       見 FLR:ClipTileByRegions。

;; 【0.8】除了改成邊走訪，另外把 (nth axis a) 從一輪三次降到零次：
;; 上一輪的 bv 就是這一輪的 av（b 會變成下一輪的 a），沿著走就好。
;; 這是整個核心最熱的一支——每組候選 × 每一格 × 每一打中的區 × 四個半平面。
(defun FLR:Clip1 (poly axis dir val / out a b ia ib den tt px py av bv)
  (if (null poly)
    '()
    (progn
      (setq out '() a (car poly) av (nth axis a))
      (foreach b (append (cdr poly) (list (car poly)))
        (setq bv  (nth axis b)
              ia  (>= (* dir (- av val)) (- FLR:EPS))
              ib  (>= (* dir (- bv val)) (- FLR:EPS))
              den (- bv av))
        (setq tt (if (equal den 0.0 FLR:EPS) 0.0 (/ (- val av) den)))
        (setq px (+ (car a)  (* tt (- (car b)  (car a))))
              py (+ (cadr a) (* tt (- (cadr b) (cadr a)))))
        (cond
          ((and ia ib)       (setq out (cons b out)))
          ((and ia (not ib)) (setq out (cons (list px py) out)))
          ((and (not ia) ib) (setq out (cons b (cons (list px py) out)))))
        (setq a b av bv))
      (reverse out))))

(defun FLR:ClipRect (poly x1 y1 x2 y2 / p)
  (setq p poly)
  (if p (setq p (FLR:Clip1 p 0  1 x1)))
  (if p (setq p (FLR:Clip1 p 0 -1 x2)))
  (if p (setq p (FLR:Clip1 p 1  1 y1)))
  (if p (setq p (FLR:Clip1 p 1 -1 y2)))
  (if (and p (>= (length p) 3) (> (FLR:Area p) FLR:EPS)) p nil))

;; 移除重合與共線頂點（S-H 會產出這些）
;;
;; 【必須分兩步，不可合併】2026-08-12 實機抓到：
;;   舊版對每個點同時檢查「與前鄰重合」和「與後鄰重合」，
;;   一組連續重複點 P,P 會被**兩份都刪掉**——真正的角點就此消失，
;;   多邊形短路成對角線，畫面上出現三角形。
;;   實測：((0 0)(10 0)(10 0)(10 10)(0 10)) → 只剩 3 點、面積 100 變 50。
;;   而 S-H 在「邊的端點落在裁切線上」時 routinely 產生重複點，
;;   區域邊界與磚格線重合處必踩。
;;
;; 註：叉積量綱是長度平方，不能直接跟長度容差比。
;;     除以 |ab| 換算成「c 偏離直線 ab 的垂距」才是同量綱。
(defun FLR:Clean (poly tol / out a b c cr dab keep)
  (if (< (length poly) 3)
    poly
    (progn
      ;; ---- 第一步：連續重複點每組只留一個（含頭尾相接）----
      (setq out '() b (car poly))
      (foreach c (append (cdr poly) (list (car poly)))
        (if (> (distance b c) tol) (setq out (cons b out)))
        (setq b c))
      (setq poly (reverse out))
      ;; ---- 第二步：去除共線點 ----
      (if (< (length poly) 3)
        poly
        (progn
          (setq out '() a (last poly) b (car poly))
          (foreach c (append (cdr poly) (list (car poly)))
            (setq dab (distance a b)
                  keep T)
            (if (< dab tol)
              (setq keep nil)
              (progn
                (setq cr (- (* (- (car b) (car a)) (- (cadr c) (cadr b)))
                            (* (- (cadr b) (cadr a)) (- (car c) (car b)))))
                (if (< (abs (/ cr dab)) tol) (setq keep nil))))
            (if keep (setq out (cons b out)))
            (setq a b b c))
          (reverse out))))))

;;; ================================================================
;;;  三、形狀判定：凹頂點數 → 凸 / L 形 / U 形
;;; ================================================================
;; 回傳凹頂點數。0 = 凸形；1 = L 形角磚（允許）；>=2 = U 形／凹槽（不允許）

(defun FLR:ReflexCount (poly tol / a b c cr dab sgn cnt)
  (setq poly (FLR:Clean poly tol))
  (if (< (length poly) 4)
    0
    (progn
      (setq sgn (if (>= (FLR:SignedArea poly) 0.0) 1.0 -1.0) cnt 0
            a (last poly) b (car poly))
      (foreach c (append (cdr poly) (list (car poly)))
        (setq cr  (- (* (- (car b) (car a)) (- (cadr c) (cadr b)))
                     (* (- (cadr b) (cadr a)) (- (car c) (car b))))
              dab (distance a b))
        ;; 同 FLR:Clean，除以 |ab| 換算成垂距再比容差
        (if (< (* sgn (/ cr dab)) (- tol)) (setq cnt (1+ cnt)))
        (setq a b b c))
      cnt)))

;;; ================================================================
;;;  四、區域重疊偵測（依裁決：偵測到只警告，不自動聯集）
;;; ================================================================
;; 回傳有實質面積重疊的區域索引對清單 ((i j) ...)
;;
;; 【0.2 修正 FLR-04】舊版只做「ri 的某頂點落在 rj 內」這個**單向**測試，
;;   於是兩種很常見的重疊完全測不到（2026-08-12 實測皆回 nil）：
;;     ① rj 整個被 ri 包住（房間裡再框一小塊）→ ri 的角點全在 rj 外
;;     ② 兩條帶狀區域十字交叉            → 雙方頂點都在對方之外
;;   正解是「雙向頂點測試 ∪ 邊相交測試」。
;;   邊相交用嚴格不等式（端點接觸不算），共邊的相鄰房間才不會被誤報。

(defun FLR:AnyVertexIn (pa pb)
  (vl-some '(lambda (p) (FLR:PtInPoly p pb)) pa))

;; 【0.8】改成邊走訪。原本的 while 命中後就跳出，foreach 沒有這個能力，
;; 改成命中後跳過內容——多的只是空轉的迴圈開銷，而本函式只在指令開頭
;; 對每組區域跑一次（FLR:FindOverlaps），不在最佳化的熱路徑上。
(defun FLR:EdgeCross (pa pb / a1 b1 lb hit)
  (if (or (null pa) (null pb))
    nil
    (progn
      (setq hit nil a1 (car pa) lb (append (cdr pb) (list (car pb))))
      (foreach a2 (append (cdr pa) (list (car pa)))
        (if (not hit)
          (progn
            (setq b1 (car pb))
            (foreach b2 lb
              (if (and (not hit) (FLR:SegX a1 a2 b1 b2)) (setq hit T))
              (setq b1 b2))))
        (setq a1 a2))
      hit)))

(defun FLR:FindOverlaps (regions tol / n i j ri rj res)
  (setq res '() n (length regions) i 0)
  (while (< i n)
    (setq j (1+ i))
    (while (< j n)
      (setq ri (nth i regions) rj (nth j regions))
      (if (and (FLR:BBoxHit (FLR:BBox ri) (FLR:BBox rj))
               (or (FLR:AnyVertexIn ri rj)
                   (FLR:AnyVertexIn rj ri)
                   (FLR:EdgeCross ri rj)))
        (setq res (cons (list i j) res)))
      (setq j (1+ j)))
    (setq i (1+ i)))
  (reverse res))

;;; ================================================================
;;;  五、網格產生（正鋪 / 交丁）
;;; ================================================================
;; cfg 為關聯表，欄位：
;;   tw th   磚寬 / 磚高（圖面單位）
;;   gap     填縫寬
;;   stagger 交丁比例 0.0=正鋪  0.5=1/2 交丁  0.3333=1/3 交丁
;;   ox oy   起鋪偏移
;; 回傳磚格清單 ((x1 y1 x2 y2 row col) ...)，涵蓋 bbox 範圍

(defun FLR:Cfg (key cfg / v) (setq v (assoc key cfg)) (if v (cdr v) nil))

(defun FLR:MakeGrid (cfg bbox / tw th gap sx sy stag ox oy
                                x1 y1 x2 y2 r cmin cmax rmin rmax
                                rowoff cx cy cells c)
  (setq tw   (FLR:Cfg 'tw cfg)      th  (FLR:Cfg 'th cfg)
        gap  (FLR:Cfg 'gap cfg)     stag (FLR:Cfg 'stagger cfg)
        ox   (FLR:Cfg 'ox cfg)      oy  (FLR:Cfg 'oy cfg)
        sx   (+ tw gap)             sy  (+ th gap)
        x1   (nth 0 bbox) y1 (nth 1 bbox) x2 (nth 2 bbox) y2 (nth 3 bbox))
  (setq rmin (FLR:Floor (/ (- y1 oy) sy))
        rmax (FLR:Ceil  (/ (- y2 oy) sy))
        cells '() r rmin)
  (while (<= r rmax)
    ;; 交丁：第 r 列的 x 偏移 = (r * stagger * sx) mod sx
    (setq rowoff (* (FLR:Frac (* r stag)) sx))
    (setq cmin (FLR:Floor (/ (- x1 ox rowoff) sx))
          cmax (FLR:Ceil  (/ (- x2 ox rowoff) sx))
          c cmin)
    (while (<= c cmax)
      (setq cx (+ ox rowoff (* c sx))
            cy (+ oy (* r sy)))
      (if (and (< cx x2) (> (+ cx tw) x1) (< cy y2) (> (+ cy th) y1))
        (setq cells (cons (list cx cy (+ cx tw) (+ cy th) r c) cells)))
      (setq c (1+ c)))
    (setq r (1+ r)))
  (reverse cells))

(defun FLR:Floor (x)
  (if (>= x 0.0) (fix x)
    (if (equal (float (fix x)) (float x) FLR:EPS) (fix x) (fix (- x 1.0)))))

(defun FLR:Ceil (x)
  (if (<= x 0.0) (fix x)
    (if (equal (float (fix x)) (float x) FLR:EPS) (fix x) (fix (+ x 1.0)))))

;; 小數部分。抽出來是因為交丁比例填 0.3333 時 r=3 會算出 0.9999，
;; 落在「幾乎是 1 但不是 1」→ 該列偏移變成整個 sx 而不是 0，逐列累積漂移。
;; 這裡把「差 1 在 1e-9 內」直接歸零，1/3 交丁才會每 3 列準確歸位。
(defun FLR:Frac (x / f)
  (setq f (- x (FLR:Floor x)))
  (if (or (< f FLR:EPS) (> f (- 1.0 FLR:EPS))) 0.0 f))

;; 使用者是用小數填交丁比例的（1/3 會打成 0.3333）。小數與真分數差一點點，
;; 第 3 列的偏移就變成 0.9999×間距 而不是 0，逐列累積漂移
;; （實測 0.3333 時 row3 的 x = -0.003），並製造出成對的假尺寸（15.00 與 15.003）。
;; 這裡把「接近簡單分數」的輸入吸附回真分數；差太多就照原值，不擅自更動使用者的意圖。
(defun FLR:SnapStag (v / best)
  (setq best v)
  (foreach f '(0.0 0.5 0.25 0.75 0.2 0.4 0.6 0.8)
    (if (< (abs (- v f)) 0.002) (setq best f)))
  (foreach n '(1.0 2.0)
    (if (< (abs (- v (/ n 3.0))) 0.002) (setq best (/ n 3.0))))
  (foreach n '(1.0 5.0)
    (if (< (abs (- v (/ n 6.0))) 0.002) (setq best (/ n 6.0))))
  best)

;;; ================================================================
;;;  六、單片磚對多區域裁切
;;; ================================================================
;; 回傳 ((regionIndex piece area reflex) ...)
;; 逐區域各裁一次 —— 牆完全穿過磚時，兩側各成一片，天然分離。

(defun FLR:ClipTileByRegions (cell regions rbboxes tol / x1 y1 x2 y2 i res p cb ar bl)
  (setq x1 (car cell) y1 (cadr cell) x2 (caddr cell) y2 (cadddr cell)
        cb (list x1 y1 x2 y2) res '() i 0 bl rbboxes)
  ;; 【0.8】bbox 與區域兩份清單並行走訪，不用 (nth i rbboxes)。
  ;; i 仍要留著——它是回傳值裡的區域索引。
  (foreach rg regions
    (if (FLR:BBoxHit cb (car bl))
      (progn
        (setq p (FLR:ClipRect rg x1 y1 x2 y2))
        (if p
          (progn
            (setq p (FLR:Clean p tol))
            ;; 面積算一次就好——這裡是每組候選 × 每一格 × 每一區都會走到的位置
            (if (and p (>= (length p) 3) (> (setq ar (FLR:Area p)) tol))
              (setq res (cons (list i p ar (FLR:ReflexCount p tol)) res)))))))
    (setq i (1+ i) bl (cdr bl)))
  (reverse res))

;;; ---- 【1.4】內部包含判定：完全落在區域內部的格子連裁剪都不必做 ----
;;
;; 實測（Tools\FLR_CostProbe.ps1，A1001）：882 格裡 614 格是整磚，
;; 而逐格裁剪佔每組候選成本的一半，其中 67% 花在這些整磚身上。
;; 整磚跳過**量測**不需要新判定（isfull 本來就算得出來）；要連**裁剪**也跳過，
;; 就得在裁剪之前先回答「這一格是不是整片都在區域裡」。
;;
;; 【怎麼答得快】把軸向區域先分解成互不重疊的矩形（垂直板條），
;; 格子塞得進其中任何一塊，就一定完全在區域內部。
;; 這是**充分不必要**條件：跨在兩塊板條之間的格子答不出來，退回逐格裁剪即可
;; ——寧可少省一點，也不要答錯。答錯的代價是整磚被當成裁切磚（或反過來），
;; 而那不會有任何錯誤訊息。
;;
;; 【為什麼只做軸向區域】斜邊在板條內的跨距會變化，取中點取樣得到的矩形是錯的
;; （會超出區域）。有斜邊的區域一律回 nil＝沒有快路徑，照舊逐格裁剪。
;; A1001 的 7 個區域裡有 1 個含斜邊，那一個就走慢路徑。

;; 軸向多邊形 → 矩形分解。非軸向回 nil。
(defun FLR:SlabRects (pc atol tol / xs out a xm ys yr)
  (if (not (FLR:IsRect (list pc) atol))
    nil
    (progn
      (setq xs '())
      (foreach p pc
        (if (not (vl-some '(lambda (u) (equal u (car p) atol)) xs))
          (setq xs (cons (car p) xs))))
      (setq xs (vl-sort xs '<) out '() a (car xs))
      (foreach b (cdr xs)
        (if (> (- b a) atol)
          (progn
            ;; 板條內部取一條垂直掃描線，交點成對即為該板條的 y 區間
            (setq xm (/ (+ a b) 2.0) ys (FLR:Cross pc 0 xm tol) yr ys)
            (while (cdr yr)
              (if (> (- (cadr yr) (car yr)) tol)
                (setq out (cons (list a (car yr) b (cadr yr)) out)))
              (setq yr (cddr yr)))))
        (setq a b))
      (reverse out))))

;; 每個區域各一份矩形分解（含斜邊者為 nil）
(defun FLR:RegionRects (regions cfg / atol tol)
  (setq atol (FLR:AxisTol cfg) tol (FLR:Cfg 'tol cfg))
  (if (null tol) (setq tol 1e-6))
  (mapcar '(lambda (rg) (FLR:SlabRects rg atol tol)) regions))

;; 把分解結果放進 cfg（已經有就不重算）。
;; 由 FLR:Evaluate／FLR:Layout／FLR:Optimize 呼叫，ClassifyTile 只讀不算
;; ——分解與起鋪點無關，每組候選重算一次是白花的。
(defun FLR:WithRects (cfg regions)
  (if (FLR:Cfg 'rrects cfg)
    cfg
    (append cfg (list (cons 'rrects (FLR:RegionRects regions cfg))))))

;; 這一格完全落在哪一個區域內部？回傳區域索引，答不出來回 nil。
;; 三個條件缺一不可：塞得進某區的某塊矩形、不碰其他區域、不碰扣除物。
;;   * 其他區域：區域重疊時該格會被裁成兩片，不是整磚（重疊只警告不阻擋，見 README）
;;   * 扣除物：碰到就要挖，當然不是整磚
;; 後兩者只用 bbox 粗篩——**可能碰到**就放棄，不細算。
;; 【順序就是成本】bbox 先篩，**只對真的打中的那一區**做包含測試。
;; 初版是反過來寫的（先掃所有區域的矩形分解，再回頭做一輪 bbox），
;; 實測每組 182 → 224 ms——判定自己多跑的那一輪，比它省下來的裁剪還貴。
;; 現在一趟迴圈同時做完「打中幾區」與「有沒有被包住」，
;; 而且 bbox 測試直接寫開不呼叫 FLR:BBoxHit：每一格 × 每一區都會走到，
;; AutoLISP 的函式呼叫成本在這裡是看得見的。
;; 【0.8】bbox 清單並行走訪（不用 nth i rbboxes），四欄一律 car/cadr/caddr/cadddr。
(defun FLR:CellHome (cell rrects rbboxes deds tol / i home hits cx1 cy1 cx2 cy2 bb ok bl)
  (setq cx1 (car cell) cy1 (cadr cell) cx2 (caddr cell) cy2 (cadddr cell)
        i 0 home nil hits 0 bl rbboxes)
  (foreach rs rrects
    (setq bb (car bl) bl (cdr bl))
    (if (and (< cx1 (caddr bb)) (> cx2 (car bb))
             (< cy1 (cadddr bb)) (> cy2 (cadr bb)))
      (progn
        (setq hits (1+ hits))
        (if (and (null home) rs)
          (progn
            (setq ok nil)
            (foreach r rs
              (if (and (not ok)
                       (>= cx1 (- (car r) tol)) (>= cy1 (- (cadr r) tol))
                       (<= cx2 (+ (caddr r) tol)) (<= cy2 (+ (cadddr r) tol)))
                (setq ok T)))
            (if ok (setq home i))))))
    (setq i (1+ i)))
  ;; 只碰到這一區、被它包住、而且沒有扣除物插進來，才算數。
  ;; 區域重疊時該格會被裁成兩片（重疊只警告不阻擋，見 README），不是整磚。
  (if (and home (= hits 1))
    (progn
      (setq ok T)
      (foreach d (if deds deds '())
        (if (and (< cx1 (caddr d)) (> cx2 (car d))
                 (< cy1 (cadddr d)) (> cy2 (cadr d)))
          (setq ok nil)))
      (if ok home nil))
    nil))

;;; ================================================================
;;;  七、條帶分解 —— 裁切尺寸與最小腳寬的唯一量測來源
;;; ================================================================
;;
;; 【0.2 取代舊的 FLR:CutLines，修 FLR-01 / FLR-02】
;;
;; 舊版有兩個獨立的錯：
;;   ① 下限檢查用**碎片 bbox 的寬高**。L 形角磚的 bbox 仍是滿格 30×30，
;;      於是兩腳只有 2 cm 的 L 磚 bad=nil，完全漏檢。
;;   ② 裁切尺寸用**碎片 bbox 的中心**決定量哪一側。L 形的 bbox 中心
;;      落在被挖掉的那個角裡，每條切割線都判到反側 → 回報 28，實際是 2。
;;   矩形碎片兩者都對（bbox 中心必在碎片內），**只有 L 形錯**。
;;
;; 正解：沿一軸把碎片切成條帶，每條條帶就是一個矩形，寬與長都是師傅要量的尺寸。
;;
;; 條帶長度**必須用「條帶內取樣線與碎片邊界的交點」求，不可用 bbox**——
;; S-H 會在裁切線上留下零寬贅邊，bbox 會被那條贅邊撐大而失真
;; （實測：L 形右側條帶的真實高度 2，bbox 卻回 30）。
;;
;; 每條條帶取三處（2%／50%／98%）取最小值：正交碎片三處必然相同，
;; 斜向碎片則能抓到往尖端收窄的那一頭，不會漏掉尖角細料。

;; 碎片邊界與「第 axis 軸座標 = v」這條線的交點，回傳另一軸座標的排序清單
;; 【0.8】邊走訪，且沿用上一輪的 bv 當這一輪的 av（同 FLR:Clip1）。
;; 每條條帶會呼叫三次（2%／50%／98% 取樣），是量測段最內層的迴圈。
(defun FLR:Cross (pc axis v tol / a b oth out d tt av bv)
  (if (null pc)
    '()
    (progn
      (setq out '() oth (- 1 axis) a (car pc) av (nth axis a))
      (foreach b (append (cdr pc) (list (car pc)))
        (setq bv (nth axis b)
              d  (- bv av))
        (if (> (abs d) tol)
          (progn
            (setq tt (/ (- v av) d))
            (if (and (> tt (- tol)) (< tt (+ 1.0 tol)))
              (setq out (cons (+ (nth oth a) (* tt (- (nth oth b) (nth oth a)))) out)))))
        (setq a b av bv))
      (vl-sort out '<))))

;; 某取樣位置上，碎片沿另一軸佔據的最短區間長度
;; 【0.8】交點成對取用，改成 cddr 走訪。
;; 舊寫法除了 nth 之外還在**迴圈條件裡**每輪重算一次 (length ys)。
(defun FLR:SpanAt (pc axis v tol / ys best d)
  (setq ys (FLR:Cross pc axis v tol) best nil)
  (while (cdr ys)
    (setq d (- (cadr ys) (car ys)))
    (if (> d tol) (if (or (null best) (< d best)) (setq best d)))
    (setq ys (cddr ys)))
  best)

;; 沿 axis 把碎片切成條帶 → ((條帶寬 條帶長) ...)
;;   axis 0：條帶寬沿 x、條帶長沿 y
;;   axis 1：條帶寬沿 y、條帶長沿 x
;; 【0.8】兩處：去重時 (nth axis p) 一個點只取一次；相鄰值改成成對走訪。
(defun FLR:Strips (pc axis tol / vs out a s best pv)
  (setq vs '())
  (foreach p pc
    (setq pv (nth axis p))
    (if (not (vl-some '(lambda (u) (equal u pv tol)) vs))
      (setq vs (cons pv vs))))
  (setq vs (vl-sort vs '<) out '() a (car vs))
  (foreach b (cdr vs)
    (if (> (- b a) tol)
      (progn
        (setq best nil)
        (foreach f '(0.02 0.5 0.98)
          (setq s (FLR:SpanAt pc axis (+ a (* f (- b a))) tol))
          (if (and s (or (null best) (< s best))) (setq best s)))
        (if best (setq out (cons (list (- b a) best) out)))))
    (setq a b))
  (reverse out))

;; 最小腳寬（供顯示與診斷）
(defun FLR:MinLeg (pc tol / m)
  (setq m nil)
  (foreach s (append (FLR:Strips pc 0 tol) (FLR:Strips pc 1 tol))
    (setq m (if m (min m (car s) (cadr s)) (min (car s) (cadr s)))))
  (if m m 0.0))

;; 下限檢查。刻意保留「寬比磚寬、長比磚高」的量綱對應，
;; 長寬不同的磚（60×30）才不會用錯門檻。
;; 矩形碎片只有一條條帶，判定結果與 0.1 版的 bbox 寫法完全相同；
;; 差別只在 L 形現在會被抓出來。
;;
;; 【1.3.2】改由 FLR:LegScan 實作。判定條件一字未改（同樣是「任一條帶的任一維度
;; 低於該軸下限」），只是掃描的同時把「違規了幾次」與「最窄多少」一起帶回來
;; ——使用者問「一塊磁磚違規兩次也只算一次，能不能更詳細」，而片數答不了這個。
(defun FLR:BadLeg (pc tw th minr tol)
  (> (car (FLR:LegScan pc tw th minr tol)) 0))

;; 條帶掃描：一次走完就同時得到「違規邊數」與「最窄腳寬」。
;; 回傳 (違規邊數 最窄腳寬)。違規邊數 0 即等同舊的 bad=nil。
;;
;; **邊數的定義**：一條條帶的寬與高各檢查一次，各自低於下限就各算一次。
;; 所以一片 2×3 的角料（下限 15）算 2——那正是使用者說的「違規兩次」。
;;
;; 【為什麼兩個方向取較大值而不是相加】
;; 一個碎片有 x、y 兩種等價的條帶分解，矩形碎片兩種結果完全相同，相加會讓
;; 上面那片角料變成 4。取較大值同時保證「只要 bad 為真，邊數必 >= 1」，
;; 清單上不會出現「違規 1 片、邊數 0」這種自相矛盾的組合。
;; （與 FLR:StripCuts 只取單向分解、以免同一刀被列兩次，是同一個理由。）
;;
;; 最窄腳寬取**兩個方向所有條帶的兩個維度**的最小值，與 FLR:MinLeg 同定義；
;; 整磚會回磚寬，所以全案最小值必然落在最窄的那片裁切磚身上。
;;
;; 【1.4.2】拆成「吃現成 x 向條帶」的 FLR:LegScanS ＋ 保留原簽章的包裝。
;; 兩個理由，都在最佳化的熱路徑上（每組候選 × 每一格 × 每一碎片）：
;;
;;   ① 呼叫端（FLR:ClassifyTile）本來就要一份 x 向條帶給裁切尺寸與排料歸類用，
;;      而舊版 LegScan 在內部又自己算了一次**同參數**的 FLR:Strips
;;      ——同一片碎片的 x 向分解一輪跑兩次。
;;   ② **軸向矩形碎片的 y 向分解是可證明冗餘的**：矩形的 axis-0 條帶必為 (w h)、
;;      axis-1 必為 (h w)，而兩軸的門檻比對剛好對調——
;;        axis-0：(car s)=w 比 tw·minr、(cadr s)=h 比 th·minr
;;        axis-1：(car s)=h 比 th·minr、(cadr s)=w 比 tw·minr
;;      逐項相同 ⇒ c1 ≡ c0、mn 相同、(max c0 c1) = c0。整輪 y 向分解可以整段跳過。
;;      裁切磚絕大多數是被軸向牆切出來的矩形，所以這條快路徑命中率很高。
;;
;; 【快路徑的兩個前提缺一不可，都不能放寬】
;;   * 4 頂點且每條邊軸向，容差用**與 FLR:Strips 同一個 tol**，不是 FLR:AxisTol。
;;     用寬容差會答錯：頂邊差 6.79e-5 的那種繪圖誤差（見 FLR:AxisTol 的註）在
;;     axis-1 會多出一條 6.79e-5 寬的贅條帶，而它低於下限＝多算一次違規邊。
;;     那是現行行為，快路徑不可以把它變不見。
;;   * x 向條帶**恰好一條**。這條同時排除「寬度 ≤ tol 而 x 值被 Strips 併掉」的
;;     退化碎片——那種形狀 axis-0 一條都沒有、axis-1 卻有，兩軸不對等。
;; 兩個前提任一不成立就走原路（兩軸都分解），寧可少省一點，不要答錯。
;; 一致性由 FLR_Tests §7c 逐格比對鎖住（不是抽樣）。

;; 碎片是不是「4 頂點的軸向矩形」
(defun FLR:RectFrag (pc tol / a ok)
  (if (/= (length pc) 4)
    nil
    (progn
      (setq ok T a (car pc))
      (foreach b (append (cdr pc) (list (car pc)))
        (if (and (> (abs (- (car a)  (car b)))  tol)
                 (> (abs (- (cadr a) (cadr b))) tol))
          (setq ok nil))
        (setq a b))
      ok)))

(defun FLR:LegScanS (pc st0 tw th minr tol / c0 c1 mn)
  (setq c0 0 c1 0 mn nil)
  (foreach s st0
    (if (< (car s)  (- (* tw minr) tol)) (setq c0 (1+ c0)))
    (if (< (cadr s) (- (* th minr) tol)) (setq c0 (1+ c0)))
    (setq mn (if mn (min mn (car s) (cadr s)) (min (car s) (cadr s)))))
  (if (and (= (length st0) 1) (FLR:RectFrag pc tol))
    (list c0 (if mn mn 0.0))                     ; 矩形：c1 ≡ c0，y 向免了
    (progn
      (foreach s (FLR:Strips pc 1 tol)
        (if (< (car s)  (- (* th minr) tol)) (setq c1 (1+ c1)))
        (if (< (cadr s) (- (* tw minr) tol)) (setq c1 (1+ c1)))
        (setq mn (if mn (min mn (car s) (cadr s)) (min (car s) (cadr s)))))
      (list (max c0 c1) (if mn mn 0.0)))))

(defun FLR:LegScan (pc tw th minr tol)
  (FLR:LegScanS pc (FLR:Strips pc 0 tol) tw th minr tol))

;; 本碎片產生的裁切尺寸。
;; 只取 x 向條帶（兩個維度都收）——一個碎片有 x、y 兩種等價的分解方式，
;; 兩種都收會把同一刀重複列成兩個尺寸。矩形碎片兩種分解結果相同，
;; 故此選擇對常見情形沒有影響。
;;
;; 【1.4】拆成「吃現成條帶」的 FLR:StripCutsOf ＋ 保留原簽章的包裝。
;; 理由是排料分類（FLR:NestClass）要的正是同一組 x 向條帶：
;; 呼叫端算一次 FLR:Strips 就能同時取得裁切尺寸與排料歸類，
;; 而 FLR:Strips 是最佳化熱路徑上最貴的一段（每組候選 × 每一格 × 每一碎片）。
(defun FLR:StripCutsOf (st tw th tol / out)
  (setq out '())
  (foreach s st
    (if (< (car s)  (- tw tol)) (setq out (cons (car s) out)))
    (if (< (cadr s) (- th tol)) (setq out (cons (cadr s) out))))
  out)

(defun FLR:StripCuts (pc tw th tol)
  (FLR:StripCutsOf (FLR:Strips pc 0 tol) tw th tol))

;; 本碎片在「邊料互補排料」裡吃掉什麼。分類邏輯與 FLR:Nest 逐條對應
;; （見第十二節的說明），兩者的一致性由 FLR_Tests §10d 的斷言鎖住。
;;   'lwhole  → 各吃一整片母磚（多條帶的 L 形，或兩向都滿）
;;   (0 . w)  → 併入「寬度」那組一維排料（全高條與角料）
;;   (1 . h)  → 併入「高度」那組（全寬條）
(defun FLR:NestClass (st tw th tol / w h)
  (if (/= (length st) 1)
    'lwhole
    (progn
      (setq w (car (car st)) h (cadr (car st)))
      (cond
        ((and (>= w (- tw tol)) (>= h (- th tol))) 'lwhole)
        ((>= w (- tw tol)) (cons 1 h))
        (T                 (cons 0 w))))))

;;; ================================================================
;;;  七之二、扣除物（柱／機坑／管道間／樓梯）
;;; ================================================================
;; 扣除物一律化簡為軸向矩形 (x1 y1 x2 y2)。非矩形者由 UI 層改用其 bbox
;; 並提出警告（保守多扣），見 FLR_FloorTile.lsp。
;;
;; 扣除方式刻意分成兩步，不可合併：
;;   ① 形狀分類（FLR:RectCut）——判定是否觸發 U 形禁則。
;;      必須在「分解成碎片之前」做，否則 U 形會被拆成兩個矩形而漏判。
;;   ② 幾何分解（FLR:SubtractRect）——把碎片切成互不重疊的矩形帶，
;;      供繪圖**與面積加總**（0.2 起面積也走這條，見 FLR-05）。

;; 矩形 r 相對於碎片 bbox 的切割型態
;;   'split  完全橫貫或縱貫 → 乾淨切開（允許，兩片各自驗 1/4）
;;   'corner 吃掉一個角     → L 形（允許）
;;   'notch  只碰一邊       → U 形（不允許）
;;   'hole   落在正中央     → 中央開洞（不允許）
(defun FLR:RectCut (pbb r tol / px1 py1 px2 py2 rx1 ry1 rx2 ry2
                                spanx spany tchx tchy)
  (setq px1 (nth 0 pbb) py1 (nth 1 pbb) px2 (nth 2 pbb) py2 (nth 3 pbb)
        rx1 (nth 0 r)   ry1 (nth 1 r)   rx2 (nth 2 r)   ry2 (nth 3 r))
  (setq spanx (and (<= rx1 (+ px1 tol)) (>= rx2 (- px2 tol)))
        spany (and (<= ry1 (+ py1 tol)) (>= ry2 (- py2 tol)))
        tchx  (or  (<= rx1 (+ px1 tol)) (>= rx2 (- px2 tol)))
        tchy  (or  (<= ry1 (+ py1 tol)) (>= ry2 (- py2 tol))))
  (cond ((or spanx spany) 'split)
        ((and tchx tchy)  'corner)
        ((or  tchx tchy)  'notch)
        (T                'hole)))

;; 從多邊形扣除一個軸向矩形 → 0~4 個互不重疊的碎片
(defun FLR:SubtractRect (poly r tol / bb out f x1 y1 x2 y2 rx1 ry1 rx2 ry2 mx1 mx2)
  (setq bb (FLR:BBox poly)
        x1 (nth 0 bb) y1 (nth 1 bb) x2 (nth 2 bb) y2 (nth 3 bb)
        rx1 (nth 0 r) ry1 (nth 1 r) rx2 (nth 2 r) ry2 (nth 3 r))
  (if (not (FLR:BBoxHit bb r))
    (list poly)
    (progn
      (setq out '() mx1 (max rx1 x1) mx2 (min rx2 x2))
      (if (> rx1 (+ x1 tol))                      ; 左條
        (if (setq f (FLR:ClipRect poly x1 y1 rx1 y2)) (setq out (cons f out))))
      (if (< rx2 (- x2 tol))                      ; 右條
        (if (setq f (FLR:ClipRect poly rx2 y1 x2 y2)) (setq out (cons f out))))
      (if (and (> ry1 (+ y1 tol)) (> mx2 (+ mx1 tol)))   ; 中下
        (if (setq f (FLR:ClipRect poly mx1 y1 mx2 ry1)) (setq out (cons f out))))
      (if (and (< ry2 (- y2 tol)) (> mx2 (+ mx1 tol)))   ; 中上
        (if (setq f (FLR:ClipRect poly mx1 ry2 mx2 y2)) (setq out (cons f out))))
      out)))

;; 對一組碎片依序扣除多個矩形。輸出恆為互不重疊的碎片，
;; 這正是 0.2 起改由碎片加總求面積的依據。
(defun FLR:SubtractRects (polys rects tol / cur nxt)
  (setq cur polys)
  (foreach r rects
    (setq nxt '())
    (foreach p cur (setq nxt (append nxt (FLR:SubtractRect p r tol))))
    (setq cur nxt))
  cur)

;;; ================================================================
;;;  八、單片磚分類
;;; ================================================================
;; 回傳關聯表：
;;   kind   'full | 'cut | 'ushape | 'none
;;   pieces 片數（觸及的相異區域數）
;;   area   淨面積
;;   cuts   本片產生的裁切尺寸清單
;;   bad    T = 有任一條條帶低於下限
;;   badlegs 違規邊數（1.3.2；寬與高各算一次，見 FLR:LegScan）
;;   minleg  本片最窄腳寬（1.3.2；np=0 時為 nil）
;;   parts  ((regionIdx 多邊形 面積) ...) 供預覽與繪圖直接取用

(defun FLR:ClassifyTile (cell regions rbboxes cfg / tw th tol minr parts np
                              area cuts bad ushape pc full1 isfull
                              x1 y1 x2 y2 nlegs mleg ls st nst
                              deds frags pbb hits ov ct dedhit fp home fa)
  (setq tw  (FLR:Cfg 'tw cfg)  th (FLR:Cfg 'th cfg)
        tol (FLR:Cfg 'tol cfg) minr (FLR:Cfg 'mincut cfg)
        x1 (nth 0 cell) y1 (nth 1 cell) x2 (nth 2 cell) y2 (nth 3 cell)
        full1 (* tw th)
        ;; 【1.4 快路徑】cfg 帶著矩形分解時，先問「這一格是不是完全在內部」。
        ;; 答得出來就整個裁剪＋量測都免了。答不出來（含沒有分解時）一律走原路。
        home (if (FLR:Cfg 'rrects cfg)
               (FLR:CellHome cell (FLR:Cfg 'rrects cfg) rbboxes
                             (FLR:Cfg 'deds cfg) tol)
               nil))
  (if home
    ;; 完全在內部＝整磚。這裡的每一欄都必須與慢路徑逐欄相同
    ;; ——兩條路徑的一致性由 FLR_Tests §7b 的斷言鎖住（逐格比對，不是抽樣）。
    (list (cons 'kind 'full)
          (cons 'pieces 1)
          (cons 'area (* (- x2 x1) (- y2 y1)))
          (cons 'cuts '())
          (cons 'bad nil)
          (cons 'badlegs 0)
          (cons 'minleg (min tw th))
          (cons 'nest '())
          (cons 'dedhit nil)
          (cons 'parts (list (list home
                                   (list (list x1 y1) (list x2 y1)
                                         (list x2 y2) (list x1 y2))
                                   (* (- x2 x1) (- y2 y1))))))
  (progn
  (setq parts (FLR:ClipTileByRegions cell regions rbboxes tol)
        np (length parts) area 0.0 cuts '() bad nil ushape nil
        nlegs 0 mleg nil nst '())
  (cond
    ((= np 0) (list (cons 'kind 'none) (cons 'pieces 0) (cons 'area 0.0)
                    (cons 'cuts '()) (cons 'bad nil) (cons 'dedhit nil)
                    (cons 'badlegs 0) (cons 'minleg nil)
                    (cons 'nest '())
                    (cons 'parts '())))
    (T
     ;; ---- 扣除物：先分類（判 U 形禁則）再分解。順序不可顛倒 ----
     (setq deds (FLR:Cfg 'deds cfg) frags '())
     (foreach pt parts
       (setq pc (nth 1 pt) pbb (FLR:BBox pc) hits '())
       ;; U 形判定：單一連通片但凹頂點 >= 2
       (if (>= (nth 3 pt) 2) (setq ushape T))
       (foreach r (if deds deds '())
         (if (FLR:BBoxHit pbb r)
           (progn
             (setq ov (FLR:ClipRect pc (nth 0 r) (nth 1 r) (nth 2 r) (nth 3 r)))
             (if (and ov (> (FLR:Area ov) tol))
               (progn
                 (setq dedhit T
                       hits   (cons r hits)
                       ct     (FLR:RectCut pbb r tol))
                 ;; 只碰一邊＝凹槽、落在正中＝開洞，兩者皆為不允許的形狀
                 (if (or (= ct 'notch) (= ct 'hole)) (setq ushape T)))))))
       (foreach fp (if hits (FLR:SubtractRects (list pc) hits tol) (list pc))
         (if (and fp (>= (length fp) 3) (> (setq fa (FLR:Area fp)) tol))
           (setq frags (cons (list (nth 0 pt) fp fa) frags)))))
     (setq frags (reverse frags))

     ;; ---- 面積：由碎片加總 ----
     ;; 【0.2 修正 FLR-05】舊版是逐個扣除物 (- area (Area ov))，
     ;; 兩個互相重疊的扣除物會把重疊處扣兩次
     ;; （實測 900−400−400 = 100，正解 900−400−400+100 = 200）。
     ;; FLR:SubtractRects 的輸出本來就互不重疊，直接加總即是排容後的正解。
     (foreach f frags (setq area (+ area (nth 2 f))))

     ;; 整磚判定必須用「面積」，不能用 bbox——L 形角磚的 bbox 仍是滿格。
     ;; 【1.4 移到量測之前】原本算在量測迴圈之後，於是整磚也要跑完整套條帶分解
     ;; ——而整磚的量測結果是**已知常數**。實測 A1001（Tools\FLR_CostProbe.ps1）：
     ;; 882 格裡 614 格是整磚（69.6%），量測佔每組候選成本三分之二、
     ;; 其中 67% 花在這些整磚身上＝每組 302 ms 有 140 ms 在算常數。
     ;; 判定本身不必多花任何幾何：area、dedhit、np 在這個位置全都算完了。
     ;; 改完實測每組 302 → 182 ms。
     (setq isfull (and (= np 1) (not dedhit) (equal area full1 (* tol (+ tw th)))))

     ;; ---- 量測：條帶分解（見上方 FLR:Strips 的說明）----
     ;; 量在**碎片**上而不是扣除前的片上——被柱子切出來的細料同樣要被抓到。
     ;; 【1.3.2】一次 LegScan 同時取得 bad／邊數／最窄，不額外多掃一次條帶
     ;; ——這裡是最佳化的熱路徑（每組候選 × 每一格都會走到）。
     ;; 【1.4】x 向條帶只分解一次，裁切尺寸與排料歸類共用
     ;; （排料歸類是「最少廢料」排序目標的比較鍵，見 FLR:BetterG）。
     (if (and isfull (not ushape))
       ;; 整磚：沒有裁切尺寸（cuts 維持空）、沒有違規腳（bad nil／nlegs 0），
       ;; 最窄＝磚的短邊，排料由呼叫端依 kind 算成一整片（故 nst 維持空）。
       ;; **最窄不可以留 nil**：FLR:Evaluate 把 nil 當 0.0，於是「整張圖都是整磚」
       ;; 會報最窄 0——所有數字裡最不可能為真的一個，而且不會有任何錯誤訊息。
       (setq mleg (min tw th))
       (progn
         ;; 【1.4.2】x 向條帶一片只分解一次，違規掃描／裁切尺寸／排料歸類三者共用
         ;; ——舊版 FLR:LegScan 在內部又算了一次同參數的 FLR:Strips。
         (foreach f frags
           (setq st  (FLR:Strips (nth 1 f) 0 tol)
                 ls  (FLR:LegScanS (nth 1 f) st tw th minr tol))
           (if (> (car ls) 0) (setq bad T nlegs (+ nlegs (car ls))))
           (setq mleg (if mleg (min mleg (cadr ls)) (cadr ls)))
           (setq nst (cons (FLR:NestClass st tw th tol) nst))
           (foreach c (FLR:StripCutsOf st tw th tol)
             (setq cuts (cons c cuts))))
         (setq nst (reverse nst))))
     (list (cons 'kind (cond (ushape 'ushape)
                             (isfull 'full)
                             ((<= area tol) 'none)
                             (T 'cut)))
           (cons 'pieces np)
           (cons 'area area)
           (cons 'cuts cuts)
           (cons 'bad bad)
           (cons 'badlegs nlegs)
           (cons 'minleg mleg)
           (cons 'nest nst)
           (cons 'dedhit dedhit)
           ;; parts 供預覽與繪圖直接取用裁切後的真實多邊形——
           ;; 因此不需要 Region 布林，預覽形狀與最終結果必然一致。
           (cons 'parts frags)))))))

;;; ================================================================
;;;  八之二、完整佈置（供預覽與繪圖）
;;; ================================================================
;; 與 FLR:Evaluate 同一套判定，但保留每片的多邊形。
;; Evaluate 走精簡路徑供 Optimize 高頻呼叫；Layout 供低頻的預覽／繪圖。
;; 回傳 ((cell . classify-result) ...)，已濾掉 kind='none

(defun FLR:Layout (cfg regions rbboxes bbox / cells out res)
  (setq cfg   (FLR:WithRects cfg regions)
        cells (FLR:MakeGrid cfg bbox) out '())
  (foreach cell cells
    (setq res (FLR:ClassifyTile cell regions rbboxes cfg))
    (if (/= (cdr (assoc 'kind res)) 'none)
      (setq out (cons (cons cell res) out))))
  (reverse out))

;;; ================================================================
;;;  九、整體評估
;;; ================================================================
;; 回傳關聯表：full cut ushape bad area cutsizes(相異尺寸種類數) tiles(總片數)

(defun FLR:Round (x q) (* (FLR:Floor (+ (/ x q) 0.5)) q))

;;; ---- 裁切尺寸分群（決定「刀數」）----
;;
;; 【0.2 改為完整連結，修 FLR-03】
;; 0.1 版是單一連結（只跟**前一個**比），會鏈式串連：
;;   0 1 2 3 4 5 6 7 8 9 10　容差 1.0 → 全部併成一組、代表值 10、誤差 10.0。
;; 而「代表值取該組最大值」這個做法的前提就是「組內落差 ≤ 容差」，
;; 鏈式串連直接違反前提，照表切會超出許可差任意倍數。
;;
;; 改成跟**該組最小值**比：組內落差保證 ≤ 容差。
;; 代表值仍取該組實際最大值——切最大的、其餘再修，不會切過頭。
(defun FLR:GroupSizes (sizes tol / srt grp lo cur)
  (setq srt (vl-sort sizes '<) grp '() lo nil cur nil)
  (foreach s srt
    (cond ((null cur)         (setq cur s lo s))
          ((> (- s lo) tol)   (setq grp (cons cur grp) cur s lo s))
          (T                  (setq cur s))))
  (if cur (setq grp (cons cur grp)))
  (reverse grp))

;; 某尺寸在分群後對應的代表值（切割時實際要切的尺寸）
(defun FLR:RepOf (s grp / best)
  (setq best nil)
  (foreach g grp
    (if (and (>= g (- s 1e-9)) (or (null best) (< g best))) (setq best g)))
  (if best best s))

;; 【1.4】多回兩個欄位，供「排序目標可選」的比較準則使用（見 FLR:BetterG）：
;;   sym  對稱誤差。與 FLR:SymErr 同一個函式，這裡只是順手算好放進結果
;;        ——比較準則拿得到的只有這張關聯表，不可能再回頭去要 regions。
;;   need 實需母磚數（＝叫料量）。**只在 cfg 有 'nest 時才算**：它要把每片的
;;        排料歸類收集起來再跑兩次 FFD，對「最少刀」目標是白花的成本，
;;        而這裡是每組候選 × 每一格都會走到的熱路徑。
(defun FLR:Evaluate (cfg regions rbboxes bbox / cells res k nf nc nu nb nd ar
                          sizes q key tiles nbl mnl
                          nstp nwh nlw nxs nys nc2 kerf need)
  (setq cfg   (FLR:WithRects cfg regions)
        cells (FLR:MakeGrid cfg bbox)
        nf 0 nc 0 nu 0 nb 0 nd 0 ar 0.0 sizes '() tiles 0
        nbl 0 mnl nil
        nstp (FLR:Cfg 'nest cfg) nwh 0 nlw 0 nxs '() nys '()
        kerf (FLR:Cfg 'kerf cfg)
        q (FLR:Cfg 'sizeq cfg))
  (if (null kerf) (setq kerf 0.0))
  (foreach cell cells
    (setq res (FLR:ClassifyTile cell regions rbboxes cfg)
          k   (cdr (assoc 'kind res)))
    (if (/= k 'none)
      (progn
        (setq tiles (1+ tiles) ar (+ ar (cdr (assoc 'area res))))
        (cond ((= k 'full)   (setq nf (1+ nf)))
              ((= k 'ushape) (setq nu (1+ nu) nc (1+ nc)))
              (T             (setq nc (1+ nc))))
        (if (cdr (assoc 'bad res))    (setq nb (1+ nb)))
        (if (cdr (assoc 'dedhit res)) (setq nd (1+ nd)))
        ;; 【1.3.2】邊數累加、最窄取最小。與 FLR:StatsOf 的同名兩行必須一致
        ;; ——兩者的一致性由 FLR_Tests §10c 的斷言鎖住。
        (setq nbl (+ nbl (cdr (assoc 'badlegs res))))
        (if (cdr (assoc 'minleg res))
          (setq mnl (if mnl (min mnl (cdr (assoc 'minleg res)))
                            (cdr (assoc 'minleg res)))))
        ;; 先以極細刻度(0.001)去重把清單長度壓住，最後才依容差分群。
        ;; 直接存原始值的話，不規則幾何可能累積上千筆而拖慢比對。
        (foreach cs (cdr (assoc 'cuts res))
          (setq key (FLR:Round cs 0.001))
          (if (not (member key sizes)) (setq sizes (cons key sizes))))
        ;; 排料歸類（與 FLR:Nest 逐項對應：整磚各算一片，其餘逐碎片歸類）
        (if nstp
          (if (= k 'full)
            (setq nwh (1+ nwh))
            (foreach nc2 (cdr (assoc 'nest res))
              (cond
                ((eq nc2 'lwhole) (setq nlw (1+ nlw)))
                ((= (car nc2) 1)  (setq nys (cons (cdr nc2) nys)))
                (T                (setq nxs (cons (cdr nc2) nxs))))))))))
  (setq sizes (FLR:GroupSizes sizes q))
  ;; 尺寸分群要先做完才裝箱——師傅照表切，排料的尺寸必須是表上的代表值
  (if nstp
    (setq need (+ nwh nlw
                  (FLR:Pack (mapcar '(lambda (s) (FLR:RepOf s sizes)) nxs)
                            (FLR:Cfg 'tw cfg) kerf)
                  (FLR:Pack (mapcar '(lambda (s) (FLR:RepOf s sizes)) nys)
                            (FLR:Cfg 'th cfg) kerf))))
  (list (cons 'full nf) (cons 'cut nc) (cons 'ushape nu) (cons 'bad nb)
        (cons 'badlegs nbl) (cons 'minleg (if mnl mnl 0.0))
        (cons 'dedhit nd)
        (cons 'area ar) (cons 'cutsizes (length sizes)) (cons 'tiles tiles)
        (cons 'sizes sizes)
        (cons 'need need)
        (cons 'sym (FLR:SymErr regions cfg))
        (cons 'ox (FLR:Cfg 'ox cfg)) (cons 'oy (FLR:Cfg 'oy cfg))))

;;; ================================================================
;;;  九之二、由 Layout 彙總統計（含分區小計）
;;; ================================================================
;; 即時預覽顯示的數字與最終統計表**必須**同源，否則就會重蹈 TAD-01
;; 「統計與圖形不一致」。故兩者都走這個函式，並以斷言鎖住其結果
;; 與 FLR:Evaluate 一致（見 FLR_Tests.lsp §10c）。
;;
;; 回傳欄位同 FLR:Evaluate，另加 byregion：((regionIdx 片數 面積 整磚 裁切 違規) ...)

(defun FLR:StatsOf (layout cfg / nf nc nu nb nd ar sizes q key tiles byr rec res k idx hit
                          nbl mnl)
  (setq nf 0 nc 0 nu 0 nb 0 nd 0 ar 0.0 sizes '() tiles 0 byr '()
        nbl 0 mnl nil
        q (FLR:Cfg 'sizeq cfg))
  (foreach it layout
    (setq res (cdr it) k (cdr (assoc 'kind res)))
    (setq tiles (1+ tiles) ar (+ ar (cdr (assoc 'area res))))
    (cond ((= k 'full)   (setq nf (1+ nf)))
          ((= k 'ushape) (setq nu (1+ nu) nc (1+ nc)))
          (T             (setq nc (1+ nc))))
    (if (cdr (assoc 'bad res))    (setq nb (1+ nb)))
    (if (cdr (assoc 'dedhit res)) (setq nd (1+ nd)))
    ;; 同 FLR:Evaluate（1.3.2）：邊數累加、最窄取最小
    (setq nbl (+ nbl (cdr (assoc 'badlegs res))))
    (if (cdr (assoc 'minleg res))
      (setq mnl (if mnl (min mnl (cdr (assoc 'minleg res)))
                        (cdr (assoc 'minleg res)))))
    ;; 同 FLR:Evaluate：先細刻度去重，最後依容差分群
    (foreach cs (cdr (assoc 'cuts res))
      (setq key (FLR:Round cs 0.001))
      (if (not (member key sizes)) (setq sizes (cons key sizes))))
    ;; 分區小計 (idx 片數 面積 整磚 裁切 違規)
    ;; 面積按碎片累加；片數與分類則以「本片磚觸及的相異區域」為準——
    ;; 同一區內被扣除物切成多個碎片時仍只算一片磚，否則碎片數會被誤當成磚數。
    ;; 注意：跨區的磚會在每個觸及的區域各計一次，故分區加總 ≥ 總計，這是刻意的。
    (setq hit '())
    (foreach pt (cdr (assoc 'parts res))
      (setq idx (nth 0 pt) rec (assoc idx byr))
      (if (not (member idx hit)) (setq hit (cons idx hit)))
      (if rec
        (setq byr (subst (list idx (nth 1 rec) (+ (nth 2 rec) (nth 2 pt))
                               (nth 3 rec) (nth 4 rec) (nth 5 rec)) rec byr))
        (setq byr (cons (list idx 0 (nth 2 pt) 0 0 0) byr))))
    (foreach idx hit
      (setq rec (assoc idx byr))
      (setq byr (subst (list idx (1+ (nth 1 rec)) (nth 2 rec)
                             (+ (nth 3 rec) (if (= k 'full) 1 0))
                             (+ (nth 4 rec) (if (= k 'full) 0 1))
                             (+ (nth 5 rec) (if (cdr (assoc 'bad res)) 1 0)))
                       rec byr))))
  (setq sizes (FLR:GroupSizes sizes q))
  (list (cons 'full nf) (cons 'cut nc) (cons 'ushape nu) (cons 'bad nb)
        (cons 'badlegs nbl) (cons 'minleg (if mnl mnl 0.0))
        (cons 'dedhit nd)
        (cons 'area ar) (cons 'cutsizes (length sizes)) (cons 'tiles tiles)
        (cons 'sizes sizes)
        (cons 'byregion (vl-sort byr '(lambda (a b) (< (car a) (car b)))))
        (cons 'ox (FLR:Cfg 'ox cfg)) (cons 'oy (FLR:Cfg 'oy cfg))))

;;; ================================================================
;;;  十、1D 前篩
;;; ================================================================
;;
;; 【0.2 新增】區域全為正交多邊形時，
;;   **x 向的裁切尺寸只由 ox 決定、y 向的只由 oy 決定，且完全不必造網格。**
;;   （24 組 (ox,oy) 逐一與 FLR:Evaluate 實算比對，0 筆不符）
;;
;;   每條軸向邊 (座標 w, 區域在正側 +1 / 負側 -1)，令 m = (w − o) mod 間距：
;;     m ≈ 0        → 磚邊剛好對齊，不裁
;;     m ≥ 磚寬     → 落在填縫內，不裁
;;     區域在正側   → 裁切尺寸 = 磚寬 − m
;;     區域在負側   → 裁切尺寸 = m
;;
;;   於是 |cx|×|cy| 組候選可以先用 O(邊數) 排序，只把前幾名送去完整佈置。
;;   實測 600 ㎡ L 形樓層：全算 20 組 ≈ 14.7 秒，前篩全跑 < 1 ms。

(defun FLR:Modp (v p / m) (setq m (- v (* (FLR:Floor (/ v p)) p))) (if (< m 0.0) (+ m p) m))

;; 【0.4】「這條邊算不算軸向」要用**與磚尺寸同量綱**的容差，不能用浮點等值容差。
;;
;; 2026-08-13 在使用者的 A1001 平面圖實測到的病因：某個區域頂邊的兩端是
;;   y = -0.0000679  與  y = 0.0
;; 在 316.5 的水平跨距上差 **0.0000679 個單位**——那是繪圖誤差，不是斜牆。
;; 但舊版 FLR:IsRect 拿 `tol`（1e-6）去比、FLR:AxisEdges 更嚴到 1e-9，
;; 於是整張圖被判定成「有斜邊界」→ **1D 前篩整包關掉 → 576 組全算，約 290 秒**。
;;
;; 磚是 30 單位，0.00007 的偏差在任何意義上都不存在。容差取磚短邊的千分之一
;;（30 → 0.03，仍遠小於任何真實的斜牆），下限 1e-6 免得磚尺寸填了怪值時失效。
(defun FLR:AxisTol (cfg / tw th)
  (setq tw (FLR:Cfg 'tw cfg) th (FLR:Cfg 'th cfg))
  (if (and tw th (> (min tw th) 0.0)) (max 1e-6 (* (min tw th) 1e-3)) 1e-6))

;; 所有邊都是軸向？（tol 請用 FLR:AxisTol，不要用幾何運算的 1e-6）
(defun FLR:IsRect (regions tol / ok a)
  (setq ok T)
  (foreach rg regions
    (setq a (car rg))
    (foreach b (append (cdr rg) (list (car rg)))
      (if (and (> (abs (- (car a)  (car b)))  tol)
               (> (abs (- (cadr a) (cadr b))) tol))
        (setq ok nil))
      (setq a b)))
  ok)

;; 交丁造成的列偏移類別（0=正鋪→(0.0)、0.5→(0.0 0.5)、1/3→(0.0 1/3 2/3)）。
;; 類別太多（使用者填了個怪比例）就回 nil，讓呼叫端退回全算。
(defun FLR:StagOffs (stag / out r v)
  (setq out '() r 0)
  (while (< r 64)
    (setq v (FLR:Frac (* r stag)))
    (if (not (vl-some '(lambda (u) (equal u v 1e-6)) out))
      (setq out (cons v out)))
    (setq r (if (> (length out) 12) 64 (1+ r))))
  (if (> (length out) 12) nil (reverse out)))

;; 軸向邊 → ((座標 . 側) ...)　側 +1 = 區域在座標的正側
;; 【0.4】atol 同 FLR:IsRect：**不可以用 1e-9**。實測有邊的兩端只差 0.0000679
;; （繪圖誤差），用 1e-9 比會把那條邊整條漏掉，前篩因此少看到一段真實的邊界，
;; 排序失準到把真正的第一名排到 20 名之外。
(defun FLR:AxisEdges (regions axis atol / out a sgn d oth)
  (setq out '() oth (- 1 axis))
  (if (null atol) (setq atol 1e-9))
  (foreach rg regions
    (setq sgn (if (>= (FLR:SignedArea rg) 0.0) 1.0 -1.0)
          a   (car rg))
    (foreach b (append (cdr rg) (list (car rg)))
      (if (equal (nth axis a) (nth axis b) atol)
        (progn
          (setq d (- (nth oth b) (nth oth a)))
          ;; 逆時針時內部在行進方向的左手邊：
          ;;   axis 0 往 +y → 內部在 −x 側；axis 1 往 +x → 內部在 +y 側
          (if (= axis 0)
            (setq out (cons (cons (car a)  (* sgn (if (> d 0.0) -1.0 1.0))) out))
            (setq out (cons (cons (cadr a) (* sgn (if (> d 0.0)  1.0 -1.0))) out)))))
      (setq a b)))
  out)

;; 某軸、某起鋪偏移下會出現的裁切尺寸集合
(defun FLR:AxisSizes (edges o tw sx offs tol / out oo m sz)
  (setq out '())
  (foreach f offs
    (setq oo (+ o (* f sx)))
    (foreach e edges
      (setq m (FLR:Modp (- (car e) oo) sx))
      (if (and (> m tol) (< m (- tw tol)))
        (progn
          (setq sz (if (> (cdr e) 0.0) (- tw m) m))
          (if (not (vl-some '(lambda (u) (equal u sz 1e-6)) out))
            (setq out (cons sz out)))))))
  out)

;;; ---- 【0.5】違規（邊料過窄）的 1D 模型 ----
;;
;; 2026-08-13 使用者問「五項裡最少 46 片，真的沒辦法壓到 5% 以下嗎？」
;; 拿他的 A1001 平面圖把 675 組起鋪點全部精算，Pareto 前緣只有三個點：
;;   18 刀 / 違規 93（10.6%）　19 刀 / 53（6.0%）　**20 刀 / 25（2.8%）**
;; 也就是壓得到，而且最低的那組 (0.00, 14.60) **刀數跟原本的第 5 名一樣是 20**
;; ——不是拿刀數換違規，是清單漏掉的解。漏掉的原因有三個，缺一不可：
;;   ① 候選集裡沒有 14.60。y 軸的零違規窗口是 [12.60, 14.60]，而候選（頂點座標
;;      mod 磚距）最近的是 12.30，**差 0.3 就掉出窗口**。
;;   ② 前篩挑送去精算的名單時以刀數為主鍵，低違規的組合連被精算的機會都沒有。
;;   ③ 排序（FLR:Better）也是刀數優先於違規。
;; 這一節解決 ①②：算得出「違規最少的起鋪窗口」，並給前篩一個像樣的違規指標。
;;
;; 模型：一道軸向牆在某起鋪點下的邊料寬是確定的，違規與否只看它是否 < 下限；
;; 牆長 ÷ **另一軸**磚距 = 這道牆會切到幾列 = 該牆貢獻的違規片數。
;; 交丁讓相鄰列的相位不同，故每個相位各算一次再平均。
;; 這是**估計值**——角落 L 形、跨區細條它看不到（實測估 33 實際 47），
;; 只拿來挑候選與排序，統計數字一律以 FLR:Evaluate 為準。

;; 軸向牆 → ((座標 側 長度) ...)　側 +1 = 區域在座標的正側。
;; 與 FLR:AxisEdges 的差別只在多帶一個長度（違規要按牆長加權，不是按條數）。
(defun FLR:AxisWalls (regions axis atol / out a sgn d oth)
  (setq out '() oth (- 1 axis))
  (if (null atol) (setq atol 1e-9))
  (foreach rg regions
    (setq sgn (if (>= (FLR:SignedArea rg) 0.0) 1.0 -1.0)
          a   (car rg))
    (foreach b (append (cdr rg) (list (car rg)))
      (if (equal (nth axis a) (nth axis b) atol)
        (progn
          (setq d (- (nth oth b) (nth oth a)))
          (setq out (cons (list (nth axis a)
                                (if (= axis 0)
                                  (* sgn (if (> d 0.0) -1.0 1.0))
                                  (* sgn (if (> d 0.0)  1.0 -1.0)))
                                (abs d))
                          out))))
      (setq a b)))
  out)

;; 該軸的列偏移類別。交丁只沿 x 移動列，故 y 軸恆為單一相位。
(defun FLR:Phases (cfg axis / p)
  (if (= axis 0)
    (progn (setq p (FLR:StagOffs (FLR:Cfg 'stagger cfg)))
           (if p p '(0.0)))
    '(0.0)))

;; 某起鋪點下、該軸的違規片數估計
;;   walls 該軸的牆　o 起鋪點　sx 該軸磚距　tw 該軸磚長　minc 下限寬
;;   phases 列偏移　so **另一軸**磚距（牆長換算列數用）
(defun FLR:BadWeight (walls o sx tw minc phases so / tot np m w)
  (setq tot 0.0 np (float (length phases)))
  (foreach wl walls
    (foreach f phases
      (setq m (FLR:Modp (- (nth 0 wl) (+ o (* f sx))) sx))
      ;; m >= tw ＝ 牆落在填縫裡，兩側都是整磚
      (if (< m tw)
        (progn
          (setq w (if (> (nth 1 wl) 0.0) (- tw m) m))
          (if (and (> w 1e-9) (< w minc))
            (setq tot (+ tot (/ (/ (nth 2 wl) so) np))))))))
  tot)

;;; ---- 【1.4】廢料（母磚數）的 1D 模型 ----
;;
;; A1001 實測：前篩若沿用刀數排序，「最少廢料」的真值第一名排在**第 490 名**
;; ——25 秒的名額只有八十幾組，等於這個目標的前篩形同亂挑。
;; 但廢料其實跟違規一樣是可以 1D 估的：某道軸向牆的邊料寬只由該軸的起鋪點決定，
;; 牆長 ÷ 另一軸磚距 = 這道牆切出幾條，把這些條丟進**同一支 FFD**（FLR:Pack）
;; 就得到該軸要吃掉幾片母磚。x 只跟 ox 有關、y 只跟 oy 有關，故整份候選集
;; 每軸只要各算一次（27+26 次），成本可以忽略。
;;
;; 是估計不是精確值：整磚數、L 形角料、扣除物切出來的細條它都看不到，
;; 而那幾項在同一張圖上各方案之間差異不大——排序要的是**相對**好壞。
;; 交丁的每個相位各算一次（列數平分），因為互補排料的節省正是來自
;; 「這一列剩 12、下一列剩 18，同一片母磚切一刀出兩條」。
(defun FLR:WasteWeight (walls o sx tw so kerf phases / sizes m w rows np)
  (setq sizes '() np (length phases))
  (if (null kerf) (setq kerf 0.0))
  (foreach wl walls
    (foreach f phases
      (setq m (FLR:Modp (- (nth 0 wl) (+ o (* f sx))) sx))
      ;; m >= tw ＝ 牆落在填縫裡，兩側都是整磚，不產生邊料
      (if (< m tw)
        (progn
          (setq w    (if (> (nth 1 wl) 0.0) (- tw m) m)
                rows (max 1 (FLR:Ceil (/ (nth 2 wl) (* so (float np))))))
          (if (> w 1e-9)
            (repeat rows (setq sizes (cons w sizes))))))))
  (FLR:Pack sizes tw kerf))

;; 違規最少的起鋪點，最多 nwin 個。
;; 每道牆的「違規起鋪區間」寬度恰為下限寬，其端點就是窗口的分界；
;; 逐段取中點評分即可，不必掃描（實測 17 道牆 × 2 相位 = 68 個分界點）。
(defun FLR:BadWindows (regions cfg axis nwin / tw gap sx so minc walls phases
                               bps a b mid scored out n i v)
  (setq tw    (FLR:Cfg (if (= axis 0) 'tw 'th) cfg)
        gap   (FLR:Cfg 'gap cfg)
        minc  (FLR:Cfg 'mincut cfg))
  (if (or (null tw) (null gap) (null minc) (<= tw 0.0))
    nil
    (progn
      (setq sx    (+ tw gap)
            so    (+ (FLR:Cfg (if (= axis 0) 'th 'tw) cfg) gap)
            minc  (* tw minc)
            walls (FLR:AxisWalls regions axis (FLR:AxisTol cfg))
            phases (FLR:Phases cfg axis))
      (if (or (null walls) (<= sx 0.0) (<= so 0.0) (<= minc 0.0))
        nil
        (progn
          (setq bps '())
          (foreach wl walls
            (foreach f phases
              ;; 側 +1：邊料 = tw − m，違規區間 o ∈ (座標−tw, 座標−tw+下限)
              ;; 側 −1：邊料 = m　 ，違規區間 o ∈ (座標−下限, 座標)
              (setq a (if (> (nth 1 wl) 0.0)
                        (- (nth 0 wl) tw (* f sx))
                        (- (nth 0 wl) minc (* f sx)))
                    b (+ a minc))
              (foreach v (list (FLR:Modp a sx) (FLR:Modp b sx))
                (if (not (vl-some '(lambda (u) (equal u v 1e-9)) bps))
                  (setq bps (cons v bps))))))
          ;; 【0.8】相鄰斷點成對走訪。最後一段回捲到「第一個斷點 + sx」，
          ;; 故把它接在尾巴上一起走，收尾就不必另外寫一個特例。
          (setq bps (vl-sort bps '<) scored '() a (car bps))
          (foreach b (append (cdr bps) (list (+ (car bps) sx)))
            (setq mid (FLR:Modp (/ (+ a b) 2.0) sx)
                  scored (cons (list (FLR:BadWeight walls mid sx tw minc phases so)
                                     (- b a) mid)
                               scored)
                  a b))
          ;; 違規少者優先；同分取窗口最寬——中點離邊界最遠，最不怕繪圖誤差
          (setq scored (vl-sort scored
                         '(lambda (p q)
                            (if (equal (car p) (car q) 1e-9)
                              (> (cadr p) (cadr q))
                              (< (car p) (car q))))))
          (setq out '() i 0)
          (foreach s scored
            (if (< i nwin) (setq out (cons (nth 2 s) out) i (1+ i))))
          (reverse out))))))

;; 前篩名額給「違規最少」的比例。
;; 【實測定出來的，不是猜的】A1001、25 秒預算 → 45 組：
;;   違規側的目標（(0.00, 13.60)，精算後 2.8%）排在違規排序的**第 1 名**
;;   ——違規的 1D 估計很準，給再多名額也是浪費；
;;   刀數側的目標（(9.00, 8.55)，18 刀）排在刀數排序的**第 32 名**
;;   ——刀數的 1D 估計看不到凹形區域的交互作用，名額不足就會漏掉。
;; 故偏心給刀數側：0.4 時刀數側只有 27 名，實測 18 刀那個方案被擠掉、
;; 清單最低變成 19 刀（等於為了低違規犧牲掉原本的第一名）。0.25 → 34 名，
;; 兩邊都涵蓋得到。
(setq FLR:BADSHARE 0.25)

;; 每軸要放幾個「低違規窗口」進候選集。實測 A1001：y 軸最佳窗口只有一個
;; （[12.60, 14.60]），取 3 個是為了讓次佳窗口也在，因為 1D 模型是估計值
;; ——最佳的那個窗口不保證精算後也最佳（實測 12.60/13.60 是 22 刀、14.60 是 20 刀）。
(setq FLR:BADCAND 3)

;; 回傳 ((ox oy) ...) 前 k 名，或 nil 表示不適用（斜邊界／怪交丁比例）
;; 【0.4 修「一條斜邊關掉整個前篩」】0.2~0.3 的門檻是「**所有**區域的每一條邊都必須軸向」，
;; 真實平面圖只要有一條斜牆就整包退回全算。
;; 2026-08-13 在使用者的 A1001 平面圖實測：7 個區域裡**只有 1 個有 1 條斜邊**，
;; 就讓 576 組全部精算——單次佈置 0.5 秒 × 576 組 ≈ **288 秒**。
;;
;; 但這道門檻本來就過度保守：
;;   ① FLR:AxisEdges 只收軸向邊，**斜邊自動被忽略**，機制本身容得下；
;;   ② 前篩只負責**排序挑前幾名**，選出來的還是逐一送 FLR:Evaluate 精算，
;;      所以斜邊只會讓排序稍微不準，**不會讓結果錯**。
;; 故改成「照跑，但非全軸向時把 shortlist 加寬」補償排序的不準（見 FLR:ShortN）。
;; 實測同一張圖：576 → 精算 20 組，前篩本身 78 ms、精算 10.9 秒。
;; 【0.5】名單分兩半：一半照刀數、一半照違規（見 FLR:BADSHARE）。
;; 原本兩個排序鍵都是刀數優先（第二鍵只是「低於下限的尺寸種類數」，不看牆長），
;; 於是低違規的起鋪點連被精算的機會都沒有——A1001 實測 2.8% 的那組就是這樣漏掉的。
;; 某軸、某起鋪點的（對稱誤差 最窄邊料）。1D，只看各區的兩端邊料。
;; 【對稱誤差是精確的，不是估計】FLR:SymErr 的定義就是「各區各軸兩端邊料差
;; 取最大」，而 x 向只與 ox 有關、y 向只與 oy 有關——兩軸各算完取大即為全圖值。
;; 故 sym 目標的前篩排序**與全算排序一致**（實測見 CHANGELOG v1.4）。
;; 最窄邊料則只是估計：真正的最窄還可能出自 L 形角料或扣除物切出來的細條。
(defun FLR:AxisSym (rbbs axis o tw sx tol / mx mn e)
  (setq mx 0.0 mn nil)
  (foreach bb rbbs
    (setq e  (FLR:EdgeCuts (nth axis bb) (nth (+ axis 2) bb) o tw sx tol)
          mx (max mx (abs (- (car e) (cadr e))))
          mn (if mn (min mn (car e) (cadr e)) (min (car e) (cadr e)))))
  (list mx (if mn mn 0.0)))

(defun FLR:PreRank (regions bbox cfg cx cy k / tol tw th gap q minr offs atol
                         ex ey wx wy sx sy phx xs ys scored byk byb bys byw nb p out i
                         goal rbbs main krf)
  (setq tol (FLR:Cfg 'tol cfg) tw (FLR:Cfg 'tw cfg) th (FLR:Cfg 'th cfg)
        gap (FLR:Cfg 'gap cfg) q (FLR:Cfg 'sizeq cfg) minr (FLR:Cfg 'mincut cfg)
        offs (FLR:StagOffs (FLR:Cfg 'stagger cfg)))
  ;; 只剩「交丁比例怪到列偏移類別算不完」這一種退回全算的情形
  (if (null offs)
    nil
    (progn
      (setq atol (FLR:AxisTol cfg)
            goal (FLR:GoalOf (FLR:Cfg 'goal cfg))
            rbbs (mapcar 'FLR:BBox regions)
            ex (FLR:AxisEdges regions 0 atol) ey (FLR:AxisEdges regions 1 atol)
            wx (FLR:AxisWalls regions 0 atol) wy (FLR:AxisWalls regions 1 atol)
            sx (+ tw gap) sy (+ th gap) phx (FLR:Phases cfg 0))
      ;; 每個候選各算一次即可（x 只跟 ox 有關、y 只跟 oy 有關）
      ;; 每筆 = (起鋪點 裁切尺寸集合 違規片數估計 對稱誤差 最窄邊料 母磚數估計)
      (setq krf (FLR:Cfg 'kerf cfg))
      (setq xs (mapcar '(lambda (o) (append
                                      (list o (FLR:AxisSizes ex o tw sx offs tol)
                                              (FLR:BadWeight wx o sx tw (* tw minr) phx sy))
                                      (FLR:AxisSym rbbs 0 o tw sx tol)
                                      (list (FLR:WasteWeight wx o sx tw sy krf phx)))) cx)
            ys (mapcar '(lambda (o) (append
                                      (list o (FLR:AxisSizes ey o th sy '(0.0) tol)
                                              (FLR:BadWeight wy o sy th (* th minr) '(0.0) sx))
                                      (FLR:AxisSym rbbs 1 o th sy tol)
                                      (list (FLR:WasteWeight wy o sy th sx krf '(0.0))))) cy)
            scored '())
      (foreach xr xs
        (foreach yr ys
          ;; (相異尺寸數 違規估計 ox oy 對稱誤差 最窄邊料 母磚數估計)
          ;; 對稱誤差兩軸取大、最窄邊料兩軸取小——與 FLR:SymErr／FLR:LegScan 同定義；
          ;; 母磚數兩軸相加——兩軸的邊料條各自裝箱，本來就是兩堆料
          (setq scored (cons (list (length (FLR:GroupSizes
                                             (append (nth 1 xr) (nth 1 yr)) q))
                                   (+ (nth 2 xr) (nth 2 yr))
                                   (nth 0 xr) (nth 0 yr)
                                   (max (nth 3 xr) (nth 3 yr))
                                   (min (nth 4 xr) (nth 4 yr))
                                   (+ (nth 5 xr) (nth 5 yr)))
                             scored))))
      (setq byk (vl-sort scored
                  '(lambda (a b)
                     (if (/= (nth 0 a) (nth 0 b))
                       (< (nth 0 a) (nth 0 b))
                       (< (nth 1 a) (nth 1 b)))))
            byb (vl-sort scored
                  '(lambda (a b)
                     (if (equal (nth 1 a) (nth 1 b) 1e-9)
                       (< (nth 0 a) (nth 0 b))
                       (< (nth 1 a) (nth 1 b)))))
            ;; 對稱小者優先；同分取邊料最寬；再同分取刀數少
            bys (vl-sort scored
                  '(lambda (a b)
                     (cond
                       ((not (equal (nth 4 a) (nth 4 b) 1e-6)) (< (nth 4 a) (nth 4 b)))
                       ((not (equal (nth 5 a) (nth 5 b) 1e-6)) (> (nth 5 a) (nth 5 b)))
                       (T (< (nth 0 a) (nth 0 b))))))
            ;; 母磚數估計小者優先；同分取刀數少（估計值相同時，切得少的通常真的省）
            byw (vl-sort scored
                  '(lambda (a b)
                     (if (= (nth 6 a) (nth 6 b))
                       (< (nth 0 a) (nth 0 b))
                       (< (nth 6 a) (nth 6 b)))))
            ;; 主序依目標。三個目標各準到什麼程度是 A1001 實測出來的，
            ;; 數字見 CHANGELOG v1.4。
            main (cond ((eq goal 'sym)   bys)
                       ((eq goal 'waste) byw)
                       (T                byk))
            nb  (FLR:Floor (* (float k) FLR:BADSHARE))
            out '() i 0)
      (foreach s main
        (if (< i (- k nb))
          (setq out (cons (list (nth 2 s) (nth 3 s)) out) i (1+ i))))
      (foreach s byb
        (if (< i k)
          (progn
            (setq p (list (nth 2 s) (nth 3 s)))
            (if (not (vl-some '(lambda (u) (and (equal (car u)  (car p)  1e-9)
                                                (equal (cadr u) (cadr p) 1e-9))) out))
              (setq out (cons p out) i (1+ i))))))
      (reverse out))))

;;; ================================================================
;;;  十一、起鋪點最佳化
;;; ================================================================
;; 候選集 = {邊界頂點座標 mod pitch} ∪ {磚置中} ∪ {縫置中}
;; 排序：① U形數(硬性排除) ② 相異裁切尺寸種類 ③ 違規磚數 ④ 總片數 ⑤ 裁切磚數

;; 「對稱」有兩個合法解：把磚的中心對到中線，或把縫對到中線。兩個都收。
;; maxcand 在前篩可用時可以放大（前篩成本與候選數幾乎無關），
;; 用不到前篩時才需要壓低，否則 |cx|×|cy| 次完整佈置會爆炸。
;; 【0.3 使用者回饋 2026-08-13】原本只放「所有區域的**整體外框**」中線那兩解。
;; 多房間時整體外框的中線對每一間都不是中線——實測兩房間（L 形 0..300 ＋ 矩形
;; 350..650）時，「磚置中」給矩形那間的邊料是 2.4 / 24.6，**完全不對稱**，
;; 而模式名稱寫著「自動置中對稱」。故把**每一區各自的中線**也放進候選，
;; 使用者至少挑得到「這一間對稱」的方案（多區共用一組網格時本來就無法同時對稱）。
;;
;; 【0.5】兩處修正，都是 A1001 平面圖實測出來的：
;;   ① 去重容差 1e-6 → FLR:AxisTol。該圖的頂點帶著 6.78861e-05 的繪圖誤差，
;;      於是 2.25 與 2.2499660 被當成兩個候選——**推薦清單的第 2、3 名因此
;;      是同一個方案**（ox=2.70 oy=2.25，統計數字一模一樣），使用者看得到。
;;   ② 加入「違規最少的起鋪窗口」中點（FLR:BadWindows）。原本的候選全都來自
;;      頂點座標與中線，y 軸的零違規窗口 [12.60, 14.60] 一個都沒命中
;;      （最近的候選 12.30，差 0.3）。上限同步放寬，不排擠既有候選。
(defun FLR:Candidates (regions bbox cfg axis / sx tw gap vals v cm cap bb pre atol inj)
  (setq tw   (FLR:Cfg (if (= axis 0) 'tw 'th) cfg)
        gap  (FLR:Cfg 'gap cfg)
        sx   (+ tw gap)
        cap  (FLR:Cfg 'maxcand cfg)
        atol (FLR:AxisTol cfg)
        vals '())
  (if (null cap) (setq cap 24))
  ;; 整體中線兩解 ＋ 每一區各自的中線兩解，全部優先放（不受上限截斷）
  (setq pre (list bbox))
  (foreach rg regions (setq pre (append pre (list (FLR:BBox rg)))))
  (foreach bb pre
    (setq cm (/ (+ (nth axis bb) (nth (+ axis 2) bb)) 2.0))
    (foreach v (list (FLR:Modp (- cm (/ tw 2.0)) sx)   ; 磚置中
                     (FLR:Modp cm sx))                  ; 縫置中
      (if (not (vl-some '(lambda (u) (equal u v atol)) vals))
        (setq vals (cons v vals)))))
  (setq vals (reverse vals))
  ;; 違規最少的起鋪窗口（同樣不受上限截斷，並把上限一起放寬）
  (setq inj (FLR:BadWindows regions cfg axis FLR:BADCAND))
  (foreach v inj
    (if (not (vl-some '(lambda (u) (equal u v atol)) vals))
      (setq vals (append vals (list v)))))
  (setq cap (+ cap (length inj)))
  ;; 每個邊界頂點：磚邊與邊界邊重合處即最佳解所在
  (foreach rg regions
    (foreach p rg
      (setq v (FLR:Modp (nth axis p) sx))
      (if (and (not (vl-some '(lambda (u) (equal u v atol)) vals))
               (< (length vals) cap))
        (setq vals (append vals (list v))))))
  vals)

;;; ---- 對稱度 ----
;; 「自動置中對稱」的排序準則裡本來沒有對稱性，於是推薦第 1 名常常左右邊料不一樣
;; （使用者 2026-08-13 回報：同一排最左 28.4、最右 28.9）。
;; 這裡把它量出來，讓推薦清單標得出來——**只量不排序**，排序準則維持不變。
;;
;; 某區域在某軸上兩端的邊料寬。邊界剛好落在磚邊或填縫裡＝那一端是整磚，回 tw。
(defun FLR:EdgeCuts (r1 r2 o tw sx tol / m1 m2 a b)
  (setq m1 (FLR:Modp (- r1 o) sx)
        m2 (FLR:Modp (- r2 o) sx)
        ;; 起始端：磚從 r1 之前就開始了，露出來的是 tw − m1
        a  (if (or (< m1 tol) (>= m1 (- tw tol))) tw (- tw m1))
        ;; 結束端：磚在 r2 之後才結束，露出來的是 m2
        b  (if (or (< m2 tol) (>= m2 (- tw tol))) tw m2))
  (list a b))

;; 全圖的對稱誤差＝各區、各軸「兩端邊料差」的最大值。0 = 每一區的兩端都一樣寬。
;; 註：交丁會讓相鄰列的邊料互換，這裡量的是**基準列**（不含交丁偏移），
;;     那正是「起鋪點置不置中」這件事本身。
(defun FLR:SymErr (regions cfg / tw th gap tol ox oy mx bb e)
  (setq tw  (FLR:Cfg 'tw cfg)  th (FLR:Cfg 'th cfg)
        gap (FLR:Cfg 'gap cfg) tol (FLR:Cfg 'tol cfg)
        ox  (FLR:Cfg 'ox cfg)  oy (FLR:Cfg 'oy cfg)
        mx  0.0)
  (if (null tol) (setq tol 1e-6))
  (foreach rg regions
    (setq bb (FLR:BBox rg)
          e  (FLR:EdgeCuts (nth 0 bb) (nth 2 bb) ox tw (+ tw gap) tol)
          mx (max mx (abs (- (car e) (cadr e)))))
    (setq e  (FLR:EdgeCuts (nth 1 bb) (nth 3 bb) oy th (+ th gap) tol)
          mx (max mx (abs (- (car e) (cadr e))))))
  mx)

;;; ---- 【1.4】排序目標可選 ----
;;
;; 使用者 2026-08-13：「同一排最左 28.4、最右 28.9」——排序準則裡沒有對稱性，
;; 於是推薦第 1 名常常左右不一樣寬。1.2.2 的處置是把對稱度**標出來**讓人自己挑，
;; 但那只是把選擇成本丟回給使用者：清單只有 8 筆，對稱的方案不見得在裡面
;; （清單本身就是照刀數挑出來的）。1.4 改成**目標可選**——換目標會換掉整份清單。
;;
;; 三個目標對應三種現場：
;;   cuts  最少刀   → 師傅切得少、工期短。原本唯一的準則，仍為預設。
;;   waste 最少廢料 → 叫料量最少（母磚數，含邊料互補排料）。磚貴或缺貨時用。
;;   sym   邊磚最寬且左右對稱 → 完成面好看。門廳、樣品屋、要拍照的場合用。
;;
;; 【為什麼不做成加權總分】權重無法在對話框裡講清楚，而且三個量綱不同
;; （刀數是整數、廢料是片數、對稱是長度），任何權重都是憑空。分成三個
;; 明確的目標，使用者知道自己選了什麼，也還原得回去。
(setq FLR:GOALS '((cuts  . "最少刀（省工）")
                  (waste . "最少廢料（省料）")
                  (sym   . "邊磚最寬且左右對稱（好看）")))

;; 未指定或給了不認得的值一律回 'cuts——目標來自對話框，
;; 而錯誤的目標會靜默改變推薦結果，是最難察覺的一種錯。
(defun FLR:GoalOf (goal)
  (if (assoc goal FLR:GOALS) goal 'cuts))

(defun FLR:GoalLabel (goal) (cdr (assoc (FLR:GoalOf goal) FLR:GOALS)))

;; 只有 waste 需要每組候選都跑排料（貴），故由目標決定要不要開 'nest。
;; 呼叫端估「每組要多久」時必須用同一份 cfg，否則估時會少算排料那一段。
(defun FLR:GoalCfg (cfg)
  (if (and (eq (FLR:GoalOf (FLR:Cfg 'goal cfg)) 'waste)
           (null (FLR:Cfg 'nest cfg)))
    (append cfg (list (cons 'nest T)))
    cfg))

;; 每個目標＝一串比較鍵 (欄位 方向 容差)；方向 1 = 小者勝、−1 = 大者勝。
;; 容差非 nil 者以 equal 比（浮點欄位必須給，否則 1e-12 的差也會被當成勝負）。
;;
;; 三串的共同點：'ushape 一律排最前面（U 形是硬性禁則，不是偏好），
;; 且每一串的最後都以片數收尾，讓比較有確定的終點（同分才回 nil）。
;;   cuts  ⑤ 裁切磚數是 2026-08-12 GUI 實測後補的：前四項並列時，
;;         整磚 73／裁切 46 會排在整磚 87／裁切 32 前面，看起來很不合理。
;;         總片數相同時，裁切少的明顯省工，必須納入比較。
;;   waste 母磚數之後才比違規：這個目標的使用者是為了省料而來，
;;         但違規仍在第三位——省下來的料不能拿去換不合規的邊磚。
;;   sym   對稱排第一，最窄邊磚第二（「最寬」是在「對稱」的前提下才有意義，
;;         否則左右差 20 cm 但最窄 14 cm 的方案會贏過完全對稱的 13 cm），
;;         違規排到第三。
;;
;; 【違規為什麼不是硬性第一】初版把 bad 排在 sym 前面，A1001 實測直接打臉：
;; 該圖各方案的違規片數落差很大（25～93），bad 一放前面就完全壓過對稱，
;; 「對稱」目標挑出來的方案對稱誤差 22.5，**比預設的「最少刀」目標還差**（20.8）
;; ——使用者選了對稱卻拿到更不對稱的結果，那這個選項等於是壞的。
;; 三個目標一律「使用者選的那個量排第一」，違規則沿用預設目標的位置（第三）。
;; 違規並沒有被忽略：清單每一列都印違規片數與百分比，且照舊附掛最多三筆
;; 「違規更低」的方案（標 [低違規]）供對照。
(defun FLR:GoalKeys (goal)
  (cond
    ((eq goal 'waste)
     '((ushape 1 nil) (need 1 nil) (bad 1 nil) (cutsizes 1 nil) (tiles 1 nil)))
    ((eq goal 'sym)
     '((ushape 1 nil) (sym 1 1e-6) (minleg -1 1e-6) (bad 1 nil)
       (cutsizes 1 nil) (tiles 1 nil)))
    (T
     '((ushape 1 nil) (cutsizes 1 nil) (bad 1 nil) (tiles 1 nil) (cut 1 nil)))))

;; T = a 依 goal 優於 b
(defun FLR:BetterG (a b goal / out done va vb tk)
  (setq out nil done nil)
  (foreach k (FLR:GoalKeys (FLR:GoalOf goal))
    (if (not done)
      (progn
        (setq va (cdr (assoc (nth 0 k) a))
              vb (cdr (assoc (nth 0 k) b))
              tk (nth 2 k))
        ;; 缺欄位當 0。只會發生在「目標是 waste 但 cfg 沒開 'nest」，
        ;; 那時兩邊都缺、整串鍵一路同分，退化成不分優劣而不是亂排。
        (if (null va) (setq va 0))
        (if (null vb) (setq vb 0))
        (if (not (if tk (equal va vb tk) (= va vb)))
          (setq done T
                out  (if (> (nth 1 k) 0) (< va vb) (> va vb)))))))
  out)

;; 舊簽章＝最少刀，維持不變（既有斷言與呼叫端不必動）
(defun FLR:Better (a b) (FLR:BetterG a b 'cuts))

;; 估計網格總格數（不實際產生），供呼叫端做規模保護
(defun FLR:CellEstimate (cfg bbox / sx sy)
  (setq sx (+ (FLR:Cfg 'tw cfg) (FLR:Cfg 'gap cfg))
        sy (+ (FLR:Cfg 'th cfg) (FLR:Cfg 'gap cfg)))
  (* (1+ (FLR:Ceil (/ (- (nth 2 bbox) (nth 0 bbox)) sx)))
     (1+ (FLR:Ceil (/ (- (nth 3 bbox) (nth 1 bbox)) sy)))))

(defun FLR:CandCount (regions bbox cfg)
  (* (length (FLR:Candidates regions bbox cfg 0))
     (length (FLR:Candidates regions bbox cfg 1))))

;; 實際要送去完整佈置的組數（前篩可用時就是 shortlist 長度）。
;; 呼叫端用它估時，估出來的秒數才會跟實際相符。
(defun FLR:WorkCount (regions bbox cfg topn / cx cy sl)
  (setq cx (FLR:Candidates regions bbox cfg 0)
        cy (FLR:Candidates regions bbox cfg 1)
        sl (FLR:PreRank regions bbox cfg cx cy
                        (FLR:ShortN topn (* (length cx) (length cy))
                                    (FLR:IsRect regions (FLR:AxisTol cfg)) cfg)))
  (if sl (length sl) (* (length cx) (length cy))))

;; shortlist 要多長。
;;
;; 【0.4 改成時間預算制】前篩的排序**不是精確的**——它假設 x 向的裁切尺寸只由 ox
;; 決定，但凹形區域的垂直邊只存在於某一段 y 範圍，會不會被裁其實同時取決於 oy。
;; 2026-08-13 在使用者的 A1001 平面圖（7 區、含 L 形）實測前篩名次 vs 全算名次：
;;
;;   全算第 1 名（18 刀）→ 前篩排 **第 24 名**
;;   全算第 2 名（19 刀）→ 前篩排 第 41 名
;;   全算第 3 名（19 刀）→ 前篩排 第 1 名
;;
;; 也就是說「取前 10 名」在這張圖上會漏掉真正的第一名，只拿到 19 刀。
;; 但要多長才夠**沒有通用答案**——誤差隨區域數與凹形程度變化。
;; 與其寫死一個猜的數字，不如換一個使用者真正在意的量：**願意等幾秒**。
;; 每組要多久由呼叫端實測（cfg 的 'perms），於是 shortlist = 預算 ÷ 每組耗時。
;;   小圖 → 每組幾十 ms → 幾百組都排得進預算，幾乎等於全算
;;   大圖 → 每組半秒   → 只精算二三十組，但那正是使用者能忍受的時間
;; 沒給 'perms 時退回原本的下限規則（全軸向 10、有斜邊 20）。
;; 最佳化最多花多久（毫秒）。**使用者裁決 2026-08-13：15 秒。**
;; 依據：A1001 平面圖每組 563 ms，15 秒＝26 組，而全算的第一名排在前篩第 24 名
;; ——剛好涵蓋得到（實測第一名與全算 576 組完全相同，306 秒 → 14.6 秒）。
;; 這個數字同時是「不必問使用者的上限」：在承諾之內就直接跑，
;; 只有連最少組數都超過它時才跳提示（見 c:FLR）。
;; 【0.5】15 秒 → 25 秒。0.5 起名單要分給兩個目標（刀數／違規，見 FLR:BADSHARE），
;; 而且候選集多了低違規窗口（576 → 702 組），刀數側的名次因此往後推：
;; A1001 實測 18 刀那組從第 24 名掉到第 32 名。15 秒只有 26 組，怎麼分都漏。
;; 25 秒 ≈ 45 組 → 刀數側 34 名、違規側 11 名，兩邊都涵蓋得到（實測 24.4 秒）。
;; 使用者裁決 2026-08-13：為了把違規從 10.6% 壓到 2.8%，接受多等 10 秒。
(setq FLR:TIMEBUDGET 25000.0)

(defun FLR:ShortN (topn ncand exact cfg / floorn per bud)
  (setq floorn (if exact (max 10 (* 2 topn)) (max 20 (* 4 topn)))
        per    (FLR:Cfg 'perms  cfg)
        bud    (FLR:Cfg 'budget cfg))
  (if (null bud) (setq bud FLR:TIMEBUDGET))
  (min ncand
       (if (and per (> per 0))
         (max floorn (FLR:Floor (/ (float bud) (float per))))
         floorn)))

;; 回傳前 topn 名（依 FLR:Better），後面再附掛最多 FLR:EXTRABAD 個
;; **違規更低**的方案，每個都標 (why . lowbad) 供 UI 區別顯示。
;; 只附「比清單內最低還低」的，附完就沒有更好的可挑——沒有可附的就不附，
;; 清單長度因此不是固定值（呼叫端不可假設 = topn）。
(setq FLR:EXTRABAD 3)

;;; ---- 起鋪點精修（2026-08-14）----------------------------------------
;; 【為什麼需要】候選集是**啟發式**產生的，實測有漏：A1001 沿真值第一名的兩條線
;; 細掃，六條裡三條找得到更好的——最少廢料 x 向少 15 片違規、y 向少 1 片母磚、
;; 對稱 y 向誤差 10.9 → 10.6。
;;
;; 【漏的原因不是 maxcand 截斷】第一個假說是「頂點候選被上限截掉了」，
;; 量下來**被推翻**：把上限拆掉只多 4 個候選（x 27→31、y 26→26），
;; 而那三個贏家**沒有一個在裡面**。
;;
;; 真正的原因是候選集的斷點模型只涵蓋一種斷點——「磚邊與區域邊界重合」，
;; 那是**刀數**的斷點。另外兩個目標的斷點在別的地方：
;;   違規    邊料寬跨過裁切下限時才變，斷點在「頂點 ± mincut·tw」
;;   母磚數  要跑完 FFD 才知道，沒有封閉解
;;   對稱誤差 是**連續量**，最優可以落在任何地方
;; 所以「刀數目標沒漏、另外兩個有漏」與實測完全對得起來，不是巧合。
;;
;; 【為什麼是取樣不是解析】違規的斷點理論上算得出來，但母磚數與對稱誤差沒有
;; 封閉解。取樣是這裡唯一做得到的事，於是重點變成**取樣要多細**——那是量出來的：
;;
;;   步長（磚距 30.3）  每軸點數  兩軸耗時  拿得回來的
;;   1.0                31        約 3.9 秒  廢料 x 違規 64→53、廢料 y 母磚 804→803
;;   0.5                61        約 7.6 秒  同上
;;   0.25               122       約 15 秒   再加 廢料 x 違規 →49、對稱 y 10.9→10.6
;;
;; 步長 1.0 就拿得到三分之二的收益。故**與 shortlist 同一套做法：由時間預算決定**
;; （預算 ÷ 每組實測耗時 ÷ 2 軸），不寫死步長——每組耗時跨機差兩倍以上，
;; 寫死步長等於在快的機器上浪費、在慢的機器上超時。
;;
;; 【只精修第一名】精修 8 筆要 8 倍的時間，而使用者真正會採用的多半是第 1 名。
;; 清單的排序不會因此壞掉：精修後的第 1 名比精修前更好，而精修前的它已經贏過第 2 名。
;; 附掛的 [低違規] 那幾筆不受影響（它們來自 all，另一套排序）。
;;
;; **預設 8 秒是我挑的，不是實測裁決的**（實測只給出上面那張表）。
;; 25 秒預算 ＋ 8 秒精修 = 33 秒，要改就動這個常數或 cfg 的 'refbudget。
(setq FLR:REFINEBUDGET 8000.0)

;; 每軸掃幾點；回 0 ＝ 這張圖不精修。
;;
;; 上限 240 是給小圖用的煞車：每組 1 ms 時預算會算出四千點，那個解析度（0.008）
;; 遠低於任何有意義的尺寸差異，純粹是空轉。
;;
;; 【下限 8 會撐破預算，所以要有出口】2026-08-14 的規模量測抓到：
;; 「多房 7×7」每組 890 ms，下限 8 點 × 2 軸 = 14.2 秒，而預算是 8 秒——
;; **下限直接把預算的承諾蓋掉了**，而且圖越大超得越多（每組 3 秒就變 48 秒）。
;; 預算的意思是「我最多花這麼久」，被一個下限無聲地推翻就不再是承諾。
;; 故：連下限都要超過預算兩倍時，乾脆不精修。
;;   兩倍是我挑的（實測只給出「會超」這件事）：完全不許超的話，
;;   每組稍微變慢就整個關掉，而實測顯示 8 點在高頂點的圖上仍然改善得到
;;   （鋸齒 40 齒、每軸 8 點：有改善）。留一倍的餘裕換那個機會。
(defun FLR:RefineN (cfg per / bud)
  (setq bud (FLR:Cfg 'refbudget cfg))
  (if (null bud) (setq bud FLR:REFINEBUDGET))
  (cond
    ((not (and per (> per 0))) 32)
    ;; 下限 8 點的成本 = 2×8×per，允許它最多到預算的兩倍
    ((> (* 16.0 (float per)) (* 2.0 bud)) 0)
    (T (max 8 (min 240 (FLR:Floor (/ bud (* 2.0 (float per)))))))))

;; 第 k 個取樣點的位置，單位是「磚距的比例」，0-based。
;;
;; 【為什麼不是 k × (1/npts)】那樣算出來的取樣點集合**不巢狀**：31 點與 63 點的
;; 格點除了 0 以外沒有任何共同點，於是「加預算」等於**換一組取樣點**而不是加密
;; ——實測抓到 4 秒的對稱誤差（10.732）比 8 秒（10.795）好、16 秒的廢料違規（48）
;; 比 32 秒（53）好。精修是確定性的，那不是雜訊。
;; 使用者踩到的症狀是「多給一倍時間，結果反而變差」，而且完全查不到原因。
;;
;; 改成固定的**階層順序**：先 8 等分的 8 個點，之後每一層補上一層的中點。
;;   層 0：0/8 1/8 … 7/8            （索引 0~7）
;;   層 1：1/16 3/16 … 15/16        （索引 8~15）
;;   層 2：1/32 3/32 … 31/32        （索引 16~31）
;; 取前 n 個必然**包含**取前 m 個（m<n），所以「預算越多結果不會更差」
;; 由建構保證，不是靠運氣。而且順序固定＝總點數不變，**成本完全相同**。
(defun FLR:RefineFrac (k / st)
  (if (< k 8)
    (/ (float k) 8.0)
    (progn
      ;; st = 不大於 k 的最大「8 的 2 冪倍」＝本層的起始索引；分母為 2×st
      (setq st 8)
      (while (<= (* st 2) k) (setq st (* st 2)))
      (/ (+ (* 2.0 (- k st)) 1.0) (* 2.0 st)))))

;; 沿 axis 掃過整個磚距（另一軸固定在 best 的值），回傳這條線上最好的一組。
;; 取樣點含 0.0、不含 pitch——offset 是模磚距的，兩端是同一個點。
(defun FLR:RefineAxis (cfg regions rbboxes bbox best goal axis npts
                       / pitch i v c2 r out vb ax)
  (setq pitch (+ (FLR:Cfg (if (= axis 0) 'tw 'th) cfg) (FLR:Cfg 'gap cfg))
        out   best
        vb    (FLR:Cfg 'verbose cfg)
        ax    (if (= axis 0) "x" "y")
        i     0)
  (while (< i npts)
    (setq v  (* pitch (FLR:RefineFrac i))
          c2 (subst (cons 'ox (if (= axis 0) v (cdr (assoc 'ox best))))
                    (assoc 'ox cfg) cfg)
          c2 (subst (cons 'oy (if (= axis 0) (cdr (assoc 'oy best)) v))
                    (assoc 'oy c2) c2)
          r  (FLR:Evaluate c2 regions rbboxes bbox))
    (if (FLR:BetterG r out goal) (setq out r))
    (setq i (1+ i))
    ;; 【1.4.7】沒有進度的話，畫面會停在「評估起鋪方案 N/N」不動好幾秒
    ;; ——精修**設計上就要吃掉整份預算**（預設 8 秒），使用者回報「卡住」。
    ;; 這正是 v1.2.4 用進度行治好的「大平面無聲跑數十秒，看起來像當掉」，
    ;; 只是換到計數器滿了之後發生，那條進度行救不到它。
    ;; 尾巴的空白是要蓋掉「評估起鋪方案 700/700 」——同一行被 \r 覆寫，
    ;; 新字串比舊的短的話，舊字尾會留在畫面上。
    (if vb (princ (strcat "\r[精修] 沿 " ax " 軸 " (itoa i) "/" (itoa npts)
                          "          "))))
  out)

;; 先 x 後 y。第二軸掃的是**穿過第一軸精修結果**的那條線，不是原點那條
;; ——否則兩軸各自的改善會互相抵銷掉一個。
(defun FLR:Refine (cfg regions rbboxes bbox best goal npts / r)
  (setq r (FLR:RefineAxis cfg regions rbboxes bbox best goal 0 npts))
  (FLR:RefineAxis cfg regions rbboxes bbox r goal 1 npts))

(defun FLR:Optimize (cfg regions rbboxes bbox topn / cx cy c2 res all sorted i out
                                                    vb tot done pairs sl lows nb goal
                                                    r0 r1 rn)
  ;; 【1.4】目標決定三件事：前篩要用哪個排序、精算時要不要順便排料、
  ;; 以及最後怎麼比大小。三者必須是同一個 goal，否則會出現「照 A 挑進來、
  ;; 照 B 排名次」這種看起來只是名次怪怪的錯。
  ;; 矩形分解與起鋪點無關，在這裡算一次就好——放在 FLR:Evaluate 裡的話
  ;; 每組候選都會重算一次（702 組 × 7 區）。
  (setq cfg  (FLR:WithRects (FLR:GoalCfg cfg) regions)
        goal (FLR:GoalOf (FLR:Cfg 'goal cfg)))
  (setq cx (FLR:Candidates regions bbox cfg 0)
        cy (FLR:Candidates regions bbox cfg 1)
        vb (FLR:Cfg 'verbose cfg))
  ;; 前篩：能用就只精算前幾名。0.4 起斜邊界也照跑（shortlist 加寬），
  ;; 只有交丁比例怪到算不出有限的列偏移類別時才退回全算。
  (setq sl (FLR:PreRank regions bbox cfg cx cy
                        (FLR:ShortN topn (* (length cx) (length cy))
                                    (FLR:IsRect regions (FLR:AxisTol cfg)) cfg)))
  (if sl
    (setq pairs sl)
    (progn
      (setq pairs '())
      (foreach ox cx (foreach oy cy (setq pairs (cons (list ox oy) pairs))))
      (setq pairs (reverse pairs))))
  (if (and vb sl)
    (princ (strcat "\n[前篩] " (itoa (* (length cx) (length cy)))
                   " 組候選 → 精算前 " (itoa (length pairs)) " 組。")))
  (setq tot (length pairs) done 0 all '())
  (foreach pr pairs
    (setq c2 (subst (cons 'ox (car pr))  (assoc 'ox cfg) cfg))
    (setq c2 (subst (cons 'oy (cadr pr)) (assoc 'oy c2) c2))
    (setq res (FLR:Evaluate c2 regions rbboxes bbox))
    (setq all (cons res all) done (1+ done))
    ;; 大平面一組要好幾秒，沒有進度會被當成當掉
    (if vb (princ (strcat "\r[計算] 評估起鋪方案 " (itoa done) "/" (itoa tot) " "))))
  ;; 插入排序取前 topn
  (setq sorted '())
  (foreach r all
    (setq sorted (FLR:InsertG r sorted goal)))
  (setq out '() i 0)
  (foreach r sorted
    (if (< i topn) (setq out (cons r out) i (1+ i))))
  (setq out (reverse out))
  ;; ---- 精修第一名（見上方 FLR:Refine 的說明）----
  ;; 【一定要在附掛 [低違規] 之前做】那一段用 out 裡最低的違規數當門檻，
  ;; 而精修可能把第 1 名的違規壓下去（實測廢料目標 64 → 49）。順序顛倒的話，
  ;; 門檻會用精修前的數字算，附上來的「低違規」方案可能其實比第 1 名還差。
  ;;
  ;; 'refine 明確給 nil 才關閉——測試要固定住結果時用得到。
  (setq rn (if out (FLR:RefineN cfg (FLR:Cfg 'perms cfg)) 0))
  (if (and out (> rn 0)
           (not (and (assoc 'refine cfg) (null (FLR:Cfg 'refine cfg)))))
    (progn
      (setq r0 (car out)
            r1 (FLR:Refine cfg regions rbboxes bbox r0 goal rn))
      (if (FLR:BetterG r1 r0 goal)
        (progn
          (setq out (cons r1 (cdr out)))
          (if vb
            (princ (strcat "\n[精修] 第 1 名沿兩軸各掃 " (itoa rn) " 點 → 起鋪點 ("
                           (rtos (cdr (assoc 'ox r1)) 2 2) ", "
                           (rtos (cdr (assoc 'oy r1)) 2 2) ")　刀 "
                           (itoa (cdr (assoc 'cutsizes r1))) "／違規 "
                           (itoa (cdr (assoc 'bad r1))) "（原 "
                           (itoa (cdr (assoc 'cutsizes r0))) "／"
                           (itoa (cdr (assoc 'bad r0))) "）"))))
        (if vb
          (princ (strcat "\n[精修] 沿兩軸各掃 " (itoa rn)
                         " 點，沒有比候選集內更好的。")))))
    ;; 略過也要講。使用者才知道「這張圖沒有精修過」，而不是以為精修了但沒效果
    (if (and vb out (= rn 0))
      (princ (strcat "\n[精修] 略過：這張圖每組 "
                     (rtos (FLR:Cfg 'perms cfg) 2 0)
                     " ms，連最少的 8 點也要 "
                     (rtos (/ (* 16.0 (FLR:Cfg 'perms cfg)) 1000.0) 2 0)
                     " 秒，超過精修預算的兩倍。"))))
  ;; 附掛低違規方案（0.5）
  (setq nb nil)
  (foreach r out
    (if (or (null nb) (< (cdr (assoc 'bad r)) nb)) (setq nb (cdr (assoc 'bad r)))))
  (setq lows (vl-sort all
               '(lambda (a b)
                  (if (= (cdr (assoc 'bad a)) (cdr (assoc 'bad b)))
                    (< (cdr (assoc 'cutsizes a)) (cdr (assoc 'cutsizes b)))
                    (< (cdr (assoc 'bad a)) (cdr (assoc 'bad b))))))
        i 0)
  ;; nb 固定為「清單內最低的違規數」，不隨附掛更新——否則第一筆附上去之後
  ;; 門檻就被拉到全域最低，後面永遠進不來，等於只附得到一個。
  (foreach r lows
    (if (and (< i FLR:EXTRABAD) nb (< (cdr (assoc 'bad r)) nb)
             (not (vl-some '(lambda (u)
                              (and (equal (cdr (assoc 'ox u)) (cdr (assoc 'ox r)) 1e-9)
                                   (equal (cdr (assoc 'oy u)) (cdr (assoc 'oy r)) 1e-9)))
                           out)))
      (setq out (append out (list (append r (list (cons 'why 'lowbad)))))
            i   (1+ i))))
  out)

(defun FLR:InsertG (item lst goal / out done)
  (setq out '() done nil)
  (foreach x lst
    (if (and (not done) (FLR:BetterG item x goal))
      (setq out (cons x (cons item out)) done T)
      (setq out (cons x out))))
  (if done (reverse out) (reverse (cons item out))))

(defun FLR:Insert (item lst) (FLR:InsertG item lst 'cuts))

;;; ================================================================
;;;  十二、邊料互補排料（叫料量實算）
;;; ================================================================
;;
;; 【0.2 新增】0.1 版的叫料是「總片數 ×(1+耗損率)」，
;;   **每片裁切磚都當成消耗一整片母磚**。但工地實際上是：
;;   左邊要 12 cm 的條、右邊要 18 cm 的條，同一片 30 cm 磚切一刀就出兩條。
;;   工具手上已經有每片的精確裁切尺寸，沒有理由用猜的。
;;
;; 分類（一律取碎片的 x 向條帶分解，與裁切尺寸同源）：
;;   單一條帶且長 ≈ 磚高 → 全高條，只吃寬度 → 併入「寬度」那組一維排料
;;   單一條帶且寬 ≈ 磚寬 → 全寬條，只吃高度 → 併入「高度」那組
;;   單一條帶兩向都不滿   → 角料，先切一條寬 w 的全高條再修高
;;                          → 也併入「寬度」組（保守：修掉的高度算廢料）
;;   多條帶（L 形等）     → 保守起見各吃一整片母磚
;;
;; 保守方向一致：**只會少估節省，不會少叫料**。

;; 一維裝箱（FFD）：回傳所需母磚數
(defun FLR:Pack (sizes cap kerf / cnt bins rem any c)
  (setq cnt '())
  (foreach s sizes
    (setq c (assoc s cnt))
    (if c
      (setq cnt (subst (cons s (1+ (cdr c))) c cnt))
      (setq cnt (cons (cons s 1) cnt))))
  (setq cnt (vl-sort cnt '(lambda (a b) (> (car a) (car b)))) bins 0)
  (while (vl-some '(lambda (x) (> (cdr x) 0)) cnt)
    (setq bins (1+ bins) rem cap any T)
    (while any
      (setq any nil)
      ;; 每次挑「放得下的最大件」＝ FFD
      (foreach p cnt
        (if (and (not any) (> (cdr p) 0) (<= (car p) (+ rem 1e-9)))
          (setq rem (- rem (car p) kerf)
                cnt (subst (cons (car p) (1- (cdr p))) p cnt)
                any T)))))
  bins)

;; 回傳 ((whole . 整磚) (xbins . 寬向母磚) (ybins . 高向母磚) (lwhole . L形吃掉的)
;;       (need . 實需母磚) (used . 鋪設淨面積) (scrap . 廢料面積) (rate . 廢料率))
(defun FLR:Nest (layout cfg / tw th kerf q whole lwhole xs ys
                              res k area sizes grp s2 need nx ny)
  (setq tw (FLR:Cfg 'tw cfg) th (FLR:Cfg 'th cfg)
        kerf (FLR:Cfg 'kerf cfg) q (FLR:Cfg 'sizeq cfg)
        whole 0 lwhole 0 xs '() ys '() area 0.0)
  (if (null kerf) (setq kerf 0.0))
  ;; 先取得全域分群，排料用的尺寸要跟表上列的一致（師傅照表切）
  (setq sizes '())
  (foreach it layout
    (foreach c (cdr (assoc 'cuts (cdr it)))
      (setq s2 (FLR:Round c 0.001))
      (if (not (member s2 sizes)) (setq sizes (cons s2 sizes)))))
  (setq grp (FLR:GroupSizes sizes q))
  (foreach it layout
    (setq res (cdr it) k (cdr (assoc 'kind res))
          area (+ area (cdr (assoc 'area res))))
    (if (= k 'full)
      (setq whole (1+ whole))
      ;; 一片磚可能被牆切成多個碎片，逐碎片各自歸類。
      ;; 【1.4】歸類本身移到 FLR:NestClass，由 FLR:ClassifyTile 在分解條帶時
      ;; 順手算好放進 'nest——這裡不再重跑一次 FLR:Strips，而且「排序時用的
      ;; 母磚數」與「統計表上的叫料量」保證同一套邏輯（原本是兩份程式碼）。
      (foreach nc (cdr (assoc 'nest res))
        (cond
          ((eq nc 'lwhole) (setq lwhole (1+ lwhole)))
          ;; 全寬條：只吃高度
          ((= (car nc) 1)  (setq ys (cons (FLR:RepOf (cdr nc) grp) ys)))
          ;; 其餘（全高條與角料）：吃寬度
          (T               (setq xs (cons (FLR:RepOf (cdr nc) grp) xs)))))))
  (setq nx (FLR:Pack xs tw kerf) ny (FLR:Pack ys th kerf)
        need (+ whole lwhole nx ny))
  (list (cons 'whole  whole)
        (cons 'xbins  nx)
        (cons 'ybins  ny)
        (cons 'lwhole lwhole)
        (cons 'need   need)
        (cons 'used   area)
        (cons 'scrap  (- (* need tw th) area))
        (cons 'rate   (if (> need 0)
                        (- 1.0 (/ area (* need tw th)))
                        0.0))))

;;; ================================================================
;;;  十三、切割清單（1.4.4）
;;; ================================================================
;;
;; 統計表原本只列「刀數（相異裁切尺寸）6 種：12.1 15.15 18.2 …」——那是**種類**，
;; 不是數量。師傅到現場要問的是「12.1 那一種要切幾片」，種類答不了。
;;
;; 【一列＝一種形狀，不是一條條帶】
;; 一個**碎片**就是師傅要切的一片。L 形是一片切兩刀，不是兩片——
;; 照條帶列會把它算成兩片，叫料與工時全部高估。
;;
;; 【為什麼列碎片而不是「裁切磚數」】
;; 一片磚被牆切成兩塊時，統計表的「裁切」只計一次（跨區磚在分區小計各計一次，
;; 見 README），但現場要切的是**兩片**。切割清單的單位必須是碎片。
;; 故本函式的片數總和 >= 統計表的裁切磚數，兩者不衝突、量的是不同東西。
;;
;; 【尺寸一律取分群後的代表值】
;; 與 FLR:Nest／統計表同一套分群（FLR:GroupSizes），因為師傅是**照表切**的：
;; 表上沒有的尺寸不該出現在清單裡。滿格的那一維維持磚的尺寸不進分群
;; （它不是裁切尺寸，FLR:StripCutsOf 本來就不收它）。
;;
;; 回傳 ((簽章 片數 面積 違規) ...)，依片數多寡排序；
;;   簽章 = ((寬 長) ...)，一條條帶一組，已取代表值
;;   違規 = T/nil，**由代表值判定**（照表切出來的尺寸違不違規），
;;          故同一列的每一片必然同號，不會出現「這一列有幾片違規」這種答不出來的欄位
;;
;; 【面積是逐片累加的，順序不同時最後幾個位元會不同】（實測 1458.0 vs
;; 1458.0000000000002）。兩條來源的碎片順序本來就不一樣，所以匯出端一律
;; 四捨五入到兩位小數再寫，讓這個差永遠浮不上來——不然同一張圖匯出兩次
;; 會 diff 出假變動。斷言 §10g 對面積用容差比，對尺寸與片數則要求完全相同。

;; 一個維度取代表值：滿格的那一維維持磚的尺寸，其餘取分群代表值
(defun FLR:CutRep (v full grp tol)
  (if (>= v (- full tol)) full (FLR:RepOf (FLR:Round v 0.001) grp)))

;; 兩個簽章誰排前面。**必須是全序**，這不是美觀問題：
;; 同片數的兩列若比不出大小，`vl-sort` 就依輸入順序決定，而輸入順序在
;; 「跑完即匯出」與「框選圖面讀回」兩條路上不一樣（後者由 ssget 決定）
;; ——同一張圖匯出兩次得到不同的 CSV，diff 起來全是假變動。
;; 初版只比第一條帶的寬，實測 (20.3 15.2) 與 (20.3 16.4) 互換，由斷言抓到。
;; 攤平後逐項比，前綴相同則條帶少者在前。
(defun FLR:SigLess (p r / a b done out)
  (setq a (apply 'append p) b (apply 'append r) done nil out nil)
  (while (and (not done) a b)
    (cond ((< (car a) (- (car b) 1e-9)) (setq out T   done T))
          ((> (car a) (+ (car b) 1e-9)) (setq out nil done T))
          (T (setq a (cdr a) b (cdr b)))))
  (if done out (< (length p) (length r))))

;; 這個簽章照表切出來會不會低於下限。判準與 FLR:LegScan 的 axis-0 那半完全相同。
(defun FLR:SigBad (sig tw th minr tol / bad)
  (setq bad nil)
  (foreach s sig
    (if (or (< (car s)  (- (* tw minr) tol))
            (< (cadr s) (- (* th minr) tol)))
      (setq bad T)))
  bad)

;; 【核心】吃一串**碎片多邊形**就好，不必是 layout。
;; 這樣拆是因為切割清單有兩個來源，而它們必須數出同一份答案：
;;   ① 這一次執行的 layout（FLR:CutList）
;;   ② 從圖面框選讀回來的多段線（分區鋪磚時要整層樓一份，見 FLR_FloorTile.lsp）
;; 分群刻意**從條帶自己導出**，不吃 layout 的 'cuts——②那條路沒有 'cuts。
;; 兩者等價：'cuts 本來就是「小於磚尺寸的那些條帶維度」（見 FLR:StripCutsOf）。
(defun FLR:CutListOf (frags cfg / tw th tol q minr sts sizes s2 grp out st sig hit a)
  (setq tw   (FLR:Cfg 'tw cfg)   th  (FLR:Cfg 'th cfg)
        tol  (FLR:Cfg 'tol cfg)  q   (FLR:Cfg 'sizeq cfg)
        minr (FLR:Cfg 'mincut cfg))
  (if (null tol) (setq tol 1e-6))
  ;; 第一趟：條帶只分解一次，收集起來供分群與計數共用
  (setq sts '() sizes '())
  (foreach f frags
    (setq st (FLR:Strips (car f) 0 tol))
    (if st
      (progn
        (setq sts (cons (list st (cadr f)) sts))
        ;; 與 FLR:StripCutsOf 同一組判準：小於磚尺寸的那一維才是裁切尺寸
        (foreach s st
          (if (< (car s) (- tw tol))
            (progn (setq s2 (FLR:Round (car s) 0.001))
                   (if (not (member s2 sizes)) (setq sizes (cons s2 sizes)))))
          (if (< (cadr s) (- th tol))
            (progn (setq s2 (FLR:Round (cadr s) 0.001))
                   (if (not (member s2 sizes)) (setq sizes (cons s2 sizes)))))))))
  (setq sts (reverse sts) grp (FLR:GroupSizes sizes q) out '())
  ;; 第二趟：取代表值、歸類、計數
  (foreach r sts
    (setq st  (car r)
          sig (mapcar '(lambda (s)
                         (list (FLR:CutRep (car s)  tw grp tol)
                               (FLR:CutRep (cadr s) th grp tol)))
                      st)
          a   (cadr r)
          hit nil)
    (foreach o out
      (if (and (null hit) (equal (car o) sig 1e-9)) (setq hit o)))
    (if hit
      (setq out (subst (list (car hit) (1+ (nth 1 hit)) (+ (nth 2 hit) a) (nth 3 hit))
                       hit out))
      (setq out (cons (list sig 1 a (FLR:SigBad sig tw th minr tol)) out))))
  ;; 片數多者在前（師傅先切量最大的那種），同數量時依簽章排
  (vl-sort out
    '(lambda (p r)
       (cond ((> (nth 1 p) (nth 1 r)) T)
             ((< (nth 1 p) (nth 1 r)) nil)
             (T (FLR:SigLess (car p) (car r)))))))

;; layout → 碎片清單 → 切割清單。整磚不進清單（它不必切）
(defun FLR:CutList (layout cfg / frags res k)
  (setq frags '())
  (foreach it layout
    (setq res (cdr it) k (cdr (assoc 'kind res)))
    (if (and (/= k 'full) (/= k 'none))
      (foreach pt (cdr (assoc 'parts res))
        ;; 與量測走同一支 FLR:Strips——清單上的尺寸與圖上標註的必然一致
        (setq frags (cons (list (nth 1 pt) (nth 2 pt)) frags)))))
  (FLR:CutListOf (reverse frags) cfg))

(princ "\nFLR_Core 0.9 已載入（純邏輯，不含 AutoCAD API）。")
(princ)
