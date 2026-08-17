# 一維下料（cutting stock）的 LP 下界 vs 現行 FFD
#
# LP = Gilmore-Gomory 延遲行生成：
#   主問題 min 1'x  s.t. A x >= d, x >= 0   （A 的每一行 = 一種切割樣式）
#   定價   用對偶價當價值跑無界背包，找得到 reduced cost < 0 的樣式就加進來
# 下界關係： 材料下界 <= LP <= OPT <= FFD
# 若 ceil(LP) == FFD，FFD 就是最佳解（沒得再省），這條路可以劃掉。
import math, sys
from collections import defaultdict
import numpy as np
from scipy.optimize import linprog

SCALE = 20  # 尺寸都是 0.05 的倍數


def read(path):
    cases, cur = [], None
    tile = kerf = None
    for ln in open(path, encoding='utf-8', errors='replace'):
        t = ln.split()
        if not t or t[0].startswith('#'):
            continue
        if t[0] == 'TILE':
            tile = (float(t[1]), float(t[2])); kerf = float(t[3])
        elif t[0] == 'ORIGIN':
            cur = {'origin': (float(t[1]), float(t[2])), 'X': [], 'Y': []}
            cases.append(cur)
        elif t[0] == 'WHOLE':
            cur['whole'], cur['lwhole'] = int(t[1]), int(t[2])
        elif t[0] in ('X', 'Y'):
            cur[t[0]].append((float(t[1]), int(t[2])))
        elif t[0] == 'FFD':
            cur['ffd'] = (int(t[1]), int(t[2]), int(t[3]))
    return tile, kerf, cases


def knapsack(cap, sizes, vals):
    """無界背包：回傳 (最大價值, 每種件數)。cap/sizes 皆為整數。"""
    best = [0.0] * (cap + 1)
    pick = [-1] * (cap + 1)
    for c in range(1, cap + 1):
        best[c] = best[c - 1]
        pick[c] = -1
        for i, s in enumerate(sizes):
            if s <= c and best[c - s] + vals[i] > best[c] + 1e-12:
                best[c] = best[c - s] + vals[i]
                pick[c] = i
    cnt = [0] * len(sizes)
    c = cap
    while c > 0:
        # pick[c] < 0 代表「容量 c 的最佳解沒用到剛好卡在 c 的那一刀」，
        # 要退到 c-1 繼續回溯。直接停手會回傳空樣式，
        # 而空樣式對 LP 一點幫助也沒有 → 下一輪對偶價不變 → 誤判成收斂。
        i = pick[c]
        if i < 0:
            c -= 1
        else:
            cnt[i] += 1
            c -= sizes[i]
    return best[cap], cnt


def solve(items, cap):
    """items = [(size, demand)]，cap = 母磚可用長度。回傳 (LP 值, 材料下界)"""
    sizes = [int(round(s * SCALE)) for s, _ in items]
    dem = [n for _, n in items]
    icap = int(round(cap * SCALE))
    material = sum(s * n for s, n in zip(sizes, dem)) / icap

    # 起始樣式：每種尺寸各自單獨切滿一片
    pats = []
    for i, s in enumerate(sizes):
        col = [0] * len(sizes); col[i] = icap // s
        pats.append(col)

    it = 0
    for it in range(1, 501):
        A = np.array(pats, dtype=float).T           # 列=尺寸, 行=樣式
        res = linprog(c=np.ones(len(pats)), A_ub=-A, b_ub=-np.array(dem, dtype=float),
                      bounds=(0, None), method='highs')
        assert res.status == 0, res.message
        duals = -res.ineqlin.marginals              # >= 0
        val, cnt = knapsack(icap, sizes, list(duals))
        if val <= 1.0 + 1e-9:
            break
        if cnt in pats:                             # 收斂但浮點雜訊讓 val 略大於 1
            break
        pats.append(cnt)
    return res.fun, material, it


def main(path):
    tile, kerf, cases = read(path)
    print(f'母磚 {tile[0]}x{tile[1]}，鋸縫 {kerf}\n')
    for c in cases:
        ox, oy = c['origin']
        need_ffd, xb, yb = c['ffd']
        print(f'=== 起鋪點 ({ox:g}, {oy:g}) ===')
        print(f'  整磚 {c["whole"]}　L形吃整片 {c["lwhole"]}　FFD 實需 {need_ffd}')
        tot = 0
        for ax, cap in (('X', tile[0]), ('Y', tile[1])):
            items = c[ax]
            npieces = sum(n for _, n in items)
            lp, mat, it = solve(items, cap)
            ffd = xb if ax == 'X' else yb
            # LP 是鬆弛下界，比可行解還大就是算錯了——寧可炸掉也不要印出一個
            # 看起來很專業、方向卻相反的數字給人拿去下判斷
            assert lp <= ffd + 1e-6, f'LP {lp} > FFD {ffd}：欄生成有錯'
            assert lp >= mat - 1e-6, f'LP {lp} < 材料下界 {mat}：主問題有錯'
            tot += math.ceil(lp - 1e-9)
            print(f'  {ax}：{npieces} 條 / {len(items)} 種　材料下界 {mat:8.2f}　'
                  f'LP {lp:8.2f}（{it} 輪）　→ 下界 {math.ceil(lp - 1e-9):4d}　'
                  f'FFD {ffd:4d}　差 {ffd - math.ceil(lp - 1e-9)}')
        best = c['whole'] + c['lwhole'] + tot
        print(f'  實需下界 {best}　vs FFD {need_ffd}　→ 最多還能省 {need_ffd - best} 片'
              f'（{100.0 * (need_ffd - best) / need_ffd:.2f}%）\n')


if __name__ == '__main__':
    main(sys.argv[1])
