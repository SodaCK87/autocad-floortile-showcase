# -*- coding: utf-8 -*-
"""AutoLISP 原始碼的靜態檢查——CI 上跑得動的那一部分。

**這不是測試套件。** 596 條核心斷言要 `accoreconsole`（AutoCAD 的無視窗核心）才跑得起來，
GitHub 的 runner 上沒有這個東西，而拿別的 Lisp 直譯器去跑會得到隔一層的證據：
語意只要差一點，綠燈就是假的。所以這支只檢查「在 runner 上真的驗得到」的三件：

1. **編碼**：`.lsp` 必須是 UTF-8 **無 BOM**。AutoCAD 的 `load` 會把 BOM 當成
   檔案的第一個字元，症狀是「載入失敗」而完全指不到編碼。
2. **括號平衡**：一個檔案漏一個 `)`，`load` 會安靜地讀完整份卻少定義最後幾支函式。
3. **控制字元**：原始碼裡不可以有 tab 以外的控制字元。

括號計數要略過註解與字串，否則 `"(未閉合"` 這種完全合法的字面值會被算進去。

    python tools/check_lsp.py src/FLR_Core.lsp tests/FLR_Tests.lsp
"""
import sys
from pathlib import Path

# ;| ... |; 是 AutoLISP 的區塊註解，可巢狀。
BLOCK_OPEN, BLOCK_CLOSE = ";|", "|;"


def scan(text):
    """回傳 (括號淨值, 最深負值出現的位置 or None)。負值＝右括號多過左括號。"""
    depth = min_depth_at = 0
    first_negative = None
    i, n = 0, len(text)
    line = 1
    in_string = False
    block = 0

    while i < n:
        ch = text[i]
        if ch == "\n":
            line += 1
            i += 1
            continue

        if in_string:
            if ch == "\\" and i + 1 < n:
                i += 2                      # \" \\ \n 一律整組跳過
                continue
            if ch == '"':
                in_string = False
            i += 1
            continue

        if block:
            if text.startswith(BLOCK_CLOSE, i):
                block -= 1
                i += 2
                continue
            if text.startswith(BLOCK_OPEN, i):
                block += 1
                i += 2
                continue
            i += 1
            continue

        if text.startswith(BLOCK_OPEN, i):
            block += 1
            i += 2
            continue
        if ch == ";":                        # 行註解吃到行尾
            j = text.find("\n", i)
            i = n if j < 0 else j
            continue
        if ch == '"':
            in_string = True
            i += 1
            continue

        if ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
            if depth < 0 and first_negative is None:
                first_negative = line
        i += 1

    if in_string:
        return depth, first_negative, "字串沒有結尾的雙引號"
    if block:
        return depth, first_negative, "區塊註解 ;| 沒有對應的 |;"
    return depth, first_negative, None


def check(path):
    problems = []
    raw = path.read_bytes()

    if raw[:3] == b"\xef\xbb\xbf":
        problems.append("有 UTF-8 BOM——AutoCAD 的 load 會把它當成第一個字元")
    try:
        text = raw.decode("utf-8")
    except UnicodeDecodeError as exc:
        problems.append(f"不是合法的 UTF-8：{exc}")
        return problems

    for no, ln in enumerate(text.splitlines(), 1):
        bad = [c for c in ln if ord(c) < 32 and c != "\t"]
        if bad:
            problems.append(f"第 {no} 行有控制字元 {[hex(ord(c)) for c in bad]}")

    depth, first_negative, fatal = scan(text)
    if fatal:
        problems.append(fatal)
    if first_negative is not None:
        problems.append(f"第 {first_negative} 行的右括號多過左括號")
    if depth != 0:
        problems.append(f"括號不平衡：淨值 {depth:+d}（正＝少了右括號）")

    return problems


def main(argv):
    targets = [Path(a) for a in argv[1:]]
    if not targets:
        root = Path(__file__).resolve().parent.parent
        targets = sorted(root.glob("src/*.lsp")) + sorted(root.glob("tests/*.lsp"))
    if not targets:
        print("沒有找到 .lsp")
        return 1

    failed = 0
    for path in targets:
        problems = check(path)
        lines = len(path.read_bytes().splitlines())
        if problems:
            failed += 1
            print(f"FAIL  {path}  （{lines} 行）")
            for p in problems:
                print(f"        {p}")
        else:
            print(f"PASS  {path}  （{lines} 行，UTF-8 無 BOM、括號平衡）")

    print(f"\n{len(targets) - failed}/{len(targets)} PASS")
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
