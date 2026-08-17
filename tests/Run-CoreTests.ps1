#Requires -Version 5.1
<#
    核心斷言執行器（節錄版：只載入 src\FLR_Core.lsp 與 tests\FLR_Tests.lsp）

    完整專案的執行器另外還跑 UI 層斷言、bundle 版本一致性與安裝腳本斷言，
    那些都需要不在這個 repo 裡的檔案。這一支只跑核心，跑得完就代表
    「核心真的不依賴 AutoCAD API」——不是宣稱，是這支腳本自己證明的。

    為什麼要有這支腳本，而不是直接 accoreconsole /s：
    accoreconsole 的 /s 參數與 .scr 檔內容**都以 ANSI(CP950) 解析**，
    資料夾名稱只要含非 ASCII 字元就會回「找不到檔案」或「載入失敗」。
    所以先把 .lsp 複製到純 ASCII 的暫存路徑再跑。

    用法：  .\Run-CoreTests.ps1  [-Acad <accoreconsole.exe 路徑>]
#>
[CmdletBinding()]
param(
    [string] $Acad
)

$ErrorActionPreference = 'Stop'

# ---------------------------------------------------------------- 找 accoreconsole
# 不硬寫版本號：換一版 AutoCAD 就得改腳本，而且錯誤訊息只會說「找不到」。
if (-not $Acad) {
    $Acad = Get-ChildItem 'C:\Program Files\Autodesk' -Directory -Filter 'AutoCAD *' -ErrorAction SilentlyContinue |
            Sort-Object Name -Descending |
            ForEach-Object { Join-Path $_.FullName 'accoreconsole.exe' } |
            Where-Object { Test-Path $_ } |
            Select-Object -First 1
}
if (-not $Acad -or -not (Test-Path $Acad)) {
    throw "找不到 accoreconsole.exe。請用 -Acad 指定完整路徑。"
}
Write-Host "使用 $Acad" -ForegroundColor DarkGray

# ---------------------------------------------------------------- 備妥 ASCII 暫存區
$Root  = Split-Path -Parent (Split-Path -Parent $MyInvocation.MyCommand.Path)
$Stage = Join-Path $env:TEMP 'flr_showcase_test'
if (-not (Test-Path $Stage)) { New-Item -ItemType Directory -Path $Stage | Out-Null }
Copy-Item (Join-Path $Root 'src\FLR_Core.lsp')    $Stage -Force
Copy-Item (Join-Path $Root 'tests\FLR_Tests.lsp') $Stage -Force

function L($n) { '(load "{0}")' -f ((Join-Path $Stage $n) -replace '\\', '/') }

# .scr 的內容必須純 ASCII（含路徑），故用暫存區的路徑
$scr = Join-Path $Stage 'run.scr'
@(
    '(setvar "SECURELOAD" 0)'
    (L 'FLR_Core.lsp')
    (L 'FLR_Tests.lsp')
    'FLRTEST'
    '(setvar "SECURELOAD" 1)'
    '(princ (strcat "RESULT SECURELOAD=" (itoa (getvar "SECURELOAD"))))'
) | Out-File -FilePath $scr -Encoding ascii

# ---------------------------------------------------------------- 執行
# stdout 一定要以 UTF-16LE 解碼。舊寫法是把 NUL 位元組濾掉，ASCII 剛好還原得回來
# 但**中文全成亂碼**——而斷言名稱正是中文，於是一旦 FAIL 就看不出是哪一條
# （實測輸出長這樣：`[FAIL] TextW c`）。最需要訊息的時刻剛好讀不到，等於沒有訊息。
Write-Host "執行核心斷言..." -ForegroundColor Cyan

$psi = New-Object System.Diagnostics.ProcessStartInfo
$psi.FileName              = $Acad
$psi.Arguments             = '/s "{0}"' -f $scr
$psi.UseShellExecute       = $false
$psi.CreateNoWindow        = $true
$psi.RedirectStandardOutput = $true
$psi.StandardOutputEncoding = [System.Text.Encoding]::Unicode

$proc = [System.Diagnostics.Process]::Start($psi)
# 先 ReadToEnd 再 WaitForExit：只有一條被導向的串流，不會互鎖
$out = $proc.StandardOutput.ReadToEnd()
$proc.WaitForExit()

# ---------------------------------------------------------------- 判讀
($out -split "`r?`n") |
    Where-Object { $_ -match 'FAIL|PASS \d|INFO|====|RESULT' } |
    ForEach-Object {
        $t = ($_ -replace '  +', ' ').Trim()
        if ($t -match '\[FAIL\]') { Write-Host $t -ForegroundColor Red } else { Write-Host $t }
    }

$pass = 0; $fail = 0
foreach ($m in [regex]::Matches($out, 'PASS (\d+)\s+FAIL (\d+)')) {
    $pass += [int]$m.Groups[1].Value; $fail += [int]$m.Groups[2].Value
}

# SECURELOAD 會外溢到 GUI 的 AutoCAD（accoreconsole 與它共用設定檔），
# 所以要**斷言**它回到 1，不能只印出來讓人自己看——沒斷言等於沒檢查。
if ($out -notmatch 'RESULT SECURELOAD=1') {
    Write-Host "`n[警告] SECURELOAD 未回到 1，正在補救..." -ForegroundColor Yellow
    $fix = Join-Path $Stage 'fix.scr'
    '(setvar "SECURELOAD" 1)' | Out-File -FilePath $fix -Encoding ascii
    & $Acad /s $fix | Out-Null
    Write-Host "[警告] 已送出還原指令，請自行確認 AutoCAD 的 SECURELOAD。" -ForegroundColor Yellow
    $fail++
}

if (($pass + $fail) -eq 0) {
    Write-Host "`n無法解析測試結果，請檢查輸出。" -ForegroundColor Yellow
    exit 1
}
Write-Host ("`n合計  PASS {0}  FAIL {1}" -f $pass, $fail) -ForegroundColor ($(if ($fail) { 'Red' } else { 'Green' }))
exit $(if ($fail) { 1 } else { 0 })
