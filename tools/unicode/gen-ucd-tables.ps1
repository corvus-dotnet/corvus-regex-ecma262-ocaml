<#
.SYNOPSIS
    Writes src/ucd_tables.ml, the Unicode data of the engine.

.DESCRIPTION
    The engine takes every Unicode property it reads from the tables this script writes, and nothing from the
    standard library or the platform, so that its answers do not depend on what built it.

    The data comes from tables_gen.rs of the corvus-regex-ecma262 Rust crate, which holds the same Unicode data as
    every other engine. Each table there is written here under the same name in lower case (GC_LU is gc_lu), with the
    same inclusive ranges: those up to U+FFFF in r16 and the rest in r32, each a flat array of bounds. The crate writes
    a table once and names it wherever it is used again, so this script does too. Each lookup by name is an OCaml match
    over strings, as the crate's is.

.PARAMETER CrateTables
    The path of corvus-regex-ecma262/src/tables_gen.rs in a checkout of the Rust crate.

.PARAMETER Output
    The file to write. The default is src/ucd_tables.ml of this repository.

.PARAMETER Check
    Compares what would be written with the file in place and writes nothing. The script fails when they differ, so
    CI can tell that the tables are the crate's.

.EXAMPLE
    pwsh tools/unicode/gen-ucd-tables.ps1 -CrateTables ../corvus-regex-ecma262-rs/corvus-regex-ecma262/src/tables_gen.rs
#>
param(
    [Parameter(Mandatory = $true)][string]$CrateTables,
    [string]$Output = (Join-Path (Join-Path (Split-Path (Split-Path $PSScriptRoot -Parent) -Parent) 'src') 'ucd_tables.ml'),
    [switch]$Check
)

$ErrorActionPreference = 'Stop'
$source = [System.IO.File]::ReadAllText((Resolve-Path $CrateTables))

function Find([string]$pattern, [string]$what) {
    $m = [regex]::Match($source, $pattern, 'Singleline')
    if (-not $m.Success) { throw "tables_gen.rs has no $what. The generator needs updating for the crate's file." }
    return $m
}

function Get-Numbers([string]$text) {
    return , @([regex]::Matches($text, '0x[0-9A-Fa-f]+|\d+') | ForEach-Object { [Convert]::ToInt64($_.Value, $(if ($_.Value.StartsWith('0x')) { 16 } else { 10 })) })
}

$version = (Find 'pub\(crate\) const VERSION: &str = "([^"]+)";' 'VERSION').Groups[1].Value

$out = [System.Text.StringBuilder]::new()
function Add-Line([string]$line = '') { [void]$out.Append($line).Append("`n") }

# Writes items separated by the separator, in rows no longer than a line of the package may be.
function Add-Wrapped([string]$indent, [string[]]$items, [string]$separator) {
    $row = [System.Text.StringBuilder]::new($indent)
    for ($k = 0; $k -lt $items.Count; $k++) {
        $item = $items[$k]
        if ($k -lt $items.Count - 1) { $item += $separator }
        if ($row.Length -gt $indent.Length -and $row.Length + 1 + $item.Length -gt 118) {
            Add-Line $row.ToString().TrimEnd()
            $row = [System.Text.StringBuilder]::new($indent)
        }
        if ($row.Length -gt $indent.Length) { [void]$row.Append(' ') }
        [void]$row.Append($item)
    }
    if ($row.Length -gt $indent.Length) { Add-Line $row.ToString().TrimEnd() }
}

function Add-Bounds([string]$field, $values, [bool]$last) {
    $end = if ($last) { '' } else { ';' }
    if ($values.Count -eq 0) { Add-Line "    $field = [||]$end"; return }
    Add-Line "    $field ="
    Add-Line '      [|'
    Add-Wrapped '        ' @($values | ForEach-Object { '0x{0:X}' -f $_ }) ';'
    Add-Line "      |]$end"
}

# The tables, in the crate's order.
$order = [System.Collections.Generic.List[string]]::new()
$body = [System.Text.StringBuilder]::new()
$header = $out
$out = $body
foreach ($m in [regex]::Matches($source, 'pub\(super\) static (\w+): Table = Table \{(.*?)\n\};', 'Singleline')) {
    $name = $m.Groups[1].Value.ToLowerInvariant()
    if ($order.Contains($name)) { throw "Two tables would be named $name." }
    $r16 = [regex]::Match($m.Groups[2].Value, 'r16: &\[(.*?)\]', 'Singleline')
    $r32 = [regex]::Match($m.Groups[2].Value, 'r32: &\[(.*?)\]', 'Singleline')
    $low = if ($r16.Success) { Get-Numbers $r16.Groups[1].Value } else { @() }
    $high = if ($r32.Success) { Get-Numbers $r32.Groups[1].Value } else { @() }
    if (($low.Count % 2) -ne 0 -or ($high.Count % 2) -ne 0) { throw "Table $name has an odd number of bounds." }
    $order.Add($name)
    Add-Line "let $name ="
    Add-Line '  {'
    Add-Bounds 'r16' $low $false
    Add-Bounds 'r32' $high $true
    Add-Line '  }'
    Add-Line
}

function Add-CaseTable([string]$name, [string]$rustName, [string]$comment) {
    $m = Find ("pub\(super\) static " + $rustName + ": &\[CaseRange\] = &\[(.*?)\n\];") $rustName
    $rows = @([regex]::Matches($m.Groups[1].Value, 'CaseRange \{ first: (-?\w+), delta: (-?\w+), length: (\w+), mask: (\w+) \}'))
    Add-Line $comment
    Add-Line "let $name ="
    Add-Line '  [|'
    $items = foreach ($row in $rows) {
        $first = [Convert]::ToInt32($row.Groups[1].Value, 16)
        '0x{0:X}; {1}; {2}; {3}' -f $first, $row.Groups[2].Value, $row.Groups[3].Value, $row.Groups[4].Value
    }
    foreach ($k in 0..($items.Count - 1)) {
        $end = if ($k -eq $items.Count - 1) { '' } else { ';' }
        Add-Line ('    ' + $items[$k] + $end)
    }
    Add-Line '  |]'
    Add-Line
}

Add-CaseTable 'case_folds' 'CASE_FOLDS' "(* The simple case folding (the C and S rows of CaseFolding.txt), four numbers a row: the first code point of a range,
   the amount to add, the length of the range, and a mask. The amount applies to the code points whose offset in the
   range has none of the bits of the mask. *)"
Add-CaseTable 'upper_cases' 'UPPER_CASES' '(* The simple uppercase mapping, in the form of case_folds. *)'

# Writes the lookup of a table by name, from a match of the crate.
function Add-Lookup([string]$rustFn, [string]$comment) {
    $m = Find ("pub\(super\) fn " + $rustFn + "\(name: &str\) -> Option<&'static Table> \{\s*match name \{(.*?)\n\s*_ => None,") $rustFn
    Add-Line $comment
    Add-Line "let $rustFn = function"
    foreach ($arm in [regex]::Matches($m.Groups[1].Value, '((?:"[^"]*"\s*\|?\s*)+)=> Some\(&(\w+)\)')) {
        $target = $arm.Groups[2].Value.ToLowerInvariant()
        if (-not $order.Contains($target)) { throw "$rustFn names $($arm.Groups[2].Value), which is not a table." }
        $names = @([regex]::Matches($arm.Groups[1].Value, '"([^"]*)"') | ForEach-Object { '"' + $_.Groups[1].Value + '"' })
        Add-Line ('  | ' + ($names -join ' | ') + " -> Some $target")
    }
    Add-Line '  | _ -> None'
    Add-Line
}

Add-Lookup 'category_table' '(* The table of each General_Category value that is not a group, by any of its names. *)'
Add-Lookup 'binary_table' '(* The table of each binary property, by its canonical name. *)'
Add-Lookup 'script_table' "(* The table of each Script value other than Unknown, by its long name and by each alias (the ISO 15924 code, for
   example). *)"
Add-Lookup 'script_extensions_table' "(* The table of each Script_Extensions value other than Unknown. It is the table of the Script value when the two have
   the same ranges. *)"

$names = (Find 'pub\(super\) static SCRIPT_NAMES: &\[&\[&str\]\] = &\[(.*?)\n\];' 'SCRIPT_NAMES').Groups[1].Value
$rows = foreach ($row in [regex]::Matches($names, '&\[([^\]]*)\]')) {
    $n = @([regex]::Matches($row.Groups[1].Value, '"([^"]*)"') | ForEach-Object { '"' + $_.Groups[1].Value + '"' })
    '[| ' + ($n -join '; ') + ' |]'
}
Add-Line '(* Every name of every script, the long name of each followed by its aliases. *)'
Add-Line 'let script_names ='
Add-Line '  [|'
Add-Wrapped '    ' @($rows) ';'
Add-Line '  |]'
Add-Line
$binary = @([regex]::Matches((Find 'pub\(super\) static BINARY_NAMES: &\[&str\] = &\[(.*?)\n\];' 'BINARY_NAMES').Groups[1].Value, '"([^"]*)"') |
        ForEach-Object { '"' + $_.Groups[1].Value + '"' })
Add-Line '(* The canonical name of every binary property that has a table. *)'
Add-Line 'let binary_names ='
Add-Line '  [|'
Add-Wrapped '    ' $binary ';'
Add-Line '  |]'
$lookups = $body.ToString()

$out = $header
Add-Line "(* Code generated by tools/unicode/gen-ucd-tables.ps1 from tables_gen.rs of the corvus-regex-ecma262 Rust crate, which"
Add-Line "   holds the Unicode $version property tables of every engine. DO NOT EDIT. *)"
Add-Line
Add-Line '(* The version of Unicode the tables hold. *)'
Add-Line "let version = `"$version`""
Add-Line
Add-Line '(* A set of code points as sorted, inclusive ranges that neither overlap nor touch: r16 the bounds of the ranges that'
Add-Line '   end at or below U+FFFF, and r32 those of the ranges that start above it, two numbers a range. *)'
Add-Line 'type table = { r16 : int array; r32 : int array }'
Add-Line
[void]$out.Append($lookups)

$text = $out.ToString().TrimEnd("`n") + "`n"
foreach ($line in $text -split "`n") {
    if ($line.Length -gt 120) { throw "A line of the output is longer than 120 characters: $line" }
}
if ($Check) {
    $current = if (Test-Path $Output) { [System.IO.File]::ReadAllText($Output) } else { '' }
    if ($current -ne $text) { throw "$Output is not what $CrateTables generates. Run tools/unicode/gen-ucd-tables.ps1." }
    Write-Host "$Output is current ($($order.Count) tables)."
    return
}
[System.IO.File]::WriteAllText($Output, $text, [System.Text.UTF8Encoding]::new($false))
Write-Host "Wrote $Output with $($order.Count) tables, Unicode $version."
