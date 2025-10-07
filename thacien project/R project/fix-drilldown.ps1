# (paste the script body here)
# fix-drilldown.ps1
# Backup app copy.r, register plotly_click for drilldown plot, and harden the click observer.

$path = 'C:\Users\harag\OneDrive\Desktop\HACKTON\thacien project\thacien project\R project\app copy.r'
$bak  = "$path.bak"

# 1) Backup
Copy-Item -Path $path -Destination $bak -Force
Write-Host "Backup created at: $bak"

# 2) Load file
$text = Get-Content -Path $path -Raw

# 3) Insert %>% event_register("plotly_click") after the onRender(...) that follows source = "drilldown"
$pos = $text.IndexOf('source = "drilldown"')
if ($pos -ge 0) {
    $sub = $text.Substring($pos)
    $onRenderRegex = [regex] '(?s)onRender\(".*?"\)\s*'    # (?s) makes . match newlines
    $m = $onRenderRegex.Match($sub)
    if ($m.Success) {
        $replacement = $m.Value + ' %>% event_register("plotly_click")'
        $sub2 = $onRenderRegex.Replace($sub, $replacement, 1)
        $text = $text.Substring(0, $pos) + $sub2
        Write-Host "Inserted %>% event_register('plotly_click') after onRender(...) following source = 'drilldown'."
    } else {
        Write-Warning "No onRender(...) found after source = 'drilldown'. Skipping event_register insertion."
    }
} else {
    Write-Warning "No 'source = \"drilldown\"' found in file. Skipping event_register insertion."
}

# 4) Replace unsafe observeEvent(...) for plotly_click (source = "drilldown") with a safer observer
$observeRegex = [regex] '(?s)observeEvent\s*\(\s*event_data\(\s*"(?:plotly_click)"\s*,\s*source\s*=\s*"(?:drilldown)"\s*\)\s*,\s*\{.*?\}\s*\)\s*'
if ($observeRegex.IsMatch($text)) {
    $newObserver = @'
observeEvent(event_data("plotly_click", source = "drilldown"), {
  d <- event_data("plotly_click", source = "drilldown")
  req(d)
  clicked_district <- d$x
  drilldown_click_data(clicked_district)
})
'@
    $text = $observeRegex.Replace($text, [System.Text.RegularExpressions.MatchEvaluator]{ param($m) $newObserver }, 1)
    Write-Host "Replaced observeEvent(... event_data('plotly_click', source = 'drilldown') ...) with a safer observer."
} else {
    Write-Warning "Could not find observeEvent(... event_data('plotly_click', source = 'drilldown') ...). No observer replacement made."
}

# 5) Save patched file
Set-Content -Path $path -Value $text -Force
Write-Host "Patched file saved to $path"
