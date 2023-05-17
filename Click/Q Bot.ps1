Clear-Host
Echo "Toggling Q..."  
$WShell = New-Object -com "Wscript.Shell" 
while ($true) { 
$WShell.sendkeys("{q}") 
Start-Sleep -Milliseconds 200   
$WShell.sendkeys("{q}") 
Start-Sleep -Seconds 120 
}